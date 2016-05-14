#include <string>
#include <cstdio>
#include <fstream>

#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/IR/Mangler.h"
#include "llvm/Support/DynamicLibrary.h"

extern "C" {
    void test_proc() {
        printf("test_proc runs!\n");
    }
}

namespace llvm { namespace orc {
class KaleidoscopeJIT {
public:
  typedef ObjectLinkingLayer<> ObjLayerT;
  typedef IRCompileLayer<ObjLayerT> CompileLayerT;
  typedef CompileLayerT::ModuleSetHandleT ModuleHandleT;

  KaleidoscopeJIT(llvm::TargetMachine* targetMachine)
      : DL(targetMachine->createDataLayout()), CompileLayer(ObjectLayer, SimpleCompiler(*targetMachine))
  {
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
  }

  ModuleHandleT addModule(std::unique_ptr<Module> M) {
    // We need a memory manager to allocate memory and resolve symbols for this
    // new module. Create one that resolves symbols by looking back into the
    // JIT.
    auto Resolver = createLambdaResolver(
        [&](const std::string &Name) {
            printf("Resolving: %s\n", Name.c_str());
          if (auto Sym = findMangledSymbol(Name))
            return RuntimeDyld::SymbolInfo(Sym.getAddress(), Sym.getFlags());
          return RuntimeDyld::SymbolInfo(nullptr);
        },
        [](const std::string &S) { return nullptr; });
    auto H = CompileLayer.addModuleSet(singletonSet(std::move(M)),
                                       make_unique<SectionMemoryManager>(),
                                       std::move(Resolver));

    ModuleHandles.push_back(H);
    return H;
  }

  void removeModule(ModuleHandleT H) {
    ModuleHandles.erase(
        std::find(ModuleHandles.begin(), ModuleHandles.end(), H));
    CompileLayer.removeModuleSet(H);
  }

  JITSymbol findSymbol(const std::string Name) {
    return findMangledSymbol(mangle(Name));
  }

private:

  std::string mangle(const std::string &Name) {
    std::string MangledName;
    {
      raw_string_ostream MangledNameStream(MangledName);
      Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
    }
    return MangledName;
  }

  template <typename T> static std::vector<T> singletonSet(T t) {
    std::vector<T> Vec;
    Vec.push_back(std::move(t));
    return Vec;
  }

  JITSymbol findMangledSymbol(const std::string &Name) {
    // Search modules in reverse order: from last added to first added.
    // This is the opposite of the usual search order for dlsym, but makes more
    // sense in a REPL where we want to bind to the newest available definition.
    for (auto H : make_range(ModuleHandles.rbegin(), ModuleHandles.rend()))
      if (auto Sym = CompileLayer.findSymbolIn(H, Name, true))
        return Sym;

    // If we can't find the symbol in the JIT, try looking in the host process.
    if (auto SymAddr = RTDyldMemoryManager::getSymbolAddressInProcess(Name))
      return JITSymbol(SymAddr, JITSymbolFlags::Exported);

    return nullptr;
  }

  const DataLayout DL;
  ObjLayerT ObjectLayer;
  CompileLayerT CompileLayer;
  std::vector<ModuleHandleT> ModuleHandles;
};
}} // llvm.orc


static std::unique_ptr<llvm::TargetMachine> TheTargetMachine;
static std::unique_ptr<llvm::Module> TheModule;
static llvm::IRBuilder<> Builder(llvm::getGlobalContext());
static std::unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;

void InitializeModuleAndPassManager(void) {
  // Open a new module.
  TheModule = llvm::make_unique<llvm::Module>("JIT module", llvm::getGlobalContext());
  TheModule->setDataLayout(TheTargetMachine->createDataLayout());

  // Create a new pass manager attached to it.
  TheFPM = llvm::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());

  // Provide basic AliasAnalysis support for GVN.
  //TheFPM->add(llvm::orc::createBasicAliasAnalysisPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  TheFPM->add(llvm::createInstructionCombiningPass());
  // Reassociate expressions.
  TheFPM->add(llvm::createReassociatePass());
  // Eliminate Common SubExpressions.
  TheFPM->add(llvm::createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM->add(llvm::createCFGSimplificationPass());

  TheFPM->doInitialization();
}

int main(int argc, char** argv) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    std::ifstream f("first.l");
    assert(f);

    std::string line;
    while(std::getline(f, line)) {
        std::printf("wop\n");
    }

    TheTargetMachine.reset(llvm::EngineBuilder().selectTarget());

    TheModule = llvm::make_unique<llvm::Module>("first module", llvm::getGlobalContext());
    std::unique_ptr<llvm::orc::KaleidoscopeJIT> TheJIT = llvm::make_unique<llvm::orc::KaleidoscopeJIT>(TheTargetMachine.get());

    InitializeModuleAndPassManager();


    llvm::Function *test_proc;
    {
        // declare void test_proc() -> void
        llvm::Type *retType = llvm::Type::getVoidTy(llvm::getGlobalContext()); // void
        std::vector<llvm::Type *> argTypes; // no args
        llvm::FunctionType *FT = llvm::FunctionType::get(retType, argTypes, false); // fn () -> void
        test_proc = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "test_proc", TheModule.get()); // fn test_proc() -> void
    }

    // Create a function!
    llvm::Type *retType = llvm::Type::getInt32Ty(llvm::getGlobalContext()); // int32
    std::vector<llvm::Type *> argTypes; // no args
    llvm::FunctionType *FT = llvm::FunctionType::get(retType, argTypes, false); // fn () -> int32
    llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", TheModule.get()); // fn main() -> int32

    // Create a basic block for function body.
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", F);
    Builder.SetInsertPoint(BB);
    
    // Call test_proc
    {
        std::vector<llvm::Value *> noargs;
        Builder.CreateCall(test_proc, noargs);
    }

    // Return 42
    llvm::Value *retVal = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()), 42, false);
    Builder.CreateRet(retVal);

    TheFPM->run(*F);
    llvm::verifyFunction(*F); // not sure what it does
    TheModule->dump(); // print llvm ir to stdout

    // JIT!
    auto H = TheJIT->addModule(std::move(TheModule));
    auto ExprSymbol = TheJIT->findSymbol("main");
    assert(ExprSymbol && "Function not found");

    int (*mainFunc)() = (int (*)())(intptr_t)ExprSymbol.getAddress();
    printf("Main returned: %d", mainFunc());
}
