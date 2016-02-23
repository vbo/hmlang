#include "hmlang.h"

#include "llvm/Pass.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Support/TargetSelect.h"

#include "llvm/IRReader/IRReader.h" // TODO: check this out for loading compiled llvm modules

using namespace llvm;

struct CodeGenState {
    LLVMContext ctx;
    TargetMachine *target_machine;
    IRBuilder<> *ir_builder;
    // TODO: one module for everything?
    Module *module;
    Type* builtin_types[Builtin::TypeLast];
    AttributeSet default_func_attr;
};

void init_builtins(CodeGenState *code_gen) {
    code_gen->builtin_types[Builtin::Void] = Type::getVoidTy(code_gen->ctx);
    code_gen->builtin_types[Builtin::I8] = Type::getInt8Ty(code_gen->ctx);
    code_gen->builtin_types[Builtin::I32] = Type::getInt32Ty(code_gen->ctx);
    code_gen->builtin_types[Builtin::Float32] = Type::getFloatTy(code_gen->ctx);

    {
        // TODO: read doxygen, not sure how it works
        SmallVector<AttributeSet, 4> Attrs;
        AttributeSet PAS;
        {
            AttrBuilder B;
            B.addAttribute(Attribute::NoUnwind);
            PAS = AttributeSet::get(code_gen->ctx, ~0U, B);
        }

        Attrs.push_back(PAS);
        code_gen->default_func_attr = AttributeSet::get(code_gen->ctx, Attrs);
    }
}

Type* get_builtin_type(CodeGenState *code_gen, Builtin::Type type_id) {
    if (type_id < 1 || type_id > Builtin::TypeLast) {
        assert(false && "bad type on code gen");
    }
    return code_gen->builtin_types[type_id];
}

CodeGenState* code_gen_init() {
    InitializeNativeTarget();
    CodeGenState *code_gen = new CodeGenState();
    code_gen->target_machine = EngineBuilder().selectTarget();
    code_gen->ir_builder = new IRBuilder<>(code_gen->ctx);
    code_gen->module = new Module("Module", code_gen->ctx);
    // TODO: Here we can initialize optimization pass manager
    code_gen->module->setDataLayout(code_gen->target_machine->createDataLayout());
    code_gen->module->setTargetTriple(code_gen->target_machine
                                      ->getTargetTriple().normalize());
    init_builtins(code_gen);
    return code_gen;
}

void code_gen_print_result(CodeGenState *code_gen) {
    code_gen->module->dump();
}

void code_gen_output_result(CodeGenState *code_gen, std::string& out_buf) {
    using llvm::legacy::PassManager;
    PassManager pass_manager;
    raw_string_ostream os(out_buf);
    pass_manager.add(createPrintModulePass(os));
    pass_manager.run(*code_gen->module);
}

void code_gen_type_decl(CodeGenState *code_gen, AstNode *node) {
    if (node->code_gen_ref) {
        return; // declaration already generated!
    }
    StructType *struct_type = StructType::create(code_gen->ctx, node->name_tok->str_content);
    node->code_gen_ref = struct_type;
    node->code_gen_done = false;
}

Type* get_type_by_ref(CodeGenState *code_gen, AstNode *node, bool declare_user_types = false) {
    while (true) {
        if (node->type == AstNode::TypeTypeRefName) {
            node = node->resolved_type_ref;
            continue;
        } else if (node->type == AstNode::TypeTypeRefBuiltin) {
            return get_builtin_type(code_gen, node->builtin_type);
        } else if (node->type == AstNode::TypeTypeDefinition) {
            if (declare_user_types) code_gen_type_decl(code_gen, node);
            return (StructType *)node->code_gen_ref;
        } else {
            printf("Code gen panic: unknown type ref type %d\n", node->type);
            assert(false && "unreachable");
        }
    }
}

Value* code_gen_dereference(CodeGenState *code_gen, AstNode *expr) {
    if (expr->type == AstNode::TypeExpressionDereference) {
        Value *base_addr = code_gen_dereference(code_gen, expr->deref_base_var);
        std::vector<Value*> gep_indices;
        gep_indices.push_back(ConstantInt::get(Type::getInt32Ty(code_gen->ctx), 0, false));
        gep_indices.push_back(ConstantInt::get(Type::getInt32Ty(code_gen->ctx),
                                               expr->deref_member->member_index, false));
        Value* gep = code_gen->ir_builder->CreateGEP(base_addr, gep_indices);
        return gep;
    } else if (expr->type == AstNode::TypeExpressionName) {
        Value *base_addr = (Value *)expr->resolved_var->code_gen_ref;
        return base_addr;
    } else {
        printf("Code gen panic: unknown dereference target type %d\n", expr->type);
        assert(false && "unreachable");
        return nullptr;
    }
}

Value* get_value(CodeGenState *code_gen, AstNode *expr) {
    assert(expr && "should be defined for code gen");
    if (expr->type == AstNode::TypeExpressionLiteralNumber) {
        auto tok = expr->expr_literal_number_tok;
        auto type = get_type_by_ref(code_gen, expr->inferred_type_ref);
        if (tok->literal_number_is_float) {
            return ConstantFP::get(type, tok->literal_number_float_value);
        } else {
            bool is_signed = true;
            return ConstantInt::get(type, tok->literal_number_int_value, is_signed);
        }
    } else if (expr->type == AstNode::TypeExpressionName) {
        // TODO: handle void properly
        std::string& name = expr->name_tok->str_content;
        AstNode *var_node = expr->resolved_var;
        if (!var_node) return nullptr; // void
        Value *var_on_stack = (Value *)var_node->code_gen_ref;
        assert(var_on_stack->getType()->isPointerTy());
        bool is_volatile = false;
        Value *var_value = code_gen->ir_builder->CreateLoad(var_on_stack, is_volatile);
        return var_value;
    } else if (expr->type == AstNode::TypeExpressionCall) {
        // assuming func decl is already built
        AstNode *proc = expr->call_proc_def;
        assert(proc && proc->code_gen_ref && "defined for call code gen");
        std::vector<llvm::Value *> call_args;
        call_args.reserve(expr->child_nodes.size());
        for (AstNode *arg_expr : expr->child_nodes) {
            call_args.push_back(get_value(code_gen, arg_expr));
        }
        Value * call_value = code_gen->ir_builder->CreateCall((Function *)proc->code_gen_ref, call_args);
        return call_value;
    } else if (expr->type == AstNode::TypeExpressionDereference) {
        // TODO: what about dereferencing a result of expression?
        Value *addr = code_gen_dereference(code_gen, expr);
        bool is_volatile = false;
        return code_gen->ir_builder->CreateLoad(addr, is_volatile);
    } else if (expr->type == AstNode::TypeExpressionBinOp) {
        assert(expr->bin_op_lexpr && expr->bin_op_rexpr && "defined for code gen");
        assert(expr->builtin_op != Builtin::OpUnknown && "for code gen");
        Value *lhs = get_value(code_gen, expr->bin_op_lexpr);
        Value *rhs = get_value(code_gen, expr->bin_op_rexpr);
        switch (expr->builtin_op) {
            // floating-point operations
            case Builtin::AddFloat:
                return code_gen->ir_builder->CreateFAdd(lhs, rhs);
            case Builtin::SubFloat:
                return code_gen->ir_builder->CreateFSub(lhs, rhs);
            case Builtin::MulFloat:
                return code_gen->ir_builder->CreateFMul(lhs, rhs);
            case Builtin::DivFloat:
                return code_gen->ir_builder->CreateFDiv(lhs, rhs);
            // integer operations
            case Builtin::AddInt:
                return code_gen->ir_builder->CreateAdd(lhs, rhs);
            case Builtin::SubInt:
                return code_gen->ir_builder->CreateSub(lhs, rhs);
            case Builtin::MulInt:
                return code_gen->ir_builder->CreateMul(lhs, rhs);
            // integer signed-unsigned
            case Builtin::DivIntSigned:
                return code_gen->ir_builder->CreateSDiv(lhs, rhs);
            case Builtin::DivIntUnsigned:
                return code_gen->ir_builder->CreateUDiv(lhs, rhs);
            // panic!
            case Builtin::OpUnknown:
            case Builtin::OpLast:
                printf("Code gen panic: don't know how to code gen bad builtin op %d\n",
                       expr->builtin_op);
                assert(false && "unreachable");
        }
        printf("Code gen panic: don't know how to code gen builtin op %d\n", expr->builtin_op);
        assert(false && "unreachable");
    } else {
        printf("Code gen panic: don't know how to code gen expression type %d\n", expr->type);
        assert(false && "unreachable");
    }
}

void code_gen_decl(CodeGenState *code_gen, AstNode *root) {
    for (AstNode* node : root->child_nodes) {
        if (node->type == AstNode::TypeProcedureDefinition) {
            if (node->code_gen_ref) {
                // declaration already generated!
                continue;
            }
            std::string& proc_name = node->name_tok->str_content;
            AstNode *type_ref = node->proc_return_type_ref;
            Type *ret_type = get_type_by_ref(code_gen, type_ref, true);
            std::vector<Type *> arg_types;
            arg_types.reserve(node->child_nodes.size());
            for (AstNode *arg_node : node->child_nodes) {
                AstNode *arg_type_ref = arg_node->var_type_ref;
                Type *arg_type = get_type_by_ref(code_gen, arg_type_ref, true);
                arg_types.push_back(arg_type);
            }
            bool is_var_arg = false;
            auto linkage = node->is_public? Function::ExternalLinkage : Function::PrivateLinkage;
            FunctionType *func_type = FunctionType::get(ret_type, arg_types, is_var_arg);
            std::string& func_name = proc_name; // TODO: namespaces in LLVM IR
            // TODO: nounwind and stuff
            Function *func = llvm::Function::Create(
                func_type, linkage, func_name, code_gen->module);
            if (node->is_public) {
                func->setCallingConv(CallingConv::C);
            }
            node->code_gen_ref = func;
            continue;
        }
        if (node->type == AstNode::TypeTypeDefinition) {
            code_gen_type_decl(code_gen, node);
            if (!node->code_gen_done) {
                StructType *struct_type = (StructType *)node->code_gen_ref;
                std::vector<Type*> member_types;
                member_types.reserve(node->child_nodes.size());
                for (AstNode *member : node->child_nodes) {
                    AstNode *member_type_ref = member->member_type_ref;
                    Type *code_gen_type = get_type_by_ref(code_gen, member_type_ref, true);
                    member_types.push_back(code_gen_type);
                }
                bool is_opaque = false;
                struct_type->setBody(member_types, is_opaque);
                node->code_gen_done = true;
            }
            continue;
        }
    }
}

void code_gen_scope(CodeGenState *code_gen, AstNode *root) {
    code_gen_decl(code_gen, root);
    for (AstNode* node : root->child_nodes) {
        if (node->type == AstNode::TypeProcedureDefinition) {
            code_gen_decl(code_gen, node->proc_body);
            assert(node->code_gen_ref && "set for code gen proc node");
            Function *func = (Function *)node->code_gen_ref;

            func->setAttributes(code_gen->default_func_attr);

            assert(node->proc_body && "code gen does't support external proc yet");
            BasicBlock *bb = BasicBlock::Create(code_gen->ctx, "", func);
            code_gen->ir_builder->SetInsertPoint(bb);

            // set arg names for IR, put them to the stack
            Function::arg_iterator args = func->arg_begin();
            for (AstNode *arg_node : node->child_nodes) {
                Value* arg_value = args++;
                arg_value->setName(arg_node->name_tok->str_content);
                Type *arg_type = arg_value->getType();
                AllocaInst *arg_on_stack = code_gen->ir_builder->CreateAlloca(arg_type);
                arg_node->code_gen_ref = arg_on_stack;
                code_gen->ir_builder->CreateStore(arg_value, arg_on_stack);
            }

            for (AstNode *stmt_node : node->proc_body->child_nodes) {
                if (stmt_node->type == AstNode::TypeStatementReturn) {
                    Value *ret_value = get_value(code_gen, stmt_node->ret_expr);
                    if (ret_value) {
                        code_gen->ir_builder->CreateRet(ret_value);
                    } else {
                        code_gen->ir_builder->CreateRetVoid();
                    }
                } else if (stmt_node->type == AstNode::TypeStatementAssign) {
                    Value *addr = code_gen_dereference(code_gen, stmt_node->assign_lexpr);
                    assert(addr && "for assignment code gen");
                    Value *rvalue = get_value(code_gen, stmt_node->assign_rexpr);
                    code_gen->ir_builder->CreateStore(rvalue, addr);
                } else if (stmt_node->type == AstNode::TypeVariableDeclaration) {
                    AstNode *var_type_ref = stmt_node->var_type_ref;
                    Type *var_type = get_type_by_ref(code_gen, var_type_ref);
                    AllocaInst *var_on_stack = code_gen->ir_builder->CreateAlloca(var_type);
                    stmt_node->code_gen_ref = var_on_stack;
                    if (stmt_node->var_init_expr) {
                        Value *init_value = get_value(code_gen, stmt_node->var_init_expr);
                        code_gen->ir_builder->CreateStore(init_value, var_on_stack);
                    } else {
                        Value *zero_value;
                        if (var_type->isIntegerTy()) {
                            zero_value = ConstantInt::get(var_type, 0);
                        } else if (var_type->isAggregateType()) {
                            zero_value = ConstantAggregateZero::get(var_type);
                        } else if (var_type->isFloatingPointTy()) {
                            zero_value = ConstantFP::get(var_type, 0.0);
                        }
                        code_gen->ir_builder->CreateStore(zero_value, var_on_stack);
                    }
                } else if (stmt_node->type == AstNode::TypeProcedureDefinition) {
                    // skip nested proc definition
                } else if (stmt_node->type == AstNode::TypeTypeDefinition) {
                    // skip nested type definition
                } else {
                    assert(false && "don't know how to code gen a stmt");
                }
            }
            // TODO: does verifier really does things?
            verifyFunction(*func);
            code_gen_scope(code_gen, node->proc_body);
            continue;
        }
        if (node->type == AstNode::TypeTypeDefinition) {
            // Already defined in code_gen_decl
            continue;
        }
    }
}

void code_gen_all(CodeGenState *code_gen, AstNode *root) {
    code_gen_scope(code_gen, root);
    // TODO: find out how to make this work
    // #include "llvm/Analysis/Verifier.h"
    //verifyModule(code_gen->module, PrintMessageAction);
}
