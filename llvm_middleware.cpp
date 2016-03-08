#include "hmlang.h"

#include "llvm/Pass.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/GlobalVariable.h"
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
    code_gen->builtin_types[Builtin::S8] = Type::getInt8Ty(code_gen->ctx);
    code_gen->builtin_types[Builtin::U8] = Type::getInt8Ty(code_gen->ctx);
    code_gen->builtin_types[Builtin::S16] = Type::getInt16Ty(code_gen->ctx);
    code_gen->builtin_types[Builtin::U16] = Type::getInt16Ty(code_gen->ctx);
    code_gen->builtin_types[Builtin::S32] = Type::getInt32Ty(code_gen->ctx);
    code_gen->builtin_types[Builtin::U32] = Type::getInt32Ty(code_gen->ctx);
    code_gen->builtin_types[Builtin::S64] = Type::getInt64Ty(code_gen->ctx);
    code_gen->builtin_types[Builtin::U64] = Type::getInt64Ty(code_gen->ctx);
    code_gen->builtin_types[Builtin::Float32] = Type::getFloatTy(code_gen->ctx);
    code_gen->builtin_types[Builtin::Float64] = Type::getDoubleTy(code_gen->ctx);
    // TODO: is it a good idea to use i1 here?
    code_gen->builtin_types[Builtin::Bool] = Type::getInt1Ty(code_gen->ctx);

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
        assert(false && "bad builtin type on code gen");
        return nullptr;
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

Type* get_type_by_ref(CodeGenState *code_gen, AstNode *node, bool gen_def = true) {
    while (true) {
        if (node->type == AstNode::TypeTypeRefName) {
            node = node->resolved_type_ref;
            continue;
        } else if (node->type == AstNode::TypeTypeRefBuiltin) {
            return get_builtin_type(code_gen, node->builtin_type);
        } else if (node->type == AstNode::TypeTypeRefPointer) {
            Type *pointee_type = get_type_by_ref(code_gen, node->pointee_type_ref);
            if (pointee_type->isVoidTy()) {
                pointee_type = Type::getInt8Ty(code_gen->ctx);
            }
            return PointerType::getUnqual(pointee_type);
        } else if (node->type == AstNode::TypeTypeDefinition) {
            if (!node->code_gen_ref) {
                StructType *struct_type = StructType::create(code_gen->ctx, node->name_tok->str_content);
                node->code_gen_ref = struct_type;
                node->code_gen_done = false;
            }
            if (!node->code_gen_done && gen_def) {
                StructType *struct_type = (StructType *)node->code_gen_ref;
                std::vector<Type*> member_types;
                member_types.reserve(node->child_nodes.size());
                for (AstNode *member : node->child_nodes) {
                    AstNode *member_type_ref = member->member_type_ref;
                    Type *code_gen_type = get_type_by_ref(code_gen, member_type_ref, false);
                    member_types.push_back(code_gen_type);
                }
                bool is_opaque = false;
                struct_type->setBody(member_types, is_opaque);
                node->code_gen_done = true;
            }
            return (StructType *)node->code_gen_ref;
        } else {
            printf("Code gen panic: unknown type ref type %d\n", node->type);
            assert(false && "unreachable");
        }
    }
}

Function *get_func_for_proc(CodeGenState *code_gen, AstNode *proc_node) {
    if (!proc_node->code_gen_ref) {
        std::string& proc_name = proc_node->name_tok->str_content;
        AstNode *type_ref = proc_node->proc_return_type_ref;
        Type *ret_type = get_type_by_ref(code_gen, type_ref);
        std::vector<Type *> arg_types;
        arg_types.reserve(proc_node->child_nodes.size());
        for (AstNode *arg_node : proc_node->child_nodes) {
            AstNode *arg_type_ref = arg_node->var_type_ref;
            Type *arg_type = get_type_by_ref(code_gen, arg_type_ref);
            arg_types.push_back(arg_type);
        }
        bool is_var_arg = false;
        auto linkage = proc_node->is_public? Function::ExternalLinkage : Function::PrivateLinkage;
        FunctionType *func_type = FunctionType::get(ret_type, arg_types, is_var_arg);
        std::string& func_name = proc_name;
        // TODO: Should nounwind and stuff be added here or on the definition?
        Function *func = llvm::Function::Create(
            func_type, linkage, func_name, code_gen->module);
        proc_node->code_gen_ref = func;
    }
    assert(proc_node->code_gen_ref && "set for code gen proc node");
    return (Function *)(proc_node->code_gen_ref);
}

Value* get_value(CodeGenState *code_gen, AstNode *expr); // forward declare for addessof
Value* code_gen_addressof(CodeGenState *code_gen, AstNode *expr) {
    if (expr->type == AstNode::TypeExpressionMemberOf) {
        AstNode *base_expr = expr->memberof_base_var;
        Value *base_addr;
        if (base_expr->inferred_type_ref->type == AstNode::TypeTypeRefPointer) {
            base_addr = get_value(code_gen, base_expr);
        } else {
            base_addr = code_gen_addressof(code_gen, base_expr);
        }
        std::vector<Value*> gep_indices;
        gep_indices.push_back(ConstantInt::get(Type::getInt32Ty(code_gen->ctx), 0, false));
        gep_indices.push_back(ConstantInt::get(Type::getInt32Ty(code_gen->ctx),
                                               expr->memberof_member->member_index, false));
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
        assert(var_node->code_gen_ref && "already filled");
        Value *var_on_stack = (Value *)var_node->code_gen_ref;
        assert(var_on_stack->getType()->isPointerTy());
        bool is_volatile = false;
        Value *var_value = code_gen->ir_builder->CreateLoad(var_on_stack, is_volatile);
        return var_value;
    } else if (expr->type == AstNode::TypeExpressionCall) {
        // assuming func decl is already built
        AstNode *proc = expr->call_proc_def;
        Function *func = get_func_for_proc(code_gen, proc);
        std::vector<llvm::Value *> call_args;
        call_args.reserve(expr->child_nodes.size());
        for (AstNode *arg_expr : expr->child_nodes) {
            call_args.push_back(get_value(code_gen, arg_expr));
        }
        Value * call_value = code_gen->ir_builder->CreateCall(func, call_args);
        return call_value;
    } else if (expr->type == AstNode::TypeExpressionMemberOf) {
        // TODO: what about dereferencing a result of expression?
        Value *addr = code_gen_addressof(code_gen, expr);
        bool is_volatile = false;
        return code_gen->ir_builder->CreateLoad(addr, is_volatile);
    } else if (expr->type == AstNode::TypeExpressionAddressOf) {
        Value *addr = code_gen_addressof(code_gen, expr->addressof_expr);
        return addr;
    } else if (expr->type == AstNode::TypeExpressionDereference) {
        Value *addr = get_value(code_gen, expr->deref_expr);
        bool is_volatile = false;
        Value *deref_value = code_gen->ir_builder->CreateLoad(addr, is_volatile);
        return deref_value;
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
            case Builtin::EqFloat:
                return code_gen->ir_builder->CreateFCmpUEQ(lhs, rhs);
            // integer operations
            case Builtin::AddInt:
                return code_gen->ir_builder->CreateAdd(lhs, rhs);
            case Builtin::SubInt:
                return code_gen->ir_builder->CreateSub(lhs, rhs);
            case Builtin::MulInt:
                return code_gen->ir_builder->CreateMul(lhs, rhs);
            case Builtin::EqInt:
                return code_gen->ir_builder->CreateICmpEQ(lhs, rhs);
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

Constant* get_zero_constant(Type *var_type) {
    if (var_type->isIntegerTy()) {
        return ConstantInt::get(var_type, 0);
    } else if (var_type->isAggregateType()) {
        return ConstantAggregateZero::get(var_type);
    } else if (var_type->isFloatingPointTy()) {
        return ConstantFP::get(var_type, 0.0);
    } else if (var_type->isPointerTy()) {
        return ConstantPointerNull::get((PointerType *)var_type);
    } else {
        assert(false && "can't zero initialize type");
        return nullptr;
    }

}
void code_gen_scope(CodeGenState *code_gen, AstNode *root); // Forward declare for code_gen_stmt
bool code_gen_statement(CodeGenState *code_gen, AstNode *proc_node, AstNode *stmt_node) {
    Function *func = (Function *)proc_node->code_gen_ref;
    assert(func && "func decl already created when coding body");
    if (stmt_node->type == AstNode::TypeStatementReturn) {
        Value *ret_value = get_value(code_gen, stmt_node->ret_expr);
        if (ret_value && !ret_value->getType()->isVoidTy()) {
            code_gen->ir_builder->CreateRet(ret_value);
        } else {
            code_gen->ir_builder->CreateRetVoid();
        }
        return true;
    } else if (stmt_node->type == AstNode::TypeStatementExpr) {
        Value *ignored_value = get_value(code_gen, stmt_node->stmt_expr);
    } else if (stmt_node->type == AstNode::TypeStatementIf) {
        Value *cond_value = get_value(code_gen, stmt_node->if_cond_expr);
        BasicBlock *after_if_bb = BasicBlock::Create(
            code_gen->ctx, "after_if", func);
        BasicBlock *then_bb = BasicBlock::Create(
            code_gen->ctx, "then", func, after_if_bb);
        BasicBlock *else_bb = stmt_node->if_else_stmt ? BasicBlock::Create(
            code_gen->ctx, "else", func, after_if_bb) : after_if_bb;
        code_gen->ir_builder->CreateCondBr(cond_value, then_bb, else_bb);
        code_gen->ir_builder->SetInsertPoint(then_bb);
        bool then_terminates = code_gen_statement(
            code_gen, proc_node, stmt_node->if_then_stmt);
        if (!then_terminates) {
            code_gen->ir_builder->CreateBr(after_if_bb);
        }
        if (stmt_node->if_else_stmt) {
            code_gen->ir_builder->SetInsertPoint(else_bb);
            bool else_terminates = code_gen_statement(
                code_gen, proc_node, stmt_node->if_else_stmt);
            if (!else_terminates) {
                code_gen->ir_builder->CreateBr(after_if_bb);
            }
        }
        code_gen->ir_builder->SetInsertPoint(after_if_bb);
    } else if (stmt_node->type == AstNode::TypeStatementRepeat) {
        BasicBlock *after_repeat_bb = BasicBlock::Create(code_gen->ctx, "after_repeat", func);
        stmt_node->code_gen_ref = after_repeat_bb;
        BasicBlock *repeat_bb = BasicBlock::Create(code_gen->ctx, "repeat", func, after_repeat_bb);
        code_gen->ir_builder->CreateBr(repeat_bb);
        code_gen->ir_builder->SetInsertPoint(repeat_bb);
        bool terminates = code_gen_statement(code_gen, proc_node, stmt_node->repeat_stmt);
        if (!terminates) code_gen->ir_builder->CreateBr(repeat_bb);
        code_gen->ir_builder->SetInsertPoint(after_repeat_bb);
    } else if (stmt_node->type == AstNode::TypeStatementBreak) {
        BasicBlock *break_label = (BasicBlock *)stmt_node->break_loop->code_gen_ref;
        code_gen->ir_builder->CreateBr(break_label);
        return true;
    } else if (stmt_node->type == AstNode::TypeStatementBlock) {
        bool terminated = false;
        for (AstNode *sub_stmt : stmt_node->child_nodes) {
            terminated = code_gen_statement(code_gen, proc_node, sub_stmt);
            if (terminated) break;
        }
        code_gen_scope(code_gen, stmt_node);
        return terminated;
    } else if (stmt_node->type == AstNode::TypeStatementAssign) {
        Value *addr;
        if (stmt_node->assign_lexpr->type == AstNode::TypeExpressionDereference) {
            addr = get_value(code_gen, stmt_node->assign_lexpr->deref_expr);
        } else {
            addr = code_gen_addressof(code_gen, stmt_node->assign_lexpr);
        }
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
            Value *zero_value = get_zero_constant(var_type);
            code_gen->ir_builder->CreateStore(zero_value, var_on_stack);
        }
    } else if (stmt_node->type == AstNode::TypeProcedureDefinition) {
        // skip nested proc definition
    } else if (stmt_node->type == AstNode::TypeTypeDefinition) {
        // skip nested type definition
    } else {
        assert(false && "don't know how to code gen a stmt");
    }
    return false;
}

void code_gen_scope(CodeGenState *code_gen, AstNode *root) {
    for (AstNode* node : root->child_nodes) {
        if (node->type == AstNode::TypeProcedureDefinition) {
            Function *func = get_func_for_proc(code_gen, node);;
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
                bool terminated = code_gen_statement(code_gen, node, stmt_node);
                if (terminated) break;
            }
            // TODO: does verifier really does things?
            verifyFunction(*func);
            code_gen_scope(code_gen, node->proc_body);
            continue;
        }
        if (node->type == AstNode::TypeTypeDefinition) {
            // ignore: coded as we go
            continue;
        }
        if (root->type == AstNode::TypeGlobalScope) {
            // only codegen global variables this way
            if (node->type == AstNode::TypeVariableDeclaration) {
                AstNode *var_type_ref = node->var_type_ref;
                Type *var_type = get_type_by_ref(code_gen, var_type_ref);
                Constant *initializer;
                if (!node->var_init_expr) {
                    initializer = get_zero_constant(var_type);
                } else {
                    assert(node->var_init_expr->type
                           == AstNode::TypeExpressionLiteralNumber);
                    initializer = (Constant *)get_value(code_gen, node->var_init_expr);
                }
                bool is_const = false;
                // TODO: do we need to free this?
                GlobalVariable *var = new GlobalVariable(
                    *code_gen->module,
                    var_type,
                    is_const,
                    GlobalValue::PrivateLinkage,
                    initializer,
                    node->name_tok->str_content
                );
                node->code_gen_ref = var;
                continue;
            }
        }
    }
}

void code_gen_all(CodeGenState *code_gen, AstNode *root) {
    code_gen_scope(code_gen, root);
    // TODO: find out how to make this work
    // #include "llvm/Analysis/Verifier.h"
    //verifyModule(code_gen->module, PrintMessageAction);
}

void code_gen_run_expression(CodeGenState *code_gen, AstNode *expr) {
    // TODO: JIT compile expression (in anon function) and dependencies.
    // Run anon function and return a value returned.
}
