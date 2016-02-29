namespace enrichment {
    using std::string;

    void print_type_ref(AstNode* node) {
        while (true) {
            if (node->type == AstNode::TypeTypeRefName) {
                if (node->resolved_type_ref) {
                    node = node->resolved_type_ref;
                    continue;
                } else {
                    printf("~>%s", node->name_tok->str_content.c_str());
                    break;
                }
            } else if (node->type == AstNode::TypeTypeRefBuiltin) {
                printf("#%s", node->name_tok->str_content.c_str());
                break;
            } else if (node->type == AstNode::TypeTypeRefPointer) {
                print_type_ref(node->pointee_type_ref);
                printf("*");
                break;
            } else if (node->type == AstNode::TypeTypeDefinition) {
                printf("!%s", node->name_tok->str_content.c_str());
                break;
            }
            printf("Don't know how to print type ref %d\n", node->type);
            return;
        }
    }

    void print_ast(AstNode* root) {
        for (AstNode* node : root->child_nodes) {
            if (node->type == AstNode::TypeProcedureDefinition) {
                printf("PROC: %s( ", node->name_tok->str_content.c_str());
                for (AstNode* arg_node : node->child_nodes) {
                    assert(arg_node->type == AstNode::TypeVariableDeclaration);
                    printf("%s: ", arg_node->name_tok->str_content.c_str());
                    print_type_ref(arg_node->var_type_ref);
                    printf(" ");
                }
                printf(") -> ");
                print_type_ref(node->proc_return_type_ref);
                printf("\n");
                continue;
            }
            if (node->type == AstNode::TypeTypeDefinition) {
                printf("TYPE: %s{ ", node->name_tok->str_content.c_str());
                for (AstNode* member_node : node->child_nodes) {
                    assert(member_node->type == AstNode::TypeTypeMember);
                    printf("%s: ", member_node->name_tok->str_content.c_str());
                    print_type_ref(member_node->member_type_ref);
                    printf(" ");
                }
                printf("}\n");
                continue;
            }
            printf("Don't know how to print node %d\n", node->type);
            exit(1);
        }
    }

    AstNode* lookup_type(string& name, AstNode *scope_node) {
        // Check current scope
        // TODO: use child lookup table
        for (AstNode* node : scope_node->child_nodes) {
            if (node->type == AstNode::TypeTypeDefinition || node->type == AstNode::TypeTypeRefBuiltin) {
                if (node->name_tok && node->name_tok->str_content == name) {
                    return node;
                }
            }
        }
        // Check parent scope
        if (scope_node->parent_scope != nullptr) {
            return lookup_type(name, scope_node->parent_scope);
        }
        // Not found
        return nullptr;
    }

    AstNode* lookup_variable(string& name, AstNode *scope_node) {
        int proc_hops = 0;
        while (true) {
            // Check current scope
            // TODO: use child lookup table
            for (AstNode* node : scope_node->child_nodes) {
                if (node->type == AstNode::TypeVariableDeclaration) {
                    if (node->name_tok && node->name_tok->str_content == name) {
                        if (!node->var_decl_enriched) {
                            return nullptr; // is not declared officially yet
                        }
                        if (scope_node->type != AstNode::TypeGlobalScope && proc_hops > 0) {
                            return nullptr;
                        }
                        return node;
                    }
                }
            }
            // Check parent scope
            if (scope_node->parent_scope != nullptr) {
                if (scope_node->type == AstNode::TypeProcedureDefinition) {
                    proc_hops++;
                }
                scope_node = scope_node->parent_scope;
                continue;
            }
            break;
        }
        // Not found
        return nullptr;
    }

    AstNode* lookup_procedure(string& name, AstNode *scope_node) {
        // TODO: copy-pasted from lookup_type
        // Check current scope
        // TODO: use child lookup table
        for (AstNode* node : scope_node->child_nodes) {
            if (node->type == AstNode::TypeProcedureDefinition) {
                if (node->name_tok && node->name_tok->str_content == name) {
                    return node;
                }
            }
        }
        // Check parent scope
        if (scope_node->parent_scope != nullptr) {
            return lookup_procedure(name, scope_node->parent_scope);
        }
        // Not found
        return nullptr;
    }

    // returns zero if types are equal
    int check_resolved_type_refs_equal(AstNode *a, AstNode *b) {
        // TODO: recode this nonsense
        while (true) {
            assert(a && "type for type equality check");
            assert(b && "type for type equality check");
            if (a->type == AstNode::TypeTypeRefName) {
                a = a->resolved_type_ref;
                if (!a) {
                    assert(false && "type equality check is called with unresolved type refs");
                    return 1;
                }
                continue;
            }
            if (b->type == AstNode::TypeTypeRefName) {
                b = b->resolved_type_ref;
                if (!b) {
                    assert(false && "type equality check is called with unresolved type refs");
                    return 1;
                }
                continue;
            }
            if (a->type == AstNode::TypeTypeRefName || b->type == AstNode::TypeTypeRefName) {
                return 1;
            } else if (a->type == AstNode::TypeTypeRefBuiltin) {
                if (b->type != AstNode::TypeTypeRefBuiltin) {
                    return 1;
                }
                if (a->builtin_type != b->builtin_type) {
                    return 1;
                } else {
                    return 0; // equal!
                }
            } else if (a->type == AstNode::TypeTypeRefPointer) {
                if (b->type != AstNode::TypeTypeRefPointer) {
                    return 1;
                }
                return check_resolved_type_refs_equal(
                    a->pointee_type_ref,
                    b->pointee_type_ref
                );
            } else if (a->type == AstNode::TypeTypeDefinition) {
                if (b->type != AstNode::TypeTypeDefinition) {
                    return 1;
                }
                if (a != b) {
                    return 1;
                } else {
                    return 0; // equal!
                }
            } else {
                assert(false && "don't know how to compare types OMG");
                return 1;
            }
        }
    }

    Builtin::Type builtin_type_for_resolved_ref(AstNode *node) {
        while (true) {
            if (node->type == AstNode::TypeTypeRefName) {
                node = node->resolved_type_ref;
                if (!node) {
                    assert(false && "not resolved type ref!");
                    return Builtin::TypeUnknown;
                }
                continue;
            } else if (node->type == AstNode::TypeTypeRefBuiltin) {
                return node->builtin_type;
            } else {
                break;
            }
        }
        return Builtin::TypeUnknown;
    }

    AstNode* user_type_for_resolved_ref(AstNode *node) {
        // TODO: copy-pasted from builtin_type_for_resolved_ref
        while (true) {
            if (node->type == AstNode::TypeTypeRefName) {
                node = node->resolved_type_ref;
                if (!node) {
                    assert(false && "I thought you have a resolved type ref!");
                    return nullptr;
                }
                continue;
            } else if (node->type == AstNode::TypeTypeRefPointer) {
                return user_type_for_resolved_ref(node->pointee_type_ref);
            } else if (node->type == AstNode::TypeTypeDefinition) {
                return node;
            } else if (node->type == AstNode::TypeTypeRefBuiltin) {
                return nullptr;
            } else {
                break;
            }
        }
        printf("Can't find udt for ref type %d\n", node->type);
        assert(false && "I thought you have a resolved type ref!");
        return nullptr;
    }

    Builtin::Op builtin_op_for_tok(Token::Type tok_type, Builtin::Type operand_type) {
        switch (operand_type) {
            case Builtin::Float32:
                switch (tok_type) {
                    case Token::TypeOperatorPlus:
                        return Builtin::AddFloat;
                    case Token::TypeOperatorMinus:
                        return Builtin::SubFloat;
                    case Token::TypeOperatorStar:
                        return Builtin::MulFloat;
                    case Token::TypeOperatorSlash:
                        return Builtin::DivFloat;
                    case Token::TypeOperatorDoubleEquals:
                        return Builtin::EqFloat;
                    default:
                        return Builtin::OpUnknown;
                }
            case Builtin::I32:
            case Builtin::I8:
                switch (tok_type) {
                    case Token::TypeOperatorPlus:
                        return Builtin::AddInt;
                    case Token::TypeOperatorMinus:
                        return Builtin::SubInt;
                    case Token::TypeOperatorStar:
                        return Builtin::MulInt;
                    case Token::TypeOperatorSlash:
                        return Builtin::DivIntSigned;
                    case Token::TypeOperatorDoubleEquals:
                        return Builtin::EqInt;
                    default:
                        return Builtin::OpUnknown;
                }
            default:
                return Builtin::OpUnknown;
        }
    }

    int check_resolved_type_ref_finite(AstNode *node, AstNode *container_type_def);
    int resolve_type_ref(AstNode *node, AstNode *scope_node, bool disallow_void = false) {
        if (node->type == AstNode::TypeTypeRefName) {
            Token* type_name_tok = node->name_tok;
            AstNode *resolved_type_node = lookup_type(
                type_name_tok->str_content, scope_node);
            if (!resolved_type_node) {
                printf("Unresolved type %s on line %d:%d\n",
                       type_name_tok->str_content.c_str(),
                       type_name_tok->line_number,
                       type_name_tok->column_number);
                return 1;
            } else {
                int status = resolve_type_ref(
                    resolved_type_node, scope_node, disallow_void);
                if (status != 0) return status;
                // TODO: that's probably not a good place
                if (disallow_void) {
                    if (resolved_type_node->type == AstNode::TypeTypeRefBuiltin &&
                        resolved_type_node->builtin_type == Builtin::Void) {
                        printf("Can't use void on line %d:%d\n",
                               node->start_tok->line_number,
                               node->start_tok->column_number);
                        return 1;
                    }
                }
                node->resolved_type_ref = resolved_type_node;
            }
            return 0;
        } else if (node->type == AstNode::TypeTypeRefPointer) {
            int status = resolve_type_ref(node->pointee_type_ref, scope_node);
            if (status != 0) return status;
            return 0;
        } else if (node->type == AstNode::TypeTypeRefBuiltin) {
            // already resolved - do nothing
            return 0;
        } else if (node->type == AstNode::TypeTypeDefinition) {
            if (!node->type_def_members_enrichment_done) {
                node->type_def_members_enrichment_done = true;
                int member_index = 0;
                for (AstNode* member_node : node->child_nodes) {
                    assert(member_node->type == AstNode::TypeTypeMember);
                    int resolve_member_status = resolve_type_ref(
                        member_node->member_type_ref, node->parent_scope, true);
                    if (resolve_member_status != 0) return resolve_member_status;
                    int finiteness_check_status = check_resolved_type_ref_finite(
                        member_node->member_type_ref, node);
                    if (finiteness_check_status != 0) {
                        printf("  as a member %s on line %d:%d\n",
                               member_node->name_tok->str_content.c_str(),
                               member_node->start_tok->line_number,
                               member_node->start_tok->column_number);
                        return finiteness_check_status;
                    }
                    member_node->member_index = member_index;
                    member_index++;
                }
            }
            // already resolved - do nothing
            return 0;
        }
        printf("Don't know how to resolve type ref %d\n", node->type);
        assert(false && "unreachable");
        return 1;
    }

    int check_resolved_type_ref_finite(AstNode *node, AstNode *container_type_def) {
        while (true) {
            if (node->type == AstNode::TypeTypeRefName) {
                if (!node->resolved_type_ref) {
                    assert(false && "is it unresolved type?");
                    return 1;
                }
                node = node->resolved_type_ref;
                continue;
            } else if (node->type == AstNode::TypeTypeRefBuiltin) {
                // builtins are always finite
                return 0;
            } else if (node->type == AstNode::TypeTypeRefPointer) {
                // pointers are always finite
                return 0;
            } else if (node->type == AstNode::TypeTypeDefinition) {
                if (node == container_type_def) {
                    printf("Infinite type %s includes itself\n",
                           container_type_def->name_tok->str_content.c_str());
                    return 1;
                } else {
                    for (AstNode *member : node->child_nodes) {
                        int resolve_member_status = resolve_type_ref(
                            member->member_type_ref, node->parent_scope, true);
                        if (resolve_member_status != 0) return resolve_member_status;
                        int member_finiteness_status = check_resolved_type_ref_finite(
                            member->member_type_ref, container_type_def);
                        if (member_finiteness_status != 0) {
                            printf("  when referenced from type %s on line %d:%d\n",
                                   node->name_tok->str_content.c_str(),
                                   member->start_tok->line_number,
                                   member->start_tok->column_number);
                            return member_finiteness_status;
                        }
                    }
                    return 0;
                }
            }
            printf("Don't know how to check finiteness of type ref %d\n", node->type);
            return 1;
        }
        return 563;
    }

    int enrich_expression(AstNode *expr, AstNode *scope);
    int enrich_memberof(AstNode *expr, AstNode *scope) {
        if (expr->type == AstNode::TypeExpressionMemberOf) {
            int status = enrich_memberof(expr->memberof_base_var, scope);
            if (status != 0) return status;
            AstNode *type_ref = expr->memberof_base_var->inferred_type_ref;
            string& member_name = expr->memberof_member_name_tok->str_content;
            // TODO: what about pointers?
            AstNode *udt = user_type_for_resolved_ref(type_ref);
            if (!udt) {
                printf("Can't dereference member %s of a builtin type on line %d:%d\n",
                       member_name.c_str(),
                       expr->start_tok->line_number,
                       expr->start_tok->column_number);
                return 1;
            }
            bool found = false;
            for (AstNode *udt_member : udt->child_nodes) {
                if (udt_member->name_tok->str_content == member_name) {
                    expr->memberof_member = udt_member;
                    int type_status = resolve_type_ref(udt_member->member_type_ref, udt, true);
                    if (type_status != 0) return type_status;
                    expr->inferred_type_ref = udt_member->member_type_ref;
                    found = true;
                }
            }
            if (!found) {
                printf("Can't dereference member %s of type ", member_name.c_str());
                print_type_ref(type_ref);
                printf(" on line %d:%d",
                       expr->start_tok->line_number, expr->start_tok->column_number);
                return 1;
            }
            return 0;
        } else if (expr->type == AstNode::TypeExpressionName) {
            string& name = expr->name_tok->str_content;
            AstNode *variable_node = lookup_variable(name, scope);
            if (variable_node == nullptr) {
                printf("Use of undeclared variable %s on line %d:%d\n",
                       name.c_str(), expr->start_tok->line_number, expr->start_tok->column_number);
                return 1;
            }
            expr->resolved_var = variable_node;
            int type_status = resolve_type_ref(variable_node->var_type_ref, scope, true);
            if (type_status != 0) return type_status;
            expr->inferred_type_ref = variable_node->var_type_ref;
            expr->expr_yields_nontemporary = true;
            assert(variable_node->var_type_ref && "defined for looked up variable");
            return 0;
        } else if (expr->type == AstNode::TypeExpressionDereference) {
            int status = enrich_expression(expr->deref_expr, scope);
            if (status != 0) return status;
            expr->inferred_type_ref = expr->deref_expr->inferred_type_ref->pointee_type_ref;
            return 0;
        } else {
            printf("Can't enrich dereference of type %d\n", expr->type);
            return 1;
        }
    }

    int enrich_expression(AstNode *expr, AstNode *scope) {
        assert(expr && "to enrich");
        if (expr->type == AstNode::TypeExpressionLiteralNumber) {
            AstNode *inferred_type;
            // TODO: literals should actually be loosely typed, right?
            if (expr->expr_literal_number_tok->literal_number_is_float) {
                inferred_type = lookup_type(Builtin::key_float32, scope->parent_scope);
            } else {
                inferred_type = lookup_type(Builtin::key_i32, scope->parent_scope);
            }
            assert(inferred_type && "of a literal number looked up");
            expr->inferred_type_ref = inferred_type;
        } else if (expr->type == AstNode::TypeExpressionName) {
            AstNode *inferred_type;
            string& name = expr->name_tok->str_content;
            if (name == Builtin::key_void) {
                // TODO: handle void properly
                inferred_type = lookup_type(Builtin::key_void, scope->parent_scope);
            } else {
                AstNode *variable_node = lookup_variable(name, scope);
                // TODO: variable type might not be resolved yet when we have real variables
                // do we want to resolve it here or up the stack?
                if (variable_node == nullptr) {
                    printf("Use of undeclared variable %s on line %d:%d\n",
                           name.c_str(),
                           expr->start_tok->line_number,
                           expr->start_tok->column_number);
                    return 1;
                } else {
                    assert(variable_node->var_type_ref && "defined for looked up variable");
                    inferred_type = variable_node->var_type_ref;
                    expr->resolved_var = variable_node;
                    expr->expr_yields_nontemporary = true;
                }
            }
            assert(inferred_type && "of a declared variable");
            expr->inferred_type_ref = inferred_type;
        } else if (expr->type == AstNode::TypeExpressionBinOp) {
            int status_left = enrich_expression(expr->bin_op_lexpr, scope);
            if (status_left != 0) return 1;
            int status_right = enrich_expression(expr->bin_op_rexpr, scope);
            if (status_right != 0) return 1;
            int check_status = check_resolved_type_refs_equal(
                expr->bin_op_lexpr->inferred_type_ref, expr->bin_op_rexpr->inferred_type_ref);
            if (check_status != 0) {
                printf("Trying to apply binary operator %s to operands of different types: ",
                    expr->name_tok->str_content.c_str());
                print_type_ref(expr->bin_op_lexpr->inferred_type_ref);
                printf(" and ");
                print_type_ref(expr->bin_op_rexpr->inferred_type_ref);
                printf(" on line %d:%d", expr->start_tok->line_number,
                       expr->start_tok->column_number);
                return check_status;
            }
            Builtin::Type operand_type = builtin_type_for_resolved_ref(
                expr->bin_op_lexpr->inferred_type_ref);
            if (operand_type == Builtin::TypeUnknown) {
                printf("Trying to apply binary operator %s to operands of user defined type: ",
                    expr->name_tok->str_content.c_str());
                print_type_ref(expr->bin_op_lexpr->inferred_type_ref);
                printf(" on line %d:%d", expr->start_tok->line_number,
                       expr->start_tok->column_number);
                return 1;
            }
            Builtin::Op builtin_op = builtin_op_for_tok(expr->name_tok->type, operand_type);
            if (builtin_op == Builtin::OpUnknown) {
                printf("Can't apply binary operator %s to operands type: ",
                    expr->name_tok->str_content.c_str());
                print_type_ref(expr->bin_op_lexpr->inferred_type_ref);
                printf(" on line %d:%d", expr->start_tok->line_number,
                       expr->start_tok->column_number);
                return 1;
            }

            bool op_returns_bool = false;
            for (int i = 0; i < sizeof(Builtin::ret_bool)/sizeof(Builtin::ret_bool[0]); ++i) {
                if (Builtin::ret_bool[i] == builtin_op) {
                    op_returns_bool = true;
                    break;
                }
            }
            if (op_returns_bool) {
                expr->inferred_type_ref = lookup_type(Builtin::key_bool, scope->parent_scope);
            } else {
                expr->inferred_type_ref = expr->bin_op_lexpr->inferred_type_ref;
            }

            expr->builtin_op = builtin_op;
        } else if (expr->type == AstNode::TypeExpressionCall) {
            string& name = expr->name_tok->str_content;
            // we are likely to call function that wasn't enriched yet by itself
            // we don't need it's body to be correct, but at least arg and ret types are resolved here
            AstNode *proc_def_node = lookup_procedure(name, scope);
            if (proc_def_node == nullptr) {
                printf("Trying to call undeclared procedure %s on line %d:%d\n",
                       name.c_str(), expr->start_tok->line_number, expr->start_tok->column_number);
                return 1;
            }
            expr->call_proc_def = proc_def_node;
            // enrich arg expressions
            for (AstNode *arg_expr : expr->child_nodes) {
                int status = enrich_expression(arg_expr, scope);
                if (status != 0) return 1;
            }
            // TODO: default arguments?
            if (expr->child_nodes.size() != proc_def_node->child_nodes.size()) {
                printf("Procedure %s takes %lu arguments, %lu arguments given for call on line %d:%d",
                       name.c_str(), proc_def_node->child_nodes.size(), expr->child_nodes.size(),
                       expr->start_tok->line_number, expr->start_tok->column_number);
                return 1;
            }
            // make sure argument types make sense, we treat call args and decl args as a parallel
            // array on this point. Wandering how we could impl default arguments.
            for (size_t argi = 0; argi < expr->child_nodes.size(); ++argi) {
                AstNode *call_arg_type = expr->child_nodes[argi]->inferred_type_ref;
                AstNode *decl_arg_type = proc_def_node->child_nodes[argi]->var_type_ref;
                assert(call_arg_type && "was inferred");
                assert(decl_arg_type && "is known");
                int decl_arg_resolve_status = resolve_type_ref(
                    decl_arg_type, proc_def_node->parent_scope, true);
                if (decl_arg_resolve_status != 0) return decl_arg_resolve_status;
                int check_status = check_resolved_type_refs_equal(call_arg_type, decl_arg_type);
                if (check_status != 0) {
                    printf("Type ");
                    print_type_ref(decl_arg_type);
                    printf(" expected, given ");
                    print_type_ref(call_arg_type);
                    printf(" for argument %lu when calling %s on line %d:%d",
                           argi + 1, name.c_str(),
                           expr->start_tok->line_number,
                           expr->start_tok->column_number);
                    return check_status;
                }
            }
            // on this point args are good, infer return type without checks - caller should
            // decide if it's good or not for him to handle.
            expr->inferred_type_ref = proc_def_node->proc_return_type_ref;
            assert(expr->inferred_type_ref && "for call expression");
            // But let's resolve it here just in case =)
            int resolve_ret_status = resolve_type_ref(expr->inferred_type_ref, proc_def_node->parent_scope);
            if (resolve_ret_status != 0) return resolve_ret_status;
        } else if (expr->type == AstNode::TypeExpressionMemberOf) {
            int status = enrich_memberof(expr, scope);
            if (status != 0) return status;
            expr->expr_yields_nontemporary = true;
        } else if (expr->type == AstNode::TypeExpressionAddressOf) {
            int status = enrich_expression(expr->addressof_expr, scope);
            if (status != 0) return status;
            if (expr->addressof_expr->expr_yields_nontemporary) {
                expr->addressof_type_ref->pointee_type_ref
                    = expr->addressof_expr->inferred_type_ref;
                expr->inferred_type_ref = expr->addressof_type_ref;
            } else {
                // This is kinda lame, but to yield a useful address
                // we need to store temporary on the stack on code gen
                // for no good reason.
                printf("Can't take an address of a temporary on line %d:%d\n",
                       expr->start_tok->line_number,
                       expr->start_tok->column_number);
                return 1;
            }
        } else if (expr->type == AstNode::TypeExpressionDereference) {
            int status = enrich_expression(expr->deref_expr, scope);
            if (status != 0) return status;
            if (expr->deref_expr->inferred_type_ref->type != AstNode::TypeTypeRefPointer) {
                printf("Can't dereference a value of non-pointer type on line %d:%d\n",
                       expr->start_tok->line_number,
                       expr->start_tok->column_number);
                return 1;
            }
            expr->inferred_type_ref = expr->deref_expr->
                inferred_type_ref->pointee_type_ref;
        } else {
            printf("Don't know how to enrich expression of type %d on line %d:%d\n",
                   expr->type, expr->start_tok->line_number, expr->start_tok->column_number);
            return 1;
        }
        return 0;
    }

    int enrich_scope(AstNode *root); // forward declare to call from enrich_statement
    int enrich_statement(
        AstNode *proc, AstNode *parent_block,
        AstNode *stmt_node,
        AstNode *loop_node,
        bool& has_ret
    ) {
        if (stmt_node->type == AstNode::TypeStatementReturn) {
            has_ret = true;
            int status = enrich_expression(stmt_node->ret_expr, parent_block);
            if (status != 0) return status;
            // check ret type correct
            int type_cmp_status = check_resolved_type_refs_equal(
                stmt_node->ret_expr->inferred_type_ref,
                proc->proc_return_type_ref
            );
            if (type_cmp_status != 0) {
                printf("Type mismatch: proc %s return type is ",
                       proc->name_tok->str_content.c_str());
                print_type_ref(proc->proc_return_type_ref);
                printf(" but return expression has type ");
                print_type_ref(stmt_node->ret_expr->inferred_type_ref);
                printf(" on line %d:%d", 
                       stmt_node->ret_expr->start_tok->line_number,
                       stmt_node->ret_expr->start_tok->column_number);
                return type_cmp_status;
            }
            return 0;
        } else if (stmt_node->type == AstNode::TypeStatementIf) {
            int cond_status = enrich_expression(
                stmt_node->if_cond_expr, parent_block);
            if (cond_status != 0) return cond_status;

            Builtin::Type cond_type = builtin_type_for_resolved_ref(
                stmt_node->if_cond_expr->inferred_type_ref);
            if (cond_type != Builtin::Bool) {
                printf("if condition should have a bool type given ");
                print_type_ref(stmt_node->if_cond_expr->inferred_type_ref);
                printf(" on line %d:%d", 
                       stmt_node->if_cond_expr->start_tok->line_number,
                       stmt_node->if_cond_expr->start_tok->column_number);
                return 1;
            }

            bool then_has_ret = false;
            int then_status = enrich_statement(proc, parent_block,
                stmt_node->if_then_stmt, loop_node, then_has_ret);
            if (then_status != 0) return then_status;
            bool else_has_ret;
            if (stmt_node->if_else_stmt) {
                int else_status = enrich_statement(proc, parent_block,
                    stmt_node->if_else_stmt, loop_node, else_has_ret);
                if (else_status != 0) return else_status;
            } else {
                else_has_ret = false;
            }
            has_ret = then_has_ret && else_has_ret;
            return 0;
        } else if (stmt_node->type == AstNode::TypeStatementBlock) {
            bool block_has_ret = false;
            for (AstNode *stmt_in_block_node : stmt_node->child_nodes) {
                bool stmt_has_ret = false;
                int status = enrich_statement(
                    proc, stmt_node, stmt_in_block_node, loop_node, stmt_has_ret);
                if (status != 0) return status;
                if (stmt_has_ret) block_has_ret = true;
            }
            has_ret = block_has_ret;
            int status = enrich_scope(stmt_node);
            if (status != 0) return status;
            return 0;
        } else if (stmt_node->type == AstNode::TypeStatementRepeat) {
            bool block_has_ret = false;
            int status = enrich_statement(proc, parent_block,
                stmt_node->repeat_stmt, stmt_node, block_has_ret);
            if (status != 0) return status;
            has_ret = false; // TODO: hast_ret for repeat block - break etc
            return 0;
        } else if (stmt_node->type == AstNode::TypeStatementBreak) {
            if (!loop_node) {
                printf("Break is only allowed inside loop on line %d:%d\n",
                       stmt_node->start_tok->line_number,
                       stmt_node->start_tok->column_number);
                return 1;
            }
            stmt_node->break_loop = loop_node;
            return 0;
        } else if (stmt_node->type == AstNode::TypeStatementAssign) {
            int status_left = enrich_expression(stmt_node->assign_lexpr, parent_block);
            if (status_left != 0) return status_left;
            int status_right = enrich_expression(stmt_node->assign_rexpr, parent_block);
            if (status_right != 0) return status_right;

            if (stmt_node->assign_lexpr->type != AstNode::TypeExpressionDereference) {
                if (!stmt_node->assign_lexpr->expr_yields_nontemporary) {
                    printf("Can't assign to a temporary on line %d:%d",
                           stmt_node->start_tok->line_number,
                           stmt_node->start_tok->column_number);
                    return 1;
                }
            }

            AstNode *var_type_ref = stmt_node->assign_lexpr->inferred_type_ref;
            AstNode *expr_type = stmt_node->assign_rexpr->inferred_type_ref;
            int type_cmp_status = check_resolved_type_refs_equal(
                var_type_ref, // assuming resolved for now
                expr_type);
            if (type_cmp_status != 0) {
                printf("Trying to assign a value of type ");
                print_type_ref(expr_type);
                printf(" to a variable of type ");
                print_type_ref(var_type_ref);
                printf(" on line %d:%d", 
                       stmt_node->start_tok->line_number,
                       stmt_node->start_tok->column_number);
                return type_cmp_status;
            }
            return 0;
        } else if (stmt_node->type == AstNode::TypeVariableDeclaration) {
            if (stmt_node->var_init_expr) {
                int status = enrich_expression(stmt_node->var_init_expr, parent_block);
                if (status != 0) return status;
            }
            if (!stmt_node->var_type_ref) {
                Token *name_tok = stmt_node->name_tok;
                if (!stmt_node->var_init_expr) {
                    printf("Type of variable %s can't be inferred without "
                           "initializer on line %d:%d",
                           name_tok->str_content.c_str(),
                           name_tok->line_number,
                           name_tok->column_number);
                    return 1;
                }
                stmt_node->var_type_ref = stmt_node->var_init_expr->inferred_type_ref;
            }
            int resolve_status = resolve_type_ref(
                stmt_node->var_type_ref, parent_block, true);
            if (resolve_status != 0) return resolve_status;
            if (stmt_node->var_init_expr) {
                int type_cmp_status = check_resolved_type_refs_equal(
                    stmt_node->var_type_ref,
                    stmt_node->var_init_expr->inferred_type_ref);
                if (type_cmp_status != 0) {
                    printf("Trying to initialize a variable of type ");
                    print_type_ref(stmt_node->var_type_ref);
                    printf(" with expression of type ");
                    print_type_ref(stmt_node->var_init_expr->inferred_type_ref);
                    printf(" on line %d:%d", 
                           stmt_node->start_tok->line_number,
                           stmt_node->start_tok->column_number);
                    return type_cmp_status;
                }
            }
            stmt_node->var_decl_enriched = true;
            return 0;
        } else if (stmt_node->type == AstNode::TypeProcedureDefinition) {
            // skip nested procedure
            return 0;
        } else if (stmt_node->type == AstNode::TypeTypeDefinition) {
            // skip nested type
            return 0;
        } else if (stmt_node->type == AstNode::TypeStatementExpr) {
            int status = enrich_expression(stmt_node->stmt_expr, parent_block);
            if (status != 0) return status;
            return 0;
        } else {
            assert(false && "don't know how to enrich statement");
            return 1;
        }
    }

    int enrich_scope(AstNode *root) {
        for (AstNode* node : root->child_nodes) {
            if (node->type == AstNode::TypeProcedureDefinition) {
                // arguments
                for (AstNode *arg_node : node->child_nodes) {
                    assert(arg_node->type == AstNode::TypeVariableDeclaration);
                    int resolve_arg_status = resolve_type_ref(
                        arg_node->var_type_ref, node->parent_scope, true);
                    if (resolve_arg_status != 0) return resolve_arg_status;
                    arg_node->var_decl_enriched = true;
                }
                // return type
                int resolve_ret_status = resolve_type_ref(
                    node->proc_return_type_ref, node->parent_scope);
                if (resolve_ret_status != 0) return resolve_ret_status;
                
                // body
                bool has_ret = false;
                for (AstNode *stmt_node : node->proc_body->child_nodes) {
                    bool stmt_has_ret = false;
                    int status = enrich_statement(
                        node, node->proc_body, stmt_node, nullptr, stmt_has_ret);
                    if (status != 0) return status;
                    if (stmt_has_ret) has_ret = true;
                }
                if (!has_ret) {
                    printf("Not all code pathes in procedure %s return a value\n", node->name_tok->str_content.c_str());
                    return 1;
                }
                // nested definitions
                int status = enrich_scope(node->proc_body);
                if (status != 0) return status;
            } else if (node->type == AstNode::TypeTypeDefinition) {
                // ignore: enriched as we go
            }
        }
        return 0;
    }

    int enrich_all(AstNode *global_scope_node) {
        return enrich_scope(global_scope_node);
    }
}
