namespace enrichment {
    using std::string;
    using errors::report_error;

    int enrich_scope(AstNode *root);
    int enrich_statement(
        AstNode *proc, AstNode *parent_block,
        AstNode *stmt_node,
        AstNode *loop_node,
        bool& has_ret
    );

    void print_type_ref(AstNode* node) {
        while (true) {
            if (node->type == AstNode::TypeTypeRefName) {
                printf("~>%s", node->name_tok->str_content.c_str());
                break;
            } else if (node->type == AstNode::TypeTypeRefBuiltin) {
                printf("#%s", node->name_tok->str_content.c_str());
                break;
            } else if (node->type == AstNode::TypeTypeRefPointer) {
                print_type_ref(node->pointee_type_ref);
                printf("*");
                break;
            } else if (node->type == AstNode::TypeTypeRefUserDefined) {
                printf("!%s", node->name_tok->str_content.c_str());
                break;
            }
            printf("Don't know how to print type ref %d\n", node->type);
            return;
        }
    }

    void print_ast(AstNode* root) {
        AST_FOREACH_CHILD(node, root) {
            if (node->type == AstNode::TypeProcedureDefinition) {
                printf("PROC: %s( ", node->name_tok->str_content.c_str());
                AST_FOREACH_CHILD(arg_node, node) {
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
                AST_FOREACH_CHILD(member_node, node) {
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
        AST_FOREACH_CHILD(node, scope_node) {
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
            AST_FOREACH_CHILD(node, scope_node) {
                if (node->type == AstNode::TypeVariableDeclaration) {
                    if (node->name_tok && node->name_tok->str_content == name) {
                        if (!node->var_decl_enriched) {
                            break; // is not declared officially yet
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
                if (scope_node->type == AstNode::TypeProcedureDefinition
                 || scope_node->type == AstNode::TypeExpressionPoundRun) {
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

    // returns zero if types are equal
    int check_resolved_type_refs_equal(AstNode *a, AstNode *b) {
        // TODO: recode this nonsense
        while (true) {
            assert(a && "type for type equality check");
            assert(b && "type for type equality check");
            if (a->type == AstNode::TypeTypeRefName || b->type == AstNode::TypeTypeRefName) {
                assert(false && "type equality check is called with unresolved type refs");
                continue;
            }
            if (a->type == AstNode::TypeTypeRefBuiltin) {
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
            } else if (a->type == AstNode::TypeTypeRefUserDefined) {
                if (b->type != AstNode::TypeTypeRefUserDefined) {
                    return 1;
                }
                if (a->user_defined_type_def != b->user_defined_type_def) {
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
        if (node->type == AstNode::TypeTypeRefName) {
            assert(false && "not resolved type ref!");
        } else if (node->type == AstNode::TypeTypeRefBuiltin) {
            return node->builtin_type;
        }
        return Builtin::TypeUnknown;
    }

    AstNode* user_type_for_resolved_ref(AstNode *node) {
        if (node->type == AstNode::TypeTypeRefName) {
            assert(false && "I thought you have a resolved type ref!");
            return nullptr;
        } else if (node->type == AstNode::TypeTypeRefPointer) {
            return user_type_for_resolved_ref(node->pointee_type_ref);
        } else if (node->type == AstNode::TypeTypeRefUserDefined) {
            return node->user_defined_type_def;
        } else if (node->type == AstNode::TypeTypeRefBuiltin) {
            return nullptr;
        }
        printf("Can't find udt for ref type %d\n", node->type);
        assert(false && "I thought you have a resolved type ref!");
        return nullptr;
    }

    Builtin::Op builtin_op_for_tok(Token::Type tok_type, Builtin::Type operand_type) {
        switch (operand_type) {
            case Builtin::Bool:
                switch (tok_type) {
                    case Token::TypeOperatorDoublePipe:
                        return Builtin::OrBool;
                    case Token::TypeOperatorDoubleAmpersand:
                        return Builtin::AndBool;
                    default:
                        return Builtin::OpUnknown;
                }
            case Builtin::Float32:
            case Builtin::Float64:
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
                    case Token::TypeOperatorBangEquals:
                        return Builtin::NeFloat;
                    case Token::TypeOperatorGreater:
                        return Builtin::GtFloat;
                    case Token::TypeOperatorLess:
                        return Builtin::LtFloat;
                    default:
                        return Builtin::OpUnknown;
                }
            case Builtin::S8:
            case Builtin::S16:
            case Builtin::S32:
            case Builtin::S64:
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
                    case Token::TypeOperatorBangEquals:
                        return Builtin::NeInt;
                    case Token::TypeOperatorGreater:
                        return Builtin::GtIntSigned;
                    case Token::TypeOperatorLess:
                        return Builtin::LtIntSigned;
                    default:
                        return Builtin::OpUnknown;
                }
            case Builtin::U8:
            case Builtin::U16:
            case Builtin::U32:
            case Builtin::U64:
                switch (tok_type) {
                    case Token::TypeOperatorPlus:
                        return Builtin::AddInt;
                    case Token::TypeOperatorMinus:
                        return Builtin::SubInt;
                    case Token::TypeOperatorStar:
                        return Builtin::MulInt;
                    case Token::TypeOperatorSlash:
                        return Builtin::DivIntUnsigned;
                    case Token::TypeOperatorDoubleEquals:
                        return Builtin::EqInt;
                    case Token::TypeOperatorBangEquals:
                        return Builtin::NeInt;
                    case Token::TypeOperatorGreater:
                        return Builtin::GtIntUnsigned;
                    case Token::TypeOperatorLess:
                        return Builtin::LtIntUnsigned;
                    default:
                        return Builtin::OpUnknown;
                }
            default:
                return Builtin::OpUnknown;
        }
    }

    int check_resolved_type_ref_finite(AstNode *node, AstNode *container_type_def);
    int enrich_type_definition(AstNode *node);
    int resolve_type_ref(AstNode *node, AstNode *scope_node) {
        // This is called when we have a type ref but not sure if it makes sense.
        // E.g. when declaring a variable we specify type by name, but what does
        // this name actually mean, right?
        // It should be OK to call this several times on a given node, and we should
        // do minimal job required for each call.
        if (node->type == AstNode::TypeTypeRefName) {
            // When type is referenced by name we lookup an actual type
            // (definition or builtin) for a name in a given scope.
            // We resolve looked up type as well recursively and put a pointer to it
            // in resolved_type_ref field.
            Token* type_name_tok = node->name_tok;
            AstNode *resolved_type_node = lookup_type(type_name_tok->str_content, scope_node);
            if (!resolved_type_node) {
                report_error(type_name_tok, "error: unresolved type ");
                printf("%s\n", type_name_tok->str_content.c_str());
                return 1;
            } else {
                if (resolved_type_node->type == AstNode::TypeTypeRefBuiltin) {
                    node->type = AstNode::TypeTypeRefBuiltin;
                    node->builtin_type = resolved_type_node->builtin_type;
                } else if (resolved_type_node->type == AstNode::TypeTypeDefinition) {
                    int status = enrich_type_definition(resolved_type_node);
                    if (status != 0) return status;
                    node->type = AstNode::TypeTypeRefUserDefined;
                    node->user_defined_type_def = resolved_type_node;
                } else {
                    assert(false && "resolved type is not builtin nor udt. wtf?");
                    return 1;
                }
            }
            return 0;
        } else if (node->type == AstNode::TypeTypeRefPointer) {
            // For pointers we just need to resolve the pointee type ref.
            int status = resolve_type_ref(node->pointee_type_ref, scope_node);
            if (status != 0) return status;
            return 0;
        } else if (node->type == AstNode::TypeTypeRefBuiltin) {
            // Count builtin as already resolved - we don't know much about them,
            // but they should be implemented by code_gen somehow.
            return 0;
        } else if (node->type == AstNode::TypeTypeRefUserDefined) {
            // If we found a definition the type is resolved,
            // but what if definition itself doesn't make sense?
            // While we should on some point come to this definition
            // in the main loop let's do this here as well, so
            // the type is ready to be coded right away.
            int status = enrich_type_definition(node->user_defined_type_def);
            if (status != 0) return status;
            return 0;
        }
        report_error(node, "oops");
        printf("Don't know how to resolve type ref %d\n", node->type);
        assert(false && "unreachable");
        return 1;
    }

    int enrich_type_definition(AstNode *node) {
        assert(node->type == AstNode::TypeTypeDefinition);
        if (!node->type_def_members_enrichment_done) {
            node->type_def_members_enrichment_done = true;
            int member_index = 0;
            AST_FOREACH_CHILD(member_node, node) {
                assert(member_node->type == AstNode::TypeTypeMember);
                int resolve_member_status = resolve_type_ref(
                    member_node->member_type_ref, node->parent_scope);
                if (resolve_member_status != 0) return resolve_member_status;
                if (member_node->member_type_ref->type == AstNode::TypeTypeRefBuiltin
                 && member_node->member_type_ref->builtin_type == Builtin::Void) {
                    report_error(member_node, "member can't be of type void!\n");
                    return 1;
                }
                int finiteness_check_status = check_resolved_type_ref_finite(
                    member_node->member_type_ref, node);
                if (finiteness_check_status != 0) {
                    report_error(member_node, " ... ");
                    printf("through the member %s\n",
                           member_node->name_tok->str_content.c_str());
                    return finiteness_check_status;
                }
                member_node->member_index = member_index;
                member_index++;
            }
        }
        return 0;
    }

    int check_resolved_type_ref_finite(AstNode *node, AstNode *container_type_def) {
        if (node->type == AstNode::TypeTypeRefName) {
            assert(false && "is it unresolved type?");
            return 1;
        } else if (node->type == AstNode::TypeTypeRefBuiltin) {
            return 0; // builtins are always finite
        } else if (node->type == AstNode::TypeTypeRefPointer) {
            return 0; // pointers are always finite
        } else if (node->type == AstNode::TypeTypeRefUserDefined) {
            if (node->user_defined_type_def == container_type_def) {
                report_error(container_type_def, "error: infinite type ");
                printf("%s depends on itself\n",
                       container_type_def->name_tok->str_content.c_str());
                return 1;
            } else {
                AST_FOREACH_CHILD(member, node->user_defined_type_def) {
                    int resolve_member_status = resolve_type_ref(
                        member->member_type_ref, node->user_defined_type_def->parent_scope);
                    if (resolve_member_status != 0) return resolve_member_status;
                    int member_finiteness_status = check_resolved_type_ref_finite(
                        member->member_type_ref, container_type_def);
                    if (member_finiteness_status != 0) {
                        report_error(member, " ... ");
                        printf("through the member %s of %s\n",
                               member->name_tok->str_content.c_str(),
                               node->user_defined_type_def->name_tok->str_content.c_str());
                        return member_finiteness_status;
                    }
                }
                return 0;
            }
            printf("Don't know how to check finiteness of type ref %d\n", node->type);
            assert(false && "unreachable");
            return 1;
        }
        return 1;
    }

    int enrich_proc_definition(AstNode *node) {
        assert(node && node->type == AstNode::TypeProcedureDefinition);
        // arguments (idempotent)
        AST_FOREACH_CHILD(arg_node, node) {
            assert(arg_node->type == AstNode::TypeVariableDeclaration);
            int resolve_arg_status = resolve_type_ref(
                arg_node->var_type_ref, node->parent_scope);
            if (resolve_arg_status != 0) return resolve_arg_status;
            if (arg_node->var_type_ref->type == AstNode::TypeTypeRefBuiltin
             && arg_node->var_type_ref->builtin_type == Builtin::Void) {
                report_error(arg_node, "proc argument can't have void type!\n");
                return 1;
            }
            arg_node->var_decl_enriched = true;
        }
        // return type (idempotent)
        int resolve_ret_status = resolve_type_ref(
            node->proc_return_type_ref, node->parent_scope);
        if (resolve_ret_status != 0) return resolve_ret_status;
        if (!node->proc_body_enriched) {
            node->proc_body_enriched = true;
            // body
            bool has_ret = false;
            AST_FOREACH_CHILD(stmt_node, node->proc_body) {
                bool stmt_has_ret = false;
                int status = enrich_statement(
                    node, node->proc_body, stmt_node, nullptr, stmt_has_ret);
                if (status != 0) return status;
                if (stmt_has_ret) has_ret = true;
            }
            if (!has_ret) {
                // TODO: this is not tested well and probably doesn't work :)
                report_error(node, "error: not all code pathes return a value ");
                printf("in procedure %s\n", node->name_tok->str_content.c_str());
                return 1;
            }
            // nested definitions
            int status = enrich_scope(node->proc_body);
            if (status != 0) return status;
        }
        return 0;
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
                report_error(expr, "error: can't dereference a member ");
                printf("%s of a builtin type\n", member_name.c_str());
                return 1;
            }
            bool found = false;
            AST_FOREACH_CHILD(udt_member, udt) {
                if (udt_member->name_tok->str_content == member_name) {
                    expr->memberof_member = udt_member;
                    int type_status = resolve_type_ref(udt_member->member_type_ref, udt);
                    if (type_status != 0) return type_status;
                    expr->inferred_type_ref = udt_member->member_type_ref;
                    found = true;
                }
            }
            if (!found) {
                report_error(expr, "error: can't dereference an undefined member ");
                printf("%s of type ", member_name.c_str());
                print_type_ref(type_ref);
                printf("\n");
                return 1;
            }
            return 0;
        } else if (expr->type == AstNode::TypeExpressionName) {
            string& name = expr->name_tok->str_content;
            AstNode *variable_node = lookup_variable(name, scope);
            if (variable_node == nullptr) {
                report_error(expr, "error: use of undeclared variable ");
                printf("%s\n", name.c_str());
                return 1;
            }
            expr->resolved_var = variable_node;
            int type_status = resolve_type_ref(variable_node->var_type_ref, scope);
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
            assert(false && "unreachable");
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
                inferred_type = lookup_type(Builtin::key_s32, scope->parent_scope);
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
                    report_error(expr, "error: use of undeclared variable ");
                    printf("%s\n", name.c_str());
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
                expr->bin_op_lexpr->inferred_type_ref,
                expr->bin_op_rexpr->inferred_type_ref);
            if (check_status != 0) {
                report_error(expr, "error: trying to apply a binary operator ");
                printf("%s to operands of different types: ",
                       expr->name_tok->str_content.c_str());
                print_type_ref(expr->bin_op_lexpr->inferred_type_ref);
                printf(" and ");
                print_type_ref(expr->bin_op_rexpr->inferred_type_ref);
                printf("\n");
                return check_status;
            }
            Builtin::Type operand_type = builtin_type_for_resolved_ref(
                expr->bin_op_lexpr->inferred_type_ref);
            if (operand_type == Builtin::TypeUnknown) {
                report_error(expr, "error: trying to apply a binary operator ");
                printf("%s to operands of user defined type: ",
                       expr->name_tok->str_content.c_str());
                print_type_ref(expr->bin_op_lexpr->inferred_type_ref);
                printf("\n");
                return 1;
            }
            Builtin::Op builtin_op = builtin_op_for_tok(expr->name_tok->type, operand_type);
            if (builtin_op == Builtin::OpUnknown) {
                report_error(expr, "error: can't apply binary operator ");
                printf("%s to operands of type: ", expr->name_tok->str_content.c_str());
                print_type_ref(expr->bin_op_lexpr->inferred_type_ref);
                printf("\n");
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
            // enrich arg expressions
            AST_FOREACH_CHILD(arg_expr, expr) {
                int status = enrich_expression(arg_expr, scope);
                if (status != 0) return 1;
            }
            // find the intended proc defitnion
            string& name = expr->name_tok->str_content;
            AstNode *scope_node = scope;
            AstNode *last_variant_proc = nullptr;
            expr->call_proc_def = nullptr;
            struct {
                AstNode *call_arg_type;
                AstNode *decl_arg_type;
                size_t argi;
            } last_arg_type_mismatch_error;
            while (true) {
                // Check current scope
                // TODO: use child lookup table
                AST_FOREACH_CHILD(proc_def_node, scope_node) {
                    if (proc_def_node->type != AstNode::TypeProcedureDefinition) continue;
                    if (proc_def_node->name_tok
                     && proc_def_node->name_tok->str_content == name) {
                        last_variant_proc = proc_def_node;
                        // we are likely to call function that wasn't
                        // enriched yet by itself we don't need its
                        // body to be correct, but at least arg
                        // and ret types are resolved here
                        int proc_status = enrich_proc_definition(proc_def_node);
                        if (proc_status != 0) return proc_status;
                        // TODO: default arguments?
                        if (expr->child_nodes_count != proc_def_node->child_nodes_count) {
                            continue;
                        }
                        // the proc takes the correct number of arguments
                        // make sure argument types make sense,
                        // we treat call args and decl args as a parallel lists on this point.
                        // Wandering how we could impl default arguments
                        // and implicit type conversions.
                        size_t argi = 0;
                        AstNode *call_arg = expr->child_nodes_list;
                        AstNode *decl_arg = proc_def_node->child_nodes_list;
                        bool types_match = true;
                        while (argi < expr->child_nodes_count) {
                            AstNode *call_arg_type = call_arg->inferred_type_ref;
                            AstNode *decl_arg_type = decl_arg->var_type_ref;
                            assert(call_arg_type && "was inferred");
                            assert(decl_arg_type && "is known");
                            int decl_arg_resolve_status = resolve_type_ref(
                                decl_arg_type, proc_def_node->parent_scope);
                            if (decl_arg_resolve_status != 0) return decl_arg_resolve_status;
                            int check_status = check_resolved_type_refs_equal(
                                call_arg_type, decl_arg_type);
                            if (check_status != 0) {
                                // type mismatch
                                last_arg_type_mismatch_error.call_arg_type = call_arg_type;
                                last_arg_type_mismatch_error.decl_arg_type = decl_arg_type;
                                last_arg_type_mismatch_error.argi = argi;
                                types_match = false;
                                break;
                            }

                            call_arg = call_arg->child_node_next;
                            decl_arg = decl_arg->child_node_next;
                            argi++;
                        }
                        if (types_match) {
                            expr->call_proc_def = proc_def_node;
                            break;
                        }
                    }
                }
                if (expr->call_proc_def) break;
                scope_node = scope_node->parent_scope;
                if (scope_node == nullptr) {
                    if (last_variant_proc) {
                        if (expr->child_nodes_count != last_variant_proc->child_nodes_count) {
                            report_error(expr, "error: incorrect procedure call\n");
                            report_error(last_variant_proc, " ... ");
                            // TODO: report all known overloads
                            // e.g. abc takes 3, 5 or 7 arguments 2 given
                            printf("%s takes %lu arguments "
                                   "(%lu given)\n", name.c_str(),
                                   last_variant_proc->child_nodes_count,
                                   expr->child_nodes_count);
                            return 1;
                        } else {
                            // TODO: report all known overloads matching arguments count
                            report_error(expr, "error: incorrect procedure call: ");
                            printf("type ");
                            print_type_ref(last_arg_type_mismatch_error.decl_arg_type);
                            printf(" expected, given ");
                            print_type_ref(last_arg_type_mismatch_error.call_arg_type);
                            printf(" for argument %lu when calling %s\n", last_arg_type_mismatch_error.argi + 1, last_variant_proc->name_tok->str_content.c_str());
                            report_error(last_variant_proc, " <-- procedure defined here\n");
                        }
                    }
                    report_error(expr, "error: trying to call an undeclared procedure ");
                    printf("%s\n", name.c_str());
                    return 1;
                }
            }
            // on this point args are good, infer return type without checks - caller should
            // decide if it's good or not for him to handle.
            expr->inferred_type_ref = expr->call_proc_def->proc_return_type_ref;
            assert(expr->inferred_type_ref && "for call expression");
            // But let's resolve it here just in case =)
            int resolve_ret_status = resolve_type_ref(
                expr->inferred_type_ref, expr->call_proc_def->parent_scope);
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
                report_error(expr, "error: can't take an address of a temporary\n");
                return 1;
            }
        } else if (expr->type == AstNode::TypeExpressionDereference) {
            int status = enrich_expression(expr->deref_expr, scope);
            if (status != 0) return status;
            if (expr->deref_expr->inferred_type_ref->type != AstNode::TypeTypeRefPointer) {
                report_error(expr, "error: can't dereference a value of a non-pointer type\n");
                return 1;
            }
            expr->inferred_type_ref = expr->deref_expr->inferred_type_ref->pointee_type_ref;
            if (expr->inferred_type_ref->type == AstNode::TypeTypeRefBuiltin &&
                expr->inferred_type_ref->builtin_type == Builtin::Void) {
                report_error(expr, "error: can't dereference a value of a void-pointer type\n");
                return 1;
            }
        } else if (expr->type == AstNode::TypeExpressionPoundRun) {
            int status = enrich_expression(expr->pound_run_expr, expr);
            if (status != 0) return status;
            code_gen_run_expression(ctce, expr->pound_run_expr);
            // TODO: substitute with constant expression
            // containing the return value.
            expr->inferred_type_ref = expr->pound_run_expr->inferred_type_ref;
        } else {
            printf("Don't know how to enrich expression of type %d\n", expr->type);
            assert(false && "unreachable");
            return 1;
        }
        return 0;
    }

    int enrich_variable_declaration(AstNode *stmt_node, AstNode *parent_block) {
        if (stmt_node->var_init_expr) {
            int status = enrich_expression(stmt_node->var_init_expr, parent_block);
            if (status != 0) return status;
        }
        if (!stmt_node->var_type_ref) {
            Token *name_tok = stmt_node->name_tok;
            if (!stmt_node->var_init_expr) {
                report_error(stmt_node, "error: ");
                printf("type of variable %s can't be inferred without "
                       "initializer\n",
                       name_tok->str_content.c_str());
                return 1;
            }
            stmt_node->var_type_ref = stmt_node->var_init_expr->inferred_type_ref;
        }
        int resolve_status = resolve_type_ref(stmt_node->var_type_ref, parent_block);
        if (resolve_status != 0) return resolve_status;
        if (stmt_node->var_type_ref->type == AstNode::TypeTypeRefBuiltin
         && stmt_node->var_type_ref->builtin_type == Builtin::Void) {
            report_error(stmt_node, "can't declare a variable of type void!\n");
            return 1;
        }
        if (stmt_node->var_init_expr) {
            int type_cmp_status = check_resolved_type_refs_equal(
                stmt_node->var_type_ref,
                stmt_node->var_init_expr->inferred_type_ref);
            if (type_cmp_status != 0) {
                report_error(stmt_node, "error: type mismatch: ");
                printf("trying to initialize a variable of type ");
                print_type_ref(stmt_node->var_type_ref);
                printf(" with expression of type ");
                print_type_ref(stmt_node->var_init_expr->inferred_type_ref);
                printf("\n");
                return type_cmp_status;
            }
        }
        stmt_node->var_decl_enriched = true;
        return 0;
    }

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
                report_error(stmt_node, "error: return type mismatch: ");
                printf("procedure %s return type is ", proc->name_tok->str_content.c_str());
                print_type_ref(proc->proc_return_type_ref);
                printf(" but return expression has type ");
                print_type_ref(stmt_node->ret_expr->inferred_type_ref);
                printf("\n");
                report_error(proc, " <-- procedure defined here\n");
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
                report_error(stmt_node, "error: type mismatch: ");
                printf("if condition should have a bool type (given ");
                print_type_ref(stmt_node->if_cond_expr->inferred_type_ref);
                printf(")\n");
                return 1;
            }

            // TODO: has_ret logic is not tested and probably doesn't work
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
            // TODO: has_ret logic is not tested and probably doesn't work
            bool block_has_ret = false;
            AST_FOREACH_CHILD(stmt_in_block_node, stmt_node) {
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
                report_error(stmt_node, "error: invalid statement: ");
                printf("break is only allowed inside loop\n");
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
                    report_error(stmt_node, "error: invalid statement: ");
                    printf("can't assign to a temporary\n");
                    return 1;
                }
            }

            AstNode *var_type_ref = stmt_node->assign_lexpr->inferred_type_ref;
            AstNode *expr_type = stmt_node->assign_rexpr->inferred_type_ref;
            int type_cmp_status = check_resolved_type_refs_equal(
                var_type_ref, // assuming resolved for now
                expr_type);
            if (type_cmp_status != 0) {
                report_error(stmt_node, "error: type mismatch: ");
                printf("trying to assign a value of type ");
                print_type_ref(expr_type);
                printf(" to a variable of type ");
                print_type_ref(var_type_ref);
                printf("\n");
                return type_cmp_status;
            }
            return 0;
        } else if (stmt_node->type == AstNode::TypeVariableDeclaration) {
            int status = enrich_variable_declaration(stmt_node, parent_block);
            if (status != 0) return status;
            return 0;
        } else if (stmt_node->type == AstNode::TypeProcedureDefinition) {
            // skip nested proc (handled by enrich_scope)
            return 0;
        } else if (stmt_node->type == AstNode::TypeTypeDefinition) {
            // skip nested type (handled by enrich_scope)
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
        AST_FOREACH_CHILD(node, root) {
            // While type and procedure definitions are enriched as we go
            // we should do it here to make sure the whole
            // program is correct even if this type is not used anywhere by value
            // or procedure is never called.
            if (node->type == AstNode::TypeProcedureDefinition) {
                int status = enrich_proc_definition(node);
                if (status != 0) return status;
            } else if (node->type == AstNode::TypeTypeDefinition) {
                int status = enrich_type_definition(node);
                if (status != 0) return status;
            } else if (root->type == AstNode::TypeGlobalScope) {
                // handle only global variable declarations here
                if (node->type == AstNode::TypeVariableDeclaration) {
                    int status = enrich_variable_declaration(node, root);
                    if (status != 0) return status;
                    if (node->var_init_expr && node->var_init_expr->type
                        != AstNode::TypeExpressionLiteralNumber) {
                        report_error(node->var_init_expr, "error: ");
                        printf("can't initialize a global variable %s with non-constant "
                               "expression\n", node->name_tok->str_content.c_str());
                        return 1;
                    }
                }
            }
        }
        return 0;
    }

    int enrich_all(AstNode *global_scope_node) {
        return enrich_scope(global_scope_node);
    }
}
