#include <array>
#include <map>

namespace parser {
    using std::string;
    using errors::report_error;

    // top-level keywords
    // TODO: check Sean Barrett syntax for top level stuff - is it better?
    // TODO: disallow name variable as keywords!
    string keywordProc = "proc";
    string keywordType = "type";
    string keywordRet = "ret";
    string keywordIf = "if";
    string keywordElse = "else";
    string keywordRepeat = "repeat";
    string keywordBreak = "break";

    string poundRun = "run";

    // TODO: unary operators
    // TODO: user defined operators?
    void init_bin_op_precedence(int table[]) {
        table[Token::TypeOperatorDoublePipe] = 10;
        table[Token::TypeOperatorDoubleAmpersand] = 15;

        table[Token::TypeOperatorDoubleEquals] = 20;
        table[Token::TypeOperatorBangEquals] = 20;
        table[Token::TypeOperatorGreater] = 20;
        table[Token::TypeOperatorLess] = 20;

        // TODO: | = 30
        // TODO: ^ = 34
        // TODO: & = 36

        // TODO: shifts = 40

        table[Token::TypeOperatorPlus] = 50;
        table[Token::TypeOperatorMinus] = 50;

        table[Token::TypeOperatorStar] = 60;
        table[Token::TypeOperatorSlash] = 60;

        // TODO: rem/mod = 60
    }

    // TODO: recode this nonsense
    struct AstNodePool {
        // Can't use vector for AST storage - it reallocs!
        // Use linked list of blocks of nodes.
        struct Block {
            // TODO: grow capacity as well?
            static const size_t capacity = 64;
            std::array<AstNode, capacity> nodes;
            size_t used;
            std::unique_ptr<Block> next;
        };

        std::unique_ptr<Block> active_block;
        AstNodePool() : active_block(std::make_unique<Block>()) {}

        AstNode& add(AstNode::Type type) {
            if (active_block->used >= Block::capacity) {
                std::unique_ptr<Block> new_block = std::make_unique<Block>();
                new_block->next = std::move(active_block);
                active_block = std::move(new_block);
            }
            AstNode& new_node = active_block->nodes[active_block->used];
            active_block->used++;
            new_node.type = type;
            return new_node;
        }
    };

    bool ast_add_child(AstNode& parent, AstNode& node) {
        parent.child_nodes.push_back(&node);
        if (node.type == AstNode::TypeTypeDefinition) {
            auto& name = node.name_tok->str_content;
            auto known = parent.child_lookup.find(name);
            if (known == parent.child_lookup.end()) {
                parent.child_lookup[name] = {&node};
            } else {
                for (auto& it : known->second) {
                    if (it->type == AstNode::TypeTypeDefinition) {
                        report_error(node);
                        printf("error: redefinition of type %s\n", node.name_tok->str_content.c_str());
                        report_error(it, " ... defined first time here\n");
                        return false;
                    }
                }
            }
        }
        if (node.type == AstNode::TypeProcedureDefinition) {
            auto& name = node.name_tok->str_content;
            auto known = parent.child_lookup.find(name);
            if (known == parent.child_lookup.end()) {
                parent.child_lookup[name] = {&node};
            } else {
                for (auto& it : known->second) {
                    if (it->type == AstNode::TypeProcedureDefinition) {
                        report_error(node);
                        printf("error: redefinition of procedure %s\n", node.name_tok->str_content.c_str());
                        report_error(it, " ... defined first time here\n");
                        return false;
                    }
                }
            }
        }
        if (node.type == AstNode::TypeVariableDeclaration) {
            auto& name = node.name_tok->str_content;
            auto known = parent.child_lookup.find(name);
            if (known == parent.child_lookup.end()) {
                parent.child_lookup[name] = {&node};
            } else {
                for (auto& it : known->second) {
                    if (it->type == AstNode::TypeVariableDeclaration) {
                        report_error(node);
                        printf("error: redefinition of a variable %s\n", node.name_tok->str_content.c_str());
                        report_error(it, " ... defined first time here\n");
                        return false;
                    }
                }
            }
        }
        return true;
    }

    // just holding parsing state to not pass it everywhere
    struct Parsing {
        std::vector<Token>& tokens;
        AstNodePool& ast_node_pool;
        int toki;
        int bin_op_precedence[Token::TypeLast];

        Parsing(std::vector<Token>& tokens, AstNodePool& ast_node_pool)
            : tokens(tokens), ast_node_pool(ast_node_pool), toki(0) {
            init_bin_op_precedence(bin_op_precedence);
        }

        int get_bin_op_precedence_for_tok(Token::Type type) {
            if (type < 1 || type >= Token::TypeLast) {
                return -1;
            }
            return bin_op_precedence[type];
        }

        int parse_all(AstNode& scope_node) {
            // Here we want to parse a whole stream
            // and produce a parse tree.
            // On this point we don't know if the code makes
            // sense semantically.
            while(toki < tokens.size()) {
                Token& tok = tokens[toki];
                if (tok.type == Token::TypeName) {
                    // top level stuff
                    // function
                    if (tok.str_content == keywordProc) {
                        int status = parse_procedure(scope_node);
                        if (status != 0) return status;
                        continue;
                    }
                    // type
                    if (tok.str_content == keywordType) {
                        int status = parse_type(scope_node);
                        if (status != 0) return status;
                        continue;
                    }
                    // assuming global statement
                    AstNode *expr = parse_expression(&scope_node);
                    if (!expr) return 1;
                    if (tokens[toki].type != Token::TypeSemicolon) {
                        // variable declaration?
                        if (tokens[toki].type == Token::TypeColon) {
                            // variable declaration
                            AstNode *decl = parse_variable_declaration(expr, scope_node);
                            if (decl == nullptr) return 1;
                            if (!ast_add_child(scope_node, *decl)) return 1;
                            if (!check_tok_type(Token::TypeSemicolon,
                                                "semicolon after statement")) return 1;
                            if (!next_tok("global scope continues or ends")) return 1;
                            continue;
                        } else {
                            report_error(expr, "parse error: ");
                            printf("unexpected token %s\n", expr->start_tok->str_content.c_str());
                            return 1;
                        }
                    }
                }
                report_error(tok, "parse error: ");
                printf("unexpected token %s\n"
                       "- was looking for top-level stuff like types and functions!\n",
                       tok.str_content.c_str());
                return 1;
            }
            return 0;
        }

        // TODO: better to have expect* routine with EOF checking baked-in
        bool next_tok(const char *message) {
            Token& tok = tokens[toki];
            toki++;
            if (toki >= tokens.size()) {
                report_error(tok, "parse error: ");
                printf("%s expected, got EOF\n", message);
                return false;
            }
            return true;
        }

        bool check_tok_type(Token::Type token_type, const char *message) {
            Token& tok = tokens[toki];
            if (tok.type != token_type) {
                report_error(tok, "parse error: ");
                printf("%s expected, got %s\n", message, tok.str_content.c_str());
                return false;
            }
            return true;
        }

        AstNode* parse_expression_bin_op_rhs(AstNode *scope, int lprec, AstNode *lexpr) {
            while (true) {
                int prec = get_bin_op_precedence_for_tok(tokens[toki].type);
                if (prec < lprec) {
                    return lexpr;
                }

                Token& bin_op_tok = tokens[toki];
                if (!next_tok("right hand side of binary operator expression")) return nullptr;
                AstNode *rexpr = parse_expression_primary(scope);
                if (!rexpr) return nullptr;

                int next_prec = get_bin_op_precedence_for_tok(tokens[toki].type);
                if (prec < next_prec) {
                    rexpr = parse_expression_bin_op_rhs(scope, prec + 1, rexpr);
                    if (!rexpr) return nullptr;
                }

                AstNode& op_node = ast_node_pool.add(AstNode::TypeExpressionBinOp);
                op_node.start_tok = lexpr->start_tok;
                op_node.name_tok = &bin_op_tok;
                op_node.bin_op_lexpr = lexpr;
                op_node.bin_op_rexpr = rexpr;
                lexpr = &op_node;
            }
        }

        AstNode* parse_expression_memberof(AstNode *base_node) {
            while (tokens[toki].type == Token::TypeOperatorDot) {
                if (!next_tok("name of a member after dot")) return nullptr;
                if (tokens[toki].type != Token::TypeName) {
                    Token& tok = tokens[toki];
                    report_error(tok, "parse error: ");
                    printf("member name expected got %s\n", tok.str_content.c_str());
                    return nullptr;
                }
                AstNode& node = ast_node_pool.add(AstNode::TypeExpressionMemberOf);
                node.start_tok = base_node->start_tok;
                node.memberof_base_var = base_node;
                node.memberof_member_name_tok = &tokens[toki];
                if (!next_tok("expression continues or ends")) return nullptr;
                base_node = &node;
            }
            return base_node;
        }

        AstNode* parse_expression_primary(AstNode *scope) {
            Token& tok = tokens[toki];
            if (tok.type == Token::TypeParenOpen) {
                if (!next_tok("expression continues")) return nullptr;
                AstNode *in_parens = parse_expression(scope);
                if (!in_parens) return nullptr;
                if (!check_tok_type(Token::TypeParenClose, "closing paren")) return nullptr;
                if (!next_tok("expression continues or ends")) return nullptr;
                return in_parens;
            } else if (tok.type == Token::TypeOperatorAmpersand) {
                if (!next_tok("expression continues")) return nullptr;
                AstNode *expr = parse_expression_primary(scope);
                if (!expr) return nullptr;
                AstNode& node = ast_node_pool.add(AstNode::TypeExpressionAddressOf);
                node.start_tok = &tok;
                node.addressof_expr = expr;
                // allocating type ref pointer here or we have to do it in enrichment
                node.addressof_type_ref = &ast_node_pool.add(AstNode::TypeTypeRefPointer);
                node.addressof_type_ref->start_tok = &tok;
                node.addressof_type_ref->name_tok = &tok;
                return &node;
            } else if (tok.type == Token::TypeOperatorStar) {
                if (!next_tok("expression continues")) return nullptr;
                AstNode *expr = parse_expression_primary(scope);
                if (!expr) return nullptr;
                AstNode& node = ast_node_pool.add(AstNode::TypeExpressionDereference);
                node.start_tok = &tok;
                node.deref_expr = expr;
                return &node;
            } else if (tok.type == Token::TypePound) {
                Token& start_tok = tokens[toki];
                if (!next_tok("pound keyword")) return nullptr;
                if (tokens[toki].type == Token::TypeName) {
                    if (tokens[toki].str_content == poundRun) {
                        Token& name_tok = tokens[toki];
                        if (!next_tok("#run expression")) return nullptr;
                        AstNode& node = ast_node_pool.add(AstNode::TypeExpressionPoundRun);
                        node.start_tok = &start_tok;
                        node.name_tok = &name_tok;
                        node.parent_scope = scope;
                        AstNode *expr = parse_expression(&node);
                        if (!expr) return nullptr;
                        node.pound_run_expr = expr;
                        return &node;
                    } else {
                        Token& tok = tokens[toki];
                        report_error(tok, "parse error: ");
                        printf("unsupported pound keyword %s\n", tok.str_content.c_str());
                        return nullptr;
                    }
                } else {
                    Token& tok = tokens[toki];
                    report_error(tok, "parse error: ");
                    printf("pound keyword expected got %s\n", tok.str_content.c_str());
                    return nullptr;
                }

            } else {
                if (tok.type == Token::TypeLiteralNumber) {
                    AstNode& node = ast_node_pool.add(AstNode::TypeExpressionLiteralNumber);
                    node.start_tok = &tok;
                    node.expr_literal_number_tok = &tok;
                    if (!next_tok("expression continues or ends")) return nullptr;
                    return &node;
                } else if (tok.type == Token::TypeName) {
                    Token& name_tok = tokens[toki];
                    if (!next_tok("expression continues or ends")) return nullptr;
                    if (tokens[toki].type == Token::TypeParenOpen) {
                        AstNode& node = ast_node_pool.add(AstNode::TypeExpressionCall);
                        node.start_tok = &name_tok;
                        node.name_tok = &name_tok;
                        if (!next_tok("call arguments list")) return nullptr;
                        while(tokens[toki].type != Token::TypeParenClose) {
                            AstNode *call_arg_node = parse_expression(scope);
                            if (!call_arg_node) return nullptr;
                            if (!ast_add_child(node, *call_arg_node)) return nullptr;
                            if (tokens[toki].type == Token::TypeComma) {
                                if (!next_tok("call arguments list continues")) return nullptr;
                            } else if (tokens[toki].type == Token::TypeParenClose) {
                                break;
                            } else {
                                Token& tok = tokens[toki];
                                report_error(tok, "parse error: ");
                                printf("unexpected token %s\n", tok.str_content.c_str());
                                return nullptr;
                            }
                        }
                        if (!next_tok("expression continues or ends")) return nullptr;
                        return &node;
                    } else {
                        // TODO: more on name based expressions!
                        AstNode& node = ast_node_pool.add(AstNode::TypeExpressionName);
                        node.start_tok = &name_tok;
                        node.name_tok = &name_tok;
                        AstNode *memberof_expr = parse_expression_memberof(&node);
                        if (!memberof_expr) return nullptr;
                        return memberof_expr;
                    }
                }
            }
            report_error(tok, "parse error: ");
            printf("unexpected token %s\n", tok.str_content.c_str());
            return nullptr;
        }

        AstNode* parse_expression(AstNode *scope) {
            AstNode *prim = parse_expression_primary(scope);
            if (!prim) return nullptr;

            AstNode *expr = parse_expression_bin_op_rhs(scope, 1, prim);
            if (!expr) {
                report_error(expr, "parse error: invalid expression\n");
                return nullptr;
            }
            return expr;
        }

        AstNode* parse_type_ref() {
            // TODO: array type?
            if (!check_tok_type(Token::TypeName, "type name")) return nullptr;
            Token& type_name_tok = tokens[toki];
            AstNode& ref_node = ast_node_pool.add(AstNode::TypeTypeRefName);
            ref_node.start_tok = &type_name_tok; // same as name
            ref_node.name_tok = &type_name_tok;
            if (!next_tok("type specification continues or ends")) return nullptr;
            AstNode *left_ref = &ref_node;
            while (tokens[toki].type == Token::TypeOperatorStar) {
                AstNode& pointer_node = ast_node_pool.add(AstNode::TypeTypeRefPointer);
                pointer_node.start_tok = &tokens[toki];
                pointer_node.name_tok = &tokens[toki];
                pointer_node.pointee_type_ref = left_ref;
                left_ref = &pointer_node;
                if (!next_tok("type specification continues or ends")) return nullptr;
            }
            return left_ref;
        }

        AstNode* parse_variable_declaration(AstNode *expr, AstNode& parent_block_node) {
            if (expr->type != AstNode::TypeExpressionName) {
                report_error(tokens[toki], "parse error: unexpected colon after expression\n");
                return nullptr;
            }
            if (!next_tok("declaration continues")) return nullptr;

            AstNode& node = ast_node_pool.add(AstNode::TypeVariableDeclaration);
            node.start_tok = expr->start_tok;
            node.name_tok = expr->name_tok;
            if (tokens[toki].type != Token::TypeEquals) {
                // explicit type specification
                AstNode *var_type_node = parse_type_ref();
                if (!var_type_node) return nullptr;
                node.var_type_ref = var_type_node;
            } else {
                // type should be inferred from the initializer
                node.var_type_ref = nullptr;
            }
            if (tokens[toki].type == Token::TypeEquals) {
                if (!next_tok("initializer expression")) return nullptr;
                AstNode *expr = parse_expression(&parent_block_node);
                if (!expr) return nullptr;
                node.var_init_expr = expr;
            }
            return &node;
        }

        int parse_statement(AstNode& parent_block_node) {
            if (tokens[toki].type == Token::TypeCurlyOpen) {
                AstNode& block_node = ast_node_pool.add(AstNode::TypeStatementBlock);
                block_node.start_tok = &tokens[toki];
                block_node.parent_scope = &parent_block_node;
                if (!next_tok("block continues")) return 1;
                while(tokens[toki].type != Token::TypeCurlyClose) {
                    int status = parse_statement(block_node);
                    if (status != 0) return status;
                }
                if (!next_tok("block continues")) return 1;
                if (!ast_add_child(parent_block_node, block_node)) return 1;
                return 0;
            }
            // check for keyword-based statements
            if (tokens[toki].type == Token::TypeName) {
                if (tokens[toki].str_content == keywordRet) {
                    AstNode& ret_stmt_node = ast_node_pool.add(AstNode::TypeStatementReturn);
                    ret_stmt_node.start_tok = &tokens[toki];
                    if (!next_tok("return expression or semicolon")) return 1;
                    if (!ast_add_child(parent_block_node, ret_stmt_node)) return 1;
                    if (tokens[toki].type == Token::TypeSemicolon) {
                        // TODO: do we want to supprt ret; wihtout expression here?
                        report_error(tokens[toki], "parse error: missing return expression\n");
                        return 1;
                    } else {
                        AstNode *expr = parse_expression(&parent_block_node);
                        if (!expr) return 1;
                        ret_stmt_node.ret_expr = expr;
                    }
                    if (!check_tok_type(Token::TypeSemicolon, "semicolon after statement")) return 1;
                    if (!next_tok("procedure body continues or ends")) return 1;
                    return 0;
                } else if (tokens[toki].str_content == keywordRepeat) {
                    AstNode& repeat_stmt_node = ast_node_pool.add(AstNode::TypeStatementRepeat);
                    repeat_stmt_node.start_tok = &tokens[toki];
                    if (!next_tok("statement to repeat")) return 1;
                    AstNode& repeat_block = ast_node_pool.add(AstNode::TypeStatementBlock);
                    repeat_block.start_tok = &tokens[toki];
                    repeat_block.parent_scope = &parent_block_node;
                    int status = parse_statement(repeat_block);
                    if (status != 0) return status;
                    repeat_stmt_node.repeat_stmt = &repeat_block;
                    if (!ast_add_child(parent_block_node, repeat_stmt_node)) return 1;
                    return 0;
                } else if (tokens[toki].str_content == keywordBreak) {
                    AstNode& break_stmt_node = ast_node_pool.add(AstNode::TypeStatementBreak);
                    break_stmt_node.start_tok = &tokens[toki];
                    if (!next_tok("semicolon after break")) return 1;
                    if (!check_tok_type(Token::TypeSemicolon, "semicolon after break")) return 1;
                    if (!next_tok("block continues or ends")) return 1;
                    if (!ast_add_child(parent_block_node, break_stmt_node)) return 1;
                    return 0;
                } else if (tokens[toki].str_content == keywordIf) {
                    AstNode& if_stmt_node = ast_node_pool.add(AstNode::TypeStatementIf);
                    if_stmt_node.start_tok = &tokens[toki];
                    if (!next_tok("if condition expression")) return 1;
                    AstNode *cond_expr = parse_expression(&parent_block_node);
                    if (!cond_expr) return 1;
                    if_stmt_node.if_cond_expr = cond_expr;
                    AstNode& then_block = ast_node_pool.add(AstNode::TypeStatementBlock);
                    then_block.start_tok = &tokens[toki];
                    then_block.parent_scope = &parent_block_node;
                    int then_status = parse_statement(then_block);
                    if (then_status != 0) return then_status;
                    if_stmt_node.if_then_stmt = &then_block;
                    if (tokens[toki].type == Token::TypeName) {
                        if (tokens[toki].str_content == keywordElse) {
                            if (!next_tok("statement continues")) return 1;
                            AstNode& else_block = ast_node_pool.add(AstNode::TypeStatementBlock);
                            else_block.start_tok = &tokens[toki];
                            else_block.parent_scope = &parent_block_node;
                            int status = parse_statement(else_block);
                            if (status != 0) return status;
                            if_stmt_node.if_else_stmt = &else_block;
                        }
                    }
                    if (!ast_add_child(parent_block_node, if_stmt_node)) return 1;
                    return 0;
                } else if (tokens[toki].str_content == keywordProc) {
                    int status = parse_procedure(parent_block_node);
                    if (status != 0) return status;
                    return 0;
                } else if (tokens[toki].str_content == keywordType) {
                    int status = parse_type(parent_block_node);
                    if (status != 0) return status;
                    return 0;
                }
            }

            AstNode *expr = parse_expression(&parent_block_node);
            if (!expr) return 1;

            if (tokens[toki].type != Token::TypeSemicolon) {
                if (tokens[toki].type == Token::TypeColon) {
                    // variable declaration
                    AstNode *decl = parse_variable_declaration(expr, parent_block_node);
                    if (decl == nullptr) return 1;
                    if (!ast_add_child(parent_block_node, *decl)) return 1;
                } else if (tokens[toki].type == Token::TypeEquals) {
                    if (!next_tok("assignment rvalue expression")) return 1;
                    AstNode& assign_stmt_node = ast_node_pool.add(
                        AstNode::TypeStatementAssign);
                    assign_stmt_node.start_tok = expr->start_tok;
                    assign_stmt_node.assign_lexpr = expr;
                    AstNode *expr = parse_expression(&parent_block_node);
                    if (!expr) return 1;
                    assign_stmt_node.assign_rexpr = expr;
                    if (!ast_add_child(parent_block_node, assign_stmt_node)) return 1;
                } else {
                    report_error(expr, "parse error: ");
                    printf("unexpected token %s\n", expr->start_tok->str_content.c_str());
                    return 1;
                }
            } else {
                AstNode& expr_stmt_node = ast_node_pool.add(AstNode::TypeStatementExpr);
                expr_stmt_node.start_tok = expr->start_tok;
                expr_stmt_node.stmt_expr = expr;
                if (!ast_add_child(parent_block_node, expr_stmt_node)) return 1;
            }
            if (!check_tok_type(Token::TypeSemicolon, "semicolon after statement")) return 1;
            if (!next_tok("procedure body continues or ends")) return 1;
            return 0;
        }

        int parse_procedure(AstNode& scope_node) {
            Token& start_tok = tokens[toki];
            if (!next_tok("procedure name")) return 1;
            bool is_public = false;
            if (tokens[toki].type == Token::TypeOperatorStar) {
                is_public = true;
                if (!next_tok("procedure name")) return 1;
            }
            if (!check_tok_type(Token::TypeName, "procedure name")) return 1;
            Token& proc_name_tok = tokens[toki];
            if (!next_tok("procedure args start")) return 1;
            if (!check_tok_type(Token::TypeParenOpen, "opening paren")) return 1;

            AstNode& proc_node = ast_node_pool.add(AstNode::TypeProcedureDefinition);
            proc_node.start_tok = &start_tok;
            proc_node.name_tok = &proc_name_tok;
            proc_node.parent_scope = &scope_node;
            proc_node.is_public = is_public;
            if (!ast_add_child(scope_node, proc_node)) return 1;

            // proc args
            if (!next_tok("procedure args or closing paren")) return 1;
            while (tokens[toki].type != Token::TypeParenClose) {
                // TODO: do we need varargs (...)?
                if (!check_tok_type(Token::TypeName, "arg name")) return 1;
                Token& arg_name_tok = tokens[toki];
                if (!next_tok("arg type (use colon)")) return 1;
                if (!check_tok_type(Token::TypeColon, "arg type (use colon)")) return 1;
                if (!next_tok("arg type specifier")) return 1;

                AstNode *arg_type_node = parse_type_ref();
                if (!arg_type_node) return 1;

                // TODO: do we need default values?
                AstNode& arg_node = ast_node_pool.add(AstNode::TypeVariableDeclaration);
                arg_node.start_tok = &arg_name_tok; // same as name
                arg_node.name_tok = &arg_name_tok;
                arg_node.var_type_ref = arg_type_node;
                if (!ast_add_child(proc_node, arg_node)) return 1;

                if (tokens[toki].type == Token::TypeComma) {
                    if (!next_tok("next proc arg")) return 1;
                    continue;
                }
                if (tokens[toki].type == Token::TypeParenClose) {
                    break;
                }
                Token& tok = tokens[toki];
                report_error(tok, "parse error: comma or closing paren expected");
                printf("got %s\n", tok.str_content.c_str());
                return 1;
            }

            if (!check_tok_type(Token::TypeParenClose, "closing paren")) return 1;
            // TODO: do we need optional return types? void?
            if (!next_tok("procedure return type spec (arrow)")) return 1;
            if (!check_tok_type(Token::TypeArrow, "procedure return type spec (arrow)")) return 1;
            if (!next_tok("procedure return type")) return 1;

            // TODO: multiple return types? Named returns?
            AstNode *ret_node = parse_type_ref();
            if (!ret_node) return 1;
            proc_node.proc_return_type_ref = ret_node;

            // TODO: do we need extern declarations? Syntax?
            if (!check_tok_type(Token::TypeCurlyOpen, "opening curly")) return 1;

            AstNode& proc_body_node = ast_node_pool.add(AstNode::TypeProcedureBody);
            proc_body_node.start_tok = &tokens[toki];
            proc_body_node.parent_scope = &proc_node;
            proc_node.proc_body = &proc_body_node;

            // proc body
            if (!next_tok("procedure body")) return 1;
            while (tokens[toki].type != Token::TypeCurlyClose) {
                int status = parse_statement(proc_body_node);
                if (status != 0) return status;
            }

            if (!check_tok_type(Token::TypeCurlyClose, "closing curly")) return 1;
            toki++;
            return 0;
        }

        int parse_type(AstNode& scope_node) {
            Token& start_tok = tokens[toki];
            if (!next_tok("type name")) return 1;
            if (!check_tok_type(Token::TypeName, "type name")) return 1;
            Token& type_name_tok = tokens[toki];
            if (!next_tok("type body starts")) return 1;
            if (!check_tok_type(Token::TypeCurlyOpen, "opening curly")) return 1;

            AstNode& type_node = ast_node_pool.add(AstNode::TypeTypeDefinition);
            type_node.start_tok = &start_tok;
            type_node.name_tok = &type_name_tok;
            type_node.parent_scope = &scope_node;
            if (!ast_add_child(scope_node, type_node)) return 1;

            // type body
            if (!next_tok("type body")) return 1;
            while(tokens[toki].type != Token::TypeCurlyClose) {
                // TODO: do we have anything besides fields in structs?
                if (!check_tok_type(Token::TypeName, "field name")) return 1;
                Token& field_name_tok = tokens[toki];
                if (!next_tok("field type (use colon)")) return 1;
                if (!check_tok_type(Token::TypeColon, "field type (use colon)")) return 1;
                if (!next_tok("field type specifier")) return 1;
                // TODO: do we have pointer types? What's the syntax?
                
                AstNode *member_type_node = parse_type_ref();
                if (!member_type_node) return 1;

                if (!check_tok_type(Token::TypeSemicolon, "semicolon after field")) return 1;
                if (!next_tok("type body continues or ends")) return 1;
                AstNode& member_node = ast_node_pool.add(AstNode::TypeTypeMember);
                member_node.start_tok = &field_name_tok; // same as name
                member_node.name_tok = &field_name_tok;
                member_node.member_type_ref = member_type_node;
                if (!ast_add_child(type_node, member_node)) return 1;
            }

            if (!check_tok_type(Token::TypeCurlyClose, "type curly")) return 1;
            toki++;
            return 0;
        }
    };
}

