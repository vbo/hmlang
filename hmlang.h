#include <vector>
#include <map>
#include <string>

namespace Builtin {
    enum Type {
        TypeUnknown,
        Void,
        I8,
        I32,
        Float32,
        Bool,
        TypeLast
    };
    enum Op {
        OpUnknown,
        // Unsigned-float operations are inefficient, so we don't provide them!
        AddFloat,
        SubFloat,
        MulFloat,
        DivFloat,
        EqFloat,
        // for two's complement code gen most integer operations are the same for signed and unsigned
        AddInt,
        SubInt,
        MulInt,
        EqInt,
        // Integer division is different for signed and unsigned
        DivIntSigned,
        DivIntUnsigned,

        OpLast
    };

    Op ret_bool[] = {EqInt, EqFloat};

    std::string key_void = "void";
    std::string key_i8 = "i8";
    std::string key_i32 = "i32";
    std::string key_float32 = "float32";
    std::string key_bool = "bool";
}

struct Token {
    enum Type {
        TypeUnknown,
        TypeName,
        // TODO: do we like brackets in separate types?
        TypeCurlyOpen, TypeCurlyClose,
        TypeParenOpen, TypeParenClose,
        TypeLiteralNumber,
        TypeOperatorDot,
        TypeOperatorMinus,
        TypeOperatorPlus,
        TypeOperatorStar,
        TypeOperatorSlash,
        TypeOperatorDoubleEquals,
        TypeOperatorAmpersand,
        TypeComma,
        TypeArrow,
        TypeSemicolon,
        TypeColon,
        TypeEquals,
        TypeLast
    } type;

    bool literal_number_is_float;
    union {
        float literal_number_float_value;
        int literal_number_int_value;
    };
    std::string str_content; // TODO: do we really need string here? PODs are cool!
    int line_number;
    int column_number;
};

struct AstNode {
    // TODO: function type definition:
    //  - type NoArgsVoid = () -> void
    //  - type FloatToFloat = (float32) -> float32
    //  Then you can fn FloatToFloat(x) { return x*x; }
    //  If you later modify func type you have errors on funcs using it.
    enum Type {
        TypeUnknown,
        TypeTypeDefinition,
        TypeTypeMember,
        TypeProcedureDefinition,
        TypeVariableDeclaration,
        TypeProcedureBody,
        TypeStatementIf,
        TypeStatementRepeat,
        TypeStatementBreak,
        TypeStatementReturn,
        TypeStatementAssign,
        TypeStatementBlock,
        TypeStatementExpr,
        TypeExpressionName,
        TypeExpressionCall,
        TypeExpressionBinOp,
        TypeExpressionMemberOf,
        TypeExpressionAddressOf,
        TypeExpressionDereference,
        TypeExpressionLiteralNumber,
        TypeGlobalScope,
        // TODO: do we need a separate structure for these?
        TypeTypeRefName, // we set resolved_type_name during enrichment
        TypeTypeRefPointer,
        TypeTypeRefBuiltin
    } type;

    union {
        struct { // TypeTypeDefinition
            // uses child_nodes for members
            // has parent_scope
        };
        struct { // TypeTypeMember
            AstNode *member_type_ref;
            int member_index;
        };
        struct { // TypeProcedureDefinition
            AstNode *proc_return_type_ref;
            AstNode *proc_body;
            bool is_public;
            // uses child_nodes for arguments
            // has parent_scope
        };
        struct { // TypeVariableDeclaration
            AstNode *var_type_ref;
            AstNode *var_init_expr;
            bool var_decl_enriched;
        };
        // TODO: proc body and a block are the same?
        struct { // TypeProcedureBody
            // uses child_nodes for statements
            // has parent_scope
        };
        struct { // TypeStatementBlock
            // uses child_nodes for statements
            // has parent_scope
        };
        struct { // TypeStatementReturn
            AstNode *ret_expr;
        };
        struct { // TypeStatementIf
            AstNode *if_cond_expr;
            AstNode *if_then_stmt;
            AstNode *if_else_stmt;
        };
        struct { // TypeStatementRepeat
            AstNode *repeat_stmt;
        };
        struct { // TypeStatementBreak
            AstNode *break_loop;
        };
        struct { // TypeStatementExpr
            AstNode *stmt_expr;
        };
        struct { // TypeStatementAssign
            AstNode *assign_lexpr;
            AstNode *assign_rexpr;
        };
        struct { // TypeExpressionName
            AstNode* resolved_var;
            // has inferred_type_ref
        };
        struct { // TypeExpressionCall
            AstNode *call_proc_def;
            // uses child_nodes for arguments
            // has inferred_type_ref
        };
        struct { // TypeExpressionBinOp
            AstNode *bin_op_lexpr;
            AstNode *bin_op_rexpr;
            Builtin::Op builtin_op;
            // has inferred_type_ref
        };
        struct { // TypeExpressionMemberOf
            AstNode *memberof_base_var;
            Token *memberof_member_name_tok;
            AstNode *memberof_member;
            // has inferred_type_ref
        };
        struct { // TypeExpressionAddressOf
            AstNode *addressof_expr;
            // just a holder: 
            AstNode *addressof_type_ref; // don't use if you are nor enriching!
            // has inferred_type_ref
        };
        struct { // TypeExpressionDereference
            AstNode *deref_expr;
        };
        struct { // TypeExpressionLiteralNumber,
            Token *expr_literal_number_tok;
            // has inferred_type_ref
        };
        struct { // TypeGlobalScope
            // has parent_scope
        };
        struct { // TypeTypeRefName
            AstNode *resolved_type_ref;
        };
        struct { // TypeTypeRefPointer
            AstNode *pointee_type_ref;
        };
        struct { // TypeTypeRefBuiltin
            Builtin::Type builtin_type;
        };
    };

    // These are used by most of the node types we have :)
    Token *start_tok;
    Token *name_tok;
    AstNode *parent_scope;
    AstNode *inferred_type_ref;
    std::vector<AstNode*> child_nodes;
    std::map<std::string, std::vector<AstNode*>> child_lookup;
    bool expr_yields_nontemporary;

    // used to store various kinds of code gen info =)
    void *code_gen_ref;
    bool code_gen_done;
};

struct CodeGenState;
CodeGenState* code_gen_init();
void code_gen_all(CodeGenState *code_gen, AstNode *root);
void code_gen_print_result(CodeGenState *code_gen);
void code_gen_output_result(CodeGenState *code_gen, std::string& out_buf);

