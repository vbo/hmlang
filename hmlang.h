#include <vector>
#include <map>
#include <string>

namespace Builtin {
    enum Type {
        TypeUnknown,
        Void,
        S8, S16, S32, S64,
        U8, U16, U32, U64,
        Float32, Float64,
        Bool,
        TypeLast
    };

    enum Op {
        OpUnknown,
        // Unsigned-float operations are inefficient, so we don't provide them!
        AddFloat, SubFloat,
        MulFloat, DivFloat,
        EqFloat, NeFloat, GtFloat, LtFloat,
        // for two's complement code_gen some integer operations are the same
        // for signed and unsigned integers
        AddInt, SubInt,
        MulInt,
        EqInt, NeInt,
        // Integer division is different for signed and unsigned
        DivIntSigned, DivIntUnsigned,
        GtIntSigned, GtIntUnsigned,
        LtIntSigned, LtIntUnsigned,
        // Bool - for now assume we don't know bool code_gen representation
        AndBool, OrBool,
        OpLast
    };

    // TODO: maybe reimplement operators as builtin procs?
    Op ret_bool[] = {
        EqInt, NeInt,
        GtIntSigned, GtIntUnsigned,
        LtIntSigned, LtIntUnsigned,
        EqFloat, NeFloat,
        GtFloat, LtFloat
    };

    std::string key_void = "void";

    std::string key_s8 = "s8";
    std::string key_s16 = "s16";
    std::string key_s32 = "s32";
    std::string key_s64 = "s64";

    std::string key_u8 = "u8";
    std::string key_u16 = "u16";
    std::string key_u32 = "u32";
    std::string key_u64 = "u64";

    std::string key_float32 = "float32";
    std::string key_float64 = "float64";

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
        TypeOperatorBangEquals,
        TypeOperatorBang,
        TypeOperatorPipe,
        TypeOperatorDoublePipe,
        TypeOperatorGreater,
        TypeOperatorLess,
        TypeOperatorAmpersand,
        TypeOperatorDoubleAmpersand,
        TypeComma,
        TypeArrow,
        TypeSemicolon,
        TypePound,
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
    char *filename;
    int line_number;
    int column_number;
};

struct AstNode {
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
        TypeExpressionPoundRun,
        TypeGlobalScope,
        TypeTypeRefName,
        TypeTypeRefPointer,
        TypeTypeRefBuiltin
    } type;

    union {
        struct { // TypeTypeDefinition
            bool type_def_members_enrichment_done;
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
            bool proc_body_enriched;
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
        struct { // TypeExpressionPoundRun
            AstNode *pound_run_expr;
            // has parent scope
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

    // Linked list used to hold ordered "child" nodes:
    // statements in a block, arguments of proc def/call
    AstNode* child_nodes_list;
    AstNode* child_node_last;
    AstNode* child_node_next;
    size_t child_nodes_count;

    // TODO: named child lookup table
    // TODO: proc overload lookup table?

    bool expr_yields_nontemporary;

    // used to store various kinds of code gen info =)
    void *code_gen_ref;
    bool code_gen_done;
};

#define AST_FOREACH_CHILD(it, parent) \
    for ( \
        auto it = (parent)->child_nodes_list; \
        it; \
        it = it->child_node_next) \

// CodeGen plugin interop
struct CodeGenState;
CodeGenState* code_gen_init();
void code_gen_all(CodeGenState *code_gen, AstNode *root);
void code_gen_run_expression(CodeGenState *code_gen, AstNode *expr);
void code_gen_print_result(CodeGenState *code_gen);
void code_gen_output_result(CodeGenState *code_gen, std::string& out_buf);
