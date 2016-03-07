#include <iostream>
#include <fstream>
#include <vector>
#include <assert.h>

#include "hmlang.h"
#include "errors.h"
#include "lexer.h"

#include "parser.h"

#include "enrichment.h"

namespace driver {
    using std::printf;
    using std::string;
    using parser::AstNodePool;

    void synthesize_builtin_type_ast(
        AstNodePool& pool,
        AstNode& root,
        Builtin::Type type,
        const char *type_name
    ) {
        Token* tok = new Token(); // just leak that, why not?
        AstNode& node = pool.add(AstNode::TypeTypeRefBuiltin);
        node.builtin_type = type;
        tok->str_content = type_name;
        tok->type = Token::TypeName;
        node.name_tok = tok;
        parser::ast_add_child(root, node);
    }

    void synthesize_builtin_type_ast(AstNodePool& pool, AstNode& root) {
        synthesize_builtin_type_ast(pool, root, Builtin::Void, "void");
        synthesize_builtin_type_ast(pool, root, Builtin::I8, "i8");
        synthesize_builtin_type_ast(pool, root, Builtin::I32, "i32");
        synthesize_builtin_type_ast(pool, root, Builtin::Float32, "float32");
        synthesize_builtin_type_ast(pool, root, Builtin::Bool, "bool");
    }

    int main(int argc, char **argv) {
        if (argc < 3) {
            printf("Usage: %s <source file> <output dir>\n", argv[0]);
            return 1;
        }

        // That's just a starting file.
        // TODO: include/import or something
        string infilename(argv[1]);
        string outfilename = string(argv[2]) + infilename + string(".out");
        std::ifstream infile(infilename);

        printf("Lexing...\n");
        std::vector<Token> source_tokens;
        int lexer_status = lexer::tokenize_stream(infile, infilename.c_str(), source_tokens);
        if (lexer_status != 0) return lexer_status;

        AstNodePool ast_node_pool;
        // Builtin scope! This should be done once for the whole copilation unit
        AstNode& builtin_scope_node = ast_node_pool.add(AstNode::TypeGlobalScope);
        builtin_scope_node.parent_scope = nullptr;
        synthesize_builtin_type_ast(ast_node_pool, builtin_scope_node);
        // Global scope for a given compilation unit
        AstNode& global_scope_node = ast_node_pool.add(AstNode::TypeGlobalScope);
        global_scope_node.parent_scope = &builtin_scope_node;

        // Parsing result - is basically a filled AstNodePool.
        // Nodes inside can reference each other!
        printf("Parsing...\n");
        parser::Parsing parsing(source_tokens, ast_node_pool);
        int parser_status = parsing.parse_all(global_scope_node);
        if (parser_status != 0) return parser_status;

#if 0
        printf("Parsed:\n");
        enrichment::print_ast(&global_scope_node);
        printf("\n");
#endif

        // On this point code does make sense syntactically,
        // but references types and procedures by name and
        // lacks some information required to do code_gen on it.
        printf("Enrichment...\n");
        int enrich_status = enrichment::enrich_all(&global_scope_node);
        if (enrich_status != 0) return enrich_status;

#if 0
        printf("Enriched:\n");
        enrichment::print_ast(&global_scope_node);
        printf("\n");
#endif

        printf("Generating intermediate code...\n");
        CodeGenState *code_gen = code_gen_init();
        code_gen_all(code_gen, &global_scope_node);

#if 0
        printf("Codegen:\n");
        code_gen_print_result(code_gen);
        printf("\n");
#endif
        
        string buf;
        code_gen_output_result(code_gen, buf);
        {
            std::ofstream of(outfilename);
            of << buf;
        }
        return 0;
    }
}

