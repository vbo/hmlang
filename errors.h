namespace errors {
    void report_error(Token *tok, const char *message = "") {
        printf("%s:%d:%d: %s", tok->filename, tok->line_number, tok->column_number, message);
    }

    void report_error(Token &tok, const char *message = "") {
        report_error(&tok, message);
    }
    void report_error(AstNode *node, const char *message = "") {
        report_error(node->start_tok, message);
    }
    void report_error(AstNode &node, const char *message = "") {
        report_error(node.start_tok, message);
    }
}
