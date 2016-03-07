namespace lexer {
    Token& make_token(std::vector<Token>& tokens, Token::Type tok_type, int line_number, int column_number) {
        tokens.emplace_back();
        Token &tok = tokens.back();
        tok.type = tok_type;
        tok.line_number = line_number;
        tok.column_number = column_number;
        return tok;
    }

    int tokenize_stream(std::istream& infile, std::vector<Token>& tokens) {
        std::string line;
        int line_number = 0;
        while(std::getline(infile, line)) {
            line_number++;
            int i = 0;
            while (i < line.size()) {
                char ch = line[i];
                // skip empty space
                if (std::isspace(ch)) {
                    i++;
                    continue;
                }
                // identifier/keyword
                if (ch == '_' || (!std::ispunct(ch) && !std::isdigit(ch))) {
                    Token& tok = make_token(tokens, Token::TypeName, line_number, i + 1);
                    int name_start_index = i;
                    while(true) {
                        i++;
                        if (i >= line.size()) break;
                        char name_ch = line[i];
                        if (std::isspace(name_ch)) break;
                        if (name_ch == '_' || !std::ispunct(name_ch)) continue;
                        break;
                    }
                    int name_len = i - name_start_index;
                    tok.str_content = line.substr(name_start_index, name_len);
                    continue;
                }
                // digits
                if (std::isdigit(ch)) {
                    // TODO: think about number literals!
                    bool dot = false;
                    Token& tok = make_token(tokens, Token::TypeLiteralNumber, line_number, i + 1);
                    int lit_start_index = i;
                    while(true) {
                        i++;
                        if (i >= line.size()) break;
                        char lit_ch = line[i];
                        if (std::isspace(lit_ch)) break;
                        if (lit_ch == '.') {
                            if (dot) {
                                printf("Lexer error: invalid number literal on line %d:%d\n", tok.line_number, tok.column_number);
                                return 1;
                            }
                            dot = true;
                            continue;
                        }
                        if (std::isdigit(lit_ch)) continue;
                        break;
                    }
                    int lit_len = i - lit_start_index;
                    tok.str_content = line.substr(lit_start_index, lit_len);
                    tok.literal_number_is_float = dot;
                    if (tok.literal_number_is_float) {
                        tok.literal_number_float_value = std::stof(
                            tok.str_content.c_str());
                    } else {
                        tok.literal_number_int_value = std::stoi(
                            tok.str_content.c_str());
                    }
                    continue;
                }
                // punctuation
                if (std::ispunct(ch)) {
                    // comments
                    if (ch == '/') {
                        if (i + 1 < line.size() && line[i + 1] == '/') {
                            // line comment
                            i += 2;
                            while(i < line.size() && line[i] != '\n') {
                                i++;
                            }
                            continue;
                        }
                    }
                    Token& tok = make_token(tokens, Token::TypeUnknown, line_number, i + 1);
                    if (ch == '-') {
                        // Could be just minus or arrow 
                        // TODO: do we need decrement? Minus-equals?
                        if (i < line.size() - 1 && line[i + 1] == '>') {
                            tok.type = Token::TypeArrow;
                            tok.str_content = "->";
                            i += 2;
                        } else {
                            tok.type = Token::TypeOperatorMinus;
                            tok.str_content = "-";
                            i++;
                        }
                        continue;
                    }
                    if (ch == '=') {
                        if (i + 1 < line.size() && line[i + 1] == '=') {
                            tok.type = Token::TypeOperatorDoubleEquals;
                            tok.str_content = "==";
                            i += 2;
                        } else {
                            tok.type = Token::TypeEquals;
                            tok.str_content = "=";
                            i++;
                        }
                        continue;
                    }
                    // The rest is just simple one-char operators
                    if (ch == '{') {
                        tok.type = Token::TypeCurlyOpen;
                    } else if (ch == '}') {
                        tok.type = Token::TypeCurlyClose;
                    } else if (ch == '(') {
                        tok.type = Token::TypeParenOpen;
                    } else if (ch == ')') {
                        tok.type = Token::TypeParenClose;
                    } else if (ch == ';') {
                        tok.type = Token::TypeSemicolon;
                    } else if (ch == '#') {
                        tok.type = Token::TypePound;
                    } else if (ch == '&') {
                        tok.type = Token::TypeOperatorAmpersand;
                    } else if (ch == '*') {
                        tok.type = Token::TypeOperatorStar;
                    } else if (ch == '.') {
                        tok.type = Token::TypeOperatorDot;
                    } else if (ch == ',') {
                        tok.type = Token::TypeComma;
                    } else if (ch == '+') {
                        // TODO: do we need increment and plus-equals?
                        tok.type = Token::TypeOperatorPlus;
                    } else if (ch == ':') {
                        tok.type = Token::TypeColon;
                    } else if (ch == '/') {
                        // comments are handled on top
                        tok.type = Token::TypeOperatorSlash;
                    } else {
                        printf("Lexer error: invalid token on line %d:%d\n", line_number, i + 1);
                        return 1;
                    }
                    tok.str_content = ch;
                    i++;
                    continue;
                }
                printf("Lexer error: invalid token on line %d:%d\n", line_number, i + 1);
                return 1;
            }
        }

#if 0
        printf("Got %lu tokens\n", tokens.size());
        for (int i = 0; i < tokens.size(); ++i) {
            Token *tok = &tokens[i];
            printf("TOK (%d, %d): %d, %s\n", tok->line_number, tok->column_number, tok->type, tok->str_content.c_str());
        }
#endif

        return 0;
    }
}
