/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include "parser.h"

int main() {
    // Create our parser
    struct parser_t *parser = parser_new();

    // Setup our input
    FILE *fp = stdin;
    struct input_t *input = input_from_file_new(fp);
    pclose(fp);

    // Parse our input
    struct ast_t *ast = parse(parser, input);

    // Print our ast
    display_ast(ast, type_strings);

    ast_recursive_delete(ast);
    input_and_data_delete(input);
    parser_delete(parser);

    return 0;
}
