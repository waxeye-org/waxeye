/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include <string.h>
#include "parser.h"

int main() {
    // Create our parser
    struct parser_t *parser = parser_new();

    // Setup our input
    char data[] = "42";
    struct input_t *input = input_new(data, strlen(data));

    // Parse our input
    struct ast_t *ast = parse(parser, input);

    // Print our ast
    display_ast(ast, type_strings);

    ast_recursive_delete(ast);
    input_delete(input);
    parser_delete(parser);

    return 0;
}
