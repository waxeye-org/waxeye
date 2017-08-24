/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

// Reads and parses the input from stdin and prints the AST.
// This is used to run e2e tests on the AST.
// This file is copied to the build directory and compiled together with a parser at test time.

#define _XOPEN_SOURCE 500  // for pclose

#include <stdio.h>
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
