/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008 Orlando D. A. R. Hill
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdio.h>
#include "parser.h"

// GNU libc extension
extern ssize_t getline(char **lineptr, size_t *n, FILE *stream);


void calc(struct parser_t *parser, struct input_t *input) {
    struct ast_t *ast = parse(parser, input);

    display_ast(ast, type_strings);
    ast_recursive_delete(ast);
}

struct input_t *rl() {
    size_t data_size = 0;
    char* data = NULL;
    ssize_t line_size = getline(&data, &data_size, stdin);

    if (line_size == -1) {
        free(data);
        return NULL;
    }

    return input_new(data, line_size);
}

int main() {
    struct input_t *input;
    struct parser_t *parser = parser_new();

    printf("calc> ");
    input = rl();

    while (input != NULL) {
        calc(parser, input);
        input_and_data_delete(input);
        printf("calc> ");
        input = rl();
    }

    printf("\n");
    parser_delete(parser);

    return 0;
}
