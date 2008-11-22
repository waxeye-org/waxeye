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

/*
 * A commandline arithmetic calculator.
 */
#include <stdio.h>
#include "parser.h"

double sum(struct ast_t *ast);

// GNU libc extension
extern ssize_t getline(char **lineptr, size_t *n, FILE *stream);


double num(struct ast_t *ast) {
    char *buf = ast_children_as_string(ast);
    double val = atof(buf);
    free(buf);
    return val;
}


double unary(struct ast_t *ast) {
    struct ast_tree_t *tree = ast->data.tree;
    switch (tree->type) {
        case TYPE_UNARY:
            return - unary(vector_get(tree->children, 1));
        case TYPE_SUM:
            return sum(ast);
        default:
            return num(ast);
    }
}


double prod(struct ast_t *ast) {
    struct vector_t *chil = ast->data.tree->children;
    size_t num_chil = chil->size;
    double val = unary(vector_get(chil, 0));

    size_t i;
    for (i = 1; i < num_chil; i +=2) {
        char operator = ((struct ast_t*) vector_get(chil, i))->data.c;
        double operand = unary(vector_get(chil, i + 1));

        if (operator == '*') {
            val *= operand;
        }
        else {
            val /= operand;
        }
    }

    return val;
}


double sum(struct ast_t *ast) {
    struct vector_t *chil = ast->data.tree->children;
    size_t num_chil = chil->size;
    double val = prod(vector_get(chil, 0));

    size_t i;
    for (i = 1; i < num_chil; i +=2) {
        char operator = ((struct ast_t*) vector_get(chil, i))->data.c;
        double operand = prod(vector_get(chil, i + 1));

        if (operator == '+') {
            val += operand;
        }
        else {
            val -= operand;
        }
    }

    return val;
}


void calc(struct parser_t *parser, struct input_t *input) {
    struct ast_t *ast = parse(parser, input);

    if (ast->type == AST_ERROR) {
        display_ast(ast, type_strings);
    }
    else {
        printf("%f\n", sum(vector_get(ast->data.tree->children, 0)));
    }

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
