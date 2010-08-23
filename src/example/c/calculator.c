/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
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
