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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define AST_C_
#include "ast.h"

void ast_recursive_delete(struct ast_t *a);
void display_ast_iter(size_t indent, struct ast_t *a, const char *type_strings[]);


void ast_tree_init(struct ast_tree_t *t, size_t type, struct vector_t *children) {
    t->type = type;
    t->children = children;
}


struct ast_tree_t* ast_tree_new(size_t type, struct vector_t *children) {
    struct ast_tree_t *t = malloc(sizeof(struct ast_tree_t));
    assert(t != NULL);
    ast_tree_init(t, type, children);
    return t;
}


void ast_tree_clear(struct ast_tree_t *t) {
    vector_delete(t->children);
    t->children = NULL;
}


void ast_tree_delete(struct ast_tree_t *t) {
    ast_tree_clear(t);
    free(t);
}


void ast_tree_recursive_clear(struct ast_tree_t *t) {
    size_t i, len = t->children->size;

    for (i = 0; i < len; i++) {
        ast_recursive_delete(vector_get(t->children, i));
    }

    vector_delete(t->children);
    t->children = NULL;
}


void ast_tree_recursive_delete(struct ast_tree_t *t) {
    ast_tree_recursive_clear(t);
    free(t);
}


void ast_init(struct ast_t *a, enum ast_type type, union ast_data data) {
    a->type = type;
    a->data = data;
}


struct ast_t* ast_new(enum ast_type type, union ast_data data) {
    struct ast_t *a = malloc(sizeof(struct ast_t));
    assert(a != NULL);
    ast_init(a, type, data);
    return a;
}


void ast_clear(struct ast_t *a) {
    switch (a->type) {
        case AST_ERROR:
            free(a->data.error);
            a->data.error = NULL;
            break;
        case AST_CHAR:
            break;
        case AST_TREE:
            ast_tree_delete(a->data.tree);
            a->data.tree = NULL;
            break;
        default:
            break;
    }
}


void ast_delete(struct ast_t *a) {
    ast_clear(a);
    free(a);
}


void ast_recursive_clear(struct ast_t *a) {
    switch (a->type) {
        case AST_ERROR:
            free(a->data.error);
            a->data.error = NULL;
            break;
        case AST_CHAR:
            break;
        case AST_TREE:
            ast_tree_recursive_delete(a->data.tree);
            a->data.tree = NULL;
            break;
        default:
            break;
    }
}


void ast_recursive_delete(struct ast_t *a) {
    ast_recursive_clear(a);
    free(a);
}


void display_parse_error(struct ast_t *a) {
    if (a == NULL) {
        printf("NULL");
        return;
    }

    switch (a->type) {
        case AST_ERROR:
        {
            struct ast_error_t *error = a->data.error;
            printf("parse error: failed to match '' at line=%d, col=%d, pos=%d\n", error->line, error->col, error->pos);
            break;
        }
        default:
            break;
    }
}


void display_c(size_t indent, struct ast_t *a) {
    if (indent > 0) {
        printf("|   ");
    }

    printf("%c\n", a->data.c);
}


void display_tree(size_t indent, struct ast_t *a, const char *type_strings[]) {
    struct ast_tree_t *tree = a->data.tree;
    struct vector_t *children = tree->children;
    size_t i, len = children->size;

    if (indent > 0) {
        printf("->  ");
    }

    printf("%s\n", type_strings[tree->type]);

    for (i = 0; i < len; i++) {
        display_ast_iter(indent + 1, vector_get(children, i), type_strings);
    }
}


void display_ast_iter(size_t indent, struct ast_t *a, const char *type_strings[]) {
    size_t i;

    for (i = 1; i < indent; i++) {
        printf("    ");
    }

    switch (a->type) {
        case AST_CHAR:
        {
            display_c(indent, a);
            break;
        }
        case AST_TREE:
        {
            display_tree(indent, a, type_strings);
            break;
        }
        default:
            break;
    }
}


void display_ast(struct ast_t *a, const char *type_strings[]) {
    if (a == NULL) {
        printf("NULL");
        return;
    }

    switch (a->type) {
        case AST_ERROR:
        {
            display_parse_error(a);
            break;
        }
        case AST_CHAR:
        case AST_TREE:
        {
            display_ast_iter(0, a, type_strings);
            break;
        }
        default:
            break;
    }
}
