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

#ifndef AST_H_
#define AST_H_

#include "vector.h"

#define INIT_CHILDREN 8

enum ast_type {
    AST_CHAR,
    AST_EMPTY,
    AST_ERROR,
    AST_TREE
};

struct ast_error_t {
    size_t pos;
    size_t line;
    size_t col;
    size_t nt;
};

struct ast_tree_t {
    size_t type;
    struct vector_t *children;
};

union ast_data {
    char c;
    struct ast_error_t *error;
    struct ast_tree_t *tree;
};

struct ast_t {
    enum ast_type type;
    union ast_data data;
};

#ifndef AST_C_

extern void ast_tree_init(struct ast_tree_t *t, size_t type, struct vector_t *children);
extern struct ast_tree_t* ast_tree_new(size_t type, struct vector_t *children);
extern void ast_tree_clear(struct ast_tree_t *t);
extern void ast_tree_delete(struct ast_tree_t *t);
extern void ast_tree_recursive_clear(struct ast_tree_t *t);
extern void ast_tree_recursive_delete(struct ast_tree_t *t);

extern void ast_init(struct ast_t *a, enum ast_type type, union ast_data data);
extern struct ast_t* ast_new(enum ast_type type, union ast_data data);
extern void ast_clear(struct ast_t *a);
extern void ast_delete(struct ast_t *a);
extern void ast_recursive_clear(struct ast_t *a);
extern void ast_recursive_delete(struct ast_t *a);

extern void display_ast(struct ast_t *a, const char *type_strings[]);

#endif /* AST_C_ */
#endif /* AST_H_ */
