/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef AST_H_
#define AST_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <waxeye/vector.h>

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
    size_t start;
    size_t end;
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

extern void ast_tree_init(struct ast_tree_t *t, size_t type, struct vector_t *children, size_t start, size_t end);
extern struct ast_tree_t* ast_tree_new(size_t type, struct vector_t *children, size_t start, size_t end);
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
extern char *ast_children_as_string(struct ast_t *a);

#endif /* AST_C_ */

#ifdef __cplusplus
}
#endif

#endif /* AST_H_ */
