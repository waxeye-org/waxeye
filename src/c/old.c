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

#include <stdlib.h>
#include <string.h>

#define OLD_C_
#include "old.h"

int name(struct input_t *b) {
    if (input_eof(b)) {
        return 0;
    }

    char c = input_peek(b);

    if (c == ' ' || c == ')') {
        return 0;
    }

    while (!input_eof(b) && c != ' ' && c != ')' && c != '\n') {
        input_consume(b);
        c = input_peek(b);
    }

    return 1;
}

struct ast_t* _char(struct input_t *b) {
    if (input_eof(b)) {
        return NULL;
    }

    union ast_data d = { .c = input_consume(b) };
    return ast_new(CHAR, d);
}

struct ast_t* tree(struct input_t *b) {
    int start = b->pos;

    if (!input_eof(b) && input_peek(b) == '(') {
        input_consume(b);
    }
    else {
        return NULL;
    }

    if (!name(b)) {
        b->pos = start;
        return NULL;
    }

    size_t nlen = b->pos - (start + 1);
    char *c = calloc(nlen + 1, sizeof(char));
    memcpy(c, &b->data[start + 1], nlen);

    struct ast_tree_t *t = ast_tree_new(c, vector_new(INIT_CHILDREN));
    union ast_data d = { .tree = t };
    struct ast_t *a = ast_new(TREE, d);

    while (1) {
        int start2 = b->pos;

        if (!input_eof(b) && input_peek(b) == ' ') {
            input_consume(b);
        }
        else {
            break;
        }

        if (input_eof(b)) {
            b->pos = start2;
            break;
        }

        struct ast_t *child = tree(b);

        if (child == NULL) {
            child = _char(b);

            if (child == NULL) {
                b->pos = start2;
                break;
            }
            else {
                vector_add(t->children, child);
            }
        }
        else {
            vector_add(t->children, child);
        }
    }

    if (!input_eof(b) && input_peek(b) == ')') {
        input_consume(b);
    }
    else {
        b->pos = start;
        ast_delete(a);
        return NULL;
    }

    return a;
}

struct ast_t* error(struct input_t *b) {
    if (input_eof(b)) {
        return NULL;
    }

    int i;
    char *c;
    size_t start, nlen;
    struct ast_error_t *e = malloc(sizeof(struct ast_error_t));

    for (i = 0; i < 13; i++) {
        // skip "Error at pos:"
        input_consume(b);
    }

    start = b->pos;
    name(b);
    nlen = b->pos - start;
    c = calloc(nlen + 1, sizeof(char));
    memcpy(c, &b->data[start], nlen);
    e->pos = atoi(c);
    free(c);

    for (i = 0; i < 6; i++) {
        // skip " line:"
        input_consume(b);
    }

    start = b->pos;
    name(b);
    nlen = b->pos - start;
    c = calloc(nlen + 1, sizeof(char));
    memcpy(c, &b->data[start], nlen);
    e->line = atoi(c);
    free(c);

    for (i = 0; i < 5; i++) {
        // skip " col:"
        input_consume(b);
    }

    start = b->pos;
    name(b);
    nlen = b->pos - start;
    c = calloc(nlen + 1, sizeof(char));
    memcpy(c, &b->data[start], nlen);
    e->col = atoi(c);
    free(c);

    union ast_data d = { .error = e };
    return ast_new(ERROR, d);
}

struct ast_t* old(struct input_t *b) {
    struct ast_t* res = tree(b);

    if (res == NULL) {
        b->pos = 0;
        res = error(b);
    }

    if (!input_eof(b) && input_peek(b) == '\n') {
        input_consume(b);
    }
    else {
        return NULL;
    }

    return b->pos == b->size ? res : NULL;
}
