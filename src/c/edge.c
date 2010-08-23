/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include <assert.h>
#include <stdlib.h>

#define EDGE_C_
#include <waxeye/edge.h>


void edge_init(struct edge_t *a, struct trans_t t, size_t s, bool v) {
    a->t = t;
    a->s = s;
    a->v = v;
}


struct edge_t* edge_new(struct trans_t t, size_t s, bool v) {
    struct edge_t *a = malloc(sizeof(struct edge_t));
    assert(a != NULL);
    edge_init(a, t, s, v);
    return a;
}


void edge_clear(struct edge_t *a) {
    trans_clear(&a->t);
}


void edge_delete(struct edge_t *a) {
    edge_clear(a);
    free(a);
}
