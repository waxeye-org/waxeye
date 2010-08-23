/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include <assert.h>
#include <stdlib.h>

#define STATE_C_
#include <waxeye/state.h>


void state_init(struct state_t *a, struct edge_t *edges, size_t num_edges, char match) {
    a->edges = edges;
    a->num_edges = num_edges;
    a->match = match;
}


struct state_t* state_new(struct edge_t *edges, size_t num_edges, char match) {
    struct state_t *a = malloc(sizeof(struct state_t));
    assert(a != NULL);
    state_init(a, edges, num_edges, match);
    return a;
}


void state_clear(struct state_t *a) {
    size_t i;

    for (i = 0; i < a->num_edges; i++) {
        edge_clear(&a->edges[i]);
    }

    free(a->edges);
    a->edges = NULL;
}


void state_delete(struct state_t *a) {
    state_clear(a);
    free(a);
}
