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
