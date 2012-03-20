/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef STATE_H_
#define STATE_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <waxeye/edge.h>

struct state_t {
    struct edge_t *edges;
    size_t num_edges;
    bool match;
};

#ifndef STATE_C_

extern void state_init(struct state_t *s, struct edge_t *edges, size_t num_edges, char match);
extern struct state_t* state_new(struct edge_t *edges, size_t num_edges, char match);
extern void state_clear(struct state_t *s);
extern void state_delete(struct state_t *s);

#endif /* STATE_C_ */

#ifdef __cplusplus
}
#endif

#endif /* STATE_H_ */
