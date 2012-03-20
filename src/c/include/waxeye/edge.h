/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef EDGE_H_
#define EDGE_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <waxeye/trans.h>

struct edge_t {
    struct trans_t t;
    size_t s;
    bool v;
};

#ifndef EDGE_C_

extern void edge_init(struct edge_t *e, struct trans_t t, size_t s, bool v);
extern struct edge_t* edge_new(struct trans_t t, size_t s, bool v);
extern void edge_clear(struct edge_t *e);
extern void edge_delete(struct edge_t *e);

#endif /* EDGE_C_ */

#ifdef __cplusplus
}
#endif

#endif /* EDGE_H_ */
