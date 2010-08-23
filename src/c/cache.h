/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef CACHE_H_
#define CACHE_H_

#include <waxeye/ast.h>
#include "ht.h"


struct cache_key_t {
    /* The index of the expression being cached. */
    size_t index;

    /* The position it was evaluated at. */
    size_t pos;
};


struct cache_value_t {
    struct ast_t *result;
    size_t pos;
    size_t line;
    size_t column;
    bool last_cr;
};


#ifndef CACHE_C_

extern void cache_value_init(struct cache_value_t *v, struct ast_t *result, size_t pos, size_t line, size_t column, bool last_cr);
extern struct cache_value_t* cache_value_new(struct ast_t *result, size_t pos, size_t line, size_t column, bool last_cr);
extern void cache_value_delete(struct cache_value_t *v);

extern struct cache_value_t* cache_get(struct ht_t *v, struct cache_key_t *key);
extern void cache_put(struct ht_t *v, struct cache_key_t *key, struct cache_value_t *value);

#endif /* CACHE_C_ */
#endif /* CACHE_H_ */
