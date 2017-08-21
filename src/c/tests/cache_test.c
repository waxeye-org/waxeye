/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../cache.h"
#include "../ht.h"

int main() {
    struct cache_key_t key = {.index = 0, .pos = 0};
    struct cache_key_t key2 = {.index = 7, .pos = 42};

    struct cache_value_t *v;
    struct cache_value_t *value;
    struct cache_value_t *value2;

    struct ht_t *cache = ht_new(1024);

    v = cache_get(cache, &key);
    assert(v == NULL);

    value = malloc(sizeof(struct cache_value_t));
    cache_put(cache, &key, value);
    assert(cache_get(cache, &key) == value);

    value2 = malloc(sizeof(struct cache_value_t));
    cache_put(cache, &key, value2);

    assert(cache_get(cache, &key) == value2);

    cache_put(cache, &key2, value2);
    assert(cache_get(cache, &key) == cache_get(cache, &key2));
    return 0;
}
