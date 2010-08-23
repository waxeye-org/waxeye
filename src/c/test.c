/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cache.h"

int main() {

    struct cache_key_t key = {.index = 0, .pos = 0};
    struct cache_key_t key2 = {.index = 7, .pos = 42};

    struct cache_value_t *v;
    struct cache_value_t *value;
    struct cache_value_t *value2;

    struct cache_t *cache = cache_new(1024);
    printf("created cache\n");

    v = cache_get(cache, &key);
    printf("%d get not there cache\n", (int)v);

    value = malloc(sizeof(struct cache_value_t));
    cache_put(cache, &key, value);
    printf("put value\n");

    v = cache_get(cache, &key);
    printf("%d get value\n", v == value);

    value2 = malloc(sizeof(struct cache_value_t));
    cache_put(cache, &key, value2);
    printf("put value2\n");

    v = cache_get(cache, &key);
    printf("%d get value2\n", v == value2);

    cache_put(cache, &key2, value2);
    printf("put value2 under different key\n");

    printf("%d get value2 under each key\n", cache_get(cache, &key) == cache_get(cache, &key2));

    return 0;
}
