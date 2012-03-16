/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include <assert.h>
#include <stdlib.h>

#define LT_C_
#include "lt.h"


size_t lt_hash_key(size_t key) {
    return 17 + 37 * key;
}


/*
 * Gets the position in the hash table that the key maps to.
 * Doesn't consider hash collisions.
 */
size_t lt_start_pos(struct ht_t *v, size_t key) {
    return abs(lt_hash_key(key) % v->capacity);
}


/*
 * Gets the position that the key maps to after hash collisions are resolved.
 */
size_t lt_final_pos(struct ht_t *v, size_t key) {
    size_t offset = 0;
    size_t pos = lt_start_pos(v, key);
    struct ht_pair_t *pair = v->pairs[pos];

    while (true) {
        /* If the position is empty */
        if (pair == NULL) {
            return pos;
        }

        /* If we found our key */
        if (pair->key.as_i == key) {
            return pos;
        }

        offset++;
        pos = abs((pos + offset * offset) % v->capacity);
        pair = v->pairs[pos];
    }
}


/*
 * Gets the value that is mapped to the given key. If there is no mapping then
 * NULL is returned.
 */
void* lt_get(struct ht_t *v, size_t key) {
    struct ht_pair_t *pair = v->pairs[lt_final_pos(v, key)];

    if (pair == NULL) {
        return NULL;
    }

    return pair->value;
}


/*
 * Maps the given value against the given key. The value is allowed to be NULL.
 * If a value has previously been mapped against an equal key, the old mapping
 * will be overwritten.
 */
void lt_put(struct ht_t *v, size_t key, void *value) {
    const float LOAD_FACTOR = 0.75;
    size_t pos;
    struct ht_pair_t *pair;

    assert(v != NULL);

    /* resize and rehash if needed */
    if (v->size >= v->capacity * LOAD_FACTOR) {
        size_t i;
        size_t old_capacity = v->capacity;
        struct ht_pair_t **old_pairs = v->pairs;

        v->capacity = v->capacity * 2;
        assert(v->capacity > v->size);

        v->pairs = calloc(v->capacity, sizeof(void*));
        assert(v->pairs != NULL);

        /* rehash the pairs */
        for (i = 0; i < old_capacity; i++) {
            pair = old_pairs[i];

            if (pair != NULL) {
                v->pairs[lt_final_pos(v, pair->key.as_i)] = pair;
            }
        }

        /* free old storage */
        free(old_pairs);
    }

    pos = lt_final_pos(v, key);
    pair = v->pairs[pos];

    /* if a value didn't already exist with that key */
    if (pair == NULL) {
        /* create a new pair */
        pair = malloc(sizeof(struct ht_pair_t));
        assert(pair != NULL);

        v->pairs[pos] = pair;
        v->size++;

        pair->key.as_i = key;
    }

    /* store the value */
    pair->value = value;
}
