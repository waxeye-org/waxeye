/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include <assert.h>
#include <stdlib.h>

#define HT_C_
#include "ht.h"

/*
 * Initializes the given hash table.
 * Each pair in the hash table is initially NULL.
 * v - The hash table to init.
 * capacity - The initial capacity.
 */
void ht_init(struct ht_t *v, size_t capacity) {
    v->pairs = calloc(capacity, sizeof(void*));
    assert(v->pairs != NULL);
    v->capacity = capacity;
    v->size = 0;
}

/*
 * Creates a new hash table with the given capacity.
 */
struct ht_t* ht_new(size_t capacity) {
    struct ht_t *v = malloc(sizeof(struct ht_t));
    assert(v != NULL);
    ht_init(v, capacity);
    return v;
}

/*
 * Frees the storage of the hash table but not the values stored.
 * v - The hash table to free.
 * keys - If the keys should be freed as pointers.
 */
void ht_clear(struct ht_t *v, bool keys) {
    size_t i;

    /* Free the pairs of the hash table */
    for (i = 0; i < v->capacity; i++) {
        struct ht_pair_t *pair = v->pairs[i];

        if (pair != NULL) {
            if (keys) {
                free(pair->key.as_p);
            }

            free(pair);
        }
    }

    free(v->pairs);
    v->pairs = NULL;
}

/*
 * Clears the hash table and deallocates the structure.
 * v - The hash table to delete.
 * keys - If the keys should be freed as pointers.
 */
void ht_delete(struct ht_t *v, bool keys) {
    ht_clear(v, keys);
    free(v);
}
