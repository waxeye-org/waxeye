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

    // Free the pairs of the hash table
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
