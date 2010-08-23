/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef HT_H_
#define HT_H_

#include <stdbool.h>

/* A hash table key can either be a pointer or a literal value. */
union ht_key_t {
    void *as_p;
    size_t as_i;
};


struct ht_pair_t {
    union ht_key_t key;
    void *value;
};


struct ht_t {
    struct ht_pair_t **pairs;

    /* The total available storage space. */
    size_t capacity;

    /* The number of spaces occupied. */
    size_t size;
};


#ifndef HT_C_

extern void ht_init(struct ht_t *v, size_t capacity);
extern struct ht_t* ht_new(size_t capacity);
extern void ht_clear(struct ht_t *v, bool keys);
extern void ht_delete(struct ht_t *v, bool keys);

#endif /* HT_C_ */
#endif /* HT_H_ */
