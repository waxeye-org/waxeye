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
