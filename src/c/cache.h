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
