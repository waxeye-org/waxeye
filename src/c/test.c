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
