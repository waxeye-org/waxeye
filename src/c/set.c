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

#define SET_C_
#include "set.h"


void set_init(struct set_t *s, char single[], size_t num_single, char min[], char max[], size_t num_range) {
    s->single = single;
    s->num_single = num_single;
    s->min = min;
    s->max = max;
    s->num_range = num_range;
}


struct set_t* set_new(char single[], size_t num_single, char min[], char max[], size_t num_range) {
    struct set_t *a = malloc(sizeof(struct set_t));
    assert(a != NULL);
    set_init(a, single, num_single, min, max, num_range);
    return a;
}


void set_clear(struct set_t *a) {
    free(a->single);
    a->single = NULL;
    free(a->min);
    a->min = NULL;
    free(a->max);
    a->max = NULL;
}


void set_delete(struct set_t *a) {
    set_clear(a);
    free(a);
}


bool set_within_set(struct set_t *s, char ch) {
    size_t i;

    for (i = 0; i < s->num_range; i++) {
        if (ch >= s->min[i] && ch <= s->max[i]) {
            return true;
        }
    }

    for (i = 0; i < s->num_single; i++) {
        if (ch == s->single[i]) {
            return true;
        }
    }

    return false;
}
