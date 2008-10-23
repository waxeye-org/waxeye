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

#define FA_C_
#include "fa.h"


void fa_init(struct fa_t *a, enum fa_mode mode, char *type, struct state_t *states, size_t num_states) {
    a->mode = mode;
    a->type = type;
    a->states = states;
    a->num_states = num_states;
}


struct fa_t* fa_new(enum fa_mode mode, char *type, struct state_t *states, size_t num_states) {
    struct fa_t *a = malloc(sizeof(struct fa_t));
    assert(a != NULL);
    fa_init(a, mode, type, states, num_states);
    return a;
}


void fa_clear(struct fa_t *a) {
    size_t i;

    for (i = 0; i < a->num_states; i++) {
        state_clear(a->states[i]);
    }

    free(a->states);
    a->states = NULL;
    free(a->type);
    a->type = NULL;
}


void fa_delete(struct fa_t *a) {
    fa_clear(a);
    free(a);
}
