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

#define TRANS_C_
#include "trans.h"


void trans_init(struct trans_t *a, enum trans_type type, union trans_data data) {
    a->type = type;
    a->data = data;
}


struct trans_t* trans_new(enum trans_type type, union trans_data data) {
    struct trans_t *a = malloc(sizeof(struct trans_t));
    assert(a != NULL);
    trans_init(a, type, data);
    return a;
}


void trans_clear(struct trans_t *a) {
    switch (a->type) {
        case TRANS_WILD:
        case TRANS_CHAR:
        case TRANS_FA:
            break;
        case TRANS_SET:
            set_delete(a->data.set);
            a->data.set = NULL;
            break;
        default:
            break;
    }
}


void trans_delete(struct trans_t *a) {
    trans_clear(a);
    free(a);
}
