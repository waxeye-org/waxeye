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

#ifndef FA_H_
#define FA_H_

#include "state.h"

enum fa_mode {
    VOID,
    PRUNE,
    LEFT,
    POS,
    NEG
};

struct fa_t {
    enum fa_mode mode;
    char *type;
    struct state_t *states;
    size_t num_states;
};

#ifndef FA_C_

extern void fa_init(struct fa_t *fa, enum fa_mode mode, char *type, struct state_t *states, size_t num_states);
extern struct fa_t* fa_new(enum fa_mode mode, char *type, struct state_t *states, size_t num_states);
extern void fa_clear(struct fa_t *fa);
extern void fa_delete(struct fa_t *fa);

#endif /* FA_C_ */
#endif /* FA_H_ */
