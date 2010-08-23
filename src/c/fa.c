/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include <assert.h>
#include <stdlib.h>

#define FA_C_
#include <waxeye/fa.h>


void fa_init(struct fa_t *a, enum fa_mode mode, size_t type, struct state_t *states, size_t num_states) {
    a->mode = mode;
    a->type = type;
    a->states = states;
    a->num_states = num_states;
}


struct fa_t* fa_new(enum fa_mode mode, size_t type, struct state_t *states, size_t num_states) {
    struct fa_t *a = malloc(sizeof(struct fa_t));
    assert(a != NULL);
    fa_init(a, mode, type, states, num_states);
    return a;
}


void fa_clear(struct fa_t *a) {
    size_t i;

    for (i = 0; i < a->num_states; i++) {
        state_clear(&a->states[i]);
    }

    free(a->states);
    a->states = NULL;
}


void fa_delete(struct fa_t *a) {
    fa_clear(a);
    free(a);
}
