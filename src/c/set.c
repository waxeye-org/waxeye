/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include <assert.h>
#include <stdlib.h>

#define SET_C_
#include <waxeye/set.h>


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
