/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#include <assert.h>
#include <stdlib.h>

#define TRANS_C_
#include <waxeye/trans.h>


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
