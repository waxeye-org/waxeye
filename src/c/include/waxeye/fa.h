/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef FA_H_
#define FA_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <waxeye/state.h>

enum fa_mode {
    MODE_VOID,
    MODE_PRUNE,
    MODE_LEFT,
    MODE_POS,
    MODE_NEG
};

struct fa_t {
    enum fa_mode mode;
    size_t type;
    struct state_t *states;
    size_t num_states;
};

#ifndef FA_C_

extern void fa_init(struct fa_t *fa, enum fa_mode mode, size_t type, struct state_t *states, size_t num_states);
extern struct fa_t* fa_new(enum fa_mode mode, size_t type, struct state_t *states, size_t num_states);
extern void fa_clear(struct fa_t *fa);
extern void fa_delete(struct fa_t *fa);

#endif /* FA_C_ */

#ifdef __cplusplus
}
#endif

#endif /* FA_H_ */
