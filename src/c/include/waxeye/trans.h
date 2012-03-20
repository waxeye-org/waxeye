/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef TRANS_H_
#define TRANS_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <waxeye/set.h>

/*
  Transition is either
  wild card
  a single character
  a character set
  an index for an automaton
 */
enum trans_type {
    TRANS_WILD,
    TRANS_CHAR,
    TRANS_SET,
    TRANS_FA
};

union trans_data {
    char c;
    struct set_t *set;
    size_t fa;
};

struct trans_t {
    enum trans_type type;
    union trans_data data;
};

#ifndef TRANS_C_

extern void trans_init(struct trans_t *s, enum trans_type type, union trans_data data);
extern struct trans_t* trans_new(enum trans_type type, union trans_data data);
extern void trans_clear(struct trans_t *s);
extern void trans_delete(struct trans_t *s);

#endif /* TRANS_C_ */

#ifdef __cplusplus
}
#endif

#endif /* TRANS_H_ */
