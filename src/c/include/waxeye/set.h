/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef SET_H_
#define SET_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

struct set_t {
    char *single;
    size_t num_single;
    char *min;
    char *max;
    size_t num_range;
};

#ifndef SET_C_

extern void set_init(struct set_t *s, char single[], size_t num_single, char min[], char max[], size_t num_range);
extern struct set_t* set_new(char single[], size_t num_single, char min[], char max[], size_t num_range);
extern void set_clear(struct set_t *s);
extern void set_delete(struct set_t *s);
extern bool set_within_set(struct set_t *s, char ch);

#endif /* SET_C_ */

#ifdef __cplusplus
}
#endif

#endif /* SET_H_ */
