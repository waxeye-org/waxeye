/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef INPUT_H_
#define INPUT_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stdio.h>

struct input_t {
    char *data;
    size_t size;
    size_t pos;
};

#ifndef INPUT_C_

extern void input_init(struct input_t *b, char *data, size_t size);
extern struct input_t* input_new(char *data, size_t size);
extern struct input_t* input_from_file_new(FILE *fp);
extern void input_delete(struct input_t *b);
extern void input_and_data_clear(struct input_t *b);
extern void input_and_data_delete(struct input_t *b);

extern bool input_eof(struct input_t *b);
extern char input_consume(struct input_t *b);
extern char input_peek(struct input_t *b);

#endif /* INPUT_C_ */

#ifdef __cplusplus
}
#endif

#endif /* INPUT_H_ */
