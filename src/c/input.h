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

#ifndef INPUT_H_
#define INPUT_H_

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
#endif /* INPUT_H_ */
