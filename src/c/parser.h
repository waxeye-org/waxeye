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

#ifndef PARSER_H_
#define PARSER_H_

#include <stdbool.h>
#include "ast.h"
#include "cache.h"
#include "fa.h"
#include "input.h"

struct parser_t {
    size_t start;
    struct fa_t *automata;
    size_t num_automata;
    bool eof_check;
};

#ifndef PARSER_C_

extern void parser_init(struct parser_t *s, size_t start, struct fa_t *automata, size_t num_automata, bool eof_check);
extern struct parser_t* parser_new(size_t start, struct fa_t *automata, size_t num_automata, bool eof_check);
extern void parser_clear(struct parser_t *s);
extern void parser_delete(struct parser_t *s);
extern struct ast_t* parse(struct parser_t *parser, struct input_t *input);

#endif /* PARSER_C_ */
#endif /* PARSER_H_ */
