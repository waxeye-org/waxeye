/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

#ifndef WPARSER_H_
#define WPARSER_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <waxeye/ast.h>
#include <waxeye/fa.h>
#include <waxeye/input.h>

struct parser_t {
    size_t start;
    struct fa_t *automata;
    size_t num_automata;
    bool eof_check;
};

#ifndef WPARSER_C_

extern void parser_init(struct parser_t *s, size_t start, struct fa_t *automata, size_t num_automata, bool eof_check);
extern struct parser_t* wparser_new(size_t start, struct fa_t *automata, size_t num_automata, bool eof_check);
extern void parser_clear(struct parser_t *s);
extern void parser_delete(struct parser_t *s);
extern struct ast_t* parse(struct parser_t *parser, struct input_t *input);

#endif /* WPARSER_C_ */

#ifdef __cplusplus
}
#endif

#endif /* WPARSER_H_ */
