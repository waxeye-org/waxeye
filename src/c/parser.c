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

#include <assert.h>
#include <stdlib.h>

#define PARSER_C_
#include "parser.h"


struct ast_t* match_automaton(struct inner_parser_t *ip, size_t index);


void parser_init(struct parser_t *a, size_t start, struct fa_t *automata, size_t num_automata, bool eof_check) {
    a->start = start;
    a->automata = automata;
    a->num_automata = num_automata;
    a->eof_check = eof_check;
}


struct parser_t* parser_new(size_t start, struct fa_t *automata, size_t num_automata, bool eof_check) {
    struct parser_t *a = malloc(sizeof(struct parser_t));
    assert(a != NULL);
    parser_init(a, start, automata, num_automata, eof_check);
    return a;
}


void parser_clear(struct parser_t *a) {
    size_t i;

    for (i = 0; i < a->num_automata; i++) {
        fa_clear(a->automata[i]);
    }

    free(automata);
    a->automata = NULL;
}


void parser_delete(struct parser_t *a) {
    parser_clear(a);
    free(a);
}


struct inner_parser_t {
    size_t start;
    struct fa_t *automata;
    size_t num_automata;
    bool eof_check;

    struct input_t *input;
    size_t line;
    size_t column;
    bool last_cr;
    size_t error_pos;
    size_t error_line;
    size_t error_col;
    struct cache_t *cache;
    struct vector_t *cache_contents;
    struct vector_t *states_stack;
    struct vector_t *to_free;
};


void inner_parser_init(struct inner_parser_t *a, size_t start,
                       struct fa_t *automata, size_t num_automata,
                       bool eof_check, struct input_t *input,
                       size_t line, size_t column, bool last_cr,
                       size_t error_pos, size_t error_line, size_t error_col,
                       struct cache_t *cache, struct vector_t *cache_contents,
                       struct vector_t *states_stack, struct vector_t *to_free) {
    a->start = start;
    a->automata = automata;
    a->num_automata = num_automata;
    a->eof_check = eof_check;

    a->input = input;
    a->line = line;
    a->column = column;
    a->last_cr = last_cr;
    a->error_pos = error_pos;
    a->error_line = error_line;
    a->error_col = error_col;
    a->cache = cache;
    a->cache_contents = cache_contents;
    a->states_stack = states_stack;
    a->to_free = to_free;
}


struct inner_parser_t* inner_parser_new(size_t start, struct fa_t *automata,
                                        size_t num_automata, bool eof_check,
                                        struct input_t *input, size_t line,
                                        size_t column, bool last_cr, size_t error_pos,
                                        size_t error_line, size_t error_col,
                                        struct cache_t *cache, struct vector_t *cache_contents,
                                        struct vector_t *states_stack, struct vector_t *to_free) {
    struct inner_parser_t *a = malloc(sizeof(struct inner_parser_t));
    assert(a != NULL);
    inner_parser_init(a, start, automata, num_automata, eof_check,
                      input, line, column, last_cr, error_pos,
                      error_line, error_col, cache, cache_contents, states_stack, to_free);
    return a;
}


void inner_parser_clear(struct inner_parser_t *a) {
    cache_delete(a->cache);
    a->cache = NULL;
    vector_delete(a->cache_contents);
    a->cache_contents = NULL;
    vector_delete(a->states_stack);
    a->states_stack = NULL;
    vector_delete(a->to_free);
    a->to_free = NULL;
}


void inner_parser_delete(struct inner_parser_t *a) {
    inner_parser_clear(a);
    free(a);
}


void restore_pos(struct inner_parser_t *ip, size_t pos, size_t line,
                 size_t col, bool last_cr) {
    ip->input->pos = pos;
    ip->line = line;
    ip->column = col;
    ip->last_cr = last_cr;
}


struct ast_t* update_error(struct inner_parser_t *ip) {
    if (ip->error_pos < ip->input->pos) {
        ip->error_pos = ip->input->pos;
        ip->error_line = ip->line;
        ip->error_col = ip->col;
    }

    return NULL;
}


struct ast_t* mv(struct inner_parser_t *ip) {
    char ch = input_consume(ip->input);

    if (ch == '\r') {
        ip->line++;
        ip->column = 0;
        ip->last_cr = true;
    }
    else {
        if (ch == '\n') {
            if (ip->last_cr == false) {
                ip->line++;
                ip->column = 0;
            }
        }
        else {
            ip->column++;
        }
        ip->last_cr = false;
    }

    union ast_data d = { .c = ch };
    return ast_new(AST_CHAR, d);
}


struct vector_t* match_edge(struct inner_parser_t *ip, struct edge_t *edge) {
    size_t start_pos = ip->input->pos;
    size_t start_line = ip->line;
    size_t start_col = ip->col;
    bool start_cr = ip->last_ct;
    struct trans_t *t = &edge->t;
    struct ast_t *res;

    switch (t->type) {
        case TRANS_WILD: {
            res = (input_eof(ip->input) ? update_error(ip) : mv(ip));
            break;
        }
        case TRANS_CHAR: {
            res = ((input_eof(ip->input) || t->data.c != input_peek(ip->input)) ? update_error(ip) : mv(ip));
            break;
        }
        case TRANS_SET: {
            res = ((input_eof(ip->input) || !set_within_set(t->data.set, input_peek(ip->input))) ? update_error(ip) : mv(ip));
            break;
        }
        case TRANS_FA: {
            res = match_automaton(ip, t->data.fa);
            break;
        }
        default: {
            res = NULL;
            break;
        }
    }

    if (res == NULL) {
        return NULL;
    }
    else {
        struct vector_t *tran_res = match_state(ip, edge->s);

        if (tran_res != NULL) {
            if (edge->v || res->type == AST_EMPTY) {
                vector_add(ip->to_free, res);
                return tran_res;
            }
            else {
                vector_add(tran_res, res);
                return tran_res;
            }
        }
        else {
            restore_pos(ip, start_pos, start_line, start_col, start_cr);
            vector_add(ip->to_free, res);
            return NULL;
        }
    }
}


/*
 * index - The index of the next edge to match.
 */
struct vector_t* match_edges(struct inner_parser_t *ip, struct edge_t *edges,
                             size_t num_edges, size_t index) {
    // If we have no more edges to try to match
    if (index == num_edges) {
        return NULL;
    }
    else {
        struct vector_t *res = match_edge(ip, edges[index]);

        if (res != NULL) {
            return res;
        }
        else {
            return match_edges(ip, edges, num_edges, index + 1);
        }
    }
}


struct vector_t* match_state(struct inner_parser_t *ip, size_t index) {
    struct state_t *state = &((fa_t*)vector_peek(states_stack))->states[index];

    struct vector_t *res = match_edges(ip, state->edges, state->num_edges, 0);

    if (res != NULL) {
        return res;
    }
    else {
        if (state->match) {
            return vector_new(INIT_CHILDREN);
        }
        else {
            return NULL;
        }
    }
}


/*
 * Adds the contents of the given vector to the to_free list of the inner parser.
 * Deletes the given vector afterwards.
 */
void add_to_free_list(struct inner_parser_t *ip, struct vector_t *to_add) {
    size_t i;

    for (i = 0; i < to_add->size; i++) {
        vector_add(ip->to_free, vector_get(to_add, i));
    }

    vector_delete(to_add);
}


struct ast_t* match_automaton(struct inner_parser_t *ip, size_t index) {
    size_t start_pos = ip->input->pos;
    struct cache_key_t key;
    key.index = index;
    key.pos = start_pos;

    struct cache_value_t *cache_value = cache_get(ip->cache, &key);

    if (cache_value != NULL) {
        restore_pos(ip, cache_value->pos, cache_value->line,
                    cache_value->column, cache_value->last_cr);
        return cache_value->result;
    }

    size_t start_line = ip->line;
    size_t start_col = ip->column;
    bool start_cr = ip->last_cr;
    struct fa_t *automaton = ip->automata[index];

    vector_push(ip->states_stack, automaton->states);
    struct vector_t *res = match_state(ip, 0);
    vector_pop(ip->states_stack);

    struct ast_t *value = NULL;

    switch (automaton->mode) {
        case POS: {
            restore_pos(ip, start_pos, start_line, start_col, start_cr);
            if (res == NULL) {
                value = update_error(ip);
            }
            else {
                union ast_data d = { .c = '\0' };
                value = ast_new(AST_EMPTY, d);
            }
            add_to_free_list(ip, res);
            break;
        }
        case NEG: {
            restore_pos(ip, start_pos, start_line, start_col, start_cr);
            if (res == NULL) {
                union ast_data d = { .c = '\0' };
                value = ast_new(AST_EMPTY, d);
            }
            else {
                value = update_error(ip);
            }
            add_to_free_list(ip, res);
            break;
        }
        case VOID: {
            if (res == NULL) {
                value = update_error(ip);
            }
            else {
                union ast_data d = { .c = '\0' };
                value = ast_new(AST_EMPTY, d);
            }
            add_to_free_list(ip, res);
            break;
        }
        case PRUNE: {
            if (res == NULL) {
                value = update_error(ip);
                add_to_free_list(ip, res);
            }
            else {
                switch (res->size) {
                    case 0: {
                        union ast_data d = { .c = '\0' };
                        value = ast_new(AST_EMPTY, d);
                        add_to_free_list(ip, res);
                        break;
                    }
                    case 1: {
                        value = vector_get(0);
                        vector_delete(res);
                        break;
                    }
                    default: {
                        struct ast_tree_t *t = ast_tree_new(strclone(automaton->type), res);
                        union ast_data d = { .tree = t };
                        value = ast_new(AST_TREE, d);
                        break;
                    }
                }
            }
            break;
        }
        case LEFT: {
            if (res == NULL) {
                value = update_error(ip);
                add_to_free_list(ip, res);
            }
            else {
                struct ast_tree_t *t = ast_tree_new(strclone(automaton->type), res);
                union ast_data d = { .tree = t };
                value = ast_new(AST_TREE, d);
            }
            break;
        }
        default: {
            add_to_free_list(ip, res);
            break;
        }
    }

    cache_value = cach_value_new(value, ip->input->pos, ip->line,
                                 ip->column, ip->last_cr);
    cache_put(ip->cache, &key, cache_value);
    vector_add(ip->cache_contents, cache_value);
    return value;
}


struct ast_t* create_parse_error(struct inner_parser_t *ip) {
    struct ast_error_t *e = malloc(sizeof(struct ast_error_t));
    e->pos = ip->error_pos;
    e->line = ip->error_line;
    e->col = ip->error_col;
    union ast_data d = { .error = e };
    return ast_new(AST_ERROR, d);
}


struct ast_t* eof_check(struct inner_parser_t *ip, struct ast_t *res) {
    if (res != NULL) {
        if (ip->eof_check && ip->input->pos < ip->input->size) {
            // Create a parse error - not all input consumed
            return create_parse_error(ip);
        }
        else {
            return res;
        }
    }
    else {
        // Create a parse error
        return create_parse_error(ip);
    }
}


struct ast_t* inner_parser_parse(struct inner_parser_t *ip) {
    struct ast_t *res = eof_check(ip, match_automaton(ip, ip->start));

    // Clean up the contents of the cache



    return res;
}


struct ast_t* parse(struct parser_t *parser, struct input_t *input) {
    struct cache_t *cache = cache_new(1024);
    struct vector_t *cache_contents = vector_new(512);
    struct vector_t *to_free = vector_new(256);
    struct vector_t *states_stack = vector_new(64);
    struct inner_parser_t *inner_parser =
        inner_parser_new(parser->start, parser->automata,
                         parser->num_automata, parser->eof_check,
                         input, 0, 0, 0, 0, 0, 0,
                         cache, cache_contents, states_stack, to_free);
    struct ast_t *res = inner_parser_parse(inner_parser);

    inner_parser_delete(inner_parser);

    return res;
}
