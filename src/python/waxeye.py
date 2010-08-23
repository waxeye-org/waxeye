# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.

class Edge:
    def __init__(self, trans, state, voided):
        self.trans = trans
        self.state = state
        self.voided = voided


class State:
    def __init__(self, edges, match):
        self.edges = edges
        self.match = match


class FA:
    VOID = 0
    PRUNE = 1
    LEFT = 2
    POS = 3
    NEG = 4
    def __init__(self, type, states, mode):
        self.type = type
        self.states = states
        self.mode = mode


class ParseError:
    def __init__(self, pos, line, col, nt):
        self.pos = pos
        self.line = line
        self.col = col
        self.nt = nt

    def __str__(self):
        return "parse error: failed to match '%s' at line=%s, col=%s, pos=%s" % (self.nt, self.line, self.col, self.pos)


class AST:
    def __init__(self, type, children, pos):
        self.type = type
        self.children = children
        self.pos = pos

    def str_iter(self, ast, indent, acc):
        for i in range(0, indent[0] - 1):
            acc.append('    ')
        if indent[0] > 0:
            acc.append('->  ')
        acc.append(ast.type)
        indent[0] += 1
        for a in ast.children:
            acc.append('\n')
            if isinstance(a, AST):
                self.str_iter(a, indent, acc)
            else:
                for i in range(0, indent[0] - 1):
                    acc.append('    ')
                if indent[0] > 0:
                    acc.append('|   ')
                acc.append(a)
        indent[0] -= 1

    def __str__(self):
        acc = []
        self.str_iter(self, [0], acc)
        return ''.join(acc)


class WaxeyeParser:
    def __init__(self, start, eof_check, automata):
        self.start = start
        self.eof_check = eof_check
        self.automata = automata

    def parse(self, input):
        return WaxeyeParser.InnerParser(self.start, self.eof_check, self.automata, input).parse()

    class InnerParser:
        def __init__(self, start, eof_check, automata, input):
            self.start = start
            self.eof_check = eof_check
            self.automata = automata
            self.input = input
            self.input_len = len(input)
            self.input_pos = 0
            self.line = 1
            self.column = 0
            self.last_cr = False
            self.error_pos = 0
            self.error_line = 1
            self.error_col = 0
            self.error_nt = automata[start].type
            self.fa_stack = []
            self.cache = {}

        def parse(self):
            return self.do_eof_check(self.match_automaton(self.start))


        def match_automaton(self, index):
            start_pos = self.input_pos
            key = (index, start_pos)

            if self.cache.has_key(key):
                cachedItem = self.cache[key]
                self.restore_pos(cachedItem[1], cachedItem[2], cachedItem[3], cachedItem[4])
                return cachedItem[0]

            start_line = self.line
            start_col = self.column
            start_cr = self.last_cr
            automaton = self.automata[index]
            type = automaton.type
            mode = automaton.mode

            self.fa_stack = [automaton] + self.fa_stack
            res = self.match_state(0)
            self.fa_stack = self.fa_stack[1:]

            if mode == FA.POS:
                self.restore_pos(start_pos, start_line, start_col, start_cr)
                if res != False:
                    value = True
                else:
                    value = False
            elif mode == FA.NEG:
                self.restore_pos(start_pos, start_line, start_col, start_cr)
                if res != False:
                    value = self.update_error()
                else:
                    value = True
            elif res != False:
                if mode == FA.VOID:
                    value = True
                elif mode == FA.PRUNE:
                    l = len(res)
                    if l == 0:
                        value = True
                    elif l == 1:
                        value = res[0]
                    else:
                        value = AST(type, res, (start_pos, self.input_pos))
                else:
                    value = AST(type, res, (start_pos, self.input_pos))
            else:
                value = self.update_error()

            self.cache[key] = (value, self.input_pos, self.line, self.column, self.last_cr)
            return value


        # Returns a list of results so, need to check != False
        def match_state(self, index):
            state = self.fa_stack[0].states[index]
            res = self.match_edges(state.edges)
            if res != False:
                return res
            else:
                return state.match and []


        def match_edges(self, edges):
            if edges == []:
                return False
            else:
                res = self.match_edge(edges[0])
                if res != False:
                    return res
                else:
                    return self.match_edges(edges[1:])


        def match_edge(self, edge):
            start_pos = self.input_pos
            start_line = self.line
            start_col = self.column
            start_cr = self.last_cr
            t = edge.trans

            if t == -1: # use -1 for wild card
                if self.input_pos < self.input_len:
                    res = self.mv()
                else:
                    res = self.update_error()
            elif isinstance(t, str):
                if self.input_pos < self.input_len and t == self.input[self.input_pos]:
                    res = self.mv()
                else:
                    res = self.update_error()
            elif isinstance(t, list):
                if self.input_pos < self.input_len and self.within_set(t, ord(self.input[self.input_pos])):
                    res = self.mv()
                else:
                    res = self.update_error()
            elif isinstance(t, int):
                res = self.match_automaton(t)
            else:
                res = False

            if res:
                tran_res = self.match_state(edge.state)
                if tran_res != False:
                    if edge.voided or res == True:
                        return tran_res
                    else:
                        return [res] + tran_res
                else:
                    self.restore_pos(start_pos, start_line, start_col, start_cr)
                    return False
            else:
                return False


        def restore_pos(self, pos, line, col, cr):
            self.input_pos = pos
            self.line = line
            self.column = col
            self.last_cr = cr


        def update_error(self):
            if self.error_pos < self.input_pos:
                self.error_pos = self.input_pos
                self.error_line = self.line
                self.error_col = self.column
                self.error_nt = self.fa_stack[0].type
            return False


        def mv(self):
            ch = self.input[self.input_pos]
            self.input_pos += 1

            if ch == '\r':
                self.line += 1
                self.column = 0
                self.last_cr = True
            else:
                if ch == '\n':
                    if not self.last_cr:
                        self.line += 1
                        self.column = 0
                else:
                    self.column += 1
                self.last_cr = False

            return ch

        def do_eof_check(self, res):
            if res:
                if self.eof_check and self.input_pos < self.input_len:
                    # Create a parse error - Not all input consumed
                    return ParseError(self.error_pos, self.error_line, self.error_col, self.error_nt)
                else:
                    return res
            else:
                # Create a parse error
                return ParseError(self.error_pos, self.error_line, self.error_col, self.error_nt)


        # Takes a set and an ordinal
        def within_set(self, set, c):
            if set == []:
                return False
            else:
                aa = set[0]
                if isinstance(aa, str):
                    if ord(aa) == c:
                        return True
                    else:
                        if ord(aa) < c:
                            return self.within_set(set[1:], c)
                        else:
                            return False
                else:
                    # If not a String then must be a range (tuple)
                    if c >= aa[0] and c <= aa[1]:
                        return True
                    else:
                        if aa[1] < c:
                            return self.within_set(set[1:], c)
                        else:
                            return False
