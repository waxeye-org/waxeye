
import sys

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

    def display(self):
        print "parse error: failed to match '%s' at line=%s, col=%s, pos=%s" % (self.nt, self.line, self.col, self.pos)


class AST:
    def __init__(self, type, children, pos):
        self.type = type
        self.children = children
        self.pos = pos

    def display_iter(self, ast, indent):
        for i in range(0, indent[0] - 1):
            sys.stdout.write('    ')
        if indent[0] > 0:
            sys.stdout.write('->  ')
        print ast.type
        indent[0] += 1
        for a in ast.children:
            if isinstance(a, AST):
                self.display_iter(a, indent)
            else:
                for i in range(0, indent[0] - 1):
                    sys.stdout.write('    ')
                if indent[0] > 0:
                    sys.stdout.write('|   ')
                print a
        indent[0] -= 1

    def display(self):
        self.display_iter(self, [0])


class WaxeyeParser:
    def __init__(self, start, eof_check, automata):
        self.start = start
        self.eof_check = eof_check
        self.automata = automata

    def parse(self, input):
        WaxeyeParser.InnerParser(self.start, self.eof_check, self.automata, input).parse()

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
            print self.input

p = WaxeyeParser(0, True, [FA(0, [], 0)])
p.parse('1+2-3*4')
