
import sys

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
        print self.type
        indent[0] += 1

    def display(self):
        self.display_iter(self, [3])





a = AST("Hello Mama!", ['a', 'b', 'c'], [0, 2])

a.display()
