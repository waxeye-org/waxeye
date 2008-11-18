
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

c = AST("num", ['1', '2'], [0, 2])
a = AST("Hello Mama!", ['a', c, 'c'], [0, 4])

a.display()
