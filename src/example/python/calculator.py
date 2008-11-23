# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008 Orlando D. A. R. Hill
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is furnished to do
# so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import sys
import waxeye
import parser

p = parser.Parser()

# A commandline arithmetic calculator.
def calc(input):
    ast = p.parse(input)
    if isinstance(ast, waxeye.ParseError):
        return ast
    else:
        return sum(ast.children[0])


def bin_op(ast, fn, ch, op1, op2):
    chil = ast.children
    val = fn(*(chil[0],))
    i = 1
    while i != len(chil):
        if chil[i] == ch:
            operator = op1
        else:
            operator = op2
        # Increment val by the operator applied to val and the operand
        operand = fn(*(chil[i + 1],))
        val = operator(*(val, operand))
        i += 2
    return val


sum = lambda ast: bin_op(ast, prod, '+', lambda a, b: a + b, lambda a, b: a - b)


prod = lambda ast: bin_op(ast, unary, '*', lambda a, b: a * b, lambda a, b: a / b)


def unary(ast):
    if ast.type == 'unary':
        return - unary(ast.children[1])
    elif ast.type == 'sum':
        return sum(ast)
    else:
        return num(ast)


def num(ast):
    return float(''.join(ast.children))


sys.stdout.write('calc> ')
line = sys.stdin.readline()
while line:
    print calc(line)
    sys.stdout.write('calc> ')
    line = sys.stdin.readline()
print
