# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.

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
