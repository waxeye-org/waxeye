# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.

import parser

# Create our parser
p = parser.Parser()

# Parse our input
ast = p.parse("42")

# Print our AST
print ast
