# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.

require './parser'

# Create our parser
p = Parser.new()

# Parse our input
ast = p.parse("42")

# Print our AST
puts ast
