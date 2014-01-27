# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.

require './parser'

p = Parser.new()
input = STDIN.read
ast = p.parse(input)
puts ast
