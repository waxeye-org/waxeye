# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.

require 'rubygems'
require 'waxeye'
require './parser'

# A commandline arithmetic calculator.
class Calculator
  @@p = Parser.new()

  def self.calc(input)
    ast = @@p.parse(input)
  
    if ast.is_a?(Waxeye::ParseError)
      ast
    else
      sum(ast.children[0])
    end
  end

  def self.bin_op(ast, fn, ch, op1, op2)
    chil = ast.children
    val = self.send(fn, (chil[0]))
    i = 1
    until i == chil.size
      # Increment val by the operator applied to val and the operand
      val = val.send(chil[i] == ch ? op1 : op2,
                     self.send(fn, chil[i + 1]))
      i += 2
    end
    val
  end

  def self.sum(ast)
    bin_op(ast, :prod, '+', :'+', :'-')
  end

  def self.prod(ast)
    bin_op(ast, :unary, '*', :'*', :'/')
  end

  def self.unary(unary)
    case unary.type
      when :unary
        - unary(unary.children[1])
      when :sum
        sum(unary)
      else
        num(unary)
    end
  end

  def self.num(num)
    num.children.join('').to_f
  end
end

print 'calc> '
STDIN.each {|input| puts Calculator.calc(input); print 'calc> ' }
puts
