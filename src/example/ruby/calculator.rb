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

require 'rubygems'
require 'waxeye'
require 'parser'

# A commandline arithmetic calculator.
class Calculator
  @@p = Parser.new()

  def self.calc(input)
    ast = @@p.parse(input)
  
    if ast.is_a?(Waxeye::ParseError)
      ast.display()
    else
      puts sum(ast.children[0])
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
    num.children.to_s.to_f
  end
end

STDIN.each {|input| Calculator.calc(input)}
