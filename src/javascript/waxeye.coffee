###
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
###

waxeye = (->

  class Edge
    constructor: (@trans, @state, @voided) ->


  class State
    constructor: (@edges, @match) ->


  class FA
    constructor: (@type, @states, @mode) ->
  FA.VOID = 0
  FA.PRUNE = 1
  FA.LEFT = 2
  FA.POS = 3
  FA.NEG = 4


  class ParseError
    constructor: (@pos, @line, @col, @nt) ->

    toString: ->
      "parse error: failed to match '#@nt' at line=#@line, col=#@col, pos=#@pos"


  class AST
    constructor: (@type, @children, @pos) ->

    toString: ->
      acc = ""
      indent = 0
      toStringIter =(ast) ->
        for i in [0...indent]
          acc += '    '
        if indent > 0
          acc += '->  '
        acc += ast.type
        indent++
        for a in ast.children
          acc += '\n'
          # if the child ast is a char
          if (typeof a) is 'string'
            for i in [0...indent]
              acc += '    '
            if indent > 0
              acc += '|   '
            acc += a
          else
            toStringIter a
        return acc
      return toStringIter this


  class WaxeyeParser
    constructor: (@start, @eofCheck, @automata) ->

    parse: (input) ->
      new InnerParser(@start, @eofCheck, @automata, input).parse()


  class InnerParser
    constructor: (@start, @eofCheck, @automata, input) ->
      @input = input
      @inputLen = input.length
      @inputPos = 0
      @line = 1
      @column = 0
      @lastCR = false
      @errorPos = 0
      @errorLine = 1
      @errorCol = 0
      @errorNT = @automata[@start].type
      @faStack = []
      @cache = {}

    parse: ->
      @doEOFCheck(@matchAutomaton @start)

    matchAutomaton: (index) ->
      startPos = @inputPos
      key = "#index,#startPos"

      cached = @cache[key]
      # if we have a cached result
      if cached?
        @restorePos cached[1], cached[2], cached[3], cached[4]
        return cached[0]


  namespace =
    Edge: Edge
    State: State
    FA: FA
    ParseError: ParseError
    AST: AST
    WaxeyeParser: WaxeyeParser

  # Add to Node.js module system
  if module?
    module.exports.AST = AST

  return namespace
)()
