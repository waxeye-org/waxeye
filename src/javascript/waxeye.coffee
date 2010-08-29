###
# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.
###

waxeye = (->

  # an edge in the FA graph
  # trans - the condition that must be met to transition along this edge
  # state - the state to transition to
  # voided - if the result of the transition condition will be included in the AST
  class Edge
    constructor: (@trans, @state, @voided) ->


  # a state in the FA graph
  # edges - the edges leading out of this state
  # match - if the state is an accepting state
  class State
    constructor: (@edges, @match) ->


  # an FA in the FA graph
  # type - the type of AST that will be created
  # states - the states of the FA. State 0 is the starting state
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


  # class for an AST node
  # the empty AST is represented by the boolean 'true'
  class AST
    constructor: (@type, @children, @pos) ->

    toString: ->
      acc = ""
      indent = 0
      toStringIter =(ast) ->
        i = 0
        while i < indent - 1
          acc += '    '
          i++
        if indent > 0
          acc += '->  '
        acc += ast.type
        indent++
        for a in ast.children
          acc += '\n'
          # if the child ast is a char
          if (typeof a) is 'string'
            i = 0
            while i < indent - 1
              acc += '    '
              i++
            if indent > 0
              acc += '|   '
            acc += a
          else
            toStringIter a
        indent--
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

      # save the parser's state
      startLine = @line
      startCol = @column
      startCR = @lastCR
      automaton = @automata[index]
      type = automaton.type
      mode = automaton.mode

      # push the current FA to top of the stack
      @faStack.push automaton
      res = @matchState 0
      # pop from the stack
      @faStack.pop()

      value = switch mode
        when FA.POS
          @restorePos startPos, startLine, startCol, startCR
          if res
            true
          else
            @updateError()
            false
        when FA.NEG
          @restorePos startPos, startLine, startCol, startCR
          if res
            @updateError()
            false
          else
            true
        else
          if res
            switch mode
              when FA.VOID then true
              when FA.PRUNE
                switch res.length
                  when 0 then true
                  when 1 then res[0]
                  else new AST type, res, [startPos, @inputPos]
              else new AST type, res, [startPos, @inputPos]
          else
            @updateError()

      @cache[key] = [value, @inputPos, @line, @column, @lastCR]
      return value

    matchState: (index) ->
      state = @faStack[@faStack.length - 1].states[index]
      res = @matchEdges state.edges, 0
      if res
        res
      else
        state.match and []

    matchEdges: (edges, index) ->
      if index == edges.length
        false
      else
        res = @matchEdge edges[index]
        if res then res else @matchEdges edges, (index + 1)

    matchEdge: (edge) ->
      startPos = @inputPos
      startLine = @line
      startCol = @column
      startCR = @lastCR
      t = edge.trans

      res = if t == -1 # wildcard is -1
        if @inputPos < @inputLen then @mv() else @updateError()
      else
        if typeof t == 'string' # a single character
          if @inputPos < @inputLen and t == @input[@inputPos]
            @mv()
          else
            @updateError()
        else
          if t instanceof Array # a set of chars
            if @inputPos < @inputLen and @withinSet t, 0, (@input[@inputPos].charCodeAt 0)
              @mv()
            else
              @updateError()
          else
            if typeof t == 'number' # an FA
              @matchAutomaton t
            else
              false

      if res
        tranRes = @matchState edge.state
        if tranRes
          # if the edge we moved along wasn't supposed to include it's result
          # or the result was empty, just return the stuff after it
          if edge.voided or res == true
            tranRes
          else
            [res].concat tranRes
        else
          @restorePos startPos, startLine, startCol, startCR
          false
      else
        false

    restorePos: (pos, line, col, cr) ->
      @inputPos = pos
      @line = line
      @column = col
      @lastCR = cr

    updateError: ->
      if @errorPos < @inputPos
        @errorPos = @inputPos
        @errorLine = @line
        @errorCol = @column
        @errorNT = @faStack[@faStack.length - 1].type
      return false

    mv: ->
      ch = @input[@inputPos]
      @inputPos++
      if ch == '\r'
        @line++
        @column = 0
        @lastCR = true
      else
        if ch == '\n'
          if not @lastCR
            @line++
            @column = 0
        else
          @column++
        @lastCR = false
      return ch

    doEOFCheck: (res) ->
      if res
        if @eofCheck and @inputPos < @inputLen
          # Create a parse error - Not all input consumed
          new ParseError @errorPos, @errorLine, @errorCol, @errorNT
        else
          res
      else
        # Create a parse error
        new ParseError @errorPos, @errorLine, @errorCol, @errorNT

    # Takes a set, an index into the set and an ordinal
    # set - an array of single char strings and arrays of integer pairs
    #       the integer pairs represent inclusive ranges of acceptable chars
    #       the chars and ranges are in assending order
    withinSet: (set, index, c) ->
      if index == set.length
        false
      else
        aa = set[index]
        if typeof aa == 'string'
          if (aa.charCodeAt 0) == c
            true
          else
            if (aa.charCodeAt 0) < c
              @withinSet set, index + 1, c
            else
              false
        else
          # If not a String then must be a range (tuple)
          if c >= aa[0] and c <= aa[1]
            true
          else
            if c > aa[1]
              @withinSet set, index + 1, c
            else
              false


  namespace =
    Edge: Edge
    State: State
    FA: FA
    ParseError: ParseError
    AST: AST
    WaxeyeParser: WaxeyeParser

  return namespace
)()

# Add to module system
if module?
  module.exports.AST = waxeye.AST
  module.exports.Edge = waxeye.Edge
  module.exports.FA = waxeye.FA
  module.exports.ParseError = waxeye.ParseError
  module.exports.State = waxeye.State
  module.exports.WaxeyeParser = waxeye.WaxeyeParser
