###
# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Copyright (c) 2015 Joshua Gross, Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.
###

assert = require('assert')

arrayPrepend = (item, a) ->
  assert.ok Array.isArray(a) || !a
  a = (a or []).slice(0)
  a.unshift(item)
  a

uniq = (a) ->
  # Why sort here? Seems useful to keep these in the original order.
  a.sort().filter((i, p) -> a.indexOf(i) == p)

getLineCol = (pos, input) ->
  explodedInput = input.split(/[\r\n]/)
  for i in [0..explodedInput.length-1]
    if pos <= explodedInput[i].length
      return [i+1, pos]
    pos -= explodedInput[i].length
  return [explodedInput.length, pos]

first = (a) ->
  assert.ok Array.isArray(a)
  a?[0]

rest = (a) ->
  assert.ok Array.isArray(a)
  Array.prototype.slice.call(a, 1)

waxeye = (->
  ###
  # An abstract syntax tree has one of three forms.
  # AST_EMPTY represents a successful parse from a voided non-terminal.
  # AST_CHAR just holds a character.
  # AST_TREE represents a successful parse from a non-terminal. It holds:
  # - the non-terminal's name
  # - a list of child asts
  ###
  class AST
    constructor: (@type, @ch, @str, @asts) ->
      assert.ok Array.isArray(@asts) || !@asts
  AST.EMPTY = () ->
    new AST("EMPTY")
  AST.CHAR = (ch) ->
    new AST("CHAR", ch)
  AST.TREE = (str, asts) ->
    new AST("TREE", null, str, asts)

  class NonTerminal
    constructor: (@mode, @exp) ->
  nonterminal = (mode, exp) ->
    new NonTerminal(mode, exp)

  class Exp
    constructor: (@type, @args) ->
  ["ANY", "NT", "VOID", "CHAR", "CHAR_CLASS", "AND", "NOT", "OPT", "ALT", "SEQ", "STAR", "PLUS"].map (expType) ->
    Exp[expType] = () ->
      return new Exp(expType, Array.prototype.slice.call(arguments))

  class Modes
    constructor: () ->
  Modes.NORMAL = "NORMAL"
  Modes.PRUNING = "PRUNING"
  Modes.VOIDING = "VOIDING"

  class ParseError
    constructor: (@pos, @line, @col, @nt, @chars) ->

    toString: ->
      @chars = @chars.charClasses if @chars.charClasses
      @chars = @chars?.map((ch) -> JSON.stringify(ch.char || ch.charClasses))
      "parse error: failed to match '"+@nt.join(',')+"' at line="+@line+", col="+@col+", pos="+@pos+" (expected '"+@chars.slice(1,-1).join(',')+"')"

  class ErrChar
    constructor: (@char) ->
  class ErrCC
    constructor: (@charClasses) ->

  class RawError
    constructor: (@pos, @nonterminals, @failedChars, @currentNT) ->
      assert.ok Array.isArray(@nonterminals)
      assert.ok Array.isArray(@failedChars)
    toParseError: (input) ->
      [line, col] = getLineCol @pos, input
      new ParseError(@pos, line, col, (uniq @nonterminals), @failedChars.reverse())

  class Value
    constructor: (@type, @err, @pos, @asts) ->
      assert.ok Array.isArray(@asts) || !@asts
      assert.ok typeof @pos == 'number' || !@pos
      assert.ok typeof @err == 'object' || !@err
  Value.FAIL = (err) ->
    new Value("FAIL", err)
  Value.VAL = (pos, asts, err) ->
    new Value("VAL", err, pos, asts)

  class MachineConfiguration
    constructor: (@type, @exp, @pos, @asts, @err, @continuations, @value) ->
      assert.ok Array.isArray(@asts) || !@asts
      assert.ok Array.isArray(@continuations)
      assert.ok typeof @pos == 'number' || !@pos
  MachineConfiguration.EVAL = (exp, pos, asts, err, continuations) ->
    new MachineConfiguration("EVAL", exp, pos, asts, err, continuations, null)
  MachineConfiguration.APPLY = (continuations, value) ->
    new MachineConfiguration("APPLY", null, null, null, null, continuations, value)

  class MachineState
    constructor: (@type, @result, @configuration) ->
  MachineState.FINAL = (result) ->
    new MachineState("FINAL", result, null)
  MachineState.INTER = (configuration) ->
    new MachineState("INTER", null, configuration)


  class Continuations
    constructor: (@type, @pos, @expressions, @expression, @asts, @err, @mode, @name, @nt) ->
      assert.ok Array.isArray(@asts) || !@asts
  Continuations.CONT_SEQ = (expressions) ->
    new Continuations("CONT_SEQ", null, expressions, null, null, null, null, null, null)
  Continuations.CONT_ALT = (expressions, pos, asts) ->
    new Continuations("CONT_ALT", pos, expressions, null, asts, null, null, null, null)
  Continuations.CONT_AND = (pos, asts, err) ->
    new Continuations("CONT_AND", pos, null, null, asts, err, null, null, null)
  Continuations.CONT_NOT = (pos, asts, err) ->
    new Continuations("CONT_NOT", pos, null, null, asts, err, null, null, null)
  Continuations.CONT_OPT = (pos, asts) ->
    new Continuations("CONT_OPT", pos, null, null, asts, null, null, null, null)
  Continuations.CONT_STAR = (exp, pos, asts) ->
    new Continuations("CONT_STAR", pos, null, exp, asts, null, null, null, null)
  Continuations.CONT_PLUS = (exp) ->
    new Continuations("CONT_PLUS", null, null, exp, null, null, null, null, null)
  Continuations.CONT_VOID = (asts) ->
    new Continuations("CONT_VOID", null, null, null, asts, null, null, null, null)
  Continuations.CONT_NT = (mode, name, asts, nt) ->
    new Continuations("CONT_NT", null, null, null, asts, null, mode, name, nt)

  updateError = (err, pos, e) ->
    if err && pos > err.pos
      new RawError(pos, [err?.currentNT], [e], err?.currentNT)
    else if pos == err.pos
      new RawError(err.pos, arrayPrepend(err.currentNT, err.nonterminals), arrayPrepend(e, err.failedChars), err.currentNT)
    else
      new RawError(err.pos, err.nonterminals, err.failedChars, err.currentNT)

  class WaxeyeParser
    constructor: (@env, @start) ->

    match: (nt, input) ->
      inputLen = input.length
      eof = (pos) -> pos >= inputLen

      # move: configuration -> state
      move = (conf) ->
        #console.log conf

        asts = conf.asts
        pos = conf.pos
        exp = conf.exp
        err = conf.err

        k = conf.continuations
        kFirst = first k if k
        kRest = rest k if k

        es = kFirst?.expressions
        firstExp = first es if es
        restExp = rest es if es

        switch conf.type
          when "EVAL"
            switch exp.type
              when "ALT"
                es = exp.args
                if es.length > 0
                  MachineState.INTER(MachineConfiguration.EVAL((first es), pos, asts, err, arrayPrepend(Continuations.CONT_ALT((rest es), pos, asts), k)))
                else
                  MachineState.INTER(MachineConfiguration.APPLY(k, err))
              when "VOID"
                MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, [], err, arrayPrepend(Continuations.CONT_VOID(asts), k)))
              when "CHAR"
                c = exp.args[0]
                if (eof pos) or c != input[pos]
                  newval = Value.FAIL(updateError(err, pos, new ErrChar(c)))
                else
                  newval = Value.VAL(pos+1, arrayPrepend(AST.CHAR(input[pos]), asts), err)
                MachineState.INTER(MachineConfiguration.APPLY(k, newval))
              when "CHAR_CLASS"
                cc = exp.args
                visit = (charClasses) ->
                  if charClasses.length == 0
                    MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(updateError(err, pos, new ErrCC(cc)))))
                  else
                    [c1, c2] = first charClasses
                    if c1 <= input[pos] and c2 >= input[pos]
                      MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos+1, arrayPrepend(AST.CHAR(input[pos]), asts), err)))
                    else
                      visit (rest charClasses)
                if eof pos
                  MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(updateError(err, pos, new ErrCC(cc)))))
                else
                  visit cc
              when "SEQ"
                # a sequence is made up of a list of expressions
                # we traverse the list, making sure each expression succeeds
                # the rest of the string return by the expression is used
                # as input to the next expression
                exprs = exp.args
                if exprs == null
                  MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos, asts, err)))
                else
                  MachineState.INTER(MachineConfiguration.EVAL((first exprs), pos, asts, err, arrayPrepend(Continuations.CONT_SEQ(rest exprs), k)))
              when "STAR"
                MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, arrayPrepend(Continuations.CONT_STAR(exp.args[0], pos, asts), k)))
              when "OPT"
                MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, arrayPrepend(Continuations.CONT_OPT(pos, asts), k)))
              when "NT"
                name = exp.args[0]
                mode = @env[name].mode
                e = @env[name].exp
                err = new RawError(err.pos, err.nonterminals, err.failedChars, name)
                MachineState.INTER(MachineConfiguration.EVAL(e, pos, [], err, arrayPrepend(Continuations.CONT_NT(mode, name, asts, conf.err.currentNT), k)))
              else
                console.log conf
                throw new Error('unsupported 2')
          when "APPLY"
            if conf.value?.type == "FAIL" and kFirst?.type != "CONT_ALT"
              if ["CONT_SEQ", "CONT_VOID"].indexOf(kFirst?.type) != -1
                MachineState.INTER(MachineConfiguration.APPLY(kRest, conf.value))
              else if ["CONT_STAR", "CONT_OPT"].indexOf(kFirst?.type) != -1
                MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(kFirst.pos, kFirst.asts, conf.value.err)))
              else if ["CONT_NT"].indexOf(kFirst?.type) != -1
                err = conf.value.err
                MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.FAIL(new RawError(err.pos, err.nonterminals, err.failedChars, kFirst.nt))))
              else
                MachineState.FINAL(conf.value.err.toParseError(input))
            else if kFirst?.type == "CONT_SEQ"
              if es.length > 0
                MachineState.INTER(MachineConfiguration.EVAL(firstExp, conf.value.pos, conf.value.asts, conf.value.err, arrayPrepend(Continuations.CONT_SEQ(restExp), kRest)))
              else
                MachineState.INTER(MachineConfiguration.APPLY(kRest, conf.value))
            else if kFirst?.type == "CONT_STAR"
              assert.ok typeof conf.value != 'undefined'
              MachineState.INTER(MachineConfiguration.EVAL(kFirst.expression, conf.value.pos, conf.value.asts, conf.value.err, arrayPrepend(Continuations.CONT_STAR(kFirst.expression, conf.value.pos, conf.value.asts), kRest)))
            else if kFirst?.type == "CONT_VOID"
              MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(conf.value.pos, kFirst.asts, conf.value.err)))
            else if kFirst?.type == "CONT_ALT"
              if conf.value?.type == "FAIL" and es.length > 0
                MachineState.INTER(MachineConfiguration.EVAL((first es), kFirst.pos, kFirst.asts, conf.value.err, arrayPrepend(Continuations.CONT_ALT((rest es), kFirst.pos, kFirst.asts), kRest)))
              else
                MachineState.INTER(MachineConfiguration.APPLY(kRest, conf.value))
            else if kFirst?.type == "CONT_OPT"
              MachineState.INTER(MachineConfiguration.APPLY(kRest, conf.value))
            else if kFirst?.type == "CONT_NT"
              { mode, name, asts, nt } = kFirst
              value = conf.value
              valAsts = value?.asts
              errPos = value?.err?.pos
              errNts = value?.err?.nonterminals
              errCcs = value?.err?.failedChars

              newErr = new RawError(errPos, errNts, errCcs, nt)

              switch mode
                when "NORMAL"
                  MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(value.pos, arrayPrepend(AST.TREE(name, valAsts.reverse()), asts), newErr)))
                when "PRUNING"
                  if valAsts.length == 0
                    MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(value.pos, asts, newErr)))
                  else if valAsts.length == 1
                    MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(value.pos, arrayPrepend((first valAsts), asts), newErr)))
                  else
                    MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(value.pos, arrayPrepend(AST.TREE(name, valAsts.reverse()), asts), newErr)))
                when "VOIDING"
                  MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(value.pos, asts, newErr)))
            else if kFirst?
              console.log(conf)
              console.log(conf.value)
              console.log(conf.value.err)
              throw new Error('unsupported 4')
            else if conf.value?.type == "VAL"
              ts = conf.value?.asts
              if eof conf.value?.pos
                if @env[@start].mode == Modes.NORMAL
                  MachineState.FINAL(AST.TREE(@start, ts.reverse()))
                else if @env[@start].mode == Modes.PRUNING
                  if ts.length == 0
                    MachineState.FINAL(AST.EMPTY())
                  else if ts.length == 1
                    MachineState.FINAL(first ts)
                  else
                    MachineState.FINAL(AST.TREE(@start, ts.reverse()))
                else
                    MachineState.FINAL(AST.EMPTY())
              else if conf.value?.err? && conf.value?.pos == conf.value?.err?.pos
                err = conf.value.err
                MachineState.FINAL((new RawError(conf.value.pos, err.nonterminals, err.failedChars)).toParseError(input))
              else
                MachineState.FINAL((new RawError(conf.value?.pos, [], [])).toParseError(input))
                ###
                    else if rest = err_pos
                         then FINAL (mk_parse_error (input, rest, nts, ccs))
                         else FINAL (mk_parse_error (input, rest, [], []))
                ###
            else if conf.value?.type == "FAIL"
              MachineState.FINAL(conf.value.err.toParseError(input))
              ###
                  | move (APPLY ([], FAIL (err_pos, nts, ccs, _)))
                    = FINAL (mk_parse_error (input, err_pos, nts, ccs))
              ###
            else
              console.log(conf)
              console.log(conf.value)
              console.log(conf.value.err)
              throw new Error('unsupported 3')


      # move from initial state to halting state
      state = move MachineConfiguration.EVAL(@env[nt].exp, 0, [], new RawError(0, [nt], [], nt), [])
      while state.type != "FINAL"
        state = move.bind(this) state.configuration
      state.result

    parse: (input) ->
      @match @start, input

  namespace =
    AST: AST
    nonterminal: nonterminal
    Exp: Exp
    Modes: Modes
    ParseError: ParseError
    ErrChar: ErrChar
    ErrCC: ErrCC
    WaxeyeParser: WaxeyeParser
)()

if module?
  module.exports = waxeye
