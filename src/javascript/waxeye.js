// Generated by CoffeeScript 1.10.0

/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (c) 2015 Joshua Gross
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

(function() {
  var arrayPrepend, assert, first, getLineCol, rest, uniq, waxeye;

  assert = require('assert');

  arrayPrepend = function(item, a) {
    assert.ok(Array.isArray(a) || !a);
    a = (a || []).slice(0);
    a.unshift(item);
    return a;
  };

  uniq = function(a) {
    return a.sort().filter(function(i, p) {
      return a.indexOf(i) === p;
    });
  };

  getLineCol = function(pos, input) {
    var col, i, j, lastLineBreak, line, ref;
    col = 0;
    line = 0;
    lastLineBreak = 0;
    for (i = j = 0, ref = pos; 0 <= ref ? j <= ref : j >= ref; i = 0 <= ref ? ++j : --j) {
      if (input[i] === '\r' && input[i + 1] === '\n') {
        continue;
      }
      if (['\r', '\n'].indexOf(input[i]) !== -1) {
        line++;
        lastLineBreak = i + 1;
      }
      col = i - lastLineBreak;
    }
    return [line + 1, col];
  };

  first = function(a) {
    assert.ok(Array.isArray(a));
    return a != null ? a[0] : void 0;
  };

  rest = function(a) {
    assert.ok(Array.isArray(a));
    return Array.prototype.slice.call(a, 1);
  };

  waxeye = (function() {

    /*
     * An abstract syntax tree has one of three forms.
     * AST_EMPTY represents a successful parse from a voided non-terminal.
     * AST_CHAR just holds a character.
     * AST_TREE represents a successful parse from a non-terminal. It holds:
     * - the non-terminal's name
     * - a list of child asts
     */
    var AST, Continuations, ErrAny, ErrCC, ErrChar, Exp, MachineConfiguration, MachineState, Modes, NonTerminal, ParseError, RawError, Value, WaxeyeParser, namespace, nonterminal, updateError;
    AST = (function() {
      function AST(type, ch1, str1, asts1) {
        this.type = type;
        this.ch = ch1;
        this.str = str1;
        this.asts = asts1;
        assert.ok(Array.isArray(this.asts) || !this.asts);
      }

      return AST;

    })();
    AST.EMPTY = function() {
      return new AST("EMPTY");
    };
    AST.CHAR = function(ch) {
      return new AST("CHAR", ch);
    };
    AST.TREE = function(str, asts) {
      return new AST("TREE", null, str, asts);
    };
    NonTerminal = (function() {
      function NonTerminal(mode1, exp1) {
        this.mode = mode1;
        this.exp = exp1;
      }

      return NonTerminal;

    })();
    nonterminal = function(mode, exp) {
      return new NonTerminal(mode, exp);
    };
    Exp = (function() {
      function Exp(type, args) {
        this.type = type;
        this.args = args;
      }

      return Exp;

    })();
    ["ANY", "NT", "VOID", "CHAR", "CHAR_CLASS", "AND", "NOT", "OPT", "ALT", "SEQ", "STAR", "PLUS"].map(function(expType) {
      return Exp[expType] = function() {
        return new Exp(expType, Array.prototype.slice.call(arguments));
      };
    });
    Modes = (function() {
      function Modes() {}

      return Modes;

    })();
    Modes.NORMAL = "NORMAL";
    Modes.PRUNING = "PRUNING";
    Modes.VOIDING = "VOIDING";
    ParseError = (function() {
      function ParseError(pos1, line1, col1, nt1, chars) {
        this.pos = pos1;
        this.line = line1;
        this.col = col1;
        this.nt = nt1;
        this.chars = chars;
      }

      ParseError.prototype.toString = function() {
        var ref;
        if (this.chars.charClasses) {
          this.chars = this.chars.charClasses;
        }
        this.chars = (ref = this.chars) != null ? ref.map(function(ch) {
          return JSON.stringify(ch.char || ch.charClasses || '');
        }) : void 0;
        return "parse error: failed to match '" + this.nt.join(',') + "' at line=" + this.line + ", col=" + this.col + ", pos=" + this.pos + " (expected '" + this.chars.map(function(s) {
          return s.slice(1, -1);
        }).join(',') + "')";
      };

      return ParseError;

    })();
    ErrChar = (function() {
      function ErrChar(char) {
        this.char = char;
      }

      return ErrChar;

    })();
    ErrCC = (function() {
      function ErrCC(charClasses1) {
        this.charClasses = charClasses1;
      }

      return ErrCC;

    })();
    ErrAny = (function() {
      function ErrAny() {}

      return ErrAny;

    })();
    RawError = (function() {
      function RawError(pos1, nonterminals, failedChars, currentNT) {
        this.pos = pos1;
        this.nonterminals = nonterminals;
        this.failedChars = failedChars;
        this.currentNT = currentNT;
        assert.ok(Array.isArray(this.nonterminals));
        assert.ok(Array.isArray(this.failedChars));
      }

      RawError.prototype.toParseError = function(input) {
        var col, line, ref;
        ref = getLineCol(this.pos, input), line = ref[0], col = ref[1];
        return new ParseError(this.pos, line, col, uniq(this.nonterminals), this.failedChars.reverse());
      };

      return RawError;

    })();
    Value = (function() {
      function Value(type, err1, pos1, asts1) {
        this.type = type;
        this.err = err1;
        this.pos = pos1;
        this.asts = asts1;
        assert.ok(Array.isArray(this.asts) || !this.asts);
        assert.ok(typeof this.pos === 'number' || !this.pos);
        assert.ok(typeof this.err === 'object' || !this.err);
      }

      return Value;

    })();
    Value.FAIL = function(err) {
      return new Value("FAIL", err);
    };
    Value.VAL = function(pos, asts, err) {
      return new Value("VAL", err, pos, asts);
    };
    MachineConfiguration = (function() {
      function MachineConfiguration(type, exp1, pos1, asts1, err1, continuations1, value1) {
        this.type = type;
        this.exp = exp1;
        this.pos = pos1;
        this.asts = asts1;
        this.err = err1;
        this.continuations = continuations1;
        this.value = value1;
        assert.ok(Array.isArray(this.asts) || !this.asts);
        assert.ok(Array.isArray(this.continuations));
        assert.ok(typeof this.pos === 'number' || !this.pos);
      }

      return MachineConfiguration;

    })();
    MachineConfiguration.EVAL = function(exp, pos, asts, err, continuations) {
      return new MachineConfiguration("EVAL", exp, pos, asts, err, continuations, null);
    };
    MachineConfiguration.APPLY = function(continuations, value) {
      return new MachineConfiguration("APPLY", null, null, null, null, continuations, value);
    };
    MachineState = (function() {
      function MachineState(type, result1, configuration1) {
        this.type = type;
        this.result = result1;
        this.configuration = configuration1;
      }

      return MachineState;

    })();
    MachineState.FINAL = function(result) {
      return new MachineState("FINAL", result, null);
    };
    MachineState.INTER = function(configuration) {
      return new MachineState("INTER", null, configuration);
    };
    Continuations = (function() {
      function Continuations(type, pos1, expressions1, expression, asts1, err1, mode1, name1, nt1) {
        this.type = type;
        this.pos = pos1;
        this.expressions = expressions1;
        this.expression = expression;
        this.asts = asts1;
        this.err = err1;
        this.mode = mode1;
        this.name = name1;
        this.nt = nt1;
        assert.ok(Array.isArray(this.asts) || !this.asts);
      }

      return Continuations;

    })();
    Continuations.CONT_SEQ = function(expressions) {
      return new Continuations("CONT_SEQ", null, expressions, null, null, null, null, null, null);
    };
    Continuations.CONT_ALT = function(expressions, pos, asts) {
      return new Continuations("CONT_ALT", pos, expressions, null, asts, null, null, null, null);
    };
    Continuations.CONT_AND = function(pos, asts, err) {
      return new Continuations("CONT_AND", pos, null, null, asts, err, null, null, null);
    };
    Continuations.CONT_NOT = function(pos, asts, err) {
      return new Continuations("CONT_NOT", pos, null, null, asts, err, null, null, null);
    };
    Continuations.CONT_OPT = function(pos, asts) {
      return new Continuations("CONT_OPT", pos, null, null, asts, null, null, null, null);
    };
    Continuations.CONT_STAR = function(exp, pos, asts) {
      return new Continuations("CONT_STAR", pos, null, exp, asts, null, null, null, null);
    };
    Continuations.CONT_PLUS = function(exp) {
      return new Continuations("CONT_PLUS", null, null, exp, null, null, null, null, null);
    };
    Continuations.CONT_VOID = function(asts) {
      return new Continuations("CONT_VOID", null, null, null, asts, null, null, null, null);
    };
    Continuations.CONT_NT = function(mode, name, asts, nt) {
      return new Continuations("CONT_NT", null, null, null, asts, null, mode, name, nt);
    };
    updateError = function(err, pos, e) {
      if (err && pos > err.pos) {
        return new RawError(pos, [err != null ? err.currentNT : void 0], [e], err != null ? err.currentNT : void 0);
      } else if (!err || pos === (err != null ? err.pos : void 0)) {
        return new RawError((err != null ? err.pos : void 0) || 0, arrayPrepend((err != null ? err.currentNT : void 0) || "", (err != null ? err.nonterminals : void 0) || []), arrayPrepend(e, (err != null ? err.failedChars : void 0) || []), (err != null ? err.currentNT : void 0) || "");
      } else {
        return err;
      }
    };
    WaxeyeParser = (function() {
      function WaxeyeParser(env, start) {
        this.env = env;
        this.start = start;
      }

      WaxeyeParser.prototype.match = function(nt, input) {
        var eof, inputLen, move, state;
        inputLen = input.length;
        eof = function(pos) {
          return pos >= inputLen;
        };
        move = function(conf) {
          var asts, c, cc, e, err, errCcs, errNts, errPos, es, exp, exprs, firstExp, k, kFirst, kRest, mode, name, newErr, newval, pos, ref, ref1, ref10, ref11, ref12, ref13, ref2, ref3, ref4, ref5, ref6, ref7, ref8, ref9, restExp, ts, valAsts, value, visit;
          asts = conf.asts;
          pos = conf.pos;
          exp = conf.exp;
          err = conf.err;
          k = conf.continuations;
          if (k) {
            kFirst = first(k);
          }
          if (k) {
            kRest = rest(k);
          }
          es = kFirst != null ? kFirst.expressions : void 0;
          if (es) {
            firstExp = first(es);
          }
          if (es) {
            restExp = rest(es);
          }
          switch (conf.type) {
            case "EVAL":
              switch (exp.type) {
                case "ANY":
                  if (eof(pos)) {
                    return MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(updateError(err, pos, new ErrAny()))));
                  } else {
                    return MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos + 1, arrayPrepend(AST.CHAR(input[pos]), asts), err)));
                  }
                  break;
                case "ALT":
                  es = exp.args;
                  if (es.length > 0) {
                    return MachineState.INTER(MachineConfiguration.EVAL(first(es), pos, asts, err, arrayPrepend(Continuations.CONT_ALT(rest(es), pos, asts), k)));
                  } else {
                    return MachineState.INTER(MachineConfiguration.APPLY(k, err));
                  }
                  break;
                case "AND":
                  return MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, [], err, arrayPrepend(Continuations.CONT_AND(pos, asts, err), k)));
                case "NOT":
                  return MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, [], err, arrayPrepend(Continuations.CONT_NOT(pos, asts, err), k)));
                case "VOID":
                  return MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, [], err, arrayPrepend(Continuations.CONT_VOID(asts), k)));
                case "CHAR":
                  c = exp.args[0];
                  if ((eof(pos)) || c !== input[pos]) {
                    newval = Value.FAIL(updateError(err, pos, new ErrChar(c)));
                  } else {
                    newval = Value.VAL(pos + 1, arrayPrepend(AST.CHAR(input[pos]), asts), err);
                  }
                  return MachineState.INTER(MachineConfiguration.APPLY(k, newval));
                case "CHAR_CLASS":
                  cc = exp.args;
                  visit = function(charClasses) {
                    var c1, c2, ref;
                    if (charClasses.length === 0) {
                      return MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(updateError(err, pos, new ErrCC(cc)))));
                    } else {
                      ref = first(charClasses), c1 = ref[0], c2 = ref[1];
                      if (c1 <= input[pos] && c2 >= input[pos]) {
                        return MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos + 1, arrayPrepend(AST.CHAR(input[pos]), asts), err)));
                      } else {
                        return visit(rest(charClasses));
                      }
                    }
                  };
                  if (eof(pos)) {
                    return MachineState.INTER(MachineConfiguration.APPLY(k, Value.FAIL(updateError(err, pos, new ErrCC(cc)))));
                  } else {
                    return visit(cc);
                  }
                  break;
                case "SEQ":
                  exprs = exp.args;
                  if (exprs === null) {
                    return MachineState.INTER(MachineConfiguration.APPLY(k, Value.VAL(pos, asts, err)));
                  } else {
                    return MachineState.INTER(MachineConfiguration.EVAL(first(exprs), pos, asts, err, arrayPrepend(Continuations.CONT_SEQ(rest(exprs)), k)));
                  }
                  break;
                case "PLUS":
                  return MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, arrayPrepend(Continuations.CONT_PLUS(exp.args[0]), k)));
                case "STAR":
                  return MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, arrayPrepend(Continuations.CONT_STAR(exp.args[0], pos, asts), k)));
                case "OPT":
                  return MachineState.INTER(MachineConfiguration.EVAL(exp.args[0], pos, asts, err, arrayPrepend(Continuations.CONT_OPT(pos, asts), k)));
                case "NT":
                  name = exp.args[0];
                  mode = this.env[name].mode;
                  e = this.env[name].exp;
                  err = new RawError(err.pos, err.nonterminals, err.failedChars, name);
                  return MachineState.INTER(MachineConfiguration.EVAL(e, pos, [], err, arrayPrepend(Continuations.CONT_NT(mode, name, asts, conf.err.currentNT), k)));
                default:
                  throw new Error('unsupported 2');
              }
              break;
            case "APPLY":
              if (((ref = conf.value) != null ? ref.type : void 0) === "FAIL" && ["CONT_ALT"].indexOf(kFirst != null ? kFirst.type : void 0) === -1) {
                if (["CONT_SEQ", "CONT_VOID", "CONT_PLUS"].indexOf(kFirst != null ? kFirst.type : void 0) !== -1) {
                  return MachineState.INTER(MachineConfiguration.APPLY(kRest, conf.value));
                } else if (["CONT_AND"].indexOf(kFirst != null ? kFirst.type : void 0) !== -1) {
                  return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.FAIL(kFirst.err)));
                } else if (["CONT_NOT"].indexOf(kFirst != null ? kFirst.type : void 0) !== -1) {
                  return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(kFirst.pos, kFirst.asts, conf.err)));
                } else if (["CONT_STAR", "CONT_OPT"].indexOf(kFirst != null ? kFirst.type : void 0) !== -1) {
                  return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(kFirst.pos, kFirst.asts, conf.value.err)));
                } else if (["CONT_NT"].indexOf(kFirst != null ? kFirst.type : void 0) !== -1) {
                  err = conf.value.err;
                  return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.FAIL(new RawError(err.pos, err.nonterminals, err.failedChars, kFirst.nt))));
                } else {
                  return MachineState.FINAL(conf.value.err.toParseError(input));
                }
              } else if ((kFirst != null ? kFirst.type : void 0) === "CONT_SEQ") {
                if (es.length > 0) {
                  return MachineState.INTER(MachineConfiguration.EVAL(firstExp, conf.value.pos, conf.value.asts, conf.value.err, arrayPrepend(Continuations.CONT_SEQ(restExp), kRest)));
                } else {
                  return MachineState.INTER(MachineConfiguration.APPLY(kRest, conf.value));
                }
              } else if ((kFirst != null ? kFirst.type : void 0) === "CONT_STAR" || (kFirst != null ? kFirst.type : void 0) === "CONT_PLUS") {
                assert.ok(typeof conf.value !== 'undefined');
                return MachineState.INTER(MachineConfiguration.EVAL(kFirst.expression, conf.value.pos, conf.value.asts, conf.value.err, arrayPrepend(Continuations.CONT_STAR(kFirst.expression, conf.value.pos, conf.value.asts), kRest)));
              } else if ((kFirst != null ? kFirst.type : void 0) === "CONT_VOID") {
                return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(conf.value.pos, kFirst.asts, conf.value.err)));
              } else if ((kFirst != null ? kFirst.type : void 0) === "CONT_ALT") {
                if (((ref1 = conf.value) != null ? ref1.type : void 0) === "FAIL" && es.length > 0) {
                  return MachineState.INTER(MachineConfiguration.EVAL(first(es), kFirst.pos, kFirst.asts, conf.value.err, arrayPrepend(Continuations.CONT_ALT(rest(es), kFirst.pos, kFirst.asts), kRest)));
                } else {
                  return MachineState.INTER(MachineConfiguration.APPLY(kRest, conf.value));
                }
              } else if ((kFirst != null ? kFirst.type : void 0) === "CONT_OPT") {
                return MachineState.INTER(MachineConfiguration.APPLY(kRest, conf.value));
              } else if ((kFirst != null ? kFirst.type : void 0) === "CONT_AND") {
                return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(kFirst.pos, kFirst.asts, kFirst.err)));
              } else if ((kFirst != null ? kFirst.type : void 0) === "CONT_NOT") {
                return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.FAIL(kFirst.err)));
              } else if ((kFirst != null ? kFirst.type : void 0) === "CONT_NT") {
                mode = kFirst.mode, name = kFirst.name, asts = kFirst.asts, nt = kFirst.nt;
                value = conf.value;
                valAsts = value != null ? value.asts : void 0;
                errPos = value != null ? (ref2 = value.err) != null ? ref2.pos : void 0 : void 0;
                errNts = value != null ? (ref3 = value.err) != null ? ref3.nonterminals : void 0 : void 0;
                errCcs = value != null ? (ref4 = value.err) != null ? ref4.failedChars : void 0 : void 0;
                newErr = new RawError(errPos, errNts, errCcs, nt);
                switch (mode) {
                  case "NORMAL":
                    return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(value.pos, arrayPrepend(AST.TREE(name, valAsts.reverse()), asts), newErr)));
                  case "PRUNING":
                    if (valAsts.length === 0) {
                      return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(value.pos, asts, newErr)));
                    } else if (valAsts.length === 1) {
                      return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(value.pos, arrayPrepend(first(valAsts), asts), newErr)));
                    } else {
                      return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(value.pos, arrayPrepend(AST.TREE(name, valAsts.reverse()), asts), newErr)));
                    }
                    break;
                  case "VOIDING":
                    return MachineState.INTER(MachineConfiguration.APPLY(kRest, Value.VAL(value.pos, asts, newErr)));
                }
              } else if (kFirst != null) {
                throw new Error('unsupported 4');
              } else if (((ref5 = conf.value) != null ? ref5.type : void 0) === "VAL") {
                ts = (ref6 = conf.value) != null ? ref6.asts : void 0;
                if (eof((ref7 = conf.value) != null ? ref7.pos : void 0)) {
                  if (this.env[this.start].mode === Modes.NORMAL) {
                    return MachineState.FINAL(AST.TREE(this.start, ts.reverse()));
                  } else if (this.env[this.start].mode === Modes.PRUNING) {
                    if (ts.length === 0) {
                      return MachineState.FINAL(AST.EMPTY());
                    } else if (ts.length === 1) {
                      return MachineState.FINAL(first(ts));
                    } else {
                      return MachineState.FINAL(AST.TREE(this.start, ts.reverse()));
                    }
                  } else {
                    return MachineState.FINAL(AST.EMPTY());
                  }
                } else if ((((ref8 = conf.value) != null ? ref8.err : void 0) != null) && ((ref9 = conf.value) != null ? ref9.pos : void 0) === ((ref10 = conf.value) != null ? (ref11 = ref10.err) != null ? ref11.pos : void 0 : void 0)) {
                  err = conf.value.err;
                  return MachineState.FINAL((new RawError(conf.value.pos, err.nonterminals, err.failedChars)).toParseError(input));
                } else {
                  return MachineState.FINAL((new RawError((ref12 = conf.value) != null ? ref12.pos : void 0, [], [])).toParseError(input));
                }
              } else if (((ref13 = conf.value) != null ? ref13.type : void 0) === "FAIL") {
                return MachineState.FINAL(conf.value.err.toParseError(input));
              } else {
                throw new Error('unsupported 3');
              }
          }
        };
        state = move(MachineConfiguration.EVAL(this.env[nt].exp, 0, [], new RawError(0, [nt], [], nt), []));
        while (state.type !== "FINAL") {
          state = move.bind(this)(state.configuration);
        }
        return state.result;
      };

      WaxeyeParser.prototype.parse = function(input) {
        return this.match(this.start, input);
      };

      return WaxeyeParser;

    })();
    return namespace = {
      AST: AST,
      nonterminal: nonterminal,
      Exp: Exp,
      Modes: Modes,
      ParseError: ParseError,
      ErrChar: ErrChar,
      ErrCC: ErrCC,
      ErrAny: ErrAny,
      WaxeyeParser: WaxeyeParser
    };
  })();

  if (typeof module !== "undefined" && module !== null) {
    module.exports = waxeye;
  }

}).call(this);
