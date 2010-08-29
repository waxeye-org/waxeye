var waxeye;
/*
# Waxeye Parser Generator
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.
*/
waxeye = (function() {
  var AST, Edge, FA, InnerParser, ParseError, State, WaxeyeParser, namespace;
  Edge = function(_a, _b, _c) {
    this.voided = _c;
    this.state = _b;
    this.trans = _a;
    return this;
  };
  State = function(_a, _b) {
    this.match = _b;
    this.edges = _a;
    return this;
  };
  FA = function(_a, _b, _c) {
    this.mode = _c;
    this.states = _b;
    this.type = _a;
    return this;
  };
  FA.VOID = 0;
  FA.PRUNE = 1;
  FA.LEFT = 2;
  FA.POS = 3;
  FA.NEG = 4;
  ParseError = function(_a, _b, _c, _d) {
    this.nt = _d;
    this.col = _c;
    this.line = _b;
    this.pos = _a;
    return this;
  };
  ParseError.prototype.toString = function() {
    return "parse error: failed to match '" + this.nt + "' at line=" + this.line + ", col=" + this.col + ", pos=" + this.pos;
  };
  AST = function(_a, _b, _c) {
    this.pos = _c;
    this.children = _b;
    this.type = _a;
    return this;
  };
  AST.prototype.toString = function() {
    var acc, indent, toStringIter;
    acc = "";
    indent = 0;
    toStringIter = function(ast) {
      var _a, _b, _c, a, i;
      i = 0;
      while (i < indent - 1) {
        acc += '    ';
        i++;
      }
      indent > 0 ? acc += '->  ' : null;
      acc += ast.type;
      indent++;
      _b = ast.children;
      for (_a = 0, _c = _b.length; _a < _c; _a++) {
        a = _b[_a];
        acc += '\n';
        if ((typeof a) === 'string') {
          i = 0;
          while (i < indent - 1) {
            acc += '    ';
            i++;
          }
          indent > 0 ? acc += '|   ' : null;
          acc += a;
        } else {
          toStringIter(a);
        }
      }
      indent--;
      return acc;
    };
    return toStringIter(this);
  };
  WaxeyeParser = function(_a, _b, _c) {
    this.automata = _c;
    this.eofCheck = _b;
    this.start = _a;
    return this;
  };
  WaxeyeParser.prototype.parse = function(input) {
    return new InnerParser(this.start, this.eofCheck, this.automata, input).parse();
  };
  InnerParser = function(_a, _b, _c, input) {
    this.automata = _c;
    this.eofCheck = _b;
    this.start = _a;
    this.input = input;
    this.inputLen = input.length;
    this.inputPos = 0;
    this.line = 1;
    this.column = 0;
    this.lastCR = false;
    this.errorPos = 0;
    this.errorLine = 1;
    this.errorCol = 0;
    this.errorNT = this.automata[this.start].type;
    this.faStack = [];
    this.cache = {};
    return this;
  };
  InnerParser.prototype.parse = function() {
    return this.doEOFCheck(this.matchAutomaton(this.start));
  };
  InnerParser.prototype.matchAutomaton = function(index) {
    var _a, automaton, cached, key, mode, res, startCR, startCol, startLine, startPos, type, value;
    startPos = this.inputPos;
    key = ("" + index + "," + startPos);
    cached = this.cache[key];
    if (typeof cached !== "undefined" && cached !== null) {
      this.restorePos(cached[1], cached[2], cached[3], cached[4]);
      return cached[0];
    }
    startLine = this.line;
    startCol = this.column;
    startCR = this.lastCR;
    automaton = this.automata[index];
    type = automaton.type;
    mode = automaton.mode;
    this.faStack.push(automaton);
    res = this.matchState(0);
    this.faStack.pop();
    value = (function() {
      if (mode === FA.POS) {
        this.restorePos(startPos, startLine, startCol, startCR);
        if (res) {
          return true;
        } else {
          this.updateError();
          return false;
        }
      } else if (mode === FA.NEG) {
        this.restorePos(startPos, startLine, startCol, startCR);
        if (res) {
          this.updateError();
          return false;
        } else {
          return true;
        }
      } else {
        if (res) {
          if (mode === FA.VOID) {
            return true;
          } else if (mode === FA.PRUNE) {
            if ((_a = res.length) === 0) {
              return true;
            } else if (_a === 1) {
              return res[0];
            } else {
              return new AST(type, res, [startPos, this.inputPos]);
            }
          } else {
            return new AST(type, res, [startPos, this.inputPos]);
          }
        } else {
          return this.updateError();
        }
      }
    }).call(this);
    this.cache[key] = [value, this.inputPos, this.line, this.column, this.lastCR];
    return value;
  };
  InnerParser.prototype.matchState = function(index) {
    var res, state;
    state = this.faStack[this.faStack.length - 1].states[index];
    res = this.matchEdges(state.edges, 0);
    return res ? res : state.match && [];
  };
  InnerParser.prototype.matchEdges = function(edges, index) {
    var res;
    if (index === edges.length) {
      return false;
    } else {
      res = this.matchEdge(edges[index]);
      return res ? res : this.matchEdges(edges, (index + 1));
    }
  };
  InnerParser.prototype.matchEdge = function(edge) {
    var res, startCR, startCol, startLine, startPos, t, tranRes;
    startPos = this.inputPos;
    startLine = this.line;
    startCol = this.column;
    startCR = this.lastCR;
    t = edge.trans;
    res = t === -1 ? this.inputPos < this.inputLen ? this.mv() : this.updateError() : typeof t === 'string' ? this.inputPos < this.inputLen && t === this.input[this.inputPos] ? this.mv() : this.updateError() : t instanceof Array ? this.inputPos < this.inputLen && this.withinSet(t, 0, (this.input[this.inputPos].charCodeAt(0))) ? this.mv() : this.updateError() : typeof t === 'number' ? this.matchAutomaton(t) : false;
    if (res) {
      tranRes = this.matchState(edge.state);
      if (tranRes) {
        return edge.voided || res === true ? tranRes : [res].concat(tranRes);
      } else {
        this.restorePos(startPos, startLine, startCol, startCR);
        return false;
      }
    } else {
      return false;
    }
  };
  InnerParser.prototype.restorePos = function(pos, line, col, cr) {
    this.inputPos = pos;
    this.line = line;
    this.column = col;
    return (this.lastCR = cr);
  };
  InnerParser.prototype.updateError = function() {
    if (this.errorPos < this.inputPos) {
      this.errorPos = this.inputPos;
      this.errorLine = this.line;
      this.errorCol = this.column;
      this.errorNT = this.faStack[this.faStack.length - 1].type;
    }
    return false;
  };
  InnerParser.prototype.mv = function() {
    var ch;
    ch = this.input[this.inputPos];
    this.inputPos++;
    if (ch === '\r') {
      this.line++;
      this.column = 0;
      this.lastCR = true;
    } else {
      if (ch === '\n') {
        if (!this.lastCR) {
          this.line++;
          this.column = 0;
        }
      } else {
        this.column++;
      }
      this.lastCR = false;
    }
    return ch;
  };
  InnerParser.prototype.doEOFCheck = function(res) {
    return res ? this.eofCheck && this.inputPos < this.inputLen ? new ParseError(this.errorPos, this.errorLine, this.errorCol, this.errorNT) : res : new ParseError(this.errorPos, this.errorLine, this.errorCol, this.errorNT);
  };
  InnerParser.prototype.withinSet = function(set, index, c) {
    var aa;
    if (index === set.length) {
      return false;
    } else {
      aa = set[index];
      return typeof aa === 'string' ? (aa.charCodeAt(0)) === c ? true : (aa.charCodeAt(0)) < c ? this.withinSet(set, index + 1, c) : false : c >= aa[0] && c <= aa[1] ? true : c > aa[1] ? this.withinSet(set, index + 1, c) : false;
    }
  };
  namespace = {
    Edge: Edge,
    State: State,
    FA: FA,
    ParseError: ParseError,
    AST: AST,
    WaxeyeParser: WaxeyeParser
  };
  return namespace;
})();
if (typeof module !== "undefined" && module !== null) {
  module.exports.AST = waxeye.AST;
  module.exports.Edge = waxeye.Edge;
  module.exports.FA = waxeye.FA;
  module.exports.ParseError = waxeye.ParseError;
  module.exports.State = waxeye.State;
  module.exports.WaxeyeParser = waxeye.WaxeyeParser;
}