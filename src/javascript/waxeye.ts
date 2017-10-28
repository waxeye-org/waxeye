/*
 * Waxeye Parser Generator http://www.waxeye.org
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

export class WaxeyeParser {
  public constructor(
      public env: ParserConfig,
      public start: keyof ParserConfig /* keyof env */) {}

  public parse(input: string, start: string = this.start): AST|ParseError {
    if (!this.env[start]) {
      throw new Error(`Invalid non-terminal ${start}. Expected one of: ${
          Object.keys(this.env).join(', ')}`);
    }
    return match(this.env, start, input);
  }
}

export interface ParserConfig {
  // name -> NonTerminal
  [key: string]: NonTerminal;
}

export interface NonTerminal {
  mode: NonTerminalMode;
  exp: Expr;
}

export const enum NonTerminalMode {
  NORMAL = 1,
  PRUNING,
  VOIDING,
}

/*
 * An abstract syntax tree holds the non-terminal's name (`type`),
 * and a list of child ASTs.
 */
export class AST {
  constructor(public type: string, public children: Array<AST|string>) {}

  /**
   * The empty AST is an AST that has an empty `type` and no children.
   */
  public isEmpty(): this is EmptyAST {
    return this.type === '' && this.children.length === 0;
  }
}

export interface EmptyAST extends AST {
  type: '';
  children: never[];
}

export function EmptyAST(): EmptyAST {
  return new AST('', []) as EmptyAST;
}

export type Expr = ExprNonTerminal|ExprAlt|ExprSeq|ExprPlus|ExprStar|ExprOpt|
    ExprAnd|ExprNot|ExprVoid|ExprAny|ExprChar|ExprCharClass;

export const enum ExprType {
  NT = 1,
  ALT,
  SEQ,
  PLUS,
  STAR,
  OPT,
  AND,
  NOT,
  VOID,
  ANY,
  CHAR,
  CHAR_CLASS,
}

export interface ExprNonTerminal {
  type: ExprType.NT;
  name: string;
}
export interface ExprAlt {
  type: ExprType.ALT;
  exprs: Expr[];
}
export interface ExprSeq {
  type: ExprType.SEQ;
  exprs: Expr[];
}
export interface ExprPlus {
  type: ExprType.PLUS;
  expr: Expr;
}
export interface ExprStar {
  type: ExprType.STAR;
  expr: Expr;
}
export interface ExprOpt {
  type: ExprType.OPT;
  expr: Expr;
}
export interface ExprAnd {
  type: ExprType.AND;
  expr: Expr;
}
export interface ExprNot {
  type: ExprType.NOT;
  expr: Expr;
}
export interface ExprVoid {
  type: ExprType.VOID;
  expr: Expr;
}
export interface ExprAny { type: ExprType.ANY; }
export interface ExprChar {
  type: ExprType.CHAR;
  // A single character (represented by a single Unicode codepoint).
  char: string;
}
export interface ExprCharClass {
  type: ExprType.CHAR_CLASS;
  // A list of Unicode codepoints / ranges of codepoints.
  codepoints: Array<number|[number, number]>;
}

export interface MatchError {
  // Expected expression in the grammar format.
  toGrammarString(): string;
}

// A failed single character match.
export class ErrChar implements MatchError {
  constructor(public char: string) {}

  public toGrammarString() {
    return `'${JSON.stringify(this.char).slice(1, -1)}'`;
  }
}

// A failed character class match.
export class ErrCC implements MatchError {
  // A list of Unicode codepoints / ranges of codepoints.
  constructor(public charClasses: Array<number|[number, number]>) {}

  public toGrammarString() {
    return `[${
        this.charClasses
            .map((charClass) => {
              return JSON
                  .stringify(
                      typeof charClass === 'number' ?
                          String.fromCodePoint(charClass) :
                          `${String.fromCodePoint(charClass[0])}-${
                              String.fromCodePoint(charClass[1])}`)
                  .slice(1, -1);
            })
            .join('')}]`;
  }
}

// A failed wildcard match.
export class ErrAny implements MatchError {
  public toGrammarString() {
    return '.';
  }
}

export class ParseError {
  constructor(
      public pos: number, public line: number, public col: number,
      public nt: string[], public chars: MatchError[]) {}

  public toString(): string {
    const chars =
        this.chars.map((err) => err.toGrammarString()).join(' | ') || '\'\'';
    return `Parse error: Failed to match '${this.nt.join(',')}' at line=${
        this.line}, col=${this.col}, pos=${this.pos}. Expected: ${chars}`;
  }
}
class RawError {
  constructor(
      public pos: number, public nonterminals: string[],
      public failedChars: MatchError[], public currentNT: string) {}

  public toParseError(input: string): ParseError {
    const [line, col] = getLineCol(this.pos, input);
    const uniqueNonterminals: string[] = [];
    const seenNonterminals = new Set<string>();
    for (const nt of this.nonterminals) {
      if (seenNonterminals.has(nt)) {
        continue;
      }
      uniqueNonterminals.push(nt);
      seenNonterminals.add(nt);
    }
    return new ParseError(
        this.pos, line, col, uniqueNonterminals, this.failedChars.reverse());
  }
}

function updateError(err: RawError, pos: number, e: MatchError): RawError {
  if (err !== null) {
    if (pos > err.pos) {
      return new RawError(pos, [err.currentNT], [e], err.currentNT);
    } else if (pos === err.pos) {
      return new RawError(
          err.pos, [err.currentNT, ...err.nonterminals],
          [e, ...err.failedChars], err.currentNT);
    } else {
      return new RawError(
          err.pos, err.nonterminals, err.failedChars, err.currentNT);
    }
  } else {
    return new RawError(0, [''], [e], '');
  }
}

type Continuation =
    ContSeq|ContAlt|ContAnd|ContNot|ContOpt|ContStar|ContPlus|ContVoid|ContNT;

const enum ContType {
  SEQ = 1,
  ALT,
  AND,
  NOT,
  OPT,
  STAR,
  PLUS,
  VOID,
  NT,
}

interface ContSeq {
  type: ContType.SEQ;
  expressions: Expr[];
}
function contSeq(expressions: Expr[]): ContSeq {
  return {type: ContType.SEQ, expressions};
}

interface ContAlt {
  type: ContType.ALT;
  expressions: Expr[];
  pos: number;
  asts: Array<string|AST>;
}
function contAlt(
    expressions: Expr[], pos: number, asts: Array<string|AST>): ContAlt {
  return {type: ContType.ALT, expressions, pos, asts};
}

interface ContAnd {
  type: ContType.AND;
  pos: number;
  asts: Array<string|AST>;
  err: RawError;
}
function contAnd(pos: number, asts: Array<string|AST>, err: RawError): ContAnd {
  return {type: ContType.AND, pos, asts, err};
}

interface ContNot {
  type: ContType.NOT;
  pos: number;
  asts: Array<string|AST>;
  err: RawError;
}
function contNot(pos: number, asts: Array<string|AST>, err: RawError): ContNot {
  return {type: ContType.NOT, pos, asts, err};
}

interface ContOpt {
  type: ContType.OPT;
  pos: number;
  asts: Array<string|AST>;
}
function contOpt(pos: number, asts: Array<string|AST>): ContOpt {
  return {type: ContType.OPT, pos, asts};
}

interface ContStar {
  type: ContType.STAR;
  expression: Expr;
  pos: number;
  asts: Array<string|AST>;
}
function contStar(
    expression: Expr, pos: number, asts: Array<string|AST>): ContStar {
  return {type: ContType.STAR, expression, pos, asts};
}

interface ContPlus {
  type: ContType.PLUS;
  expression: Expr;
}
function contPlus(expression: Expr): ContPlus {
  return {type: ContType.PLUS, expression};
}

interface ContVoid {
  type: ContType.VOID;
  asts: Array<string|AST>;
}
function contVoid(asts: Array<string|AST>): ContVoid {
  return {type: ContType.VOID, asts};
}

interface ContNT {
  type: ContType.NT;
  mode: NonTerminalMode;
  name: string;
  asts: Array<string|AST>;
  nt: string;
}
function contNT(
    mode: NonTerminalMode, name: string, asts: Array<string|AST>,
    nt: string): ContNT {
  return {type: ContType.NT, mode, name, asts, nt};
}

type MatchResult = Accepted|Rejected;

const enum MatchResultType {
  ACCEPT = 1,
  REJECT,
}

interface Accepted {
  type: MatchResultType.ACCEPT;
  pos: number;
  asts: Array<AST|string>;
  err: RawError;
}

function accept(pos: number, asts: Array<AST|string>, err: RawError): Accepted {
  return {type: MatchResultType.ACCEPT, pos, asts, err};
}

interface Rejected {
  type: MatchResultType.REJECT;
  err: RawError;
}

function reject(err: RawError): Rejected {
  return {type: MatchResultType.REJECT, err};
}

const enum ActionType {
  EVAL = 1,
  APPLY,
}

interface ActionEval {
  type: ActionType.EVAL;
  exp: Expr;
  pos: number;
  asts: Array<AST|string>;
  err: RawError;
  continuations: Continuation[];
}

function evalNext(
    exp: Expr, pos: number, asts: Array<AST|string>, err: RawError,
    continuations: Continuation[]): ActionEval {
  return {type: ActionType.EVAL, asts, continuations, err, exp, pos};
}

interface ActionApply {
  type: ActionType.APPLY;
  continuations: Continuation[];
  value: MatchResult;
}

function applyNext(
    continuations: Continuation[], value: MatchResult): ActionApply {
  return {type: ActionType.APPLY, continuations, value};
}

function match(env: ParserConfig, start: keyof ParserConfig, input: string):
    AST|ParseError {
  // move from initial state to halting state
  let action = moveEval(
      env, input,
      evalNext(env[start].exp, 0, [], new RawError(0, [start], [], start), []));
  while (true) {
    switch (action.type) {
      case ActionType.EVAL:
        action = moveEval(env, input, action);
        break;
      case ActionType.APPLY:
        const {continuations, value} = action;
        if (continuations.length === 0) {
          return moveReturn(env, start, input, value);
        }
        const [evaluated, ...rest] = continuations;
        action = moveApply(env, input, value, evaluated, rest);
        break;
    }
  }
}

// Evaluates the result of the expression given in `action`.
function moveEval(env: ParserConfig, input: string, action: ActionEval):
    ActionEval|ActionApply {
  const {exp, pos, asts, err, continuations} = action;
  const eof = pos >= input.length;
  switch (exp.type) {
    case ExprType.ANY:
      if (eof) {
        return applyNext(
            continuations, reject(updateError(err, pos, new ErrAny())));
      } else {
        // Advance one position if the input code-point is in BMP, two positions
        // otherwise.
        return applyNext(
            continuations,
            isSingleCharCodepoint(codePointAtOrFail(input, pos)) ?
                accept(pos + 1, [input[pos], ...asts], err) :
                // A non single-char code-point implies !eof(pos + 1)
                accept(pos + 2, [input[pos] + input[pos + 1], ...asts], err));
      }
    case ExprType.ALT: {
      const {exprs} = exp;
      if (exprs.length > 0) {
        return evalNext(
            exprs[0], pos, asts, err,
            [contAlt(exprs.slice(1), pos, asts), ...continuations]);
      } else {
        return applyNext(continuations, reject(err));
      }
    }
    case ExprType.AND:
      return evalNext(
          exp.expr, pos, [], err, [contAnd(pos, asts, err), ...continuations]);
    case ExprType.NOT:
      return evalNext(
          exp.expr, pos, [], err, [contNot(pos, asts, err), ...continuations]);
    case ExprType.VOID:
      return evalNext(
          exp.expr, pos, [], err, [contVoid(asts), ...continuations]);
    case ExprType.CHAR:
      const c = exp.char;
      return applyNext(
          continuations,
          c.length === 1 ?
              eof || c !== input[pos] ?
              reject(updateError(err, pos, new ErrChar(c))) :
              accept(pos + 1, [input[pos], ...asts], err) :
              // c.length === 2:
              pos + 1 >= input.length || c[0] !== input[pos] ||
                      c[1] !== input[pos + 1] ?
              reject(updateError(err, pos, new ErrChar(c))) :
              accept(pos + 2, [input[pos] + input[pos + 1], ...asts], err));
    case ExprType.CHAR_CLASS:
      const cc = exp.codepoints;
      if (eof) {
        return applyNext(
            continuations, reject(updateError(err, pos, new ErrCC(cc))));
      }
      // JavaScript string comparison does not compare Unicode characters
      // correctly, so we must compare codepoints. Example:
      //   'ﬆ' > '𝌆' //=> true
      //   'ﬆ'.codePointAt(0) > '𝌆'.codePointAt(0) //=> false
      const inputCodePoint = codePointAtOrFail(input, pos);
      // Loop over cc instead of recursing to avoid stack overflow on large
      // character classes.
      for (const charClass of cc) {
        const isMatch = typeof charClass === 'number' ?
            // Single character
            charClass === inputCodePoint :
            // Range
            charClass[0] <= inputCodePoint && charClass[1] >= inputCodePoint;
        if (isMatch) {
          return applyNext(
              continuations,
              isSingleCharCodepoint(inputCodePoint) ?
                  accept(pos + 1, [input[pos], ...asts], err) :
                  accept(pos + 2, [input[pos] + input[pos + 1], ...asts], err));
        }
      }
      return applyNext(
          continuations, reject(updateError(err, pos, new ErrCC(cc))));
    case ExprType.SEQ: {
      // A sequence is made up of a list of expressions.
      // We traverse the list, making sure each expression succeeds.
      // The rest of the string returned by the expression is used
      // as input to the next expression.
      const {exprs} = exp;
      if (exprs.length === 0) {
        return applyNext(continuations, accept(pos, asts, err));
      } else {
        return evalNext(
            exprs[0], pos, asts, err,
            [contSeq(exprs.slice(1)), ...continuations]);
      }
    }
    case ExprType.PLUS:
      return evalNext(
          exp.expr, pos, asts, err, [contPlus(exp.expr), ...continuations]);
    case ExprType.STAR:
      return evalNext(
          exp.expr, pos, asts, err,
          [contStar(exp.expr, pos, asts), ...continuations]);
    case ExprType.OPT:
      return evalNext(
          exp.expr, pos, asts, err, [contOpt(pos, asts), ...continuations]);
    case ExprType.NT:
      const {name} = exp;
      const nt = env[name];
      return evalNext(
          nt.exp, pos, [],
          new RawError(err.pos, err.nonterminals, err.failedChars, name), [
            contNT(nt.mode, name, asts, err.currentNT),
            ...continuations,
          ]);
    default:
      throw new Error(`Unsupported exp.type in exp=${
          (exp as any).type} action=${JSON.stringify(action)}`);
  }
}

// Handles the result of a processed continuation.
function moveApply(
    env: ParserConfig, input: string, value: MatchResult,
    evaluated: Continuation, rest: Continuation[]): ActionEval|ActionApply {
  switch (value.type) {
    case MatchResultType.ACCEPT:
      return moveApplyOnAccept(env, input, value, evaluated, rest);
    case MatchResultType.REJECT:
      return moveApplyOnReject(env, input, value, evaluated, rest);
  }
}

// Called after the `evaluated` continuation got accepted (matched).
function moveApplyOnAccept(
    env: ParserConfig, input: string, accepted: Accepted,
    evaluated: Continuation, rest: Continuation[]): ActionEval|ActionApply {
  switch (evaluated.type) {
    case ContType.SEQ:
      const es = evaluated.expressions;
      if (es.length > 0) {
        return evalNext(
            es[0], accepted.pos, accepted.asts, accepted.err,
            [contSeq(es.slice(1)), ...rest]);
      } else {
        return applyNext(rest, accepted);
      }
    case ContType.STAR:
    case ContType.PLUS:
      return evalNext(
          evaluated.expression, accepted.pos, accepted.asts, accepted.err, [
            contStar(evaluated.expression, accepted.pos, accepted.asts),
            ...rest,
          ]);
    case ContType.ALT:
    case ContType.OPT:
      return applyNext(rest, accepted);
    case ContType.AND:
      return applyNext(
          rest, accept(evaluated.pos, evaluated.asts, evaluated.err));
    case ContType.VOID:
      return applyNext(
          rest, accept(accepted.pos, evaluated.asts, accepted.err));
    case ContType.NOT:
      return applyNext(rest, reject(evaluated.err));
    case ContType.NT:
      const {mode, name, asts, nt} = evaluated;
      const valAsts = accepted.asts;
      const newErr = new RawError(
          accepted.err.pos, accepted.err.nonterminals, accepted.err.failedChars,
          nt);
      switch (mode) {
        case NonTerminalMode.NORMAL:
          return applyNext(
              rest,
              accept(
                  accepted.pos, [new AST(name, valAsts.reverse()), ...asts],
                  newErr));
        case NonTerminalMode.PRUNING:
          switch (valAsts.length) {
            case 0:
              return applyNext(rest, accept(accepted.pos, asts, newErr));
            case 1:
              return applyNext(
                  rest, accept(accepted.pos, [valAsts[0], ...asts], newErr));
            default:
              return applyNext(
                  rest,
                  accept(
                      accepted.pos, [new AST(name, valAsts.reverse()), ...asts],
                      newErr));
          }
        case NonTerminalMode.VOIDING:
          return applyNext(rest, accept(accepted.pos, asts, newErr));
        default:
          // Without this check, the TypeScript compiler doesn't
          // realize that the outer case is also exhaustive.
          // tslint:disable-next-line:no-unused-variable
          const checkExhaustive: never = mode;
          throw new Error(`Invalid mode: ${JSON.stringify(mode)}`);
      }
  }
}

// Called after the `evaluated` continuation got rejected (did not match).
function moveApplyOnReject(
    env: ParserConfig, input: string, rejected: Rejected,
    evaluated: Continuation, continuations: Continuation[]): ActionEval|
    ActionApply {
  switch (evaluated.type) {
    case ContType.ALT:
      const es = evaluated.expressions;
      if (es.length > 0) {
        return evalNext(es[0], evaluated.pos, evaluated.asts, rejected.err, [
          contAlt(es.slice(1), evaluated.pos, evaluated.asts),
          ...continuations,
        ]);
      } else {
        return applyNext(continuations, rejected);
      }
    case ContType.SEQ:
    case ContType.VOID:
    case ContType.PLUS:
      return applyNext(continuations, rejected);
    case ContType.AND:
      return applyNext(continuations, reject(evaluated.err));
    case ContType.NOT:
    case ContType.STAR:
    case ContType.OPT:
      return applyNext(
          continuations, accept(evaluated.pos, evaluated.asts, rejected.err));
    case ContType.NT:
      const err = rejected.err;
      return applyNext(
          continuations,
          reject(new RawError(
              err.pos, err.nonterminals, err.failedChars, evaluated.nt)));
    default:
      // tslint:disable-next-line:no-unused-variable
      const checkExhaustive: never = evaluated;
      throw new Error(`Invalid continuation: ${JSON.stringify(evaluated)}`);
  }
}

// Called after the final continuation was processed.
function moveReturn(
    env: ParserConfig, start: keyof ParserConfig, input: string,
    value: MatchResult): AST|ParseError {
  switch (value.type) {
    case MatchResultType.ACCEPT:
      const asts = value.asts;
      if (value.pos >= input.length) {
        switch (env[start].mode) {
          case NonTerminalMode.NORMAL:
            return new AST(start, asts.reverse());
          case NonTerminalMode.PRUNING:
            switch (asts.length) {
              case 0:
                return EmptyAST();
              case 1:
                const ast = asts[0];
                if (typeof ast === 'string') {
                  throw new Error(`Expected an AST, got a string ${
                      JSON.stringify(ast)}, in ${value}`);
                }
                return ast;
              default:
                return new AST(start, asts.reverse());
            }
          case NonTerminalMode.VOIDING:
            return EmptyAST();
        }
      } else if (value.err && value.pos === value.err.pos) {
        return new RawError(
                   value.pos, value.err.nonterminals, value.err.failedChars, '')
            .toParseError(input);
      } else {
        return new RawError(value.pos, [], [], '').toParseError(input);
      }
    case MatchResultType.REJECT:
      return value.err.toParseError(input);
  }
}

function getLineCol(pos: number, input: string): [number, number] {
  let lineNumber = 1;
  let lineStartPos = 0;
  let newlinePos = -1;
  // tslint:disable-next-line:no-conditional-assignment
  while ((newlinePos = input.indexOf('\n', lineStartPos)) !== -1 &&
         newlinePos < pos) {
    ++lineNumber;
    lineStartPos = newlinePos + 1;
  }
  return [lineNumber, pos - lineStartPos + 1];
}

// Whether the given Unicode code-point can be represented
// by a single JavaScript String (UTF-16) character.
function isSingleCharCodepoint(codePoint: number) {
  return codePoint <= 0xFFFF;
}

function codePointAtOrFail(input: string, pos: number): number {
  const codePoint = input.codePointAt(pos);
  if (typeof codePoint === 'undefined') {
    throw new Error(
        `Undefined input codepoint at ${pos} in ${JSON.stringify(input)}`);
  }
  return codePoint;
}
