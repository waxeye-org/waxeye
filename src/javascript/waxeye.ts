/*
 * Waxeye Parser Generator http://www.waxeye.org
 * Licensed under the MIT license. See 'LICENSE' for details.
 */

import {cons, ConsList, empty} from './cons_list';
import {Expr, exprToRuntimeExpr, ExprType, RuntimeExpr} from './expr';

export {Expr, ExprType} from './expr';

export class WaxeyeParser {
  private readonly env: RuntimeParserConfig;
  public constructor(
      public readonly config: ParserConfig,
      public readonly start: string /* keyof env */) {
    this.env = parserConfigToRuntimeParserConfig(config);
  }

  public parse(input: string, start: string = this.start): AST|ParseError {
    if (!this.env[start]) {
      throw new Error(`Invalid non-terminal ${start}. Expected one of: ${
          Object.keys(this.env).join(', ')}`);
    }
    return match(this.env, start, input);
  }
}

export interface ParserConfig {
  // name -> ParserConfigNonTerminal
  [key: string]: {mode: NonTerminalMode, exp: Expr};
}
interface RuntimeParserConfig {
  // name -> ParserConfigNonTerminal
  [key: string]: {mode: NonTerminalMode, exp: RuntimeExpr};
}

function parserConfigToRuntimeParserConfig(config: ParserConfig):
    RuntimeParserConfig {
  const result: RuntimeParserConfig = {};
  for (const [name, nonterminal] of Object.entries(config)) {
    result[name] = {
      mode: nonterminal.mode,
      exp: exprToRuntimeExpr(nonterminal.exp),
    };
  }
  return result;
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
      public pos: number, public nonterminals: ConsList<string>,
      public failedChars: ConsList<MatchError>, public currentNT: string) {}

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
        this.pos, line, col, uniqueNonterminals,
        this.failedChars.toArray().reverse());
  }
}

function updateError(err: RawError, pos: number, e: MatchError): RawError {
  if (err !== null) {
    if (pos > err.pos) {
      return new RawError(
          pos, cons(err.currentNT, empty()), cons(e, empty()), err.currentNT);
    } else if (pos === err.pos) {
      return new RawError(
          err.pos, cons(err.currentNT, err.nonterminals),
          cons(e, err.failedChars), err.currentNT);
    } else {
      return new RawError(
          err.pos, err.nonterminals, err.failedChars, err.currentNT);
    }
  } else {
    return new RawError(0, cons('', empty()), cons(e, empty()), '');
  }
}

type ASTList = ConsList<AST|string>;

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
  expressions: ConsList<RuntimeExpr>;
}
function contSeq(expressions: ConsList<RuntimeExpr>): ContSeq {
  return {type: ContType.SEQ, expressions};
}

interface ContAlt {
  type: ContType.ALT;
  expressions: ConsList<RuntimeExpr>;
  pos: number;
  asts: ASTList;
}
function contAlt(
    expressions: ConsList<RuntimeExpr>, pos: number, asts: ASTList): ContAlt {
  return {type: ContType.ALT, expressions, pos, asts};
}

interface ContAnd {
  type: ContType.AND;
  pos: number;
  asts: ASTList;
  err: RawError;
}
function contAnd(pos: number, asts: ASTList, err: RawError): ContAnd {
  return {type: ContType.AND, pos, asts, err};
}

interface ContNot {
  type: ContType.NOT;
  pos: number;
  asts: ASTList;
  err: RawError;
}
function contNot(pos: number, asts: ASTList, err: RawError): ContNot {
  return {type: ContType.NOT, pos, asts, err};
}

interface ContOpt {
  type: ContType.OPT;
  pos: number;
  asts: ASTList;
}
function contOpt(pos: number, asts: ASTList): ContOpt {
  return {type: ContType.OPT, pos, asts};
}

interface ContStar {
  type: ContType.STAR;
  expression: RuntimeExpr;
  pos: number;
  asts: ASTList;
}
function contStar(
    expression: RuntimeExpr, pos: number, asts: ASTList): ContStar {
  return {type: ContType.STAR, expression, pos, asts};
}

interface ContPlus {
  type: ContType.PLUS;
  expression: RuntimeExpr;
}
function contPlus(expression: RuntimeExpr): ContPlus {
  return {type: ContType.PLUS, expression};
}

interface ContVoid {
  type: ContType.VOID;
  asts: ASTList;
}
function contVoid(asts: ASTList): ContVoid {
  return {type: ContType.VOID, asts};
}

interface ContNT {
  type: ContType.NT;
  mode: NonTerminalMode;
  name: string;
  asts: ASTList;
  nt: string;
}
function contNT(
    mode: NonTerminalMode, name: string, asts: ASTList, nt: string): ContNT {
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
  asts: ASTList;
  err: RawError;
}

function accept(pos: number, asts: ASTList, err: RawError): Accepted {
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
  exp: RuntimeExpr;
  pos: number;
  asts: ASTList;
  err: RawError;
  continuations: ConsList<Continuation>;
}

function evalNext(
    exp: RuntimeExpr, pos: number, asts: ASTList, err: RawError,
    continuations: ConsList<Continuation>): ActionEval {
  return {type: ActionType.EVAL, asts, continuations, err, exp, pos};
}

interface ActionApply {
  type: ActionType.APPLY;
  continuations: ConsList<Continuation>;
  value: MatchResult;
}

function applyNext(
    continuations: ConsList<Continuation>, value: MatchResult): ActionApply {
  return {type: ActionType.APPLY, continuations, value};
}

function match(
    env: RuntimeParserConfig, start: string /* keyof env */,
    input: string): AST|ParseError {
  // move from initial state to halting state
  let action = moveEval(
      env, input,
      evalNext(
          env[start].exp, /*pos=*/0, /*asts=*/empty(),
          new RawError(
              /*pos=*/0, /*nonterminals=*/cons(start, empty()),
              /*failedChars=*/empty(),
              /*currentNT=*/start),
          /*continuations=*/empty()));
  while (true) {
    switch (action.type) {
      case ActionType.EVAL:
        action = moveEval(env, input, action);
        break;
      case ActionType.APPLY:
        const {continuations, value} = action;
        if (continuations.isEmpty()) {
          return moveReturn(env, start, input, value);
        }
        action =
            moveApply(input, value, continuations.head, continuations.tail);
        break;
    }
  }
}

// Evaluates the result of the expression given in `action`.
function moveEval(env: RuntimeParserConfig, input: string, action: ActionEval):
    ActionEval|ActionApply {
  const {exp, pos, asts, err, continuations} = action;
  const eof = pos >= input.length;
  switch (exp.type) {
    case ExprType.ANY_CHAR:
      if (eof) {
        return applyNext(
            continuations, reject(updateError(err, pos, new ErrAny())));
      } else {
        // Advance one position if the input code-point is in BMP, two positions
        // otherwise.
        return applyNext(
            continuations,
            isSingleCharCodepoint(codePointAtOrFail(input, pos)) ?
                accept(pos + 1, cons(input[pos], asts), err) :
                // A non single-char code-point implies !eof(pos + 1)
                accept(pos + 2, cons(input[pos] + input[pos + 1], asts), err));
      }
    case ExprType.ALT: {
      const {exprs} = exp;
      if (exprs.isEmpty()) {
        return applyNext(continuations, reject(err));
      }
      return evalNext(
          exprs.head, pos, asts, err,
          cons(contAlt(exprs.tail, pos, asts), continuations));
    }
    case ExprType.AND:
      return evalNext(
          exp.expr, pos, /*asts=*/empty(), err,
          cons(contAnd(pos, asts, err), continuations));
    case ExprType.NOT:
      return evalNext(
          exp.expr, pos, /*asts=*/empty(), err,
          cons(contNot(pos, asts, err), continuations));
    case ExprType.VOID:
      return evalNext(
          exp.expr, pos, /*asts=*/empty(), err,
          cons(contVoid(asts), continuations));
    case ExprType.CHAR:
      const c = exp.char;
      return applyNext(
          continuations,
          c.length === 1 ?
              eof || c !== input[pos] ?
              reject(updateError(err, pos, new ErrChar(c))) :
              accept(pos + 1, cons(input[pos], asts), err) :
              // c.length === 2:
              pos + 1 >= input.length || c[0] !== input[pos] ||
                      c[1] !== input[pos + 1] ?
              reject(updateError(err, pos, new ErrChar(c))) :
              accept(pos + 2, cons(input[pos] + input[pos + 1], asts), err));
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
                  accept(pos + 1, cons(input[pos], asts), err) :
                  accept(
                      pos + 2, cons(input[pos] + input[pos + 1], asts), err));
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
      if (exprs.isEmpty()) {
        return applyNext(continuations, accept(pos, asts, err));
      }
      return evalNext(
          exprs.head, pos, asts, err, cons(contSeq(exprs.tail), continuations));
    }
    case ExprType.PLUS:
      return evalNext(
          exp.expr, pos, asts, err, cons(contPlus(exp.expr), continuations));
    case ExprType.STAR:
      return evalNext(
          exp.expr, pos, asts, err,
          cons(contStar(exp.expr, pos, asts), continuations));
    case ExprType.OPT:
      return evalNext(
          exp.expr, pos, asts, err, cons(contOpt(pos, asts), continuations));
    case ExprType.NT:
      const {name} = exp;
      const nt = env[name];
      return evalNext(
          nt.exp, pos, /*asts=*/empty(),
          new RawError(err.pos, err.nonterminals, err.failedChars, name),
          cons(contNT(nt.mode, name, asts, err.currentNT), continuations));
    default:
      throw new Error(`Unsupported exp.type in exp=${
          (exp as any).type} action=${JSON.stringify(action)}`);
  }
}

// Handles the result of a processed continuation.
function moveApply(
    input: string, value: MatchResult, evaluated: Continuation,
    rest: ConsList<Continuation>): ActionEval|ActionApply {
  switch (value.type) {
    case MatchResultType.ACCEPT:
      return moveApplyOnAccept(input, value, evaluated, rest);
    case MatchResultType.REJECT:
      return moveApplyOnReject(input, value, evaluated, rest);
  }
}

// Called after the `evaluated` continuation got accepted (matched).
function moveApplyOnAccept(
    input: string, accepted: Accepted, evaluated: Continuation,
    rest: ConsList<Continuation>): ActionEval|ActionApply {
  switch (evaluated.type) {
    case ContType.SEQ: {
      const {expressions} = evaluated;
      if (expressions.isEmpty()) {
        return applyNext(rest, accepted);
      }
      return evalNext(
          expressions.head, accepted.pos, accepted.asts, accepted.err,
          cons(contSeq(expressions.tail), rest));
    }
    case ContType.STAR:
    case ContType.PLUS:
      return evalNext(
          evaluated.expression, accepted.pos, accepted.asts, accepted.err,
          cons(
              contStar(evaluated.expression, accepted.pos, accepted.asts),
              rest));
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
                  accepted.pos,
                  cons(new AST(name, valAsts.toArray().reverse()), asts),
                  newErr));
        case NonTerminalMode.PRUNING:
          if (valAsts.isEmpty()) {
            return applyNext(rest, accept(accepted.pos, asts, newErr));
          } else if (valAsts.tail.isEmpty()) {
            return applyNext(
                rest, accept(accepted.pos, cons(valAsts.head, asts), newErr));
          } else {
            return applyNext(
                rest,
                accept(
                    accepted.pos,
                    cons(new AST(name, valAsts.toArray().reverse()), asts),
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
    input: string, rejected: Rejected, evaluated: Continuation,
    continuations: ConsList<Continuation>): ActionEval|ActionApply {
  switch (evaluated.type) {
    case ContType.ALT: {
      const {expressions} = evaluated;
      if (expressions.isEmpty()) {
        return applyNext(continuations, rejected);
      }
      return evalNext(
          expressions.head, evaluated.pos, evaluated.asts, rejected.err,
          cons(
              contAlt(expressions.tail, evaluated.pos, evaluated.asts),
              continuations));
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
    env: RuntimeParserConfig, start: string /* keyof env */, input: string,
    value: MatchResult): AST|ParseError {
  switch (value.type) {
    case MatchResultType.ACCEPT:
      const asts = value.asts;
      if (value.pos >= input.length) {
        switch (env[start].mode) {
          case NonTerminalMode.NORMAL:
            return new AST(start, asts.toArray().reverse());
          case NonTerminalMode.PRUNING:
            if (asts.isEmpty()) {
              return EmptyAST();
            } else if (asts.tail.isEmpty()) {
              const ast = asts.head;
              if (typeof ast === 'string') {
                throw new Error(`Expected an AST, got a string ${
                    JSON.stringify(ast)}, in ${value}`);
              }
              return ast;
            } else {
              return new AST(start, asts.toArray().reverse());
            }
          case NonTerminalMode.VOIDING:
            return EmptyAST();
        }
      } else if (value.err && value.pos === value.err.pos) {
        return new RawError(
                   value.pos, value.err.nonterminals, value.err.failedChars, '')
            .toParseError(input);
      } else {
        return new RawError(value.pos, empty(), empty(), '')
            .toParseError(input);
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
