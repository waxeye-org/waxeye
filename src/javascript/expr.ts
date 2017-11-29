// We have two representations for expression:
// the config one, and the internal one.

// The config representation is JSON-compatible.
// The internal one is optimized for performance.

import {cons, ConsList, empty} from './cons_list';

export function exprToRuntimeExpr(expr: Expr): RuntimeExpr {
  switch (expr.type) {
    case ExprType.NT:
    case ExprType.CHAR:
    case ExprType.CHAR_CLASS:
    case ExprType.ANY_CHAR:
      return expr;
    case ExprType.PLUS:
    case ExprType.STAR:
    case ExprType.OPT:
    case ExprType.AND:
    case ExprType.NOT:
    case ExprType.VOID:
      return {type: expr.type, expr: exprToRuntimeExpr(expr.expr)} as
          RuntimeExpr;
    case ExprType.ALT:
    case ExprType.SEQ:
      return {
        type: expr.type,
        exprs: expr.exprs.reduceRight(
            (result: ConsList<RuntimeExpr>, value: Expr):
                ConsList<RuntimeExpr> => cons(exprToRuntimeExpr(value), result),
            empty()),
      } as RuntimeExpr;
  }
}

export type Expr = NonRecursiveExpr|ExprPlus|ExprStar|ExprOpt|ExprAnd|ExprNot|
    ExprVoid|ExprAlt|ExprSeq;
export type RuntimeExpr =
    NonRecursiveExpr|RuntimeExprPlus|RuntimeExprStar|RuntimeExprOpt|
    RuntimeExprAnd|RuntimeExprNot|RuntimeExprVoid|RuntimeExprAlt|RuntimeExprSeq;
export type NonRecursiveExpr =
    ExprNonTerminal|ExprChar|ExprCharClass|ExprAnyChar;

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
  ANY_CHAR,
  CHAR,
  CHAR_CLASS,
}

export interface ExprNonTerminal {
  type: ExprType.NT;
  name: string;
}

export interface ExprAnyChar { type: ExprType.ANY_CHAR; }

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

export interface ExprAlt {
  type: ExprType.ALT;
  exprs: Expr[];
}
export interface RuntimeExprAlt {
  type: ExprType.ALT;
  exprs: ConsList<RuntimeExpr>;
}

export interface ExprSeq {
  type: ExprType.SEQ;
  exprs: Expr[];
}
export interface RuntimeExprSeq {
  type: ExprType.SEQ;
  exprs: ConsList<RuntimeExpr>;
}

export interface ExprPlus {
  type: ExprType.PLUS;
  expr: Expr;
}
export interface RuntimeExprPlus {
  type: ExprType.PLUS;
  expr: RuntimeExpr;
}

export interface ExprStar {
  type: ExprType.STAR;
  expr: Expr;
}
export interface RuntimeExprStar {
  type: ExprType.STAR;
  expr: RuntimeExpr;
}

export interface ExprOpt {
  type: ExprType.OPT;
  expr: Expr;
}
export interface RuntimeExprOpt {
  type: ExprType.OPT;
  expr: RuntimeExpr;
}

export interface ExprAnd {
  type: ExprType.AND;
  expr: Expr;
}
export interface RuntimeExprAnd {
  type: ExprType.AND;
  expr: RuntimeExpr;
}

export interface ExprNot {
  type: ExprType.NOT;
  expr: Expr;
}
export interface RuntimeExprNot {
  type: ExprType.NOT;
  expr: RuntimeExpr;
}

export interface ExprVoid {
  type: ExprType.VOID;
  expr: Expr;
}
export interface RuntimeExprVoid {
  type: ExprType.VOID;
  expr: RuntimeExpr;
}
