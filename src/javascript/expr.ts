// We have two representations for expression:
// the config one, and the internal one.

// The config representation is JSON-compatible.
// The internal one is optimized for performance.

import {cons, ConsList, empty} from './cons_list';

export function configExprToExpr(expr: ConfigExpr): Expr {
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
      return {type: expr.type, expr: configExprToExpr(expr.expr)} as Expr;
    case ExprType.ALT:
    case ExprType.SEQ:
      return {
        type: expr.type,
        exprs: expr.exprs.reduceRight(
            (result: ConsList<Expr>, value: ConfigExpr): ConsList<Expr> =>
                cons(configExprToExpr(value), result),
            empty()),
      } as Expr;
  }
}

export type ConfigExpr =
    NonRecursiveExpr|ConfigExprPlus|ConfigExprStar|ConfigExprOpt|ConfigExprAnd|
    ConfigExprNot|ConfigExprVoid|ConfigExprAlt|ConfigExprSeq;
export type Expr = NonRecursiveExpr|ExprPlus|ExprStar|ExprOpt|ExprAnd|ExprNot|
    ExprVoid|ExprAlt|ExprSeq;
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

export interface ConfigExprAlt {
  type: ExprType.ALT;
  exprs: ConfigExpr[];
}
export interface ExprAlt {
  type: ExprType.ALT;
  exprs: ConsList<Expr>;
}

export interface ConfigExprSeq {
  type: ExprType.SEQ;
  exprs: ConfigExpr[];
}
export interface ExprSeq {
  type: ExprType.SEQ;
  exprs: ConsList<Expr>;
}

export interface ConfigExprPlus {
  type: ExprType.PLUS;
  expr: ConfigExpr;
}
export interface ExprPlus {
  type: ExprType.PLUS;
  expr: Expr;
}

export interface ConfigExprStar {
  type: ExprType.STAR;
  expr: ConfigExpr;
}
export interface ExprStar {
  type: ExprType.STAR;
  expr: Expr;
}

export interface ConfigExprOpt {
  type: ExprType.OPT;
  expr: ConfigExpr;
}
export interface ExprOpt {
  type: ExprType.OPT;
  expr: Expr;
}

export interface ConfigExprAnd {
  type: ExprType.AND;
  expr: ConfigExpr;
}
export interface ExprAnd {
  type: ExprType.AND;
  expr: Expr;
}

export interface ConfigExprNot {
  type: ExprType.NOT;
  expr: ConfigExpr;
}
export interface ExprNot {
  type: ExprType.NOT;
  expr: Expr;
}

export interface ConfigExprVoid {
  type: ExprType.VOID;
  expr: ConfigExpr;
}
export interface ExprVoid {
  type: ExprType.VOID;
  expr: Expr;
}
