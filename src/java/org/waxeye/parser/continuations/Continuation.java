/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.continuations;

import org.waxeye.parser.expression.Expression;
import org.waxeye.parser.NonterminalMode;

import org.waxeye.parser.RawError;
import org.waxeye.ast.IAST;
import java.util.List;

public class Continuation
{
  protected Continuation () {
  }

  public static Continuation VOID (List asts) {
    return new ContinuationVoid(asts);
  }

  public static Continuation SEQ (List<Expression> exprs) {
    return new ContinuationSeq(exprs);
  }

  public static <E extends Enum<?>> Continuation PLUS (Expression expr) {
    return new ContinuationPlus(expr);
  }

  public static <E extends Enum<?>> Continuation STAR (Expression expr, int pos, List<IAST<E>> asts) {
    return new ContinuationStar(expr, pos, asts);
  }

  public static <E extends Enum<?>> Continuation AND (int pos, List<IAST<E>> asts, RawError err) {
    return new ContinuationAnd(pos, asts, err);
  }

  public static <E extends Enum<?>> Continuation NOT (int pos, List<IAST<E>> asts, RawError err) {
    return new ContinuationNot(pos, asts, err);
  }

  public static <E extends Enum<?>> Continuation ALT (List<Expression> exprs, int pos, List<IAST<E>> asts) {
    return new ContinuationAlt(exprs, pos, asts);
  }

  public static <E extends Enum<?>> Continuation OPT (int pos, List<IAST<E>> asts) {
    return new ContinuationOpt(pos, asts);
  }

  public static <E extends Enum<?>> Continuation NT (NonterminalMode mode, E nt, List<IAST<E>> asts, E currentNT) {
    return new ContinuationNT(mode, nt, asts, currentNT);
  }
}
