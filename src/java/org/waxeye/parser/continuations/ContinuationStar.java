/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.continuations;

import org.waxeye.parser.expression.Expression;

import org.waxeye.ast.IAST;
import java.util.List;

public class ContinuationStar <E extends Enum<?>> extends Continuation
implements ContinuationWithPos, ContinuationWithAsts, ContinuationWithExpression
{
  private Expression expr;
  private int pos;
  private List<IAST<E>> asts;

  public ContinuationStar (Expression expr, int pos, List<IAST<E>> asts) {
    this.expr = expr;
    this.pos = pos;
    this.asts = asts;
  }

  public Expression getExpression () {
    return expr;
  }

  public int getPos() {
    return pos;
  }

  public List<IAST<E>> getAsts () {
    return asts;
  }
}
