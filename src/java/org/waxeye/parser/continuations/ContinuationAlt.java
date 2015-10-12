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

public class ContinuationAlt <E extends Enum<?>> extends Continuation
implements ContinuationWithPos, ContinuationWithAsts
{
  private List<Expression> exprs;
  private int pos;
  private List<IAST<E>> asts;

  public ContinuationAlt (List<Expression> exprs, int pos, List<IAST<E>> asts) {
    this.exprs = exprs;
    this.pos = pos;
    this.asts = asts;
  }

  public List<Expression> getExpressions () {
    return exprs;
  }

  public int getPos() {
    return pos;
  }

  public List<IAST<E>> getAsts () {
    return asts;
  }
}
