/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.continuations;

import org.waxeye.parser.expression.Expression;

import java.util.List;

public class ContinuationSeq extends Continuation
{
  private List<Expression> exprs;

  public ContinuationSeq (List<Expression> exprs) {
    this.exprs = exprs;
  }

  public List<Expression> getExpressions () {
    return exprs;
  }
}
