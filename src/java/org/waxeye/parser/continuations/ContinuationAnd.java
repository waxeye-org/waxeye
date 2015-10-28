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
import org.waxeye.parser.RawError;
import java.util.List;

public class ContinuationAnd <E extends Enum<?>> extends Continuation
implements ContinuationWithPos, ContinuationWithAsts
{
  private int pos;
  private List<IAST<E>> asts;
  private RawError err;

  public ContinuationAnd (int pos, List<IAST<E>> asts, RawError err) {
    this.pos = pos;
    this.asts = asts;
    this.err = err;
  }

  public int getPos () {
    return pos;
  }

  public List<IAST<E>> getAsts () {
    return asts;
  }

  public RawError getError () {
    return err;
  }
}
