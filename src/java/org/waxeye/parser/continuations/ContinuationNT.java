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

import org.waxeye.ast.IAST;
import java.util.List;

public class ContinuationNT <E extends Enum<?>> extends Continuation
implements ContinuationWithAsts
{
  private NonterminalMode mode;
  private E nt;
  private List<IAST<E>> asts;
  private E currentNT;

  public ContinuationNT (NonterminalMode mode, E nt, List<IAST<E>> asts, E currentNT) {
    this.mode = mode;
    this.nt = nt;
    this.asts = asts;
    this.currentNT = currentNT;
  }

  public NonterminalMode getMode() {
    return mode;
  }

  public List<IAST<E>> getAsts () {
    return asts;
  }

  public E getNT() {
    return nt;
  }

  public E getCurrentNT() {
    return currentNT;
  }
}
