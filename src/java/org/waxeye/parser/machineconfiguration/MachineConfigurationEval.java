/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.parser.expression.Expression;

import java.util.List;
import java.util.Stack;

public class MachineConfigurationEval <E extends Enum<?>> extends MachineConfiguration<E>
{
  private Expression expr;
  private int pos;
  private List asts;
  private RawError err;
  private Stack continuations;

  public MachineConfigurationEval (Expression expr, int pos, List asts, RawError err, Stack continuations) {
    this.expr = expr;
    this.pos = pos;
    this.asts = asts;
    this.err = err;
    this.continuations = continuations;
  }

  public boolean isEval () {
    return true;
  }

  public Expression getExpression () {
    return expr;
  }

  public Stack getContinuations () {
    return continuations;
  }

  public int getPos () {
    return pos;
  }

  public List getAsts () {
    return asts;
  }

  public RawError getError () {
    return err;
  }
}
