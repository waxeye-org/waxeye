/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.expression;

public class And extends Expression
{
  private Expression expression;

  public And (Expression expression) {
    this.expression = expression;
  }

  public String getType () {
    return "AND";
  }

  public Expression getExpression () {
    return expression;
  }
}
