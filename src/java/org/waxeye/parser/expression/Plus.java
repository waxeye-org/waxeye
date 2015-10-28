/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.expression;

public class Plus extends Expression
{
  private Expression expression;

  public Plus (Expression expression) {
    this.expression = expression;
  }

  public String getType () {
    return "PLUS";
  }

  public Expression getExpression () {
    return expression;
  }
}
