/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.expression;

import java.util.List;

public class Seq extends Expression
{
  private List<Expression> expressions;

  public Seq (List<Expression> expressions) {
    this.expressions = expressions;
  }

  public String getType () {
    return "SEQ";
  }

  public List<Expression> getExpressions () {
    return expressions;
  }
}
