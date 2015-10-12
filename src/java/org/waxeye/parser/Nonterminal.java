/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.parser.expression.*;

public class Nonterminal
{
  NonterminalMode mode;
  Expression expression;

  public Nonterminal (NonterminalMode mode, Expression expression) {
    this.mode = mode;
    this.expression = expression;
  }

  public NonterminalMode getMode () {
    return mode;
  }

  public Expression getExpression () {
    return expression;
  }
}
