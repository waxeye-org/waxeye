/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.expression;

public class Expression
{
  protected Expression () {
  }

  public String getType () {
    throw new RuntimeException("Not implemented");
  }
}
