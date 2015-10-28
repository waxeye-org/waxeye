/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.expression;

public class CharExpr extends Expression
{
  private char ch;

  public CharExpr (char ch) {
    this.ch = ch;
  }

  public String getType () {
    return "CHAR";
  }

  public char getChar () {
    return ch;
  }
}
