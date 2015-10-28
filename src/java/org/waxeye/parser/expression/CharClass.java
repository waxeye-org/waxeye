/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.expression;

import java.util.List;

public class CharClass extends Expression
{
  private List<List<String>> charClass;

  public CharClass (List<List<String>> charClass) {
    this.charClass = charClass;
  }

  public String getType () {
    return "CHAR_CLASS";
  }

  public List<List<String>> getCharClass () {
    return charClass;
  }
}
