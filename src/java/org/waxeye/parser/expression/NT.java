/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.expression;

public class NT<E extends Enum<?>> extends Expression
{
  private E nt;

  public NT (E nt) {
    this.nt = nt;
  }

  public String getType () {
    return "NT";
  }

  public E getNT () {
    return nt;
  }
}
