/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.machinevalue;

import org.waxeye.parser.RawError;
import org.waxeye.ast.IAST;
import java.util.List;

public class MachineValueVal <E extends Enum<?>> extends MachineValue
{
  private int pos;
  private List<IAST<E>> asts;
  private RawError e;

  public MachineValueVal (int pos, List<IAST<E>> asts, RawError e) {
    this.pos = pos;
    this.asts = asts;
    this.e = e;
  }

  public RawError getError () {
    return (RawError)e.clone();
  }

  public List<IAST<E>> getAsts () {
    return asts;
  }

  public int getPos () {
    return pos;
  }
}
