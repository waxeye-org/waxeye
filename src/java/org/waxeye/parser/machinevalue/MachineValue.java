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

public class MachineValue
{
  protected MachineValue () {
  }

  public static MachineValueFail FAIL (RawError e) {
    return new MachineValueFail(e);
  }

  public static <E extends Enum<?>> MachineValueVal VAL (int pos, List<IAST<E>> asts, RawError e) {
    return new MachineValueVal<E>(pos, asts, e);
  }
}
