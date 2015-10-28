/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.machinevalue;

import org.waxeye.parser.RawError;
import java.util.List;

public class MachineValueFail extends MachineValue
{
  private RawError e;

  public MachineValueFail (RawError e) {
    this.e = e;
  }

  public RawError getError () {
    return e;
  }
}
