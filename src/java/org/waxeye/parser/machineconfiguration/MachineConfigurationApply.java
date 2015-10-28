/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import java.util.Stack;
import org.waxeye.parser.continuations.Continuation;
import org.waxeye.parser.machinevalue.MachineValue;

public class MachineConfigurationApply <E extends Enum<?>> extends MachineConfiguration<E>
{
  private Stack<Continuation> continuations;
  private MachineValue value;

  public MachineConfigurationApply (Stack<Continuation> continuations, MachineValue value) {
    this.continuations = continuations;
    this.value = value;
  }

  public boolean isApply () {
    return true;
  }

  public Stack<Continuation> getContinuations () {
    return continuations;
  }

  public MachineValue getValue () {
    return value;
  }
}
