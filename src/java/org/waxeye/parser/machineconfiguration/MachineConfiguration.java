/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.parser.expression.Expression;
import org.waxeye.parser.machinevalue.MachineValue;

import java.util.List;
import java.util.Stack;

enum MachineConfigurationType { EVAL, APPLY };

public class MachineConfiguration <E extends Enum<?>>
{
  /**
   * Factory method. Construct a new EVAL configuration.
   */
  public static <F extends Enum<?>> MachineConfiguration EVAL (Expression expr, int pos, List asts, RawError err, Stack continuations) {
    return new MachineConfigurationEval<F>(expr, pos, asts, err, continuations);
  }

  /**
   * Factory method. Construct a new APPLY configuration.
   */
  public static <F extends Enum<?>> MachineConfiguration APPLY (Stack continuations, MachineValue value) {
    return new MachineConfigurationApply<F>(continuations, value);
  }

  public boolean isApply () {
    return false;
  }

  public boolean isEval () {
    return false;
  }
}
