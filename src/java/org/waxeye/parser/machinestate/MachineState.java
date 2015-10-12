/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.ast.AST;

public class MachineState <E extends Enum<?>>
{
  public static <F extends Enum<?>> MachineState FINAL (ParseResult<F> result) {
    return new MachineStateFinal<F>(result);
  }

  public static <F extends Enum<?>> MachineState INTER (MachineConfiguration<F> configuration) {
    return new MachineStateInter<F>(configuration);
  }

  public boolean isFinal () {
    return false;
  }
}
