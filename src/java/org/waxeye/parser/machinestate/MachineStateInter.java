/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.ast.AST;

public class MachineStateInter <E extends Enum<?>> extends MachineState<E>
{
  private MachineConfiguration<E> configuration;

  public MachineStateInter (MachineConfiguration<E> configuration) {
    this.configuration = configuration;
  }

  public MachineConfiguration<E> getConfiguration () {
    return configuration;
  }

  public boolean isFinal () {
    return false;
  }
}
