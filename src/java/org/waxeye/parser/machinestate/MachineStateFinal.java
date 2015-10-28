/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

public class MachineStateFinal <E extends Enum<?>> extends MachineState<E>
{
  private ParseResult<E> result;

  public MachineStateFinal (ParseResult<E> result) {
    this.result = result;
  }

  public ParseResult<E> getResult () {
    return result;
  }

  public boolean isFinal () {
    return true;
  }
}
