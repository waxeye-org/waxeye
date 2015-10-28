/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.err;

import java.util.List;

public class ErrCC extends ErrBase
{
  List<List<String>> cc;

  public ErrCC (List<List<String>> cc) {
    this.cc = cc;
  }

  public String toString () {
    return cc.toString();
  }
}
