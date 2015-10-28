/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.continuations;

import java.util.List;

public class ContinuationVoid extends Continuation
implements ContinuationWithAsts
{
  private List asts;

  public ContinuationVoid (List asts) {
    this.asts = asts;
  }

  public List getAsts () {
    return asts;
  }
}
