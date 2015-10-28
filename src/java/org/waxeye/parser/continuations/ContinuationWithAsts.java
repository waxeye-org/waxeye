/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Copyright (C) 2015 Joshua Gross, Yahoo Inc
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser.continuations;

import org.waxeye.ast.IAST;
import java.util.List;

public interface ContinuationWithAsts
{
  public <E extends Enum<?>> List<IAST<E>> getAsts();
}
