/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.testng.annotations.Test;

import org.waxeye.EqualsTester;

/**
 * Tests for CacheKey.
 *
 * @author Orlando Hill
 */
public final class CacheKeyTest
{
    /** Test for equals(). */
    @Test
    public void testEquals()
    {
        // original object
        final Object a = new CacheKey(20, 20);

        // another object that has the same values as the original
        final Object b = new CacheKey(20, 20);

        // another object with different values
        final Object c = new CacheKey(6, 6);

        new EqualsTester(a, b, c, null);
    }
}
