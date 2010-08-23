/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye;

import static org.testng.Assert.*;

/**
 * A class to test the equals contract.
 *
 * @author Orlando Hill
 */
public final class EqualsTester
{
    /**
     * Creates a new equality tester.
     *
     * @param original An instance of the class to test.
     *
     * @param duplicate Another instance of the class to test with the same
     * values.
     *
     * <code>null</code> if the class is a singleton.
     *
     * @param different Another instance of the class to test with different
     * values.
     *
     * <code>null</code> if the values can't differ.
     *
     * @param subClass A subclass of the class to test.
     *
     * <code>null</code> if the class is final.
     */
    public EqualsTester(final Object original, final Object duplicate,
        final Object different, final Object subClass)
    {
        assertFalse(original.equals(null), "Instance 'equal' to null.");
        assertEquals(original, original, "Instance not 'equal' to itself.");

        if (duplicate != null)
        {
            assertNotSame(duplicate, original,
                "Duplicate same instance as original object.");
            assertEquals(duplicate, original,
                "Duplicate not 'equal' to original.");
            assertEquals(original, duplicate,
                "Original not 'equal' to duplicate.");

            assertEquals(duplicate.hashCode(), original.hashCode(),
                "Hashcode not same for 'equal' objects.");
        }

        if (different != null)
        {
            assertNotSame(different, original,
                "Different same instance as original object.");
            assertFalse(different.equals(original),
                "Different 'equal' to original.");
            assertFalse(original.equals(different),
                "Original 'equal' to different.");
        }

        if (subClass != null)
        {
            assertNotSame(subClass, original,
                "SubClass same instance as original object.");
            assertFalse(subClass.equals(original),
                "SubClass 'equal' to original.");
            assertFalse(original.equals(subClass),
                "Original 'equal' to subClass.");
        }
    }
}
