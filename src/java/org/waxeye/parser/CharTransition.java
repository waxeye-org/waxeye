/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.ast.IAST;

/**
 * A transition cost of matching a char from a set.
 *
 * @param <E> The AST type.
 *
 * @author Orlando Hill
 */
public final class CharTransition <E extends Enum<?>> implements ITransition<E>
{
    /** Individual chars in the set. */
    private final char[] single;

    /** The min values for the ranges in the set. */
    private final char[] min;

    /** The max values for the ranges in the set. */
    private final char[] max;

    /**
     * Creates a new CharTransition.
     *
     * @param single Individual chars in the set.
     *
     * @param min The min values for the ranges in the set.
     *
     * @param max The max values for the ranges in the set.
     */
    public CharTransition(final char[] single, final char[] min, final char[] max)
    {
        this.single = single;
        this.min = min;
        this.max = max;
    }

    /**
     * Whether the char is within the char transition set.
     *
     * @param ch The char to test.
     *
     * @return Whether the char is within the char transition set.
     */
    public boolean withinSet(final char ch)
    {
        for (int i = 0; i < min.length; i++)
        {
            if (ch >= min[i] && ch <= max[i])
            {
                return true;
            }
        }

        for (int i = 0; i < single.length; i++)
        {
            if (ch == single[i])
            {
                return true;
            }
        }

        return false;
    }

    /** {@inheritDoc} */
    public IAST<E> acceptVisitor(final ITransitionVisitor<E> visitor)
    {
        return visitor.visitCharTransition(this);
    }
}
