/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008 Orlando D. A. R. Hill
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
