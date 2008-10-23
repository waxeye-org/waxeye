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
 * A cached result.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
final class CacheItem <E extends Enum<?>>
{
    /** The resulting parse tree. */
    private final IAST<E> result;

    /** The position on the input string. */
    private final int position;

    /** The line number. */
    private final int line;

    /** The column number. */
    private final int column;

    /** Whether the last character was a carriage return. */
    private final boolean lastCR;

    /**
     * Creates a new CacheItem.
     *
     * @param result The resulting parse tree.
     *
     * @param position The position on the input string.
     *
     * @param line The line number.
     *
     * @param column The column number.
     *
     * @param lastCR Whether the last character was a carriage return.
     */
    public CacheItem(final IAST<E> result, final int position,
        final int line, final int column, final boolean lastCR)
    {
        this.result = result;
        this.position = position;
        this.line = line;
        this.column = column;
        this.lastCR = lastCR;

        assert invariants();
    }

    /**
     * Checks the invariants of the object.
     *
     * @return <code>true</code>.
     */
    private boolean invariants()
    {
        assert position >= 0;
        assert line >= 0;

        return true;
    }

    /**
     * Returns the position.
     *
     * @return Returns the position.
     */
    public final int getPosition()
    {
        return position;
    }

    /**
     * Returns the result.
     *
     * @return Returns the result.
     */
    public final IAST<E> getResult()
    {
        return result;
    }

    /**
     * Returns the column.
     *
     * @return Returns the column.
     */
    public int getColumn()
    {
        return column;
    }

    /**
     * Returns the lastCR.
     *
     * @return Returns the lastCR.
     */
    public boolean getLastCR()
    {
        return lastCR;
    }

    /**
     * Returns the line.
     *
     * @return Returns the line.
     */
    public int getLine()
    {
        return line;
    }
}
