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
package org.waxeye.ast;

/**
 * The position of an AST relative to the input.
 *
 * @author Orlando Hill
 */
public final class Position
{
    /** The start index of the AST position. */
    private final int startIndex;

    /** The end index of the AST position. */
    private final int endIndex;

    /**
     * Creates a new Position.
     *
     * @param startIndex The start index of the AST position.
     *
     * @param endIndex The end index of the AST position.
     */
    public Position(final int startIndex, final int endIndex)
    {
        this.startIndex = startIndex;
        this.endIndex = endIndex;

        assert invariants();
    }

    /**
     * Checks the invariants of the object.
     *
     * @return <code>true</code>.
     */
    private boolean invariants()
    {
        assert startIndex >= 0;
        assert endIndex >= startIndex;

        return true;
    }

    /**
     * Returns the startIndex.
     *
     * @return Returns the startIndex.
     */
    public int getStartIndex()
    {
        return startIndex;
    }

    /**
     * Returns the endIndex.
     *
     * @return Returns the endIndex.
     */
    public int getEndIndex()
    {
        return endIndex;
    }
}
