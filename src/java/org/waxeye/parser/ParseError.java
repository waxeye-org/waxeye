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

/**
 * An error that occurred during parsing.
 *
 * @author Orlando Hill
 */
public final class ParseError
{
    /** The position of the error. */
    private final int position;

    /** The line of the error. */
    private final int line;

    /** The column of the error. */
    private final int column;

    /** The non-terminal being matched when the error occured. */
    private final String nt;

    /**
     * Creates a new ParseError.
     *
     * @param position The position of the error.
     *
     * @param line The line of the error.
     *
     * @param column The column of the error.
     *
     * @param nt The non-terminal being matched when the error occured.
     */
    public ParseError(final int position, final int line, final int column,
                      final String nt)
    {
        this.position = position;
        this.line = line;
        this.column = column;
        this.nt = nt;
    }

    /** {@inheritDoc} */
    public String toString()
    {
        final StringBuilder buf = new StringBuilder();

        buf.append("parse error: failed to match '");
        buf.append(nt);
        buf.append("' at line=");
        buf.append(line);
        buf.append(", col=");
        buf.append(column);
        buf.append(", pos=");
        buf.append(position);

        return buf.toString();
    }

    /**
     * Returns the position.
     *
     * @return Returns the position.
     */
    public int getPosition()
    {
        return position;
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
     * Returns the nt.
     *
     * @return Returns the nt.
     */
    public String getNT()
    {
        return nt;
    }
}
