/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
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
