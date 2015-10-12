/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.parser.err.ErrBase;

import java.util.List;

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

    /** The non-terminals being matched when the error occured. */
    private final List<String> nt;

    /** The characters that failed to get matched when the error occurred. */
    private final List<ErrBase> failedMatches;

    /**
     * Creates a new ParseError.
     *
     * @param position The position of the error.
     *
     * @param line The line of the error.
     *
     * @param column The column of the error.
     *
     * @param nt The non-terminals being matched when the error occured.

     * @param failedChars The characters that failed to match.
     */
    public ParseError(final int position, final int line, final int column,
                      final List<String> nt, final List<ErrBase> failedMatches)
    {
        this.position = position;
        this.line = line;
        this.column = column;
        this.nt = nt;
        this.failedMatches = failedMatches;
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
        buf.append(" (expected \"");
        buf.append(failedMatches);
        buf.append("\")");

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
     * Returns the nts.
     *
     * @return Returns the nts.
     */
    public List<String> getNTs()
    {
        return nt;
    }
}
