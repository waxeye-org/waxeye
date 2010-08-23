/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
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
    public int getPosition()
    {
        return position;
    }

    /**
     * Returns the result.
     *
     * @return Returns the result.
     */
    public IAST<E> getResult()
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
