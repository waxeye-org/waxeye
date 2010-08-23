/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
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
