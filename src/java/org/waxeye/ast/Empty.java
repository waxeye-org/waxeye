/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.ast;

/**
 * A class to represent an empty AST.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public final class Empty <E extends Enum<?>> extends NoChildren<E>
implements IEmpty
{
    /**
     * Creates a new Empty AST.
     *
     * @param type The type of the AST.
     */
    public Empty(final E type)
    {
        super(type);
    }

    /** {@inheritDoc} */
    public void acceptASTVisitor(final IASTVisitor visitor)
    {
        visitor.visitEmpty(this);
    }

    /** {@inheritDoc} */
    public String toString()
    {
        return "";
    }
}
