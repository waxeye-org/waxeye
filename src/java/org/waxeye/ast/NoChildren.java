/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.ast;

import java.util.ArrayList;
import java.util.List;

/**
 * A class to represent an AST with no children.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public abstract class NoChildren <E extends Enum<?>> implements IAST<E>
{
    /** The type of AST node. */
    private final E type;

    /**
     * Creates a new NoChildren AST.
     *
     * @param type The type of the AST.
     */
    public NoChildren(final E type)
    {
        this.type = type;

        assert invariants();
    }

    /** {@inheritDoc} */
    public final int hashCode()
    {
        final int start = 17;
        return Math.abs(start * type.hashCode());
    }

    /**
     * Checks the invariants of the object.
     *
     * @return <code>true</code>.
     */
    private boolean invariants()
    {
        assert type != null;

        return true;
    }

    /**
     * Returns a new empty list since this node doesn't allow children.
     *
     * @return Returns a new empty list.
     */
    public final List<IAST<E>> getChildren()
    {
        return new ArrayList<IAST<E>>();
    }

    /** {@inheritDoc} */
    public final Position getPosition()
    {
        return null;
    }

    /** {@inheritDoc} */
    public final E getType()
    {
        return type;
    }

    /** {@inheritDoc} */
    public final String childrenAsString()
    {
        return "";
    }
}
