/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.ast.IAST;

/**
 * A transition cost of matching an automaton.
 *
 * @param <E> The AST type.
 *
 * @author Orlando Hill
 */
public final class AutomatonTransition <E extends Enum<?>> implements ITransition<E>
{
    /** The index of the automaton. */
    private final int index;

    /**
     * Create a new AutomatonTransition.
     *
     * @param index The index of the automaton.
     */
    public AutomatonTransition(final int index)
    {
        this.index = index;
    }

    /**
     * Returns the index.
     *
     * @return Returns the index.
     */
    public int getIndex()
    {
        return index;
    }

    /** {@inheritDoc} */
    public IAST<E> acceptVisitor(final ITransitionVisitor<E> visitor)
    {
        return visitor.visitAutomatonTransition(this);
    }
}
