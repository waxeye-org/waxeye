/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

/**
 * An edge in an automaton state.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public final class Edge <E extends Enum<?>>
{
    /** The transition cost. */
    private final ITransition<E> trans;

    /** The state to transition to. */
    private final int state;

    /** Whether the edge is voided. */
    private final boolean voided;

    /**
     * Creates a new Edge.
     *
     * @param trans The transition cost.
     *
     * @param state The state to transition to.
     *
     * @param voided Whether the edge is voided.
     */
    public Edge(final ITransition<E> trans, final int state, final boolean voided)
    {
        this.trans = trans;
        this.state = state;
        this.voided = voided;
    }

    /**
     * Returns the trans.
     *
     * @return Returns the trans.
     */
    public ITransition<E> getTrans()
    {
        return trans;
    }

    /**
     * Returns the state.
     *
     * @return Returns the state.
     */
    public int getState()
    {
        return state;
    }

    /**
     * Returns the voided.
     *
     * @return Returns the voided.
     */
    public boolean isVoided()
    {
        return voided;
    }
}
