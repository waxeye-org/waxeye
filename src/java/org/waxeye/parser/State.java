/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import java.util.List;

/**
 * An automaton state.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public final class State <E extends Enum<?>>
{
    /** The edges of the state. */
    private final List<Edge<E>> edges;

    /** Whether the state is a matching state. */
    private final boolean match;

    /**
     * Creates a new State.
     *
     * @param edges The edges of the state.
     *
     * @param match Whether the state is a matching state.
     */
    public State(final List<Edge<E>> edges, final boolean match)
    {
        this.edges = edges;
        this.match = match;
    }

    /**
     * Returns the edges.
     *
     * @return Returns the edges.
     */
    public List<Edge<E>> getEdges()
    {
        return edges;
    }

    /**
     * Returns the match.
     *
     * @return Returns the match.
     */
    public boolean isMatch()
    {
        return match;
    }
}
