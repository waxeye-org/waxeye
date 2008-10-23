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
