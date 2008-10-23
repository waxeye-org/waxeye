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
