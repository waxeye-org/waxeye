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
 * A finite state automaton.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public final class FA <E extends Enum<?>>
{
    /** The type of the automaton. */
    private final E type;

    /** The automaton mode. */
    private final int mode;

    /** The states of the automaton. */
    private final List<State<E>> states;

    /** The normal automaton mode. */
    public static final int LEFT = 0;

    /** The void mode. */
    public static final int VOID = 1;

    /** The vertical prune mode. */
    public static final int PRUNE = 2;

    /**
     * Creates a new finite state automaton.
     *
     * @param type The type of the automaton.
     *
     * @param mode The automaton mode.
     *
     * @param states The states of the automaton.
     */
    public FA(final E type, final int mode, final List<State<E>> states)
    {
        this.type = type;
        this.mode = mode;
        this.states = states;
    }

    /**
     * Returns the type.
     *
     * @return Returns the type.
     */
    public E getType()
    {
        return type;
    }

    /**
     * Returns the mode.
     *
     * @return Returns the mode.
     */
    public int getMode()
    {
        return mode;
    }

    /**
     * Returns the states.
     *
     * @return Returns the states.
     */
    public List<State<E>> getStates()
    {
        return states;
    }
}
