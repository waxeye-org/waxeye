/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
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
