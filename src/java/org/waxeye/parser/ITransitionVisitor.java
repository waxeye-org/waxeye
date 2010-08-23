/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.ast.IAST;

/**
 * Visits transitions.
 *
 * @param <E> The AST type.
 *
 * @author Orlando Hill
 */
public interface ITransitionVisitor <E extends Enum<?>>
{
    /**
     * Visits the automaton transition.
     *
     * @param t The transition.
     *
     * @return The result of the visit.
     */
    IAST<E> visitAutomatonTransition(AutomatonTransition<E> t);

    /**
     * Visits the char transition.
     *
     * @param t The transition.
     *
     * @return The result of the visit.
     */
    IAST<E> visitCharTransition(CharTransition<E> t);

    /**
     * Visits the wildcard transition.
     *
     * @param t The transition.
     *
     * @return The result of the visit.
     */
    IAST<E> visitWildCardTransition(WildCardTransition<E> t);
}
