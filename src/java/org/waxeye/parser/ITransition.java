/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.ast.IAST;

/**
 * An edge transition cost.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public interface ITransition <E extends Enum<?>>
{
    /**
     * If this transition cost can be accepted by the parser.
     *
     * @param visitor The transition visitor.
     *
     * @return If this transition cost can be accepted by the parser.
     */
    IAST<E> acceptVisitor(ITransitionVisitor<E> visitor);
}
