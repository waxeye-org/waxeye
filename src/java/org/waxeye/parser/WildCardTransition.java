/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.ast.IAST;

/**
 * A transition cost of matching any character.
 *
 * @param <E> The AST type.
 *
 * @author Orlando Hill
 */
public final class WildCardTransition <E extends Enum<?>> implements ITransition<E>
{
    /** {@inheritDoc} */
    public IAST<E> acceptVisitor(final ITransitionVisitor<E> visitor)
    {
        return visitor.visitWildCardTransition(this);
    }
}
