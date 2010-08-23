/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.ast;

import java.util.List;

/**
 * An interface for an AST node.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public interface IAST <E extends Enum<?>>
{
    /**
     * Returns the children.
     *
     * @return Returns the children.
     */
    List<IAST<E>> getChildren();

    /**
     * Returns the position.
     *
     * Can be <code>null</code> if the AST doesn't have a position set.
     *
     * @return Returns the position;
     */
    Position getPosition();

    /**
     * Returns the type.
     *
     * @return Returns the type.
     */
    E getType();

    /**
     * Accepts the visitor.
     *
     * @param visitor The visitor to accept.
     */
    void acceptASTVisitor(IASTVisitor visitor);

    /**
     * Treats the children of the AST as IChar's and concatenates their values
     * into a String.
     *
     * @return The AST's IChar children as a String.
     */
    String childrenAsString();
}
