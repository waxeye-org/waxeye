/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.ast;

/**
 * A visitor for IAST nodes.
 *
 * @author Orlando Hill
 */
public interface IASTVisitor
{
    /**
     * Visits the tree as a AST.
     *
     * @param tree The tree to visit.
     */
    void visitAST(IAST<?> tree);

    /**
     * Visits the tree as Empty.
     *
     * @param tree The tree to visit.
     */
    void visitEmpty(IEmpty tree);

    /**
     * Visits the tree as a Char.
     *
     * @param tree The tree to visit.
     */
    void visitChar(IChar tree);
}
