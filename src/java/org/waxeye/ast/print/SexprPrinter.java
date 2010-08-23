/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.ast.print;

import org.waxeye.ast.IAST;
import org.waxeye.ast.IChar;
import org.waxeye.ast.IEmpty;
import org.waxeye.ast.IASTVisitor;

/**
 * A class to print the AST as s-expressions.
 *
 * @author Orlando Hill
 */
public final class SexprPrinter implements IASTVisitor
{
    /** The buffer to build the string. */
    private StringBuilder buf;

    /**
     * Creates a new SexprPrinter.
     *
     * @param tree The ast to print.
     */
    public SexprPrinter(final IAST<?> tree)
    {
        this.buf = new StringBuilder();
        tree.acceptASTVisitor(this);
    }

    /** {@inheritDoc} */
    public void visitAST(final IAST<?> tree)
    {
        buf.append('(');
        buf.append(tree.getType());

        for (IAST<?> child : tree.getChildren())
        {
            buf.append(' ');
            child.acceptASTVisitor(this);
        }

        buf.append(')');
    }

    /** {@inheritDoc} */
    public void visitEmpty(final IEmpty tree)
    {
    }

    /** {@inheritDoc} */
    public void visitChar(final IChar tree)
    {
        buf.append(tree.getValue());
    }

    /** {@inheritDoc} */
    public String toString()
    {
        return buf.toString();
    }
}
