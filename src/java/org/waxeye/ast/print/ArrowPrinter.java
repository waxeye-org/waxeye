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
 * A class to print the AST with arrows.
 *
 * @author Orlando Hill
 */
public final class ArrowPrinter implements IASTVisitor
{
    /** The buffer to build the string. */
    private StringBuilder buf;

    /** The level of indentation. */
    private int indentLevel;

    /**
     * Creates a new ArrowPrinter.
     *
     * @param tree The ast to print.
     */
    public ArrowPrinter(final IAST<?> tree)
    {
        this.buf = new StringBuilder();
        this.indentLevel = 0;
        tree.acceptASTVisitor(this);
    }

    /** {@inheritDoc} */
    public void visitAST(final IAST<?> tree)
    {
        for (int i = 1; i < indentLevel; i++)
        {
            buf.append("    ");
        }

        if (indentLevel > 0)
        {
            buf.append("->  ");
        }

        buf.append(tree.getType());

        indentLevel++;

        for (IAST<?> child : tree.getChildren())
        {
            buf.append("\n");
            child.acceptASTVisitor(this);
        }

        indentLevel--;
    }

    /** {@inheritDoc} */
    public void visitEmpty(final IEmpty tree)
    {
    }

    /** {@inheritDoc} */
    public void visitChar(final IChar tree)
    {
        for (int i = 1; i < indentLevel; i++)
        {
            buf.append("    ");
        }

        if (indentLevel > 0)
        {
            buf.append("|   ");
        }

        buf.append(tree.getValue());
    }

    /** {@inheritDoc} */
    public String toString()
    {
        return buf.toString();
    }
}
