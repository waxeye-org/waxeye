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
package org.waxeye.ast.print;

import java.util.List;

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
