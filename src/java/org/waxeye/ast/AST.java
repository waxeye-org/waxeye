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
package org.waxeye.ast;

import java.util.List;

/**
 * A class to represent an abstract syntax tree.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public final class AST <E extends Enum<?>> implements IAST<E>
{
    /** The type of AST node. */
    private final E type;

    /** The children of the AST. */
    private final List<IAST<E>> children;

    /** The position of the AST. */
    private final Position position;

    /**
     * Creates a new AST.
     *
     * @param type The type of the AST.
     *
     * @param children The children of the AST.
     *
     * @param position The position of the AST.
     */
    public AST(final E type, final List<IAST<E>> children,
               final Position position)
    {
        this.type = type;
        this.children = children;
        this.position = position;

        assert invariants();
    }

    /**
     * Checks the invariants of the object.
     *
     * @return <code>true</code>.
     */
    private boolean invariants()
    {
        assert type != null;
        assert children != null;

        return true;
    }

    /** {@inheritDoc} */
    public boolean equals(final Object object)
    {
        if (this == object)
        {
            return true;
        }

        if (object != null && object.getClass() == this.getClass())
        {
            final AST<E> p = (AST<E>) object;

            if (type.equals(p.type) && children.equals(p.children))
            {
                return true;
            }
        }

        return false;
    }

    /** {@inheritDoc} */
    public int hashCode()
    {
        final int start = 17;
        final int mult = 37;
        int result = start * type.hashCode();

        for (IAST<E> t : children)
        {
            result = mult * result + t.hashCode();
        }

        return Math.abs(result);
    }

    /** {@inheritDoc} */
    public List<IAST<E>> getChildren()
    {
        return children;
    }

    /** {@inheritDoc} */
    public Position getPosition()
    {
        return position;
    }

    /** {@inheritDoc} */
    public E getType()
    {
        return type;
    }

    /** {@inheritDoc} */
    public void acceptASTVisitor(final IASTVisitor<E> visitor)
    {
        visitor.visitAST(this);
    }
}
