/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.ast;

import java.util.List;
import org.waxeye.ast.print.ArrowPrinter;

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
    public void acceptASTVisitor(final IASTVisitor visitor)
    {
        visitor.visitAST(this);
    }

    /** {@inheritDoc} */
    public final String childrenAsString()
    {
        final StringBuilder buf = new StringBuilder();

        for (IAST<E> c : children) {
            buf.append(((IChar) c).getValue());
        }

        return buf.toString();
    }

    /** {@inheritDoc} */
    public String toString()
    {
        return new ArrowPrinter(this).toString();
    }
}
