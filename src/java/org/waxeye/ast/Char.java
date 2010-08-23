/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.ast;

/**
 * An AST node with a character.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public final class Char <E extends Enum<?>> extends NoChildren<E>
implements IChar
{
    /** The value of the char. */
    private final char value;

    /**
     * Creates a new Char AST.
     *
     * @param value The character value.
     *
     * @param type The type of the AST.
     */
    public Char(final char value, final E type)
    {
        super(type);
        this.value = value;
    }

    /** {@inheritDoc} */
    public char getValue()
    {
        return value;
    }

    /** {@inheritDoc} */
    public void acceptASTVisitor(final IASTVisitor visitor)
    {
        visitor.visitChar(this);
    }

    /** {@inheritDoc} */
    public String toString()
    {
        return Character.toString(value);
    }
}
