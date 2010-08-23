/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.ast.IAST;

/**
 * The result of a parse.
 *
 * @param <E> The AST type.
 *
 * @author Orlando Hill
 */
public final class ParseResult <E extends Enum<?>>
{
    /** The AST of a successful parse. */
    private final IAST<E> ast;

    /** The error of an unsuccessful parse. */
    private final ParseError error;

    /**
     * Creates a new ParseResult.
     *
     * @param ast The AST of a successful parse.
     *
     * @param error The error of an unsuccessful parse.
     */
    public ParseResult(final IAST<E> ast, final ParseError error)
    {
        this.ast = ast;
        this.error = error;
    }

    /**
     * Returns the ast.
     *
     * @return Returns the ast.
     */
    public IAST<E> getAST()
    {
        return ast;
    }

    /**
     * Returns the error.
     *
     * @return Returns the error.
     */
    public ParseError getError()
    {
        return error;
    }

    /** {@inheritDoc} */
    public String toString()
    {
        if (ast != null)
        {
            return ast.toString();
        }

        if (error != null)
        {
            return error.toString();
        }

        return "ParseResult(null, null)";
    }
}
