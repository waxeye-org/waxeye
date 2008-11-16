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
package org.waxeye.parser;

import org.waxeye.ast.IAST;

/**
 * The result of a parse.
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
