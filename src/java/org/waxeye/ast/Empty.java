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

import org.waxeye.ast.print.ArrowPrinter;

/**
 * A class to represent an empty AST.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public final class Empty <E extends Enum<?>> extends NoChildren<E>
implements IEmpty
{
    /**
     * Creates a new Empty AST.
     *
     * @param type The type of the AST.
     */
    public Empty(final E type)
    {
        super(type);
    }

    /** {@inheritDoc} */
    public void acceptASTVisitor(final IASTVisitor visitor)
    {
        visitor.visitEmpty(this);
    }

    /** {@inheritDoc} */
    public String toString()
    {
        return new ArrowPrinter(this).toString();
    }
}
