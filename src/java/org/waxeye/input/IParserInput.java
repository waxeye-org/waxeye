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
package org.waxeye.input;

/**
 * An interface for parser input.
 *
 * @author Orlando Hill
 */
public interface IParserInput
{
    /** The end of the file marker. */
    int EOF = -1;

    /**
     * Gets the next character from the input and moves the position forward 1.
     *
     * @return The next character or EOF if end of input reached.
     */
    int consume();

    /**
     * Gets the next character from the input but maintains the position.
     *
     * @return The next character or EOF if end of input reached.
     */
    int peek();

    /**
     * Gets the position marker in the input.
     *
     * @return Returns the position marker in the input.
     */
    int getPosition();

    /**
     * Sets the position marker of the input to the given value.
     *
     * If the position given is less than 0 then the position is set to 0.
     *
     * @param position The position to set.
     */
    void setPosition(int position);
}
