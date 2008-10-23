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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

/**
 * A class to read and return the contents of an InputStream.
 *
 * @author Orlando Hill
 */
public final class BufferFiller
{
    /** The initial capacity of the buffer list. */
    private static final int FILLED_INIT = 128;

    /** The size for each buffer array. */
    private static final int BLOCK_SIZE = 1024;

    /**
     * Empty constructor.
     */
    private BufferFiller()
    {
    }

    /**
     * Reads the contents of the given stream from its current position.
     *
     * Treats IOExceptions from the input stream as if the end of the stream
     * had been reached.
     *
     * @param input The stream to read from.
     *
     * @return The contents of the stream.
     */
    public static char[] asArray(final InputStream input)
    {
        final ArrayList<char[]> filled = new ArrayList<char[]>(FILLED_INIT);
        char[] current = new char[BLOCK_SIZE];
        int currentIndex = 0;
        int resultSize = 0;

        try
        {
            int in = input.read();

            // While still able to read
            while (in != -1)
            {
                // If we have filled our current block
                if (currentIndex == BLOCK_SIZE)
                {
                    // Make a new block
                    filled.add(current);
                    current = new char[BLOCK_SIZE];
                    currentIndex = 0;
                }

                // Put our input into the current block
                current[currentIndex] = (char) in;
                currentIndex++;
                resultSize++;

                // Read again from the input
                in = input.read();
            }
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }

        final char[] result = new char[resultSize];
        int resultIndex = 0;

        // Copy the filled blocks
        for (char[] block : filled)
        {
            System.arraycopy(block, 0, result, resultIndex, BLOCK_SIZE);
            resultIndex += BLOCK_SIZE;
        }

        // Copy the left over
        System.arraycopy(current, 0, result, resultIndex, currentIndex);

        return result;
    }
}
