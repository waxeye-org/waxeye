/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
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
