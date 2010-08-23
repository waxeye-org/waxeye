/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.input;

/**
 * A class to represent the buffer to hold the input string.
 *
 * @author Orlando Hill
 */
public final class InputBuffer implements IParserInput
{
    /** The internal buffer. */
    private final char[] input;

    /** The current position in the buffer. */
    private int position;

    /** The size of the buffer. */
    private final int inputSize;

    /**
     * Creates a new InputBuffer for the given char[]. The position starts
     * at index 0.
     *
     * @param input The char[] to use for our buffer.
     */
    public InputBuffer(final char[] input)
    {
        this.input = input;
        this.position = 0;
        this.inputSize = input.length;

        assert invariants();
    }

    /**
     * Checks the invariants of the object.
     *
     * @return <code>true</code>.
     */
    private boolean invariants()
    {
        assert input != null;
        assert position >= 0 && position <= inputSize;

        return true;
    }

    /** {@inheritDoc} */
    public int consume()
    {
        if (position < inputSize)
        {
            return input[position++];
        }

        return EOF;
    }

    /** {@inheritDoc} */
    public int peek()
    {
        if (position < inputSize)
        {
            return input[position];
        }

        return EOF;
    }

    /** {@inheritDoc} */
    public int getPosition()
    {
        return position;
    }

    /**
     * Returns the inputSize.
     *
     * @return Returns the inputSize.
     */
    public int getInputSize()
    {
        return inputSize;
    }

    /**
     * Sets the position of the input buffer to the given value. If the value
     * given is less than 0 then the position is set to 0.
     *
     * @param position The position to set.
     */
    public void setPosition(final int position)
    {
        if (position < 0)
        {
            this.position = 0;
        }
        else
        {
            this.position = position;
        }
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
            final InputBuffer b = (InputBuffer) object;

            if (input != b.input || position != b.position)
            {
                return false;
            }

            return true;
        }

        return false;
    }

    /** {@inheritDoc} */
    public int hashCode()
    {
        final int start = 17;
        final int mult = 37;
        int result = start;

        if (input != null)
        {
            for (char c : input)
            {
                result = mult * (result + c);
            }
        }

        result = mult * result + position;

        return Math.abs(result);
    }
}
