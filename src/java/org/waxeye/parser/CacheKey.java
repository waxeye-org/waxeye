/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

/**
 * A class to represent an index into the parsing cache.
 *
 * @author Orlando Hill
 */
final class CacheKey
{
    /** The index of the expression which was evaluated. */
    private final int expressionIndex;

    /** The position of evaluation on the input string. */
    private final int inputIndex;

    /** The context of the parser. */
    private final Object context;

    /** The hash code. */
    private final int hashCode;

    /**
     * Creates a new CacheKey for the given expression and input position.
     *
     * @param expressionIndex The index of the expression which was evaluated.
     *
     * @param inputIndex The position of evaluation on the input string.
     */
    public CacheKey(final int expressionIndex, final int inputIndex)
    {
        this(expressionIndex, inputIndex, null);
    }

    /**
     * Creates a new CacheKey for the given expression and input position.
     *
     * @param expressionIndex The index of the expression which was evaluated.
     *
     * @param inputIndex The position of evaluation on the input string.
     *
     * @param context The context of the parser.
     */
    public CacheKey(final int expressionIndex, final int inputIndex,
        final Object context)
    {
        this.expressionIndex = expressionIndex;
        this.inputIndex = inputIndex;
        this.context = context;
        this.hashCode = makeHashCode();

        assert invariants();
    }

    /**
     * Checks the invariants of the object.
     *
     * @return <code>true</code>.
     */
    private boolean invariants()
    {
        assert inputIndex >= 0;

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
            final CacheKey m = (CacheKey) object;

            if (expressionIndex == m.expressionIndex
                && inputIndex == m.inputIndex)
            {
                if (context == null && m.context == null
                    || context.equals(m.context))
                {
                    return true;
                }
            }
        }

        return false;
    }

    /** {@inheritDoc} */
    public int hashCode()
    {
        return hashCode;
    }

    /**
     * Makes a hash code.
     *
     * @return The hash code.
     */
    private int makeHashCode()
    {
        final int start = 17;
        final int mult = 37;
        int result = start + expressionIndex;

        result = mult * result + inputIndex;

        if (context != null)
        {
            result = mult * result + context.hashCode();
        }

        return Math.abs(result);
    }
}
