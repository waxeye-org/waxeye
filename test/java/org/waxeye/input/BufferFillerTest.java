/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.input;

import java.io.ByteArrayInputStream;
import java.io.StringReader;
import static org.testng.Assert.*;
import org.testng.annotations.Test;

/**
 * Tests for BufferFiller.
 *
 * @author Orlando Hill
 */
public final class BufferFillerTest
{
    private final String data = "ù%*é=^``~&°.:!§,?/#çà][-|";
    private final char[] expected = data.toCharArray();

    /** Test for asArray(InputStream). */
    @Test
    public void asArrayInputStream()
    {
        final byte[] bytes = data.getBytes();
        final ByteArrayInputStream bs = new ByteArrayInputStream(bytes);
        assertEquals(BufferFiller.asArray(bs), expected);
    }

    /** Test for asArray(Reader). */
    @Test
    public void asArrayReader()
    {
        final StringReader sr = new StringReader(data);
        assertEquals(BufferFiller.asArray(sr), expected);
    }
}
