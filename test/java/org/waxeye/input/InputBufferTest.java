/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.input;

import static org.testng.Assert.*;
import org.testng.annotations.Test;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.AfterMethod;

import org.waxeye.EqualsTester;

/**
 * Tests for InputBuffer.
 *
 * @author Orlando Hill
 */
public final class InputBufferTest
{
    /** The fixture. */
    private InputBuffer fixture;

    /** Sets up the test. */
    @BeforeTest
    public void setupTest()
    {
    }

    /** Tears down the test method. */
    @AfterMethod
    public void tearDownMethod()
    {
        fixture = null;
    }

    /** Test for constructor given null. */
    @Test
    public void constructorNull()
    {
        boolean failed;

        try
        {
            failed = false;
            new InputBuffer(null);
        }
        catch (Exception e)
        {
            failed = true;
        }

        assertTrue(failed, "Constructor allowed null argument.");
    }

    /** Tests the value of EOF. */
    @Test
    public void testEOFValue()
    {
        assertEquals(InputBuffer.EOF, -1);
    }

    /** Test for nextChar(). */
    @Test
    public void nextCharValid()
    {
        fixture = new InputBuffer(new char[]{'a', 'b', 'c'});
        assertEquals(fixture.consume(), 'a');
        assertEquals(fixture.consume(), 'b');
        assertEquals(fixture.consume(), 'c');
        assertEquals(fixture.consume(), InputBuffer.EOF);
    }

    /** Test for nextChar(). */
    @Test
    public void nextCharEmpty()
    {
        fixture = new InputBuffer(new char[0]);
        assertEquals(fixture.consume(), InputBuffer.EOF);
        assertEquals(fixture.consume(), InputBuffer.EOF);
    }

    /** Test for peek(). */
    @Test
    public void peekEmpty()
    {
        fixture = new InputBuffer(new char[0]);
        assertEquals(fixture.peek(), InputBuffer.EOF);
        fixture.consume();
        assertEquals(fixture.peek(), InputBuffer.EOF);
    }

    /** Test for peek(). */
    @Test
    public void peekValid()
    {
        fixture = new InputBuffer(new char[]{'a', 'b'});
        assertEquals(fixture.peek(), 'a');
        assertEquals(fixture.peek(), 'a');

        fixture.consume();
        assertEquals(fixture.peek(), 'b');

        fixture.consume();
        assertEquals(fixture.peek(), InputBuffer.EOF);

        fixture.consume();
        assertEquals(fixture.peek(), InputBuffer.EOF);
    }

    /** Test for getInputPosition(). */
    @Test
    public void getInputPositionEmpty()
    {
        fixture = new InputBuffer(new char[0]);
        assertEquals(fixture.getPosition(), 0);
    }

    /** Test for getInputPosition(). */
    @Test
    public void getInputPositionValid()
    {
        fixture = new InputBuffer(new char[]{'a', 'b', 'c'});
        assertEquals(fixture.getPosition(), 0);

        fixture.consume();
        assertEquals(fixture.getPosition(), 1);

        fixture.consume();
        assertEquals(fixture.getPosition(), 2);

        fixture.consume();
        assertEquals(fixture.getPosition(), 3);

        fixture.consume();
        assertEquals(fixture.getPosition(), 3);
    }

    /** Test for setInputPosition(). */
    @Test
    public void setInputPosition()
    {
        fixture = new InputBuffer(new char[]{'a', 'b', 'c'});
        fixture.setPosition(-1);
        assertEquals(fixture.getPosition(), 0);

        fixture.setPosition(0);
        assertEquals(fixture.getPosition(), 0);

        fixture.setPosition(1);
        assertEquals(fixture.getPosition(), 1);

        fixture.setPosition(3);
        assertEquals(fixture.getPosition(), 3);

        fixture.setPosition(50);
        assertEquals(fixture.getPosition(), 50);
    }

    /** Test for getInputSize(). */
    @Test
    public void getInputSize()
    {
        fixture = new InputBuffer(new char[0]);
        assertEquals(fixture.getInputSize(), 0);

        fixture = new InputBuffer(new char[]{'a', 'b', 'c'});
        assertEquals(fixture.getInputSize(), 3);
    }

    /** Test for equals(). */
    @Test
    public void testEquals()
    {
        final char[] input = new char[]{'a', 'b'};
        final char[] inputDiff = new char[]{'z', 'w'};

        // original object
        final Object a = new InputBuffer(input);

        // another object that has the same values as the original
        final Object b = new InputBuffer(input);

        // another object with different values
        final Object c = new InputBuffer(inputDiff);

        new EqualsTester(a, b, c, null);
    }
}
