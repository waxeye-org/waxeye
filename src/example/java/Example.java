/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
import org.waxeye.input.InputBuffer;
import org.waxeye.parser.ParseResult;

public class Example {
    public static void main(final String[] args) {
        // Create our parser
        final Parser parser = new Parser();

        // Setup our input
        final InputBuffer input = new InputBuffer("42".toCharArray());

        // Parse our input
        final ParseResult<Type> result = parser.parse(input);

        // Print our ast
        System.out.println(result);
    }
}
