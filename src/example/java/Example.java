/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
import org.waxeye.parser.ParseResult;

public class Example {
    public static void main(final String[] args) {
        // Create our parser
        final Parser parser = new Parser();

        // Parse our input
        final ParseResult<Type> result = parser.parse("42");

        // Print our AST
        System.out.println(result);
    }
}
