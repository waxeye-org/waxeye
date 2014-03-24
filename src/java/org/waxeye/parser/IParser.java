/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import org.waxeye.input.IParserInput;

/**
 * An interface to a Waxeye parser.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public interface IParser <E extends Enum<?>>
{
    /**
     * Parses the input.
     *
     * @param input The input to parse.
     *
     * @return A ParseResult with either an AST or an error.
     */
    ParseResult<E> parse(char[] input);

    /**
     * Parses the input.
     *
     * @param input The input to parse.
     *
     * @return A ParseResult with either an AST or an error.
     */
    ParseResult<E> parse(String input);

    /**
     * Parses the input.
     *
     * @param input The input to parse.
     *
     * @return A ParseResult with either an AST or an error.
     *
     * @deprecated Will be removed in a later version. The user shouldn't need to construct an IParserInput.
     */
    @Deprecated ParseResult<E> parse(IParserInput input);
}
