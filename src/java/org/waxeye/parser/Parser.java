/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Stack;
import org.waxeye.ast.AST;
import org.waxeye.ast.Char;
import org.waxeye.ast.Empty;
import org.waxeye.ast.IAST;
import org.waxeye.ast.Position;
import org.waxeye.input.IParserInput;

/**
 * Implements the logic of the parser.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public abstract class Parser <E extends Enum<?>> implements IParser<E>
{
    /** The empty node. */
    private final IAST<E> empty;

    /** The char type. */
    private final E charType;

    /** The pos type. */
    private final E posType;

    /** The neg type. */
    private final E negType;

    /** The automata of the parser. */
    private final List<FA<E>> automata;

    /** Whether to check that all input gets parsed. */
    private final boolean eofCheck;

    /** The starting automaton. */
    private final int start;

    /**
     * Creates a new Parser.
     *
     * @param automata The automata of the parser.
     *
     * @param eofCheck Whether to check that all input gets parsed.
     *
     * @param start The starting automaton.
     *
     * @param emptyType The empty type.
     *
     * @param charType The char type.
     *
     * @param posType The positive check type.
     *
     * @param negType The negative check type.
     */
    public Parser(final List<FA<E>> automata,  final boolean eofCheck,
        final int start,
        final E emptyType, final E charType, final E posType, final E negType)
    {
        this.automata = automata;
        this.eofCheck = eofCheck;
        this.start = start;
        this.empty = new Empty<E>(emptyType);
        this.charType = charType;
        this.posType = posType;
        this.negType = negType;
    }

    /** {@inheritDoc} */
    public final ParseResult<E> parse(final IParserInput input)
    {
        return new InnerParser(input).parse();
    }

    /**
     * A hidden inner class so that we can visit the transition costs without
     * exposing things to the API user.
     *
     * @author Orlando Hill
     */
    private final class InnerParser implements ITransitionVisitor<E>
    {
        /** The input to parse. */
        private final IParserInput input;

        /** The automata stack. */
        private final Stack<FA<E>> faStack;

        /** The result cache. */
        private final HashMap<CacheKey, CacheItem<E>> cache;

        /** The line number. */
        private int line;

        /** The column number. */
        private int column;

        /** Whether the last character was a carriage return. */
        private boolean lastCR;

        /** The position of the deepest error. */
        private int errorPos;

        /** The line of the deepest error. */
        private int errorLine;

        /** The column of the deepest error. */
        private int errorCol;

        /** The nt deepest error. */
        private String errorNT;

        /**
         * Creates a new Parser.
         *
         * @param input The input to parse.
         */
        InnerParser(final IParserInput input)
        {
            this.input = input;
            this.faStack = new Stack<FA<E>>();
            this.cache = new HashMap<CacheKey, CacheItem<E>>();
            this.line = 1;
            this.column = 0;
            this.lastCR = false;
            this.errorPos = 0;
            this.errorLine = 1;
            this.errorCol = 0;
            this.errorNT = automata.get(start).getType().name();
        }

        /**
         * Parses the input.
         *
         * @return The result of the parse.
         */
        ParseResult<E> parse()
        {
            IAST<E> ast = matchAutomaton(start);
            ParseError error = null;

            if (ast == null)
            {
                // Create a parse error
                error = new ParseError(errorPos, errorLine, errorCol, errorNT);
            }
            else
            {
                // Check that all input was consumed
                if (eofCheck && input.peek() != IParserInput.EOF)
                {
                    // Create a parse error - Not all input consumed
                    error = new ParseError(errorPos, errorLine, errorCol, errorNT);
                    ast = null;
                }
            }

            return new ParseResult<E>(ast, error);
        }

        /**
         * Restores the input position to the given values.
         *
         * @param pos The position.
         *
         * @param line The line.
         *
         * @param col The column.
         *
         * @param cr Whether the last character was a CR.
         */
        private void restorePos(final int pos, final int line, final int col,
            final boolean cr)
        {
            this.input.setPosition(pos);
            this.line = line;
            this.column = col;
            this.lastCR = cr;
        }

        /**
         * Matches the automaton at the given index.
         *
         * @param index The index.
         *
         * @return The result.
         */
        private IAST<E> matchAutomaton(final int index)
        {
            final int startPos = input.getPosition();
            final CacheKey key = new CacheKey(index, startPos);
            final CacheItem<E> cachedItem = cache.get(key);

            if (cachedItem != null)
            {
                restorePos(cachedItem.getPosition(), cachedItem.getLine(),
                    cachedItem.getColumn(), cachedItem.getLastCR());
                return cachedItem.getResult();
            }

            final int startLine = line;
            final int startCol = column;
            final boolean startCR = lastCR;
            final FA<E> automaton = automata.get(index);
            final E type = automaton.getType();
            final int mode = automaton.getMode();

            faStack.push(automaton);
            final List<IAST<E>> res = matchState(0);
            faStack.pop();

            IAST<E> value;

            if (type.equals(posType))
            {
                restorePos(startPos, startLine, startCol, startCR);

                if (res == null)
                {
                    value = null;
                }
                else
                {
                    value = empty;
                }
            }
            else
            {
                if (type.equals(negType))
                {
                    restorePos(startPos, startLine, startCol, startCR);

                    if (res == null)
                    {
                        value = empty;
                    }
                    else
                    {
                        updateError();
                        value = null;
                    }
                }
                else
                {
                    if (res == null)
                    {
                        updateError();
                        value = null;
                    }
                    else
                    {
                        switch (mode)
                        {
                            case FA.VOID:
                            {
                                value = empty;
                                break;
                            }
                            case FA.PRUNE:
                            {
                                switch (res.size())
                                {
                                    case 0:
                                    {
                                        value = empty;
                                        break;
                                    }
                                    case 1:
                                    {
                                        value = res.get(0);
                                        break;
                                    }
                                    default:
                                    {
                                        value = new AST<E>(type, res, new Position(startPos, input.getPosition()));
                                        break;
                                    }
                                }
                                break;
                            }
                            default:
                            {
                                value = new AST<E>(type, res, new Position(startPos, input.getPosition()));
                                break;
                            }
                        }
                    }
                }
            }

            cache.put(key, new CacheItem<E>(value, input.getPosition(), line,
                column, lastCR));

            return value;
        }

        /**
         * Matches the state at the given index.
         *
         * @param index The index.
         *
         * @return The result.
         */
        private List<IAST<E>> matchState(final int index)
        {
            final State<E> state = faStack.peek().getStates().get(index);
            final List<IAST<E>> res = matchEdges(state.getEdges(), 0);

            if (res == null)
            {
                if (state.isMatch())
                {
                    return new ArrayList<IAST<E>>();
                }
                else
                {
                    return null;
                }
            }
            else
            {
                return res;
            }
        }

        /**
         * Matches the given edges starting from the given index.
         *
         * @param edges The edges.
         *
         * @param index The index.
         *
         * @return The result.
         */
        private List<IAST<E>> matchEdges(final List<Edge<E>> edges, final int index)
        {
            if (index < edges.size())
            {
                final List<IAST<E>> res = matchEdge(edges.get(index));

                if (res == null)
                {
                    return matchEdges(edges, index + 1);
                }
                else
                {
                    return res;
                }
            }
            else
            {
                return null;
            }
        }

        /**
         * Matches the given edge.
         *
         * @param edge The edge.
         *
         * @return The result.
         */
        private List<IAST<E>> matchEdge(final Edge<E> edge)
        {
            final int startPos = input.getPosition();
            final int startLine = line;
            final int startCol = column;
            final boolean startCR = lastCR;
            final IAST<E> res = edge.getTrans().acceptVisitor(this);

            if (res == null)
            {
                return null;
            }
            else
            {
                final List<IAST<E>> transRes = matchState(edge.getState());

                if (transRes == null)
                {
                    restorePos(startPos, startLine, startCol, startCR);
                    return null;
                }
                else
                {
                    if (edge.isVoided() || res.equals(empty))
                    {
                        return transRes;
                    }
                    else
                    {
                        // Note: If we were to memoize state results,
                        //       this would need to be changed.
                        transRes.add(0, res);
                        return transRes;
                    }
                }
            }
        }

        /**
         * Updates the line and column numbers.
         *
         * @param ch The character being consumed.
         */
        private void updateLineCol(final char ch)
        {
            if (ch == '\r')
            {
                line++;
                column = 0;
                lastCR = true;
            }
            else
            {
                if (ch == '\n')
                {
                    if (!lastCR)
                    {
                        line++;
                        column = 0;
                    }
                }
                else
                {
                    column++;
                }

                lastCR = false;
            }
        }

        /**
         * Updates the error info if needed.
         */
        private void updateError()
        {
            if (errorPos < input.getPosition())
            {
                errorPos = input.getPosition();
                errorLine = line;
                errorCol = column;
                errorNT = faStack.peek().getType().name();
            }
        }

        /** {@inheritDoc} */
        public IAST<E> visitAutomatonTransition(final AutomatonTransition<E> t)
        {
            return matchAutomaton(t.getIndex());
        }

        /** {@inheritDoc} */
        public IAST<E> visitCharTransition(final CharTransition<E> t)
        {
            if (input.peek() != IParserInput.EOF)
            {
                final char c = (char) input.peek();

                if (t.withinSet(c))
                {
                    input.consume();
                    updateLineCol(c);
                    return new Char<E>(c, charType);
                }
            }

            updateError();
            return null;
        }

        /** {@inheritDoc} */
        public IAST<E> visitWildCardTransition(final WildCardTransition<E> t)
        {
            if (input.peek() == IParserInput.EOF)
            {
                updateError();
                return null;
            }

            final char c = (char) input.consume();
            updateLineCol(c);
            return new Char<E>(c, charType);
        }
    }
}
