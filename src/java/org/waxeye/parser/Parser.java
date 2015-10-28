/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.Stack;
import java.util.Collections;
import org.waxeye.ast.AST;
import org.waxeye.ast.Char;
import org.waxeye.ast.Empty;
import org.waxeye.ast.IAST;
import org.waxeye.ast.Position;
import org.waxeye.input.InputBuffer;
import org.waxeye.input.IParserInput;
import org.waxeye.parser.expression.*;
import org.waxeye.parser.machinevalue.*;
import org.waxeye.parser.err.*;
import org.waxeye.parser.continuations.*;

/**
 * Implements the logic of the parser.
 *
 * @param <E> The node types for the AST.
 *
 * @author Orlando Hill
 */
public abstract class Parser <E extends Enum<?>>
{
    /** The rules of the parser. */
    private final Map<E, Nonterminal> rules;

    /** The starting nonterminal. */
    private final E start;

    /** The character type. */
    private final E charType;

    /** The "none" type. */
    private final E noneType;

    /**
     * Creates a new Parser.
     *
     * @param rules The rules of the parser.
     *
     * @param start The starting rule.
     */
    public Parser(final Map<E, Nonterminal> rules, final E charType, final E noneType, final E start)
    {
        this.rules = rules;
        this.start = start;
        this.charType = charType;
        this.noneType = noneType;
    }

    /** {@inheritDoc} */
    public ParseResult<E> parse(final char[] input)
    {
        return new InnerParser(input).parse();
    }

    /** {@inheritDoc} */
    public ParseResult<E> parse(final String input)
    {
        return new InnerParser(input.toCharArray()).parse();
    }

    /**
     * A hidden inner class so that we can parse without exposing things to the API user.
     *
     * @author Orlando Hill
     * @author Joshua Gross, Yahoo
     */
    private class InnerParser
    {
        /** The input to parse. */
        private final char[] input;

        /**
         * Creates a new Parser.
         *
         * @param input The input to parse.
         */
        InnerParser(final char[] input)
        {
            this.input = input;
        }

        public ParseResult<E> parse () {
          // Move from initial state to halting state
          ArrayList<E> ntList = new ArrayList<E>();
          ntList.add(start);
          MachineState state = move(MachineConfiguration.EVAL(rules.get(start).getExpression(), 0, new ArrayList(), new RawError(0, ntList, new ArrayList(), start), new Stack<Continuation>()));
          while (!state.isFinal()) {
            state = move(((MachineStateInter)state).getConfiguration());
          }
          return ((MachineStateFinal)state).getResult();
        }

        private MachineState<E> move (MachineConfiguration<E> config) {
          if (config.isEval()) {
            return moveEval((MachineConfigurationEval<E>)config);
          }

          return moveApply((MachineConfigurationApply<E>)config);
        }

        private MachineState<E> moveEval (MachineConfigurationEval<E> config) {
          Expression expr = config.getExpression();
          int pos = config.getPos();
          boolean eof = (pos >= input.length);
          List<IAST<E>> asts = config.getAsts();
          Stack<Continuation> k = config.getContinuations();
          RawError err = config.getError();

          if (expr instanceof VoidExpr) {
            VoidExpr e = (VoidExpr)expr;
            k.push(Continuation.VOID(asts));
            return MachineState.INTER(MachineConfiguration.EVAL(e.getExpression(), pos, new ArrayList(), err, k));
          }
          if (expr instanceof Any) {
            if (eof) {
              return MachineState.INTER(MachineConfiguration.APPLY(k, MachineValue.FAIL(updateError(err, pos, new ErrAny()))));
            } else {
              asts.add(0, new Char(input[pos], charType));
              return MachineState.INTER(MachineConfiguration.APPLY(k, MachineValue.VAL(pos+1, asts, err)));
            }
          }
          if (expr instanceof CharExpr) {
            CharExpr e = (CharExpr)expr;
            char c = e.getChar();
            MachineValue newval;
            if (eof || c != input[pos]) {
              newval = MachineValue.FAIL(updateError(err, pos, new ErrChar(c)));
            } else {
              asts.add(0, new Char(input[pos], charType));
              newval = MachineValue.VAL(pos+1, asts, err);
            }
            return MachineState.INTER(MachineConfiguration.APPLY(k, newval));
          }
          if (expr instanceof CharClass) {
            CharClass e = (CharClass)expr;
            List<List<String>> charClasses = e.getCharClass();

            if (!eof) {
              for (List<String> chars : charClasses) {
                char c1 = chars.get(0).charAt(0);
                char c2 = chars.get(1).charAt(0);

                if (c1 <= input[pos] && c2 >= input[pos]) {
                  asts.add(0, new Char<E>(input[pos], charType));
                  return MachineState.INTER(MachineConfiguration.APPLY(k, MachineValue.VAL(pos+1, asts, err)));
                }
              }
            }

            return MachineState.INTER(MachineConfiguration.APPLY(k, MachineValue.FAIL(updateError(err, pos, new ErrCC(charClasses)))));
          }
          if (expr instanceof Seq) {
            // a sequence is made up of a list of expressions
            // we traverse the list, making sure each expression succeeds
            // the rest of the string return by the expression is used
            // as input to the next expression
            Seq e = (Seq)expr;
            List<Expression> exprs = new ArrayList<Expression>(e.getExpressions());
            if (exprs.isEmpty()) {
              return MachineState.INTER(MachineConfiguration.APPLY(k, MachineValue.VAL(pos, asts, err)));
            }

            Expression firstExpr = exprs.remove(0);
            k.push(Continuation.SEQ(exprs));
            return MachineState.INTER(MachineConfiguration.EVAL(firstExpr, pos, asts, err, k));
          }
          if (expr instanceof Plus) {
            Plus e = (Plus)expr;
            k.push(Continuation.PLUS(e.getExpression()));
            return MachineState.INTER(MachineConfiguration.EVAL(e.getExpression(), pos, asts, err, k));
          }
          if (expr instanceof Star) {
            Star e = (Star)expr;
            k.push(Continuation.STAR(e.getExpression(), pos, asts));
            return MachineState.INTER(MachineConfiguration.EVAL(e.getExpression(), pos, asts, err, k));
          }
          if (expr instanceof Opt) {
            Opt e = (Opt)expr;
            k.push(Continuation.OPT(pos, asts));
            return MachineState.INTER(MachineConfiguration.EVAL(e.getExpression(), pos, asts, err, k));
          }
          if (expr instanceof And) {
            And e = (And)expr;
            k.push(Continuation.AND(pos, asts, err));
            return MachineState.INTER(MachineConfiguration.EVAL(e.getExpression(), pos, new ArrayList(), err, k));
          }
          if (expr instanceof Not) {
            Not e = (Not)expr;
            k.push(Continuation.NOT(pos, asts, err));
            return MachineState.INTER(MachineConfiguration.EVAL(e.getExpression(), pos, new ArrayList(), err, k));
          }
          if (expr instanceof NT) {
            NT e = (NT)expr;
            Nonterminal ntRef = rules.get(e.getNT());
            k.push(Continuation.NT(ntRef.getMode(), (E)e.getNT(), asts, (E)err.getCurrentNT()));
            err = new RawError(err.getPos(), err.getNonterminals(), err.getFailedMatches(), e.getNT());
            return MachineState.INTER(MachineConfiguration.EVAL(ntRef.getExpression(), pos, new ArrayList(), err, k));
          }
          if (expr instanceof Alt) {
            Alt e = (Alt)expr;
            List<Expression> exprs = new ArrayList<Expression>(e.getExpressions());
            if (exprs.isEmpty()) {
              return MachineState.INTER(MachineConfiguration.APPLY(k, MachineValue.VAL(pos, asts, err)));
            } else {
              Expression firstExpr = exprs.remove(0);
              k.push(Continuation.ALT(exprs, pos, asts));
              return MachineState.INTER(MachineConfiguration.EVAL(firstExpr, pos, asts, err, k));
            }
          }

          throw new RuntimeException("Unsupported machine configuration: eval");
        }

        private MachineState<E> moveApply (MachineConfigurationApply<E> config) {
          Stack<Continuation> kRest = config.getContinuations();
          Continuation kFirst = (!kRest.empty() ? kRest.pop() : null);

          MachineValue v = config.getValue();
          MachineValueVal val = (v instanceof MachineValueVal) ? (MachineValueVal)v : null;
          MachineValueFail failVal = (v instanceof MachineValueFail) ? (MachineValueFail)v : null;

          if (failVal != null && (kFirst instanceof ContinuationVoid || kFirst instanceof ContinuationSeq || kFirst instanceof ContinuationPlus)) {
            return MachineState.INTER(MachineConfiguration.APPLY(kRest, v));
          }
          if (failVal != null && (kFirst instanceof ContinuationAnd)) {
            ContinuationAnd cont = (ContinuationAnd)kFirst;
            return MachineState.INTER(MachineConfiguration.APPLY(kRest, MachineValue.FAIL(cont.getError())));
          }
          if (failVal != null && (kFirst instanceof ContinuationNot || kFirst instanceof ContinuationStar || kFirst instanceof ContinuationOpt)) {
            int pos = ((ContinuationWithPos)kFirst).getPos();
            List<IAST<E>> asts = ((ContinuationWithAsts)kFirst).getAsts();
            return MachineState.INTER(MachineConfiguration.APPLY(kRest, MachineValue.VAL(pos, asts, failVal.getError())));
          }
          if (failVal != null && kFirst instanceof ContinuationNT) {
            ContinuationNT cont = (ContinuationNT)kFirst;
            RawError err = failVal.getError();
            return MachineState.INTER(MachineConfiguration.APPLY(kRest, MachineValue.FAIL(new RawError(err.getPos(), err.getNonterminals(), err.getFailedMatches(), cont.getCurrentNT()))));
          }
          if (failVal != null && (kFirst instanceof ContinuationAlt)) {
            ContinuationAlt cont = (ContinuationAlt)kFirst;
            List<Expression> exprs = cont.getExpressions();
            if (!exprs.isEmpty()) {
              Expression firstExp = exprs.remove(0);
              kRest.push(Continuation.ALT(exprs, cont.getPos(), cont.getAsts()));
              RawError err = failVal.getError();
              return MachineState.INTER(MachineConfiguration.EVAL(firstExp, cont.getPos(), cont.getAsts(), err, kRest));
            } else {
              return MachineState.INTER(MachineConfiguration.APPLY(kRest, v));
            }
          }
          if (failVal != null && kFirst != null) {
            throw new RuntimeException("Unsupported machine configuration: apply kFirst fail");
          }
          if (failVal != null) {
            return MachineState.FINAL(new ParseResult<E>(null, failVal.getError().toParseError(input)));
          }

          if (val != null && kFirst == null) {
            List<IAST<E>> asts = val.getAsts();
            Collections.reverse(asts);

            if (val.getPos() >= input.length) {
              if (rules.get(start).mode == NonterminalMode.NORMAL) {
                return MachineState.FINAL(new ParseResult(new AST<E>(start, asts, val.getPos()), null));
              } else if (rules.get(start).mode == NonterminalMode.PRUNING) {
                if (asts.isEmpty()) {
                  return MachineState.FINAL(new ParseResult(new Empty<E>(start), null));
                } else if (asts.size() == 1) {
                  return MachineState.FINAL(new ParseResult(asts.get(0), null));
                } else {
                  return MachineState.FINAL(new ParseResult(new AST<E>(start, asts, val.getPos()), null));
                }
              } else {
                return MachineState.FINAL(new ParseResult(new Empty<E>(start), null));
              }
            } else if (val.getError() != null && val.getPos() == val.getError().getPos()) {
              return MachineState.FINAL(new ParseResult(null, (new RawError(val.getPos(), val.getError().getNonterminals(), val.getError().getFailedMatches(), start)).toParseError(input)));
            } else {
              return MachineState.FINAL(new ParseResult(null, (new RawError(val.getPos(), new ArrayList(), new ArrayList(), start)).toParseError(input)));
            }
          }
          if (kFirst instanceof ContinuationVoid) {
            ContinuationVoid cont = (ContinuationVoid)kFirst;
            return MachineState.INTER(MachineConfiguration.APPLY(kRest, MachineValue.VAL(val.getPos(), cont.getAsts(), val.getError())));
          }
          if (kFirst instanceof ContinuationAnd) {
            ContinuationAnd cont = (ContinuationAnd)kFirst;
            return MachineState.INTER(MachineConfiguration.APPLY(kRest, MachineValue.VAL(cont.getPos(), cont.getAsts(), cont.getError())));
          }
          if (kFirst instanceof ContinuationNot) {
            ContinuationNot cont = (ContinuationNot)kFirst;
            return MachineState.INTER(MachineConfiguration.APPLY(kRest, MachineValue.FAIL(cont.getError())));
          }
          if (kFirst instanceof ContinuationSeq) {
            ContinuationSeq cont = (ContinuationSeq)kFirst;
            List<Expression> exprs = cont.getExpressions();
            if (exprs.isEmpty()) {
              return MachineState.INTER(MachineConfiguration.APPLY(kRest, v));
            }
            Expression firstExp = exprs.remove(0);
            kRest.push(Continuation.SEQ(exprs));
            return MachineState.INTER(MachineConfiguration.EVAL(firstExp, val.getPos(), val.getAsts(), val.getError(), kRest));
          }
          if (kFirst instanceof ContinuationAlt || kFirst instanceof ContinuationOpt) {
            return MachineState.INTER(MachineConfiguration.APPLY(kRest, v));
          }
          // The second continuation used to evaluate PLUS
          // is the same continuation as for STAR
          if (kFirst instanceof ContinuationStar || kFirst instanceof ContinuationPlus) {
            Expression expr = ((ContinuationWithExpression)kFirst).getExpression();
            kRest.push(Continuation.STAR(expr, val.getPos(), val.getAsts()));
            return MachineState.INTER(MachineConfiguration.EVAL(expr, val.getPos(), val.getAsts(), val.getError(), kRest));
          }
          if (kFirst instanceof ContinuationNT) {
            ContinuationNT cont = (ContinuationNT)kFirst;
            List<IAST<E>> asts = cont.getAsts();
            NonterminalMode mode = cont.getMode();
            E nt = (E)cont.getNT();
            E prevNT = (E)cont.getCurrentNT();
            RawError err = val.getError();
            int errPos = (err != null ? err.getPos() : -1);
            List<Nonterminal> errNTs = (err != null ? err.getNonterminals() : new ArrayList());
            List<ErrBase> errFailedMatches = (err != null ? err.getFailedMatches() : new ArrayList());

            RawError newErr = new RawError(errPos, errNTs, errFailedMatches, prevNT);

            List<IAST<E>> valAsts = new ArrayList<IAST<E>>(val.getAsts());
            Collections.reverse(valAsts);

            if (mode == NonterminalMode.NORMAL) {
              asts.add(0, new AST<E>(nt, valAsts, val.getPos()));
              return MachineState.INTER(MachineConfiguration.APPLY(kRest, MachineValue.VAL(val.getPos(), asts, newErr)));
            } else if (mode == NonterminalMode.PRUNING) {
              if (valAsts.size() == 1) {
                asts.add(0, valAsts.get(0));
              } else if (!valAsts.isEmpty()) {
                asts.add(0, new AST<E>(nt, valAsts, val.getPos()));
              }
              return MachineState.INTER(MachineConfiguration.APPLY(kRest, MachineValue.VAL(val.getPos(), asts, newErr)));
            } else {
              // VOIDING
              return MachineState.INTER(MachineConfiguration.APPLY(kRest, MachineValue.VAL(val.getPos(), asts, newErr)));
            }
          }

          throw new RuntimeException("Unsupported machine configuration: apply");
        }

        private RawError updateError (RawError err, int pos, ErrBase newErr) {
          if (err != null && pos > err.getPos()) {
            List<ErrBase> newFailureList = new ArrayList<ErrBase>();
            newFailureList.add(newErr);

            List<E> nonterminals = new ArrayList<E>();
            nonterminals.add((E)err.getCurrentNT());

            return new RawError(pos, nonterminals, newFailureList, err.getCurrentNT());
          } else if (err == null || pos == err.getPos()) {
            List<E> nonterminals;
            List<ErrBase> failures;
            E currentNT = start;
            if (err != null) {
              nonterminals = err.getNonterminals();
              failures = new ArrayList<ErrBase>(err.getFailedMatches());
              pos = err.getPos();
              currentNT = (E)err.getCurrentNT();
            } else {
              nonterminals = new ArrayList<E>();
              failures = new ArrayList<ErrBase>();
            }

            nonterminals.add(0, currentNT);
            failures.add(0, newErr);

            return new RawError(pos, nonterminals, failures, currentNT);
          }

          return err;
        }
    }
}
