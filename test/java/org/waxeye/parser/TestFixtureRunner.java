/*
 * Waxeye Parser Generator
 * www.waxeye.org
 * Copyright (C) 2008-2010 Orlando Hill
 * Licensed under the MIT license. See 'LICENSE' for details.
 */
package org.waxeye.parser;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;

import org.testng.annotations.Test;

import org.waxeye.ast.Char;
import org.waxeye.ast.Empty;
import org.waxeye.ast.AST;
import org.waxeye.ast.IAST;
import org.waxeye.parser.Nonterminal;
import org.waxeye.parser.NonterminalMode;
import org.waxeye.parser.expression.*;
import org.waxeye.parser.err.*;

import com.eclipsesource.json.*;

enum TestEnvironmentTypes { _None, _Char, A, B, C, Int, Unary, Prod, Sum, V1, V2, V3, WS, Nums, lc, S };

class TestEnvironmentParser extends Parser<TestEnvironmentTypes> {
  public TestEnvironmentParser (Map<TestEnvironmentTypes, Nonterminal> rules, TestEnvironmentTypes start) {
    super(rules, TestEnvironmentTypes._Char, TestEnvironmentTypes._None, start);
  }
}

/**
 * Tests for CacheKey.
 *
 * @author Orlando Hill
 */
public class TestFixtureRunner
{
  static HashMap<TestEnvironmentTypes, Nonterminal> env1;

  /** Run all test fixtures. */
  @Test
  public void runTests () throws IOException
  {
    env1 = new HashMap<TestEnvironmentTypes, Nonterminal>();

    // Simple rules
    env1.put(TestEnvironmentTypes.A, new Nonterminal(NonterminalMode.NORMAL, new CharExpr('a')));
    env1.put(TestEnvironmentTypes.B, new Nonterminal(NonterminalMode.NORMAL, new Alt(Arrays.asList((Expression)new CharExpr('b'), (Expression)new CharExpr('B')))));

    // Direct recursion
    env1.put(TestEnvironmentTypes.C, new Nonterminal(NonterminalMode.NORMAL, new Alt(Arrays.asList((Expression)new NT(TestEnvironmentTypes.A), (Expression)new Seq(Arrays.asList((Expression)new CharExpr('('), (Expression)new NT(TestEnvironmentTypes.C), (Expression)new CharExpr(')')))))));

    // Mutual recursion
    env1.put(TestEnvironmentTypes.Int, new Nonterminal(NonterminalMode.NORMAL, new Alt(Arrays.asList((Expression)new CharExpr('0'), (Expression)new Seq(Arrays.asList((Expression)new CharClass(Arrays.asList((List<String>)Arrays.asList("1", "9"))), (Expression)new Star(new CharClass(Arrays.asList((List<String>)Arrays.asList("0", "9"))))))))));
    env1.put(TestEnvironmentTypes.Unary, new Nonterminal(NonterminalMode.NORMAL, new Alt(Arrays.asList((Expression)new NT(TestEnvironmentTypes.Int), (Expression)new Seq(Arrays.asList((Expression)new CharExpr('('), (Expression)new NT(TestEnvironmentTypes.Sum), (Expression)new CharExpr(')')))))));
    env1.put(TestEnvironmentTypes.Prod, new Nonterminal(NonterminalMode.NORMAL, new Seq(Arrays.asList((Expression)new NT(TestEnvironmentTypes.Unary), new Star(new Seq(Arrays.asList((Expression)new CharClass(Arrays.asList((List<String>)Arrays.asList("*", "*"), (List<String>)Arrays.asList("/", "/"))), (Expression)new NT(TestEnvironmentTypes.Unary))))))));
    env1.put(TestEnvironmentTypes.Sum, new Nonterminal(NonterminalMode.NORMAL, new Seq(Arrays.asList((Expression)new NT(TestEnvironmentTypes.Prod), new Star(new Seq(Arrays.asList((Expression)new CharClass(Arrays.asList((List<String>)Arrays.asList("+", "+"), (List<String>)Arrays.asList("-", "-"))), (Expression)new NT(TestEnvironmentTypes.Prod))))))));

    // voided expressions
    env1.put(TestEnvironmentTypes.V1, new Nonterminal(NonterminalMode.NORMAL, new VoidExpr(new CharExpr('a'))));
    env1.put(TestEnvironmentTypes.V2, new Nonterminal(NonterminalMode.NORMAL, new VoidExpr(new Seq(Arrays.asList((Expression)new CharExpr('a'), (Expression)new CharExpr('b'))))));
    env1.put(TestEnvironmentTypes.V3, new Nonterminal(NonterminalMode.NORMAL, new Seq(Arrays.asList((Expression)new CharExpr('a'), (Expression)new VoidExpr(new CharExpr('b')), (Expression)new CharExpr('c')))));

    env1.put(TestEnvironmentTypes.WS, new Nonterminal(NonterminalMode.VOIDING, new Star(new Alt(Arrays.asList((Expression)new CharExpr(' '), (Expression)new CharExpr('\t'), (Expression)new CharExpr('\n'), (Expression)new CharExpr('\r'))))));

    env1.put(TestEnvironmentTypes.Nums, new Nonterminal(NonterminalMode.PRUNING, new Opt(new Seq(Arrays.asList((Expression)new NT(TestEnvironmentTypes.Int), (Expression)new Star(new Seq(Arrays.asList((Expression)new NT(TestEnvironmentTypes.WS), (Expression)new VoidExpr(new CharExpr(',')), (Expression)new NT(TestEnvironmentTypes.WS), (Expression)new NT(TestEnvironmentTypes.Int)))))))));

    env1.put(TestEnvironmentTypes.lc, new Nonterminal(NonterminalMode.VOIDING, new Star(new Alt(Arrays.asList((Expression)new CharExpr('a'), (Expression)new CharExpr('\t'), (Expression)new CharExpr('\n'), (Expression)new CharExpr('\r'))))));

    String fixtureSource = new String(Files.readAllBytes(Paths.get("src/test-fixtures.json")));
    JsonArray tests = Json.parse(fixtureSource).asArray();
    int i = 0;
    for (JsonValue test : tests) {
      if (test.isArray()) {
        runTest(i, test.toString(), (JsonArray)test);
      }
      i++;
    }
  }

  private void runTest (int testNo, String spec, JsonArray test) {
    String testType = test.get(0).asString();

    if (testType.equals("Comment")) {
      return;
    }

    // Get result of running expression on input
    ParseResult testValue = getTestValue((JsonArray)test.get(1));
    ParseResult expectedValue = getExpectedValue((JsonArray)test.get(2));
    if (testType.equals("MatchError")) {
      if (!testValue.toString().equals(expectedValue.toString())) {
        System.out.println("Test #" + testNo + " failed");
        System.out.println(spec);
        System.out.println("Test value:");
        System.out.println(testType);
        System.out.println(testValue);
        System.out.println(expectedValue);
        System.exit(-1);
      }
    } else if (testType.equals("MatchAST")) {
      if (!testValue.equals(expectedValue)) {
        System.out.println("Test #" + testNo + " failed");
        System.out.println(spec);
        System.out.println("Test value:");
        System.out.println(testType);
        System.out.println(expectedValue.getAST().getChildren());
        System.out.println(expectedValue.getAST().getType());
        System.out.println(testValue);
        System.out.println(testValue.getAST());
        System.out.println(testValue.getAST().getChildren());
        System.out.println(testValue.getAST().getType());
        System.exit(-1);
      }
    }
  }

  private ParseResult getTestValue (JsonArray runConfig) {
    String testRunType = runConfig.get(0).asString();
    if (testRunType.equals("match")) {
      return (new TestEnvironmentParser(env1, TestEnvironmentTypes.valueOf(runConfig.get(1).asString()))).parse(runConfig.get(2).asString());
    }
    if (testRunType.equals("eval")) {
      Expression e = getExprFromArray((JsonArray)runConfig.get(1));
      HashMap<TestEnvironmentTypes, Nonterminal> env2 = new HashMap<TestEnvironmentTypes, Nonterminal>(env1);
      env2.put(TestEnvironmentTypes.S, new Nonterminal(NonterminalMode.VOIDING, e));
      return (new TestEnvironmentParser(env2, TestEnvironmentTypes.S)).parse(runConfig.get(2).asString());
    }
    return null;
  }

  private Expression getExprFromArray (JsonArray ruleConfig) {
    String exprType = ruleConfig.get(0).asString();

    if (exprType.equals("SEQ")) {
      ruleConfig.remove(0);
      return new Seq(toExprList(ruleConfig));
    }
    if (exprType.equals("NT")) {
      return new NT(TestEnvironmentTypes.valueOf(ruleConfig.get(1).asString()));
    }
    if (exprType.equals("OPT")) {
      return new Opt(getExprFromArray((JsonArray)ruleConfig.get(1)));
    }
    if (exprType.equals("ANY")) {
      return new Any();
    }
    if (exprType.equals("CHAR")) {
      return new CharExpr(ruleConfig.get(1).asString().charAt(0));
    }
    if (exprType.equals("CHAR_CLASS")) {
      ruleConfig.remove(0);
      return new CharClass(toCharClasses(ruleConfig));
    }
    if (exprType.equals("STAR")) {
      return new Star(getExprFromArray((JsonArray)ruleConfig.get(1)));
    }
    if (exprType.equals("PLUS")) {
      return new Plus(getExprFromArray((JsonArray)ruleConfig.get(1)));
    }
    if (exprType.equals("AND")) {
      return new And(getExprFromArray((JsonArray)ruleConfig.get(1)));
    }
    if (exprType.equals("NOT")) {
      return new Not(getExprFromArray((JsonArray)ruleConfig.get(1)));
    }
    if (exprType.equals("ALT")) {
      ruleConfig.remove(0);
      return new Alt(toExprList(ruleConfig));
    }

    System.out.println(exprType);
    throw new RuntimeException("Unknown rule type");
  }

  private List<List<String>> toCharClasses (JsonArray classes) {
    List<List<String>> lst = new ArrayList<List<String>>();

    for (JsonValue clazzCfg : classes) {
      JsonArray clazzAry = (JsonArray)clazzCfg;
      List<String> clazz = new ArrayList<String>();
      clazz.add(clazzAry.get(0).asString());
      clazz.add(clazzAry.get(1).asString());
      lst.add(clazz);
    }

    return lst;
  }

  private List<Expression> toExprList (JsonArray ruleConfig) {
    ArrayList<Expression> lst = new ArrayList<Expression>();
    for (JsonValue rule : ruleConfig) {
      lst.add(getExprFromArray((JsonArray)rule));
    }
    return lst;
  }

  private ParseResult getExpectedValue (JsonArray config) {
    String resultType = config.get(0).asString();
    if (resultType.equals("ParseError")) {
      int pos = config.get(1).asInt();
      int line = config.get(2).asInt();
      int col = config.get(3).asInt();
      return new ParseResult(null, new ParseError(pos, line, col, toStringList((JsonArray)config.get(4)), toErrList((JsonArray)config.get(5))));
    }
    if (resultType.equals("Tree")) {
      // TODO: not actually testing or comparing "pos" right now
      List<IAST<TestEnvironmentTypes>> children = new ArrayList<IAST<TestEnvironmentTypes>>();
      for (JsonValue child : config.get(2).asArray()) {
        children.add(getExpectedValue((JsonArray)child).getAST());
      }
      return new ParseResult(new AST<TestEnvironmentTypes>(TestEnvironmentTypes.valueOf(config.get(1).asString()), children, 0), null);
    }
    if (resultType.equals("Empty")) {
      return new ParseResult(new Empty<TestEnvironmentTypes>(TestEnvironmentTypes.valueOf(config.get(1).asString())), null);
    }
    if (resultType.equals("Char")) {
      return new ParseResult(new Char<TestEnvironmentTypes>(config.get(1).asString().charAt(0), TestEnvironmentTypes._Char), null);
    }

    // TODO
    return null;
  }

  private List<String> toStringList (JsonArray ary) {
    ArrayList<String> lst = new ArrayList<String>();

    for (JsonValue str : ary) {
      lst.add(str.asString());
    }

    return lst;
  }

  private List<ErrBase> toErrList (JsonArray ary) {
    ArrayList<ErrBase> lst = new ArrayList<ErrBase>();

    for (JsonValue e : ary) {
      JsonObject err = (JsonObject)e;
      String errType = err.get("type").asString();
      if (errType.equals("ErrAny")) {
        lst.add(new ErrAny());
      }
      if (errType.equals("ErrChar")) {
        lst.add(new ErrChar(err.get("arg").asString().charAt(0)));
      }
      if (errType.equals("ErrCC")) {
        List<List<String>> ccs = new ArrayList<List<String>>();
        for (JsonValue ccSpec : err.get("arg").asArray()) {
          List<String> cc = new ArrayList<String>();
          cc.add(((JsonArray)ccSpec).get(0).asString());
          cc.add(((JsonArray)ccSpec).get(1).asString());
          ccs.add(cc);
        }
        lst.add(new ErrCC(ccs));
      }
    }

    return lst;
  }
}
