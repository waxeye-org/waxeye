/*
# Waxeye Parser Generator - test
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.
*/

var Waxeye = require('./waxeye2.js');
var WaxeyeParser = Waxeye.WaxeyeParser;
var assert = require('assert');

var AST = Waxeye.AST;

var testFixtures = require('../test-fixtures.json');

function rest (a) {
  return Array.prototype.slice.call(a, 1);
}

var env1 = {
  // simple
  "A": Waxeye.nonterminal(Waxeye.Modes.NORMAL, Waxeye.Exp.CHAR("a")),
  "B": Waxeye.nonterminal(Waxeye.Modes.NORMAL, Waxeye.Exp.ALT(Waxeye.Exp.CHAR("b"), Waxeye.Exp.CHAR("B"))),

  // direct recursion
  "C": Waxeye.nonterminal(Waxeye.Modes.NORMAL, Waxeye.Exp.ALT(Waxeye.Exp.NT("A"), Waxeye.Exp.SEQ(Waxeye.Exp.CHAR("("), Waxeye.Exp.NT("C"), Waxeye.Exp.CHAR(")")))),

  // mutual recursion
  "Int": Waxeye.nonterminal(Waxeye.Modes.NORMAL, Waxeye.Exp.ALT(Waxeye.Exp.CHAR("0"), Waxeye.Exp.SEQ(Waxeye.Exp.CHAR_CLASS(["1", "9"]), Waxeye.Exp.STAR(Waxeye.Exp.CHAR_CLASS(["0", "9"]))))),
  "Unary": Waxeye.nonterminal(Waxeye.Modes.NORMAL, Waxeye.Exp.ALT(Waxeye.Exp.NT("Int"), Waxeye.Exp.SEQ(Waxeye.Exp.CHAR("("), Waxeye.Exp.NT("Sum"), Waxeye.Exp.CHAR(")")))),

  "Prod": Waxeye.nonterminal(Waxeye.Modes.NORMAL, Waxeye.Exp.SEQ(Waxeye.Exp.NT("Unary"), Waxeye.Exp.STAR(Waxeye.Exp.SEQ(Waxeye.Exp.CHAR_CLASS(["*", "*"], ["/", "/"]), Waxeye.Exp.NT("Unary"))))),
  "Sum": Waxeye.nonterminal(Waxeye.Modes.NORMAL, Waxeye.Exp.SEQ(Waxeye.Exp.NT("Prod"), Waxeye.Exp.STAR(Waxeye.Exp.SEQ(Waxeye.Exp.CHAR_CLASS(["+", "+"], ["-", "-"]), Waxeye.Exp.NT("Prod"))))),

  // voided expressions
  "V1": Waxeye.nonterminal(Waxeye.Modes.NORMAL, Waxeye.Exp.VOID(Waxeye.Exp.CHAR("a"))),
  "V2": Waxeye.nonterminal(Waxeye.Modes.NORMAL, Waxeye.Exp.VOID(Waxeye.Exp.SEQ(Waxeye.Exp.CHAR("a"), Waxeye.Exp.CHAR("b")))),
  "V3": Waxeye.nonterminal(Waxeye.Modes.NORMAL, Waxeye.Exp.SEQ(Waxeye.Exp.CHAR("a"), Waxeye.Exp.VOID(Waxeye.Exp.CHAR("b")), Waxeye.Exp.CHAR("c"))),

  "WS": Waxeye.nonterminal(Waxeye.Modes.VOIDING, Waxeye.Exp.STAR(Waxeye.Exp.ALT(Waxeye.Exp.CHAR(" "), Waxeye.Exp.CHAR("\t"), Waxeye.Exp.CHAR("\n"), Waxeye.Exp.CHAR("\r")))),

  "Nums": Waxeye.nonterminal(Waxeye.Modes.PRUNING, Waxeye.Exp.OPT(Waxeye.Exp.SEQ(Waxeye.Exp.NT("Int"),
    Waxeye.Exp.STAR(Waxeye.Exp.SEQ(Waxeye.Exp.NT("WS"), Waxeye.Exp.VOID(Waxeye.Exp.CHAR(",")), Waxeye.Exp.NT("WS"), Waxeye.Exp.NT("Int")))
  ))),

  "lc": Waxeye.nonterminal(Waxeye.Modes.VOIDING, Waxeye.Exp.STAR(Waxeye.Exp.ALT(Waxeye.Exp.CHAR("a"), Waxeye.Exp.CHAR("\t"), Waxeye.Exp.CHAR("\n"), Waxeye.Exp.CHAR("\r"))))
};

function testEval (env, rule, input) {
  env = JSON.parse(JSON.stringify(env));
  env["S"] = Waxeye.nonterminal(Waxeye.Modes.VOIDING, rule);
  return (new WaxeyeParser(env, "S")).parse(input);
}

function match (env, nt, input) {
  return (new WaxeyeParser(env, nt)).parse(input);
}

function parseErrChars (errs) {
  return errs.map(function (err) {
    if (err.type === "ErrChar") {
      return new Waxeye.ErrChar(err.arg);
    }
    if (err.type === "ErrCC") {
      return new Waxeye.ErrCC(err.arg);
    }
    if (err.type === "ErrAny") {
      return new Waxeye.ErrAny();
    }

    console.log(err);
    throw new Error("Unsupported");
  });
}

function buildTestExpectation (expectation) {
  var expType = expectation.shift();

  if (expType === "ParseError") {
    return new Waxeye.ParseError(expectation[0], expectation[1], expectation[2], expectation[3], parseErrChars(expectation[4]));
  }
  if (expType === "Tree") {
    return AST.TREE(expectation[0], expectation[1].map(buildTestExpectation));
  }
  if (expType === "Char") {
    return AST.CHAR(expectation[0]);
  }
  if (expType === "Empty") {
    return AST.EMPTY();
  }

  console.log(expectation);
  throw new Error("Unsupported");
}

function buildRule (rule) {
  if (!Waxeye.Exp[rule[0]]) {
    return rule;
  }
  var ruleType = rule[0];

  rule = rule.map(function (r) {
    if (Array.isArray(r)) {
      return buildRule(r);
    }
    return r;
  });
  return Waxeye.Exp[ruleType].apply(null, rest(rule));
}

function getTestOutput (spec) {
  var runType = spec.shift();
  if (runType === "match") {
    return match(env1, spec[0], spec[1]);
  }
  if (runType === "eval") {
    return testEval(env1, buildRule(spec[0]), spec[1]);
  }

  return testEval(env1, spec[0], spec[1]);
}

testFixtures.map(function (test, i) {
  if (test[0] === "Comment") {
    return;
  }

  var testType = test[0]
    , testOutput = getTestOutput(test[1])
    , testExpectation = buildTestExpectation(test[2])
    , testOutputStr = testOutput.toString()
    , testExpectationStr = testExpectation.toString()
    , matches;

  if (testType === "MatchError") {
    matches = (testOutputStr === testExpectationStr);
  }
  if (testType === "MatchAST") {
    matches = JSON.stringify(testOutput) === JSON.stringify(testExpectation);
  }

  if (!matches) {
    console.log("Fixture", i+1, "failed: ", JSON.stringify(test, null, 2));
    console.log("Expected STR:", testExpectationStr);
    console.log("Actual STR:", testOutputStr);
    console.log("Expected:", JSON.stringify(testExpectation, null, 2));
    console.log("Actual:", JSON.stringify(testOutput, null, 2));
    assert.deepEqual(testOutputStr, testExpectationStr);
  }
});
