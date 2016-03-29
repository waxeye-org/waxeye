/*
# Waxeye Parser Generator - test
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.
*/

var Waxeye = require('../../src/javascript/waxeye.js');
var WaxeyeParser = Waxeye.WaxeyeParser;
var assert = require('assert');
var exec = require('child_process').execSync;
var fs = require('fs');

var AST = Waxeye.AST;

var testFixtures = require('../test-fixtures.json');

function rest (a) {
  return Array.prototype.slice.call(a, 1);
}

function getEnv () {
  exec('racket ./src/waxeye/waxeye.scm -g javascript . -p test ./test/grammars/env1.waxeye');
  var TestParser = require('../../test_parser').TestParser;
  var parser = new TestParser;
  fs.unlink('./test_parser.js');

  return parser.env;
}

var env1 = getEnv();

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
    return expectation[0];
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
