/*
# Waxeye Parser Generator - test
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.
*/

const assert = require('assert');
const testLib = require('./lib');
const testEnv = new testLib.TestEnv((new (require('../../tmp/js/test_parser').TestParser)).env);
const testFixtures = require('./test-fixtures.json');


testFixtures.map(function (test, i) {
  if (test[0] === "Comment") {
    return;
  }

  var testType = test[0]
      , testOutput = testEnv.getTestOutput(test[1])
      , testExpectation = testLib.fixtureExpectationToOutput(test[2])
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
    console.log("Fixture", i + 1, "failed: ", JSON.stringify(test, null, 2));
    console.log("Expected STR:", testExpectationStr);
    console.log("Actual STR:", testOutputStr);
    console.log("Expected:", JSON.stringify(testExpectation, null, 2));
    console.log("Actual:", JSON.stringify(testOutput, null, 2));
    assert.deepEqual(testOutputStr, testExpectationStr);
  }
});
