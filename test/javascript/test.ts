/*
# Waxeye Parser Generator - test
# www.waxeye.org
# Copyright (C) 2008-2010 Orlando Hill
# Licensed under the MIT license. See 'LICENSE' for details.
*/

import * as assert from 'assert';
import {AST, ParseError} from 'waxeye';

import {TestParser as TestParserJS} from '../../tmp/js/test_parser';
import {TestParser as TestParserTS} from '../../tmp/ts/test_parser';

import {fixtureExpectationToOutput, TestEnv} from './lib';

/* tslint:disable */
const testFixtures = require('./test-fixtures.json');
/* tslint:enable */

const testEnvTS = new TestEnv(new TestParserTS().config);
const testEnvJS = new TestEnv(new (TestParserJS as any)().config);

function toString(
    testType: 'MatchError'|'MatchAST', data: AST|ParseError): string {
  switch (testType) {
    case 'MatchError':
      return data.toString();
    case 'MatchAST':
      return JSON.stringify(data);
    default:
      throw new Error(`Unknown test type ${testType}`);
  }
}

testFixtures.map((test: any[], i: number) => {
  if (test[0] === 'Comment') {
    return;
  }
  const testType = test[0];
  const testOutputTS = toString(testType, testEnvTS.getTestOutput(test[1]));
  const testOutputJS = toString(testType, testEnvJS.getTestOutput(test[1]));
  const testExpectation =
      toString(testType, fixtureExpectationToOutput(test[2]));
  assert.equal(
      testOutputTS, testOutputJS,
      `Fixture ${i + 1}: TypeScript output != JavaScript output`);
  assert.equal(
      testOutputTS, testExpectation,
      `Fixture ${i + 1} failed:
Expected: ${testExpectation}
  Actual: ${testOutputTS}
Test Fixture: ${JSON.stringify(test, null, 2)}`);
});
