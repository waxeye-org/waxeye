/* Updates test fixtures using the current version of the parser.

 Run with:
 test/javascript/bin/update-test-fixtures
 */

import * as fs from 'fs';

import {TestParser} from '../../../tmp/ts/test_parser';
import {outputToFixtureExpectation, TestEnv} from '../lib';

const testEnv = new TestEnv(new (TestParser as any)().env);

const path = 'test/javascript/test-fixtures.json';
fs.writeFileSync(
    path,
    `[\n${
        JSON.parse(fs.readFileSync(path, {encoding: 'utf8'}))
            .map((test: any[]) => {
              if (test[0] === 'Comment') {
                return test;
              }
              return [
                test[0],
                test[1],
                outputToFixtureExpectation(testEnv.getTestOutput(test[1])),
              ];
            })
            .map((test: any[]) => `  ${JSON.stringify(test)}`)
            .join(',\n')}\n]\n`,
    {encoding: 'utf8'});
