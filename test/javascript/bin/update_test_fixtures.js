/** Updates test fixtures using the current version of the parser.

 Run with:
 test/javascript/bin/update-test-fixtures
 */

const fs = require('fs');
const testLib = require('../lib');
const testEnv = new testLib.TestEnv((new (require('../../../tmp/js/test_parser').TestParser)).env);

const path = 'test/javascript/test-fixtures.json';
fs.writeFileSync(
    path,
    "[\n" +
    JSON.parse(fs.readFileSync(path, {encoding: 'utf8'}))
        .map(function(test) {
          if (test[0] === "Comment") {
            return test;
          }
          return [test[0], test[1], testLib.outputToFixtureExpectation(testEnv.getTestOutput(test[1]))];
        })
        .map(function(test) {
          return "  " + JSON.stringify(test);
        })
        .join(",\n") +
    "\n]\n",
    {encoding: 'utf8'});

