const waxeye = require('../../src/javascript/waxeye.js');

function TestEnv(env) {
  this.env = env;
}

TestEnv.prototype.getTestOutput = function(spec) {
  const runType = spec[0];
  const data = spec[1];
  const input = spec[2];
  if (runType === 'match') {
    return this.match(data, input);
  }
  if (runType === 'eval') {
    return this.testEval(this.buildRule(data), input);
  }
  throw new Error("Unsupported runType " + JSON.stringify(spec));
};

TestEnv.prototype.buildRule = function(rule) {
  if (!waxeye.Exp[rule[0]]) {
    return rule;
  }
  const ruleType = rule[0];
  if (ruleType === 'CHAR_CLASS') {
    rule = [ruleType].concat(fromFixtureExpectationCharClasses(rule.slice(1)));
  }
  rule = rule.map(function(r) {
    if (Array.isArray(r)) {
      return this.buildRule(r);
    }
    return r;
  }, this);
  return waxeye.Exp[ruleType].apply(null, rule.slice(1));
};

TestEnv.prototype.testEval = function(rule, input) {
  const env = JSON.parse(JSON.stringify(this.env));
  env["S"] = waxeye.nonterminal(waxeye.Modes.VOIDING, rule);
  return (new waxeye.WaxeyeParser(env, "S")).parse(input);
};

TestEnv.prototype.match = function(nt, input) {
  return (new waxeye.WaxeyeParser(this.env, nt)).parse(input);
};

function fromFixtureExpectationCharClasses(charClasses) {
  if (typeof charClasses === 'string') {
    return charClasses.codePointAt(0);
  } else {
    return charClasses.map(fromFixtureExpectationCharClasses);
  }
}

function toFixtureExpectationCharClasses(charClasses) {
  if (typeof charClasses === 'number') {
    return String.fromCodePoint(charClasses);
  } else {
    return charClasses.map(toFixtureExpectationCharClasses);
  }
}

function fromFixtureExpectationErrChars(errs) {
  return errs.map(function(err) {
    if (err.type === 'ErrChar') {
      return new waxeye.ErrChar(err.arg);
    }
    if (err.type === 'ErrCC') {
      return new waxeye.ErrCC(fromFixtureExpectationCharClasses(err.arg));
    }
    if (err.type === 'ErrAny') {
      return new waxeye.ErrAny();
    }
    console.log(err);
    throw new Error('Unsupported' + err);
  });
}

function toFixtureExpectationErrChars(errs) {
  return errs.map(function(err) {
    if (err instanceof waxeye.ErrChar) {
      return {type: 'ErrChar', arg: err.char};
    }
    if (err instanceof waxeye.ErrCC) {
      return {type: 'ErrCC', arg: toFixtureExpectationCharClasses(err.charClasses)};
    }
    if (err instanceof waxeye.ErrAny) {
      return {type: 'ErrAny'};
    }
    console.log(err);
    throw new Error('Unsupported: ' + err);
  });
}

function fixtureExpectationToOutput(expectation) {
  const expType = expectation[0];
  if (expType === 'ParseError') {
    return new waxeye.ParseError(expectation[1], expectation[2], expectation[3], expectation[4],
        fromFixtureExpectationErrChars(expectation[5]));
  }
  if (expType === 'Tree') {
    return waxeye.AST.TREE(expectation[1], expectation[2].map(fixtureExpectationToOutput));
  }
  if (expType === 'Char') {
    return expectation[1];
  }
  if (expType === 'Empty') {
    return waxeye.AST.EMPTY();
  }
  console.log(expectation);
  throw new Error('Unsupported: ' + expectation);
}

function outputToFixtureExpectation(node) {
  if (node instanceof waxeye.ParseError) {
    return ['ParseError', node.pos, node.line, node.col, node.nt, toFixtureExpectationErrChars(node.chars)]
  }
  if (node instanceof waxeye.AST) {
    if (node.form === 'TREE') {
      return ['Tree', node.type, node.children.map(outputToFixtureExpectation)]
    } else if (node.form === 'EMPTY') {
      return ['Empty'];
    }
  }
  if (typeof node === 'string') {
    return ['Char', node];
  }
  console.log(node);
  throw new Error('Unsupported: ' + node);
}

module.exports = {
  TestEnv: TestEnv,
  // A bi-directional transformation between AST and fixture AST representation.
  fixtureExpectationToOutput: fixtureExpectationToOutput,
  outputToFixtureExpectation: outputToFixtureExpectation
};
