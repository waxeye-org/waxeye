import * as waxeye from 'waxeye';

export class TestEnv {
  constructor(public env: waxeye.ParserConfig) {}

  public getTestOutput(spec: ['match', string, string]|['eval', any[], string]):
      waxeye.AST|waxeye.ParseError {
    const [runType, data, input] = spec;
    if (runType === 'match') {
      return this.match(data as string, input);
    }
    if (runType === 'eval') {
      return this.testEval(this.buildRule(data as any[]), input);
    }
    throw new Error('Unsupported runType ' + JSON.stringify(spec));
  }

  public buildRule(rule: any[]): waxeye.Expr {
    const ruleType = exprTypeFromName(rule[0]);
    if (ruleType === waxeye.ExprType.CHAR_CLASS) {
      return {
        type: ruleType,
        args: fromFixtureExpectationCharClasses(rule.slice(1)),
      };
    }
    return {
      type: ruleType,
      args: rule.slice(1).map((r) => {
        if (Array.isArray(r)) {
          return this.buildRule(r);
        }
        return r;
      }),
    } as waxeye.Expr;
  }

  public testEval(rule: waxeye.Expr, input: string) {
    const env = JSON.parse(JSON.stringify(this.env));
    env.S = {mode: waxeye.NonTerminalMode.VOIDING, exp: rule};
    return (new waxeye.WaxeyeParser(env, 'S')).parse(input);
  }

  public match(nt: string, input: string) {
    return (new waxeye.WaxeyeParser(this.env, nt)).parse(input);
  }
}

export function fixtureExpectationToOutput(expectation: any[]) {
  const expType = expectation[0] as string;
  if (expType === 'ParseError') {
    return new waxeye.ParseError(
        expectation[1], expectation[2], expectation[3], expectation[4],
        fromFixtureExpectationErrChars(expectation[5]));
  }
  if (expType === 'Tree') {
    return new waxeye.AST(
        expectation[1], expectation[2].map(fixtureExpectationToOutput));
  }
  if (expType === 'Char') {
    return expectation[1];
  }
  if (expType === 'Empty') {
    return waxeye.EmptyAST();
  }
  console.log(expectation);
  throw new Error('Unsupported: ' + expectation);
}

export function outputToFixtureExpectation(node: waxeye.AST|waxeye.ParseError|
                                           string): any[] {
  if (node instanceof waxeye.ParseError) {
    return [
      'ParseError',
      node.pos,
      node.line,
      node.col,
      node.nt,
      toFixtureExpectationErrChars(node.chars),
    ];
  }
  if (node instanceof waxeye.AST) {
    if (node.isEmpty()) {
      return ['Empty'];
    } else {
      return ['Tree', node.type, node.children.map(outputToFixtureExpectation)];
    }
  }
  if (typeof node === 'string') {
    return ['Char', node];
  }
  console.log(node);
  throw new Error('Unsupported: ' + node);
}

// We use a const enum for waxeye.ExprType, so we manually create this mapping.
const NAME_TO_EXPR_TYPE: {[key: string]: waxeye.ExprType} = {
  NT: waxeye.ExprType.NT,
  ALT: waxeye.ExprType.ALT,
  SEQ: waxeye.ExprType.SEQ,
  PLUS: waxeye.ExprType.PLUS,
  STAR: waxeye.ExprType.STAR,
  OPT: waxeye.ExprType.OPT,
  AND: waxeye.ExprType.AND,
  NOT: waxeye.ExprType.NOT,
  VOID: waxeye.ExprType.VOID,
  ANY: waxeye.ExprType.ANY,
  CHAR: waxeye.ExprType.CHAR,
  CHAR_CLASS: waxeye.ExprType.CHAR_CLASS,
};
function exprTypeFromName(name: string): waxeye.ExprType {
  const result = NAME_TO_EXPR_TYPE[name];
  if (!result) {
    throw new Error(`Unknown ExprType ${name}`);
  }
  return result;
}

function fromFixtureExpectationCharClasses(charClasses: string): number;
function fromFixtureExpectationCharClasses(charClasses: [string, string]):
    [number, number];
function fromFixtureExpectationCharClasses(
    charClasses: Array<string|[string, string]>):
    Array<number|[number, number]>;
function fromFixtureExpectationCharClasses(charClasses: any): any {
  if (typeof charClasses === 'string') {
    return charClasses.codePointAt(0) as number;
  } else {
    return charClasses.map(
               fromFixtureExpectationCharClasses) as [number, number];
  }
}

function toFixtureExpectationCharClasses(charClasses: number): string;
function toFixtureExpectationCharClasses(charClasses: [number, number]):
    [string, string];
function toFixtureExpectationCharClasses(
    charClasses: Array<number|[number, number]>):
    Array<string|[string, string]>;
function toFixtureExpectationCharClasses(charClasses: any): any {
  if (typeof charClasses === 'number') {
    return String.fromCodePoint(charClasses);
  } else {
    return charClasses.map(toFixtureExpectationCharClasses);
  }
}

interface ErrChar {
  type: 'ErrChar';
  arg: string;
}
interface ErrCC {
  type: 'ErrCC';
  arg: Array<string|[string, string]>;
}
interface ErrAny {
  type: 'ErrAny';
}
function fromFixtureExpectationErrChars(errs: Array<ErrChar|ErrCC|ErrAny>):
    Array<waxeye.ErrChar|waxeye.ErrCC|waxeye.ErrAny> {
  return errs.map((err) => {
    switch (err.type) {
      case 'ErrChar':
        return new waxeye.ErrChar(err.arg);
      case 'ErrCC':
        return new waxeye.ErrCC(fromFixtureExpectationCharClasses(err.arg));
      case 'ErrAny':
        return new waxeye.ErrAny();
      default:
        throw new Error(`Unsupported ${err}`);
    }
  });
}

function toFixtureExpectationErrChars(
    errs: Array<waxeye.ErrChar|waxeye.ErrCC|waxeye.ErrAny>):
    Array<ErrChar|ErrCC|ErrAny> {
  return errs.map((err) => {
    if (err instanceof waxeye.ErrChar) {
      return {type: 'ErrChar', arg: err.char} as ErrChar;
    }
    if (err instanceof waxeye.ErrCC) {
      return {
        type: 'ErrCC',
        arg: toFixtureExpectationCharClasses(err.charClasses),
      } as ErrCC;
    }
    if (err instanceof waxeye.ErrAny) {
      return {type: 'ErrAny'} as ErrAny;
    }
    throw new Error(`Unsupported: ${err}`);
  });
}
