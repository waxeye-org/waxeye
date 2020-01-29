// Test fixture (de-)serialization and matching.
import * as waxeye from 'waxeye';

export type FixtureOrComment = Fixture|['Comment', string];
export type Fixture = ['MatchAST', Input, Node]|['MatchError', Input, Error];

// Test input
export type Input = MatchTestInput|ExprTestInput;
export type InputText = string;

// Match non-terminal of the test grammar.
export type MatchTestInput = ['match', NonTerminalName, InputText];
export type NonTerminalName = string;

// Match the input text against the given (dynamically-constructed) grammar.
export type Expr = any[];
export type ExprTestInput = ['eval', Expr, InputText];

// Expected AST
export type Node = NonTerminalNode|CharNode|EmptyNode;
export type EmptyNode = ['Empty', /*start*/ number, /*end*/ number];
export type NonTerminalNode =
    ['Tree', NonTerminalName, Node[], /*start*/ number, /*end*/ number];
export type CharNode = ['Char', /*char value*/ string];

// Expected error
export type Error = [
  'ParseError', /*pos*/ number, /*line*/ number, /*col*/ number,
  NonTerminalName[], Array<ErrChar|ErrCC|ErrAny>
];
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

export class TestEnv {
  constructor(private config: waxeye.ParserConfig) {}

  public getTestOutput(input: Input): waxeye.AST|waxeye.ParseError {
    switch (input[0]) {
      case 'match': {
        const nt = input[1];
        const text = input[2];
        return this.match(nt, text);
      }
      case 'eval': {
        const expr = input[1];
        const text = input[2];
        return this.testEval(this.buildRule(expr), text);
      }
      default:
        throw new Error('Unsupported runType ' + JSON.stringify(input));
    }
  }

  public buildRule(rule: Expr): waxeye.Expr {
    const ruleType = exprTypeFromName(rule[0]);
    switch (ruleType) {
      case waxeye.ExprType.NT:
        return {type: ruleType, name: rule[1]};
      case waxeye.ExprType.ALT:
      case waxeye.ExprType.SEQ:
        return {
          type: ruleType,
          exprs: rule.slice(1).map((r) => this.buildRule(r)),
        } as waxeye.Expr;
      case waxeye.ExprType.CHAR:
        return {type: ruleType, char: rule[1]};
      case waxeye.ExprType.CHAR_CLASS:
        return {
          type: ruleType,
          codepoints: fromFixtureExpectationCharClasses(rule.slice(1)),
        };
      case waxeye.ExprType.PLUS:
      case waxeye.ExprType.STAR:
      case waxeye.ExprType.OPT:
      case waxeye.ExprType.AND:
      case waxeye.ExprType.NOT:
      case waxeye.ExprType.VOID:
        return {type: ruleType, expr: this.buildRule(rule[1])} as waxeye.Expr;
      case waxeye.ExprType.ANY_CHAR:
        return {type: ruleType};
      default:
        // tslint:disable-next-line:no-unused-variable
        const exhaustive: never = ruleType;
        throw new Error(`Invalid rule type in rule: ${JSON.stringify(rule)}`);
    }
  }

  public testEval(rule: waxeye.Expr, input: string) {
    const config = Object.assign(
        {}, this.config,
        {S: {mode: waxeye.NonTerminalMode.VOIDING, exp: rule}});
    return (new waxeye.WaxeyeParser(config, 'S')).parse(input);
  }

  public match(nt: string, input: string) {
    return (new waxeye.WaxeyeParser(this.config, nt)).parse(input);
  }
}

export function deserializeExpectation(char: CharNode): string;
export function deserializeExpectation(node: NonTerminalNode|
                                       EmptyNode): waxeye.AST;
export function deserializeExpectation(charOrNode: Node): waxeye.AST|string;
export function deserializeExpectation(error: Error): waxeye.ParseError;
export function deserializeExpectation(expectation: Node|Error):
    waxeye.AST|waxeye.ParseError|string;
export function deserializeExpectation(expectation: Node|Error):
    waxeye.AST|waxeye.ParseError|string {
  switch (expectation[0]) {
    case 'ParseError':
      return new waxeye.ParseError(
          expectation[1], expectation[2], expectation[3], expectation[4],
          fromFixtureExpectationErrChars(expectation[5]));
    case 'Tree':
      return new waxeye.AST(
          expectation[1],
          expectation[2].map((child) => deserializeExpectation(child)),
          expectation[3], expectation[4]);
    case 'Char':
      return expectation[1];
    case 'Empty':
      return waxeye.EmptyAST(expectation[1], expectation[2]);
    default:
      console.log(expectation);
      throw new Error('Unsupported: ' + expectation);
  }
}

export function serializeExpectation(char: string): CharNode;
export function serializeExpectation(node: waxeye.AST): NonTerminalNode|
    EmptyNode;
export function serializeExpectation(charOrNode: string|waxeye.AST): Node;
export function serializeExpectation(error: waxeye.ParseError): Error;
export function serializeExpectation(result: waxeye.AST|waxeye.ParseError|
                                     string): Node|Error;
export function serializeExpectation(result: waxeye.AST|waxeye.ParseError|
                                     string): Node|Error {
  if (result instanceof waxeye.ParseError) {
    return [
      'ParseError',
      result.pos,
      result.line,
      result.col,
      result.nt,
      toFixtureExpectationErrChars(result.chars),
    ];
  }
  if (result instanceof waxeye.AST) {
    if (result.isEmpty()) {
      return ['Empty', result.start, result.end];
    } else {
      const children =
          result.children.map((child) => serializeExpectation(child));
      return ['Tree', result.type, children, result.start, result.end];
    }
  }
  if (typeof result === 'string') {
    return ['Char', result];
  }
  console.log(result);
  throw new Error('Unsupported: ' + result);
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
  ANY_CHAR: waxeye.ExprType.ANY_CHAR,
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
