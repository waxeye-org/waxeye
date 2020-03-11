<?php


namespace parser;


use ast\IASTs;
use parser\action\ActionType;
use parser\action\ApplyAction;
use parser\action\EvalAction;
use parser\config\Automata;
use parser\config\ParserConfig;
use parser\continuation\AltContinuation;
use parser\continuation\Continuations;
use parser\continuation\NonTerminalContinuation;
use parser\error\CharacterError;
use parser\error\MatchError;
use parser\error\ParseErrors;
use parser\error\RawError;
use parser\expression\Expression;
use parser\expression\Expressions;
use parser\expression\ExpressionType;
use parser\result\Accepted;
use parser\result\Rejected;
use RuntimeException;

class Parser
{
    private FAs $fas;
    private string $start;
    private FAStack $faStack;
    private string $input;
    private int $inputPosition;

    private int $line = 1;
    private int $column = 0;
    private bool $lastCR = false;

    private int $errorPosition = 0;
    private int $errorLine = 1;
    private int $errorColumn = 0;
    private string $errorNonTerminal;

    private ParseErrors $parseErrors;

    private Automata $automata;

    public function __construct(ParserConfig $parserConfig)
    {
        $this->automata = $parserConfig->getAutomata();
        $this->start = $parserConfig->getStart();
    }

    public function parse(string $input, string $start = null): string
    {
        $this->input = $input;

        if (null !== $start) {
            $this->start = $start;
        }

        if (!array_key_exists($this->start, $this->automata->getArrayCopy())) {
            $keys = join(", ", array_keys($this->automata->getArrayCopy()));
            throw new RuntimeException("Invalid non-terminal $this->start. Expected one of $keys!");
        }

        $this->match($input);

        return "x";
    }

    private function match(string $input)
    {
        $firstEval = $this->moveEval(new EvalAction(new Continuations(), $this->automata[$this->start]->getExpression(), 0, new IASTs(), new RawError(0, $this->start)));
        $action = $this->moveEval($firstEval);

        while (true) {
            switch ($action->getType()) {
                case ActionType::EVAL:
                {
                    $action = $this->moveEval($action);
                    printf("\tresult: %s\n", $action);
                    break;
                }
                case ActionType::APPLY:
                {
                    printf("\tresult: %s\n", $action);
                    return;
                    break;
                }
                default:
                {
                    $type = $action->getType();
                    throw new RuntimeException("Unsupported action type $type!");
                }
            }
        }
    }

    private function moveEval(EvalAction $action)
    {
        printf("evaluating action %s with input %s at position %s\n", $action, $this->input, $action->getPosition());
        $expression = $action->getExpression();
        $position = $action->getPosition();
        $asts = $action->getAsts();
        $error = $action->getError();
        $continuations = $action->getContinuations();
        $eof = $position >= strlen($this->input);

        switch ($expression->getType()) {
            case ExpressionType::NT:
            {
                $expression = Expression::asNonTerminalExpression($expression);
                $automaton = $this->automata[$expression->getName()];
                $cons = new Continuations(new NonTerminalContinuation($automaton->getMode(), $expression->getName(), $asts, $error->getCurrentNonTerminal(), $position));

                return new EvalAction($cons, $automaton->getExpression(), $position, $asts, $error);
            }
            case ExpressionType::ALT:
            {
                $expression = Expression::asAltExpression($expression);
                $expressions = $expression->getExpressions();

                if (0 === sizeof($expressions)) {

                } else {
                    $cons = new Continuations(new AltContinuation(Expressions::from(array_slice($expressions->getArrayCopy(), 1)), $position, $asts));
                    return new EvalAction($cons, $expressions[0], $position, $asts, $error);
                }
                break;
            }
            case ExpressionType::CHAR:
            {
                $expression = Expression::asCharExpression($expression);
                $char = $expression->getChar();

                if (1 === strlen($char)) {
                    if ($eof || $char !== $this->input[$position]) {
                        $matchResult = Rejected($this->updateError($error, $position, new CharacterError($char)));
                    } else {
                        $matchResult = new Accepted($position + 1, $asts, $error);
                    }
                } elseif (2 === strlen($char)) {
                    if (($position + 1) >= strlen($this->input) || $char[0] !== $this->input[$position] || $char[1] !== $this->input[$position + 1]) {
                        $matchResult = Rejected($this->updateError($error, $position, new CharacterError($char)));
                    } else {
                        $matchResult = new Accepted($position + 2, $asts, $error);
                    }
                } else {
                    $length = strlen($char);
                    throw new RuntimeException("Unsupported char length ($length) of character $char.");
                }

                return new ApplyAction($continuations, $matchResult);

                break;
            }
            default:
            {
                $type = $expression->getType();
                throw new RuntimeException("Unsupported expression type $type!");
            }
        }
    }

    private function updateError(RawError $rawError, int $position, MatchError $matchError): RawError
    {
        if (null === $rawError) {
            return new RawError(0, "");
        } else {
            if ($position > $rawError->getPosition()) {
                // TODO: return new RawError(pos, cons(err.currentNT, empty()), cons(e, empty()), err.currentNT);
                return new RawError($position, $rawError->getCurrentNonTerminal());
            } elseif ($position === $rawError->getPosition()) {
                // TODO:  return new RawError(err.pos, cons(err.currentNT, err.nonterminals), cons(e, err.failedChars), err.currentNT);
                return new RawError($position, $rawError->getCurrentNonTerminal());
            } else {
                // TODO: return new RawError(err.pos, err.nonterminals, err.failedChars, err.currentNT);
                return new RawError($position, $rawError->getCurrentNonTerminal());
            }
        }
    }

    private function evalNext(Expression $expression, int $position, IASTs $asts, RawError $error, Continuations $continuations): EvalAction
    {
        return new EvalAction($continuations, $expression, $position, $asts, $error);
    }

    /*
    public function __construct(FAs $fas, string $start)
    {
        $this->faStack = new FAStack();
        $this->parseErrors = new ParseErrors();
        $this->fas = $fas;
        $this->start = $start;
        $this->errorNonTerminal = $fas[0]->getType();
    }

    public function parse(string $input, bool $eofCheck = true): ParseResult
    {
        $this->input = $input;
        $this->inputPosition = 0;

        $result = $this->matchAutomaton();

        // TODO: apply parser errors
        if (null === $result) {
            $this->parseErrors[] = new ParseError($this->errorPosition, $this->errorLine, $this->errorColumn, $this->errorNonTerminal);
        } else if ($eofCheck && $this->inputPosition < strlen($input)) {
            $this->parseErrors[] = new ParseError($this->errorPosition, $this->errorLine, $this->errorColumn, $this->errorNonTerminal);
            $result = null;
        }

        return new ParseResult($result, $this->parseErrors);
    }

    private function matchAutomaton(int $index = 0): ?IAST
    {
        $startPosition = $this->inputPosition;

        $startLine = $this->line;
        $startColumn = $this->column;
        $startCR = $this->lastCR;

        $automaton = $this->fas[$index];
        $mode = $automaton->getMode();
        $type = $automaton->getType();


        $this->faStack->push($automaton);
        printf("matching automaton %s: %s\n", $index, $automaton);
        $result = $this->matchState();
        $this->faStack->pop();

        if (null === $result) {
            $this->updateError();
            return null;
        } else {
            switch ($automaton->getMode()) {
                case FA::VOID:
                {
                    $value = new EmptyAST($startPosition);
                    break;
                }
                case FA::PRUNE:
                {
                    $value = new AST($automaton->getType(), $startPosition, new IASTs());
                    break;
                }
                default:
                {
                    if ($result->getType() != "EMPTY") {
                        $value = new AST($automaton->getType(), $startPosition, new IASTs($result));
                    } else {
                        $value = new AST($automaton->getType(), $startPosition, $result->getChildren());
                    }
                    break;
                }
            }

            return $value;
        }
    }

    private function matchState(int $stateIndex = 0): ?IAST
    {
        $state = $this->faStack->top()->getStates()[$stateIndex];
        printf("\tmatching state %s\n", $state);
        $result = $this->matchEdges($state->getEdges());

        if (null == $result && $state->isMatch()) {
            return new AST("EMPTY", $this->inputPosition, new IASTs());
        } else {
            return $result;
        }
    }

    private function matchEdges(Edges $edges, int $edgeIndex = 0): ?IAST
    {
        if ($edgeIndex < $edges->count()) {
            printf("\t\tmatching edge %s with input %s at position %s\n", $edges[$edgeIndex], $this->input, $this->inputPosition);
            $result = $this->matchEdge($edges[$edgeIndex]);

            if (null === $result) {
                return $this->matchEdges($edges, $edgeIndex + 1);
            } else {
                return $result;
            }
        } else {
            return null;
        }
    }

    private function matchEdge(Edge $edge): ?IAST
    {
        $transition = $edge->getTransition();
        $startPosition = $this->inputPosition;
        $startLine = $this->line;
        $startColumn = $this->column;
        $startCR = $this->lastCR;
        printf("\t\t\tvisiting transition %s\n", $transition);

        if ($transition instanceof AutomatonTransition) {
            $result = $this->visitAutomatonTransition($transition);
        } else if ($transition instanceof CharTransition) {
            $result = $this->visitCharTransition($transition);
        } else if ($transition instanceof WildcardTransition) {
            $result = $this->visitWildcardTransition($transition);
        } else {
            throw new RuntimeException("Unsupported transition type: " . get_class($transition));
        }

        if (null === $result) {
            return $result;
        } else {
            $tranRes = $this->matchState($edge->getState());

            if (null !== $tranRes) {
                if ($edge->isVoided() || $result->getType() === "EMPTY") {
                    return $tranRes;
                } else {
                    $tranRes->setChildren($this->addAtBegin($result, $tranRes->getChildren()));

                    return $tranRes;
                }
            } else {
                $this->restorePosition($startPosition, $startLine, $startColumn, $startCR);
                return null;
            }
        }
    }

    private function visitCharTransition(CharTransition $transition): ?IAST
    {
        $result = $transition->visitTransition($this->input, $this->inputPosition);
        if ($result) {
            $this->inputPosition++;
        }

        return $result;
    }

    private function visitAutomatonTransition(AutomatonTransition $transition): ?IAST
    {
        return $this->matchAutomaton($transition->getIndex());
    }

    private function visitWildcardTransition(WildcardTransition $transition): ?IAST
    {
        $result = $transition->visitTransition($this->input, $this->inputPosition);
        if ($result) {
            $this->inputPosition++;
        }

        return $result;
    }

    private function addAtBegin(IAST $iast, IASTs $iasts): IASTs
    {
        $val = new IASTs();
        $val[] = $iast;
        foreach ($iasts as $current) {
            $val[] = $current;
        }

        return $val;
    }

    private function getLineCol(int $position, string $input): array
    {
        $lineNumber = 1;
        $lineStartPos = 0;
        $newLinePos = strpos($input, "\n", $lineStartPos);

        while ($newLinePos !== false && $newLinePos < $this->inputPosition) {
            $lineNumber++;
            $lineStartPos = $newLinePos + 1;

            $newLinePos = strpos($input, "\n", $lineStartPos);
        }

        return array("line" => $lineNumber, "col" => ($this->inputPosition - $lineStartPos + 1));
    }

    private function restorePosition(int $position, int $line, int $column, bool $cr)
    {
        $this->inputPosition = $position;
        $this->line = $line;
        $this->column = $column;
        $this->lastCR = $cr;
    }

    private function updateError()
    {
        if ($this->errorPosition < $this->inputPosition) {
            $this->errorPosition = $this->inputPosition;
            $this->errorLine = $this->line;
            $this->errorColumn = $this->column;
            $this->errorNonTerminal = $this->faStack->top()->getType();
        }
    }
    */
}
