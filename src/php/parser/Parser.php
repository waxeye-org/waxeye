<?php


namespace parser;


use ast\AST;
use ast\EmptyAST;
use ast\IAST;
use ast\IASTs;
use parser\error\ParseError;
use parser\error\ParseErrors;
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
}
