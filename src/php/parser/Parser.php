<?php


namespace parser;


use ast\AST;
use ast\IAST;
use ast\IASTs;
use RuntimeException;

class Parser
{
    private FAs $fas;
    private FAStack $faStack;
    private string $input;
    private int $inputPosition;

    public function __construct(FAs $fas)
    {
        $this->faStack = new FAStack();

        $this->fas = $fas;
    }

    public function parse(string $input)
    {
        $this->input = $input;
        $this->inputPosition = 0;
        $result = $this->matchAutomaton();

        if ($result && $this->inputPosition < strlen($this->input)) {
            throw new RuntimeException("Could not parse remaining input " . substr($this->input, $this->inputPosition));
        }

        return $result;
    }

    private function matchAutomaton(int $index = 0)
    {
        $startPosition = $this->inputPosition;
        $automaton = $this->fas[$index];


        $this->faStack->push($automaton);
        $result = $this->matchState();
        $this->faStack->pop();

        switch ($automaton->getMode()) {
            default:
            {
                if ($result->getType() != "EMPTY") {
                    $value = new AST($automaton->getType(), $startPosition, new IASTs($result));
                } else {
                    $value = new AST($automaton->getType(), $startPosition, $result->getChildren());
                }
            }
        }

        return $value;
    }

    private function matchState(int $stateIndex = 0): ?IAST
    {
        $state = $this->faStack->top()->getStates()[$stateIndex];
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
            $result = $this->matchEdge($edges[$edgeIndex]);

            if ($result) {
                return $result;
            } else {
                return $this->matchEdges($edges, $edgeIndex + 1);
            }
        } else {
            return null;
        }
    }

    private function matchEdge(Edge $edge): ?IAST
    {
        $transition = $edge->getTransition();

        if ($transition instanceof AutomatonTransition) {
            $result = $this->matchAutomaton($transition->getIndex());
        } else if ($transition instanceof CharTransition) {
            $result = $edge->getTransition()->visitTransition($this->input, $this->inputPosition);
            if ($result) {
                $this->inputPosition++;
            }
        } else {
            throw new RuntimeException("Unsupported transition type: " . get_class($transition));
        }

        if (null == $result) {
            return $result;
        } else {
            $tranRes = $this->matchState($edge->getState());

            if ($tranRes) {
                if ($edge->isVoided() || $result->getType() === "EMPTY") {
                    return $tranRes;
                } else {
                    $tranRes->setChildren($this->addAtBegin($result, $tranRes->getChildren()));

                    return $tranRes;
                }
            } else {
                // TODO restore position
                return null;
            }
        }
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
}
