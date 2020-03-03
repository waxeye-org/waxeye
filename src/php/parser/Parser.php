<?php


namespace parser;


use ast\AST;
use RuntimeException;
use SplDoublyLinkedList;

class Parser
{
    private SplDoublyLinkedList $fas;
    private AST $ast;

    public function __construct(SplDoublyLinkedList $fas)
    {
        $this->fas = $fas;
        $this->ast = new AST($this->fas[0]->getType(), 0, new SplDoublyLinkedList());
    }

    public function parse(string $input)
    {
        $this->matchAutomaton($input);

        printf("%s\n", $this->ast);
    }

    private function matchAutomaton($input, int $position = 0, int $index = 0)
    {
        $fa = $this->fas->offsetGet($index);

        printf("matching automaton at index %s: %s\n", $index, $fa);

        $result = $this->matchStates($input, $fa);

        if ($result) {
            printf("matched automaton %s\n", $fa);
        }

        printf("result: %s\n", $result);
    }

    private function matchStates(string $input, FA $automaton, int $stateIndex = 0, int $position = 0): bool
    {
        $state = $automaton->getStates()[$stateIndex];
        $substr = substr($input, $position);

        printf("\tmatching state at stateIndex %s: %s with input %s\n", $stateIndex, $state, $substr);


        $result = $this->matchState($input, $position, $state);

        if (null == $result) {
            if ($state->isMatch()) {
                return true;
            } else {
                return false;
            }
        } else {
            printf("\t\t\tmatched with result: %s\n", $result);
            return $this->matchStates($input, $automaton, $result->getState(), $position + 1);
        }
    }

    private function matchState(string $input, int $position, State $state): ?Edge
    {
        foreach ($state->getEdges() as $edge) {
            $transition = $edge->getTransition();
            printf("\t\tchecking transition %s\n", $transition);

            if ($transition instanceof AutomatonTransition) {
                return $this->matchAutomaton($input, $position, $transition->getIndex());
            } else if ($transition instanceof CharTransition) {
                $result = $edge->getTransition()->visitTransition($input, $position);

                if (null != $result) {
                    $this->ast->getChildren()->push($result);
                    return $edge;
                } else {
                    continue;
                }
            } else {
                throw new RuntimeException("Unsupported transition type: " . get_class($transition));
            }
        }

        return null;
    }
}
