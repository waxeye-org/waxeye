<?php


namespace parser;

include "FA.php";
include "Edge.php";
include "State.php";
include "CharTransition.php";


class Parser
{
    private FA $fa;

    public function __construct(FA $fa)
    {
        $this->fa = $fa;
    }

    public function parse(string $input)
    {
        print "parsing input " . $input . "\n";
        $this->matchState(0);
    }

    protected function matchState(int $index)
    {
        $state = $this->fa->getStates()[$index];

        printf("matching state at index %s\n", $index);

        $res = $this->matchEdges($state->getEdges(), 0);
    }

    protected function matchEdges(Edges $edges, int $index)
    {
        printf("\tmatching edge at %s\n", $index);
        $this->matchEdge($edges[$index]);
    }

    protected function matchEdge(Edge $edge)
    {
        printf("\t\tmatching edge %s\n", $edge);
    }
}
