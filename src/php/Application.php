<?php

use parser\CharTransition;
use parser\Edge;
use parser\Edges;
use parser\FA;
use parser\Parser;
use parser\State;
use parser\States;

include "parser\Parser.php";

class TestParser extends Parser
{
    public function __construct()
    {
        $states = new States();

        $edges = new Edges();
        $edges->append(new Edge(new CharTransition("HDR", "", ""), 1, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $states->append(new State($edges, true));

        $fa = new FA($states);

        parent::__construct($fa);
    }
}


$input = "HALLO WELT";

$parser = new TestParser();
$parser->parse($input);

