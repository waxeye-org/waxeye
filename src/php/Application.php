<?php

use parser\AutomatonTransition;
use parser\CharTransition;
use parser\Edge;
use parser\Edges;
use parser\FA;
use parser\Parser;
use parser\State;
use parser\States;
use util\CharArray;

spl_autoload_register(function ($class_name) {
    include $class_name . ".php";
});


class TestParser extends Parser
{
    public function __construct()
    {
        $fas = new SplDoublyLinkedList();

        // CWR File
        $states = new States();
        $edges = new Edges();
        $edges->append(new Edge(new AutomatonTransition(1), 1, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $edges->append(new Edge(new AutomatonTransition(2), 2, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $states->append(new State($edges, true));
        $fas->push(new FA("CWRFile", $states));


        // TransmissionHeader
        $states = new States();
        $edges = new Edges();
        $edges->append(new Edge(new CharTransition(new CharArray('X'), new CharArray(), new CharArray()), 1, false));
        $edges->append(new Edge(new CharTransition(new CharArray('H'), new CharArray(), new CharArray()), 1, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $edges->append(new Edge(new CharTransition(new CharArray('D'), new CharArray(), new CharArray()), 2, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $edges->append(new Edge(new CharTransition(new CharArray('R'), new CharArray(), new CharArray()), 3, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $states->append(new State($edges, true));
        $fas->push(new FA("TransmissionHeader", $states));


        // SenderType
        $states = new States();
        $edges = new Edges();
        $edges->append(new Edge(new CharTransition(new CharArray("P"), new CharArray(), new CharArray()), 1, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $edges->append(new Edge(new CharTransition(new CharArray("B"), new CharArray(), new CharArray()), 2, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $states->append(new State($edges, true));
        $fas->push(new FA("SenderType", $states));

        foreach ($fas as $fa) {
            printf("FA: %s\n", $fa);
        }

        parent::__construct($fas);
    }
}


$parser = new TestParser();
$parser->parse("HDRPB");//512590964KICK  THE FLAME MUSIKVERLAG, RAJK BARTHEL    01.102019112512513620191125               ");

