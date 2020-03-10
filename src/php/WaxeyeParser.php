<?php


use parser\AutomatonTransition;
use parser\CharTransition;
use parser\Edge;
use parser\Edges;
use parser\FA;
use parser\FAs;
use parser\Parser;
use parser\State;
use parser\States;
use parser\WildcardTransition;
use util\CharArray;

class WaxeyeParser extends Parser
{
    public function __construct()
    {
        $fas = new FAs();

        // CWR File
        $states = new States();
        $edges = new Edges();
        $edges->append(new Edge(new AutomatonTransition(1), 1, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $edges->append(new Edge(new AutomatonTransition(2), 2, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $edges->append(new Edge(new AutomatonTransition(3), 3, false));
        $states->append(new State($edges, false));

        $edges = new Edges();
        $states->append(new State($edges, true));
        $fas[] = new FA("CWRFile", $states, FA::LEFT);


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
        $fas[] = new FA("TransmissionHeader", $states, FA::LEFT);


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
        $fas[] = new FA("SenderType", $states, FA::LEFT);

        $states = new States();
        $edges = new Edges();
        $edges->append(new Edge(new WildcardTransition(), 1, false));
        $states->append(new State($edges, false));
        $edges = new Edges();
        $edges->append(new Edge(new WildcardTransition(), 2, false));
        $states->append(new State($edges, false));
        $edges = new Edges();
        $edges->append(new Edge(new WildcardTransition(), 3, false));
        $states->append(new State($edges, false));
        $edges = new Edges();
        $edges->append(new Edge(new WildcardTransition(), 4, false));
        $states->append(new State($edges, false));
        $edges = new Edges();
        $edges->append(new Edge(new WildcardTransition(), 5, false));
        $states->append(new State($edges, false));
        $edges = new Edges();
        $edges->append(new Edge(new WildcardTransition(), 6, false));
        $states->append(new State($edges, false));
        $edges = new Edges();
        $edges->append(new Edge(new WildcardTransition(), 7, false));
        $states->append(new State($edges, false));
        $edges = new Edges();
        $edges->append(new Edge(new WildcardTransition(), 8, false));
        $states->append(new State($edges, false));
        $edges = new Edges();
        $edges->append(new Edge(new WildcardTransition(), 9, false));
        $states->append(new State($edges, false));
        $edges = new Edges();
        $states->append(new State($edges, true));
        $fas[] = new FA("SenderId", $states, FA::LEFT);


        printf("%s\n", $fas);

        parent::__construct($fas);
    }
}
