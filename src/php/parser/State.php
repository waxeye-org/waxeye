<?php


namespace parser;


use ArrayIterator;

class State
{
    private Edges $edges;
    private bool $match;

    public function __construct(Edges $edges, bool $match)
    {
        $this->edges = $edges;
        $this->match = $match;
    }

    /**
     * @return Edges
     */
    public function getEdges(): Edges
    {
        return $this->edges;
    }

    /**
     * @return bool
     */
    public function isMatch(): bool
    {
        return $this->match;
    }
}

class States extends ArrayIterator
{
    public function __construct(State...$states)
    {
        parent::__construct($states);
    }

    public function current(): State
    {
        return parent::current();
    }

    public function offsetGet($index): State
    {
        return parent::offsetGet($index);
    }
}
