<?php


namespace parser;


use JsonSerializable;

class State implements JsonSerializable
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

    public function __toString()
    {
        return json_encode($this);
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return array("State" => get_object_vars($this));
    }
}


