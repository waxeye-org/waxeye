<?php


namespace parser;


use ArrayIterator;

class Edge
{
    private ITransition $transition;
    private int $state;
    private bool $voided;

    public function __construct(ITransition $transition, int $state, bool $voided)
    {
        $this->transition = $transition;
        $this->state = $state;
        $this->voided = $voided;
    }

    public function __toString()
    {
        return "[edge " . $this->state . ": " . $this->voided . "]";
    }

    /**
     * @return int
     */
    public function getState(): int
    {
        return $this->state;
    }

    /**
     * @return ITransition
     */
    public function getTransition(): ITransition
    {
        return $this->transition;
    }

    /**
     * @return bool
     */
    public function isVoided(): bool
    {
        return $this->voided;
    }
}

class Edges extends ArrayIterator
{
    public function __construct(Edge...$edges)
    {
        parent::__construct($edges);
    }

    public function current(): Edge
    {
        return parent::current();
    }

    public function offsetGet($index): Edge
    {
        return parent::offsetGet($index);
    }
}
