<?php


namespace parser;


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
        return "Edge{transition: " . $this->transition . ", state: " . $this->state . ", voided: " . $this->voided . "}";
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
