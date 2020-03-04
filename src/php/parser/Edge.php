<?php


namespace parser;


use JsonSerializable;

class Edge implements JsonSerializable
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
        return json_encode($this);
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

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return array("Edge" => get_object_vars($this));
    }
}
