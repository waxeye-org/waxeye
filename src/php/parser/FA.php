<?php


namespace parser;


use JsonSerializable;

class FA implements JsonSerializable
{
    const LEFT = 0;
    const PRUNE = 1;
    const VOID = 2;

    private int $mode;
    private string $type;
    private States $states;

    public function __construct(string $type, States $states, int $mode)
    {
        $this->type = $type;
        $this->states = $states;
        $this->mode = $mode;
    }

    /**
     * @return int
     */
    public function getMode(): int
    {
        return $this->mode;
    }

    /**
     * @return States
     */
    public function getStates(): States
    {
        return $this->states;
    }

    /**
     * @return string
     */
    public function getType(): string
    {
        return $this->type;
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
        return array("FA" => get_object_vars($this));
    }
}
