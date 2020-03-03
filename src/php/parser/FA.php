<?php


namespace parser;


class FA
{
    private int $mode;
    private string $type;
    private States $states;

    public function __construct(string $type, States $states)
    {
        $this->type = $type;
        $this->states = $states;
        $this->mode = 0;
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
        return "Automaton{mode: " . $this->mode . ", type: " . $this->type . ", states: " . $this->states . "}";
    }
}
