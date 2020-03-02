<?php


namespace parser;


class FA
{
    private int $mode;
    private States $states;

    public function __construct(States $states)
    {
        $this->states = $states;
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
}
