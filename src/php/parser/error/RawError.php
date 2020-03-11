<?php


namespace parser\error;


class RawError
{
    private int $position;
    private string $currentNonTerminal;

    /**
     * RawError constructor.
     * @param int $position
     * @param string $currentNonTerminal
     */
    public function __construct(int $position, string $currentNonTerminal)
    {
        $this->position = $position;
        $this->currentNonTerminal = $currentNonTerminal;
    }


    /**
     * @return string
     */
    public function getCurrentNonTerminal(): string
    {
        return $this->currentNonTerminal;
    }

    /**
     * @return int
     */
    public function getPosition(): int
    {
        return $this->position;
    }
}
