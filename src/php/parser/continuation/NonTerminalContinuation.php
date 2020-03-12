<?php

namespace parser\continuation;

use parser\ast\ASTs;

class NonTerminalContinuation extends Continuation
{
    private string $mode;
    private string $name;
    private ASTs $asts;
    private string $nonTerminal;
    private int $startPosition;

    /**
     * NonTerminalContinuation constructor.
     * @param string $mode
     * @param string $name
     * @param ASTs $asts
     * @param string $nonTerminal
     * @param int $startPosition
     */
    public function __construct(string $mode, string $name, ASTs $asts, string $nonTerminal, int $startPosition)
    {
        parent::__construct(ContinuationType::NT);

        $this->mode = $mode;
        $this->name = $name;
        $this->asts = $asts;
        $this->nonTerminal = $nonTerminal;
        $this->startPosition = $startPosition;
    }

    public static function asNTContinuation($continuation): NonTerminalContinuation
    {
        return $continuation;
    }

    /**
     * @return string
     */
    public function getMode(): string
    {
        return $this->mode;
    }

    /**
     * @return string
     */
    public function getName(): string
    {
        return $this->name;
    }

    /**
     * @return ASTs
     */
    public function getAsts(): ASTs
    {
        return $this->asts;
    }

    /**
     * @return string
     */
    public function getNonTerminal(): string
    {
        return $this->nonTerminal;
    }

    /**
     * @return int
     */
    public function getStartPosition(): int
    {
        return $this->startPosition;
    }

    public function __toString()
    {
        return json_encode($this);
    }

    public function jsonSerialize()
    {
        return get_object_vars($this);
    }
}
