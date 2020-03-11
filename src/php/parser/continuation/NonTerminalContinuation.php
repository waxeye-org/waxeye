<?php

namespace parser\continuation;

use ast\IASTs;
use parser\NonTerminalMode;

class NonTerminalContinuation extends Continuation
{
    private int $mode;
    private string $name;
    private IASTs $asts;
    private string $nonTerminal;
    private int $startPosition;

    /**
     * NonTerminalContinuation constructor.
     * @param int $mode
     * @param string $name
     * @param IASTs $asts
     * @param string $nonTerminal
     * @param int $startPosition
     */
    public function __construct(int $mode, string $name, IASTs $asts, string $nonTerminal, int $startPosition)
    {
        parent::__construct(ContinuationType::NT);

        $this->mode = $mode;
        $this->name = $name;
        $this->asts = $asts;
        $this->nonTerminal = $nonTerminal;
        $this->startPosition = $startPosition;
    }


}
