<?php

namespace parser\action;


use JsonSerializable;
use parser\continuation\Continuations;

abstract class Action implements JsonSerializable
{
    protected int $type;
    protected Continuations $continuations;

    /**
     * Action constructor.
     * @param int $type
     * @param Continuations $continuations
     */
    public function __construct(int $type, Continuations $continuations)
    {
        $this->type = $type;
        $this->continuations = $continuations;
    }

    /**
     * @return int
     */
    public function getType(): int
    {
        return $this->type;
    }

    /**
     * @return Continuations
     */
    public function getContinuations(): Continuations
    {
        return $this->continuations;
    }
}
