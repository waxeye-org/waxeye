<?php

namespace parser\continuation;

use parser\ast\ASTs;

class OptContinuation extends Continuation
{
    private int $position;
    private ASTs $asts;

    /**
     * OptContinuation constructor.
     * @param int $position
     * @param ASTs $asts
     */
    public function __construct(int $position, ASTs $asts)
    {
        parent::__construct(ContinuationType::OPT);

        $this->position = $position;
        $this->asts = $asts;
    }

    /**
     * @return int
     */
    public function getPosition(): int
    {
        return $this->position;
    }

    /**
     * @return ASTs
     */
    public function getAsts(): ASTs
    {
        return $this->asts;
    }

    public function jsonSerialize()
    {
        return get_object_vars($this);
    }}
