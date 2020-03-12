<?php

namespace parser\continuation;

use parser\ast\ASTs;
use parser\error\RawError;

class AndContinuation extends Continuation
{
    private int $position;
    private ASTs $asts;
    private RawError $error;

    public function __construct(int $position, ASTs $asts, RawError $error)
    {
        parent::__construct(ContinuationType:: AND);

        $this->position = $position;
        $this->asts = $asts;
        $this->error = $error;
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

    /**
     * @return RawError
     */
    public function getError(): RawError
    {
        return $this->error;
    }


    public static function asAndContinuation($continuation): AndContinuation
    {
        return $continuation;
    }
}
