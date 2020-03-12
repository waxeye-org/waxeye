<?php

namespace parser\continuation;

use parser\ast\ASTs;

class VoidContinuation extends Continuation
{
    private ASTs $asts;

    /**
     * VoidContinuation constructor.
     * @param ASTs $asts
     */
    public function __construct(ASTs $asts)
    {
        parent::__construct(ContinuationType::VOID);

        $this->asts = $asts;
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
    }
}
