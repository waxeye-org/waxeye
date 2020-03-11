<?php

namespace parser\continuation;

class StarContinuation extends Continuation
{
    public function __construct()
    {
        parent::__construct(ContinuationType::STAR);
    }
}
