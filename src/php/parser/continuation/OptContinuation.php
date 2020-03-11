<?php

namespace parser\continuation;

class OptContinuation extends Continuation
{
    public function __construct()
    {
        parent::__construct(ContinuationType::OPT);
    }
}
