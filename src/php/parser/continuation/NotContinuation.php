<?php

namespace parser\continuation;

class NotContinuation extends Continuation
{
    public function __construct()
    {
        parent::__construct(ContinuationType::NOT);
    }
}
