<?php

namespace parser\continuation;

class VoidContinuation extends Continuation
{
    public function __construct()
    {
        parent::__construct(ContinuationType::VOID);
    }
}
