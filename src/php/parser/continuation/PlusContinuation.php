<?php

namespace parser\continuation;

class PlusContinuation extends Continuation
{
    public function __construct()
    {
        parent::__construct(ContinuationType::PLUS);
    }
}
