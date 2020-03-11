<?php

namespace parser\continuation;

class SeqContinuation extends Continuation
{
    public function __construct()
    {
        parent::__construct(ContinuationType::SEQ);
    }
}
