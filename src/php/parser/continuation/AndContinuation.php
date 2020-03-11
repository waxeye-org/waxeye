<?php

namespace parser\continuation;
class AndContinuation extends Continuation
{
    public function __construct()
    {
        parent::__construct(ContinuationType::AND);
    }

}
