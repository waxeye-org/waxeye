<?php

namespace parser\continuation;

use parser\expression\Expressions;

class SeqContinuation extends Continuation
{
    private Expressions $expressions;

    public function __construct(Expressions $expressions)
    {
        parent::__construct(ContinuationType::SEQ);

        $this->expressions = $expressions;
    }

    /**
     * @return Expressions
     */
    public function getExpressions(): Expressions
    {
        return $this->expressions;
    }


    public static function asSeqContinuation($continuation): SeqContinuation
    {
        return $continuation;
    }
}
