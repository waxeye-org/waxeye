<?php

namespace parser\continuation;

use parser\expression\Expression;

class PlusContinuation extends Continuation
{
    private Expression $expression;

    /**
     * PlusContinuation constructor.
     * @param Expression $expression
     */
    public function __construct(Expression $expression)
    {
        parent::__construct(ContinuationType::PLUS);

        $this->expression = $expression;
    }

    /**
     * @return Expression
     */
    public function getExpression(): Expression
    {
        return $this->expression;
    }

    public function jsonSerialize()
    {
        return get_object_vars($this);
    }
}
