<?php

namespace parser\expression;


abstract class ExpressionWithExpressions extends Expression
{
    protected Expressions $expressions;

    protected function __construct(Expressions $expressions, string $type)
    {
        parent::__construct($type);

        $this->expressions = $expressions;
    }

    /**
     * @return Expressions
     */
    public function getExpressions(): Expressions
    {
        return $this->expressions;
    }
}
