<?php

namespace parser\expression;


class PlusExpression extends ExpressionWithExpression
{
    public function __construct(Expression $expression)
    {
        parent::__construct($expression, ExpressionType::PLUS);
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return get_object_vars($this);
    }

    public function __toString()
    {
        return json_encode($this);
    }

    public static function asPlusExpression($expression): PlusExpression
    {
        return $expression;
    }
}
