<?php

namespace parser\expression;


class NotExpression extends ExpressionWithExpression
{
    public function __construct(Expression $expression)
    {
        parent::__construct($expression, ExpressionType::NOT);
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
}
