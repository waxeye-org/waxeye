<?php

namespace parser\expression;


class AltExpression extends ExpressionWithExpressions
{
    public function __construct(Expressions $expressions)
    {
        parent::__construct($expressions, ExpressionType::ALT);
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
