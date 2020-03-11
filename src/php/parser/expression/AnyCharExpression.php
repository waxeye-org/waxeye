<?php

namespace parser\expression;


class AnyCharExpression extends Expression
{
    public function __construct()
    {
        parent::__construct(ExpressionType::ANY_CHAR);
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
