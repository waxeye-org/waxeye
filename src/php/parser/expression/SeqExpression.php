<?php

namespace parser\expression;


class SeqExpression extends ExpressionWithExpressions
{
    public function __construct(Expressions $expressions)
    {
        parent::__construct($expressions, ExpressionType::SEQ);
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

    public static function asSeqExpression($expression): SeqExpression
    {
        return $expression;
    }
}
