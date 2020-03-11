<?php

namespace parser\expression;


class CharExpression extends Expression
{
    private string $char;

    public function __construct(string $char)
    {
        parent::__construct(ExpressionType::CHAR);

        $this->char = $char;
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

    /**
     * @return string
     */
    public function getChar(): string
    {
        return $this->char;
    }


}
