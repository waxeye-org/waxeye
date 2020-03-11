<?php

namespace parser\expression;


class CharClassExpression extends Expression
{
    private string $char;

    public function __construct(string $char)
    {
        parent::__construct(ExpressionType::CHAR_CLASS);

        $this->char = $char;
    }

    /**
     * @return string
     */
    public function getChar(): string
    {
        return $this->char;
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
