<?php

namespace parser\expression;


class NonTerminalExpression extends Expression
{
    private string $name;

    public function __construct(string $name)
    {
        parent::__construct(ExpressionType::NT);

        $this->name = $name;
    }

    /**
     * @return string
     */
    public function getName(): string
    {
        return $this->name;
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
