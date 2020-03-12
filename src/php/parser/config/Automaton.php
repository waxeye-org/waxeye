<?php


namespace parser\config;


use JsonSerializable;
use parser\expression\Expression;

class Automaton implements JsonSerializable
{
    private string $key;
    private string $mode;
    private Expression $expression;

    /**
     * ParserConfig constructor.
     * @param string $key
     * @param string $mode
     * @param Expression $expression
     */
    public function __construct(string $key, string $mode, Expression $expression)
    {
        $this->key = $key;
        $this->mode = $mode;
        $this->expression = $expression;
    }

    /**
     * @return string
     */
    public function getKey(): string
    {
        return $this->key;
    }

    /**
     * @return string
     */
    public function getMode(): string
    {
        return $this->mode;
    }

    /**
     * @return Expression
     */
    public function getExpression(): Expression
    {
        return $this->expression;
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
