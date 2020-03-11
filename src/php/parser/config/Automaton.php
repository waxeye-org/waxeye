<?php


namespace parser\config;


use JsonSerializable;
use parser\expression\Expression;

class Automaton implements JsonSerializable
{
    private string $key;
    private int $mode;
    private Expression $expression;

    /**
     * ParserConfig constructor.
     * @param string $key
     * @param int $mode
     * @param Expression $expression
     */
    public function __construct(string $key, int $mode, Expression $expression)
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
     * @return int
     */
    public function getMode(): int
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
