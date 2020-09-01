<?php


namespace parser\ast;


class Char extends AST
{
    private string $value;

    /**
     * Char constructor.
     * @param string $value
     * @param int $position
     */
    public function __construct(string $value, int $position)
    {
        parent::__construct("CHAR", new ASTs(), $position, $position + 1);

        $this->value = $value;
    }

    /**
     * @return string
     */
    public function getValue(): string
    {
        return $this->value;
    }

    public function jsonSerialize()
    {
        return get_object_vars($this);
    }

    public static function asCharAST($ast): Char
    {
        return $ast;
    }
}
