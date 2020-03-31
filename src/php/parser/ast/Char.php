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
        parent::__construct("CHAR", new ASTs(), $position, $position + strlen($value));

        $this->value = $value;
    }

    /**
     * @return mixed|string
     */
    public function getValue()
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
