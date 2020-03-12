<?php


namespace parser\ast;


class Char extends AST
{
    private string $value;

    /**
     * Char constructor.
     * @param string $input
     * @param int $position
     */
    public function __construct(string $input, int $position)
    {
        parent::__construct("CHAR", new ASTs(), $position, $position + 1);

        $this->value = $input[$position];
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
}
