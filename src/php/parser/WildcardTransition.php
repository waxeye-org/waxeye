<?php


namespace parser;


use ast\Char;
use ast\IAST;

class WildcardTransition implements ITransition
{
    public function visitTransition(string $input, int $position): ?IAST
    {
        return new Char($input[$position], $position, "wild");
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return array("WildcardTransition" => get_object_vars($this));
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
