<?php


namespace parser\transition;


use ast\Char;
use ast\IAST;

class WildcardTransition implements ITransition
{
    public function visitTransition(string $input, int $position): ?IAST
    {
        $result = substr($input, $position, 1);

        if ($result !== false) {
            return new Char($input[$position], $position, "char");
        } else {
            return null;
        }
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
