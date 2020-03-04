<?php


namespace parser;

use ast\Char;
use ast\IAST;
use util\CharArray;


/**
 * Class CharTransition
 * @package parser
 */
class CharTransition implements ITransition
{
    private CharArray $single;
    private CharArray $min;
    private CharArray $max;

    public function __construct(CharArray $single, CharArray $min, CharArray $max)
    {
        $this->single = $single;
        $this->min = $min;
        $this->max = $max;
    }

    public function matches(string $input): bool
    {
        if (strlen($input) == 1) {
            foreach ($this->single as $single) {
                if ($input === $single) {
                    return true;
                }
            }
        }

        return false;
    }

    public function visitTransition(string $input, int $position): ?IAST
    {
        $result = substr($input, $position, 1);
        if ($this->matches($result)) {
            return new Char($input[$position], $position, "char");
        }

        return null;
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return array("CharTransition" => get_object_vars($this));
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
