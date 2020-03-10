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
        //printf("matching %s\n", $input);
        if (strlen($input) == 1) {
            foreach ($this->single as $single) {
                //printf("\twith %s\n", $this->single);
                if ($input === $single) {
                    return true;
                }
            }

            for ($i = 0; $i < $this->min->count(); $i++) {
                //printf("\t with [%s,%s]\n", $this->min[$i], $this->max[$i]);
                if ($input >= $this->min[$i] && $input <= $this->max[$i]) {
                    return true;
                }
            }
        }

        return false;
    }

    public function visitTransition(string $input, int $position): ?IAST
    {
        $substr = substr($input, $position, 1);
        //printf("matching char: %s, %s; (%s), matches: %s\n", $input, $position, $substr, $this->matches($substr));


        if (($substr !== false) && ($this->matches($substr))) {
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
        return array("CharTransition" => get_object_vars($this));
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
