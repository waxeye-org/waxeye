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
}
