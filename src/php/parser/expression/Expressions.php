<?php

namespace parser\expression;


use util\ArrayIteratorStr;

class Expressions extends ArrayIteratorStr
{
    public static function from(array $expressions): Expressions
    {
        // TODO: add check for expression typ (if !(expression instanceof Expression) => Exception)
        $result = new Expressions();
        foreach ($expressions as $expression) {
            $result[] = $expression;
        }

        return $result;
    }

    public function __construct(Expression...$expressions)
    {
        parent::__construct($expressions);
    }

    public function current(): Expression
    {
        return parent::current();
    }

    public function offsetGet($index): Expression
    {
        return parent::offsetGet($index);
    }
}
