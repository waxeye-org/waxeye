<?php

namespace parser\continuation;

use util\ArrayIteratorStr;

class Continuations extends ArrayIteratorStr
{
    public static function from(array $continuations): Continuations
    {
        // TODO: add check for expression typ (if !(expression instanceof Expression) => Exception)
        $result = new Continuations();
        foreach ($continuations as $continuation) {
            $result[] = $continuation;
        }

        return $result;
    }

    public function __construct(Continuation...$continuations)
    {
        parent::__construct($continuations);
    }

    public function current(): Continuations
    {
        return parent::current();
    }

    public function offsetGet($index): Continuations
    {
        return parent::offsetGet($index);
    }
}
