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

    public static function cons(Continuation $head, Continuations $tail): Continuations
    {
        $result = new Continuations($head);
        foreach ($tail as $continuation) {
            $result[] = $continuation;
        }

        return $result;
    }

    public function __construct(Continuation...$continuations)
    {
        parent::__construct($continuations);
    }

    public function current(): Continuation
    {
        return parent::current();
    }

    public function offsetGet($index): Continuation
    {
        return parent::offsetGet($index);
    }

    public function head(): Continuation
    {
        return $this->offsetGet(0);
    }

    public function tail(): Continuations
    {
        return Continuations::from(array_slice($this->getArrayCopy(), 1));
    }
}
