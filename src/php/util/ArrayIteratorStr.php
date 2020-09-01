<?php


namespace util;


use ArrayIterator;

abstract class ArrayIteratorStr extends ArrayIterator
{
    public function __toString()
    {
        return "[" . implode(",", $this->getArrayCopy()) . "]";
    }

    public function isEmpty(): bool
    {
        return 0 === $this->count();
    }
}
