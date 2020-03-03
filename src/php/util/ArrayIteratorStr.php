<?php


namespace util;


use ArrayIterator;

abstract class ArrayIteratorStr extends ArrayIterator
{
    public function __toString()
    {
        return "[" . implode(",", $this->getArrayCopy()) . "]";
    }
}
