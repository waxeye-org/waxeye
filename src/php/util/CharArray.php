<?php


namespace util;


use RuntimeException;

class CharArray extends ArrayIteratorStr
{
    public function __construct(string...$chars)
    {
        foreach ($chars as $char) {
            if (strlen($char) != 1) {
                throw new RuntimeException("char length must equal 1 (not given for char " . $char . ")");
            }
        }

        parent::__construct($chars);
    }

    public function current(): string
    {
        return parent::current();
    }

    public function offsetGet($index): string
    {
        return parent::offsetGet($index);
    }
}
