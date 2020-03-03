<?php


namespace parser;


use util\ArrayIteratorStr;

class States extends ArrayIteratorStr
{
    public function __construct(State...$states)
    {
        parent::__construct($states);
    }

    public function current(): State
    {
        return parent::current();
    }

    public function offsetGet($index): State
    {
        return parent::offsetGet($index);
    }
}
