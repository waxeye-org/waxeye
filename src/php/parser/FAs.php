<?php


namespace parser;


use util\ArrayIteratorStr;

class FAs extends ArrayIteratorStr
{
    public function __construct(FA...$fas)
    {
        parent::__construct($fas);
    }

    public function current(): FA
    {
        return parent::current();
    }

    public function offsetGet($index): FA
    {
        return parent::offsetGet($index);
    }
}
