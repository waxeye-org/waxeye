<?php


namespace parser\error;


use util\ArrayIteratorStr;

class ParseErrors extends ArrayIteratorStr
{
    public function __construct(ParseError...$parseErrors)
    {
        parent::__construct($parseErrors);
    }

    public function current(): ParseError
    {
        return parent::current();
    }

    public function offsetGet($index): ParseError
    {
        return parent::offsetGet($index);
    }
}
