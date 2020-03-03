<?php


namespace parser;


use util\ArrayIteratorStr;

class Edges extends ArrayIteratorStr
{
    public function __construct(Edge...$edges)
    {
        parent::__construct($edges);
    }

    public function current(): Edge
    {
        return parent::current();
    }

    public function offsetGet($index): Edge
    {
        return parent::offsetGet($index);
    }
}
