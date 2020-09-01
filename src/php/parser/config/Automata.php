<?php


namespace parser\config;


use util\ArrayIteratorStr;

class Automata extends ArrayIteratorStr
{
    public function __construct(Automaton...$automata)
    {
        parent::__construct($automata);
    }

    public function current(): Automaton
    {
        return parent::current();
    }

    public function offsetGet($index): Automaton
    {
        return parent::offsetGet($index);
    }
}
