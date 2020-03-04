<?php


namespace parser;


use SplStack;

class FAStack extends SplStack
{
    public function pop(): FA
    {
        return parent::pop();
    }

    public function top(): FA
    {
        return parent::top();
    }
}
