<?php


namespace parser\ast;


use util\ArrayIteratorStr;

class ASTs extends ArrayIteratorStr
{
    public function __construct(AST...$asts)
    {
        parent::__construct($asts);
    }

    public static function asts(AST $head, ASTs $tail): ASTs
    {
        $result = new ASTs($head);
        foreach ($tail as $ast) {
            $result[] = $ast;
        }

        return $result;
    }

    public function current(): AST
    {
        return parent::current();
    }

    public function offsetGet($index): AST
    {
        return parent::offsetGet($index);
    }

    public function head(): AST
    {
        return $this->offsetGet(0);
    }

    public function tail(): ASTs
    {
        return ASTs::from(array_slice($this->getArrayCopy(), 1));
    }

    public static function from(array $asts): ASTs
    {
        // TODO: add check for expression typ (if !(ast instanceof AST) => Exception)
        $result = new ASTs();
        foreach ($asts as $ast) {
            $result[] = $ast;
        }

        return $result;
    }

}
