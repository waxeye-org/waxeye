<?php


namespace ast;


use util\ArrayIteratorStr;

class IASTs extends ArrayIteratorStr
{
    public function __construct(IAST...$iasts)
    {
        parent::__construct($iasts);
    }

    public static function asts(IAST $head, IASTs $tail): IASTs
    {
        $result = new IASTs($head);
        foreach ($tail as $ast) {
            $result[] = $ast;
        }

        return $result;
    }

    public function current(): IAST
    {
        return parent::current();
    }

    public function offsetGet($index): IAST
    {
        return parent::offsetGet($index);
    }

    public function head(): IAST
    {
        return $this->offsetGet(0);
    }

    public function tail(): IASTs
    {
        return IASTs::from(array_slice($this->getArrayCopy(), 1));
    }
}
