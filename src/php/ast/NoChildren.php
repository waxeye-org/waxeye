<?php

namespace ast;


use SplDoublyLinkedList;

abstract class NoChildren implements IAST
{
    protected string $type;

    public function __construct(string $type)
    {
        $this->type = $type;
    }

    public function getType(): string
    {
        return $this->type;
    }


    public function getChildren(): IASTs
    {
        return new IASTs();
    }

    public function setChildren(IASTs $children)
    {
        return false;
    }
}


