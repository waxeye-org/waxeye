<?php


namespace parser\ast;


class EmptyAST extends AST
{
    public function __construct(int $start, int $end)
    {
        parent::__construct('', new ASTs(), $start, $end);
    }
}
