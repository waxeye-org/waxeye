<?php


namespace parser;


use ast\IAST;

interface ITransition
{
    public function visitTransition(string $input, int $position): ?IAST;
}
