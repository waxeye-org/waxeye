<?php


namespace parser;


use ast\IAST;
use JsonSerializable;

interface ITransition extends JsonSerializable
{
    public function visitTransition(string $input, int $position): ?IAST;
}
