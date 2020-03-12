<?php


namespace parser\error;


use JsonSerializable;

interface MatchError extends JsonSerializable
{
    public function toGrammarString(): string;
}
