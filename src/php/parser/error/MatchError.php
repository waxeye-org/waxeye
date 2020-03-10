<?php


namespace parser\error;


interface MatchError
{
    public function toGrammarString(): string;
}
