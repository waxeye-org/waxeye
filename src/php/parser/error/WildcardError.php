<?php


namespace parser\error;


class WildcardError implements MatchError
{

    public function toGrammarString(): string
    {
        return ".";
    }
}
