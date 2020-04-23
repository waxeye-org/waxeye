<?php


namespace parser\error;


class WildcardError implements MatchError
{

    public function toGrammarString(): string
    {
        return ".";
    }

    public function jsonSerialize()
    {
        return get_object_vars($this);
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
