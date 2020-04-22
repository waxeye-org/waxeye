<?php


namespace parser\error;


use JsonSerializable;

class CharacterError implements MatchError, JsonSerializable
{
    private string $char;

    /**
     * CharacterError constructor.
     * @param string $char
     */
    public function __construct(string $char)
    {
        $this->char = $char;
    }


    public function toGrammarString(): string
    {
        return substr($this->char, 0, strlen($this->char) - 1);
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return get_object_vars($this);
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
