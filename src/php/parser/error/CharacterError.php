<?php


namespace parser\error;


use JsonSerializable;
use parser\expression\CharExpression;

class CharacterError implements MatchError, JsonSerializable
{
    private CharExpression $char;

    /**
     * CharacterError constructor.
     * @param CharExpression $char
     */
    public function __construct(CharExpression $char)
    {
        $this->char = $char;
    }


    public function toGrammarString(): string
    {
        return substr($this->char, 0, strlen($this->char->getChar()) - 1);
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
