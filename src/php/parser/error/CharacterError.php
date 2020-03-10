<?php


namespace parser\error;


class CharacterError implements MatchError
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
        return substr($this->char, 1, strlen($this->char) - 1);
    }
}
