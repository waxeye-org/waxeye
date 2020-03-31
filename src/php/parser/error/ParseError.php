<?php


namespace parser\error;


use JsonSerializable;

class ParseError implements JsonSerializable
{
    private int $position;
    private int $line;
    private int $column;
    private array $nonTerminals;
    private MatchErrors $chars;

    /**
     * ParseError constructor.
     * @param int $position
     * @param int $line
     * @param int $column
     * @param array $nonTerminals
     * @param MatchErrors $chars
     */
    public function __construct(int $position, int $line, int $column, array $nonTerminals, MatchErrors $chars)
    {
        $this->position = $position;
        $this->line = $line;
        $this->column = $column;
        $this->nonTerminals = $nonTerminals;
        $this->chars = $chars;
    }

    /**
     * @return int
     */
    public function getPosition(): int
    {
        return $this->position;
    }

    /**
     * @return int
     */
    public function getLine(): int
    {
        return $this->line;
    }

    /**
     * @return int
     */
    public function getColumn(): int
    {
        return $this->column;
    }

    /**
     * @return array
     */
    public function getNonTerminals(): array
    {
        return $this->nonTerminals;
    }

    /**
     * @return MatchErrors
     */
    public function getChars(): MatchErrors
    {
        return $this->chars;
    }

    public function __toString()
    {
        return json_encode($this);
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return array("ParseError" => get_object_vars($this));
    }
}
