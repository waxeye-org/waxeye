<?php


namespace parser\error;


use JsonSerializable;

class ParseError implements JsonSerializable
{
    private int $position;
    private int $line;
    private int $column;
    private string $nt;

    /**
     * ParseError constructor.
     * @param int $position
     * @param int $line
     * @param int $column
     * @param string $nt
     */
    public function __construct(int $position, int $line, int $column, string $nt)
    {
        $this->position = $position;
        $this->line = $line;
        $this->column = $column;
        $this->nt = $nt;
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
