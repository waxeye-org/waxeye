<?php


namespace ast;

include_once "NoChildren.php";


class Char extends NoChildren implements IChar
{
    private string $value;
    private int $position;

    public function __construct(string $value, int $position, string $type)
    {
        parent::__construct($type);

        $this->value = $value;
        $this->position = $position;
    }


    /**
     * @return string
     */
    public function getValue(): string
    {
        return $this->value;
    }

    /**
     * @param string $value
     */
    public function setValue(string $value): void
    {
        $this->value = $value;
    }

    /**
     * @return int
     */
    public function getPosition(): int
    {
        return $this->position;
    }

    /**
     * @param int $position
     */
    public function setPosition(int $position): void
    {
        $this->position = $position;
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
        return array("Char" => get_object_vars($this));
    }
}
