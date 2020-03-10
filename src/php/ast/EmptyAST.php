<?php


namespace ast;


class EmptyAST extends NoChildren
{
    const TYPE = '';

    private int $position;

    public function __construct(int $position)
    {
        parent::__construct(self::TYPE);

        $this->position = $position;
    }

    public function getValue(): string
    {
        return "";
    }

    public function getPosition(): int
    {
        return $this->position;
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return array("Empty" => get_object_vars($this));
    }
}
