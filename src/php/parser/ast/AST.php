<?php


namespace parser\ast;


use JsonSerializable;

class AST implements JsonSerializable
{
    private string $type;
    private ASTs $children;
    private int $start;
    private int $end;

    /**
     * AST constructor.
     * @param string $type
     * @param ASTs $children
     * @param int $start
     * @param int $end
     */
    public function __construct(string $type, ASTs $children, int $start, int $end)
    {
        $this->type = $type;
        $this->children = $children;
        $this->start = $start;
        $this->end = $end;
    }

    /**
     * @return string
     */
    public function getType(): string
    {
        return $this->type;
    }

    /**
     * @return ASTs
     */
    public function getChildren(): ASTs
    {
        return $this->children;
    }

    /**
     * @return int
     */
    public function getStart(): int
    {
        return $this->start;
    }

    /**
     * @return int
     */
    public function getEnd(): int
    {
        return $this->end;
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
