<?php


namespace ast;


class AST implements IAST
{
    private string $type;
    private int $position;
    private IASTs $children;

    public function __construct(string $type, int $position, IASTs $children)
    {
        $this->type = $type;
        $this->position = $position;
        $this->children = $children;
    }

    public function setChildren(IASTs $children): void
    {
        $this->children = $children;
    }


    public function getValue(): string
    {
        // TODO: Implement getValue() method.
    }

    /**
     * @return string
     */
    public function getType(): string
    {
        return $this->type;
    }

    public function getPosition(): int
    {
        return $this->position;
    }

    public function getChildren(): IASTs
    {
        return $this->children;
    }

    public function isEmpty(): bool
    {
        return $this->type === EmptyAST::TYPE && $this->children->count() === 0;
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
        return array("AST" => get_object_vars($this));
    }
}
