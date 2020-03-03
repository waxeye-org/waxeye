<?php


namespace ast;


use SplDoublyLinkedList;

class AST implements IAST
{
    private string $type;
    private int $position;
    private SplDoublyLinkedList $children;

    /**
     * AST constructor.
     * @param string $type
     * @param int $position
     * @param SplDoublyLinkedList $children
     */
    public function __construct(string $type, int $position, SplDoublyLinkedList $children)
    {
        $this->type = $type;
        $this->position = $position;
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

    public function getChildren(): SplDoublyLinkedList
    {
        return $this->children;
    }

    public function __toString()
    {
        $result = "{AST{type: " . $this->type . ", position: " . $this->position . ", children: {";
        foreach ($this->children as $child) {
            $result .= $child;
        }
        $result .= "}}";

        return $result;
    }
}
