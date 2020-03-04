<?php


namespace parser;


use ast\IAST;

class AutomatonTransition implements ITransition
{
    private int $index;

    /**
     * AutomatonTransition constructor.
     * @param int $index
     */
    public function __construct(int $index)
    {
        $this->index = $index;
    }

    /**
     * @return int
     */
    public function getIndex(): int
    {
        return $this->index;
    }


    public function visitTransition(string $input, int $position): ?IAST
    {
    }

    /**
     * @return string
     */
    public function __toString()
    {
        return json_encode($this);
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return array("AutomatonTransition" => get_object_vars($this));
    }
}
