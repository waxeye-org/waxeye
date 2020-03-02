<?php


namespace parser;

include "ITransition.php";

class CharTransition implements ITransition
{
    private string $single;
    private string $min;
    private string $max;

    public function __construct(string $single, string $min, string $max)
    {
        $this->single = $single;
        $this->min = $min;
        $this->max = $max;
    }

    public function matches(string $input): bool
    {
        return false;
    }

    public function acceptVisitor(ITransitionVisitor $visitor)
    {
        return $visitor->visitCharTransition($this);
    }
}
