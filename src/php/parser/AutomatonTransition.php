<?php


namespace parser;


class AutomatonTransition implements ITransition
{
    private int $index;

    public function acceptVisitor(ITransitionVisitor $visitor)
    {
        return $visitor->visitAutomationTransition($this);
    }
}
