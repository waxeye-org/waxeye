<?php


namespace parser\action;


use parser\continuation\Continuations;
use parser\result\MatchResult;

class ApplyAction extends Action
{
    private MatchResult $value;

    /**
     * ApplyAction constructor.
     * @param Continuations $continuations
     * @param MatchResult $value
     */
    public function __construct(Continuations $continuations, MatchResult $value)
    {
        parent::__construct(ActionType::APPLY, $continuations);

        $this->value = $value;
    }

    public function jsonSerialize()
    {
        return get_object_vars($this);
    }
}
