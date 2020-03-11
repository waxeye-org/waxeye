<?php


namespace parser\result;


use parser\error\RawError;

class Rejected extends MatchResult
{

    /**
     * Rejected constructor.
     * @param RawError $error
     */
    public function __construct(RawError $error)
    {
        parent::__construct(MatchResultType::REJECTED, $error);
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
