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

    public function jsonSerialize()
    {
        return get_object_vars($this);
    }

    public static function asRejected($matchResult): Rejected
    {
        return $matchResult;
    }
}
