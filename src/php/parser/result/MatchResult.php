<?php


namespace parser\result;


use JsonSerializable;
use parser\error\RawError;

abstract class MatchResult implements JsonSerializable
{
    protected int $type;
    protected RawError $error;

    /**
     * MatchResult constructor.
     * @param int $type
     * @param RawError $error
     */
    public function __construct(int $type, RawError $error)
    {
        $this->type = $type;
        $this->error = $error;
    }

    /**
     * @return int
     */
    public function getType(): int
    {
        return $this->type;
    }

    /**
     * @return RawError
     */
    public function getError(): RawError
    {
        return $this->error;
    }


}
