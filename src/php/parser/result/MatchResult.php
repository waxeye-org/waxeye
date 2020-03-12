<?php


namespace parser\result;


use JsonSerializable;
use parser\error\RawError;

abstract class MatchResult implements JsonSerializable
{
    private int $type;
    private RawError $error;

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

    public function __toString()
    {
        return json_encode($this);
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
