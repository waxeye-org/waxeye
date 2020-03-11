<?php


namespace parser\result;


use ast\IASTs;
use parser\error\RawError;

class Accepted extends MatchResult
{
    private int $position;
    private IASTs $asts;

    /**
     * Accepted constructor.
     * @param int $position
     * @param IASTs $asts
     * @param RawError $error
     */
    public function __construct(int $position, IASTs $asts, RawError $error)
    {
        parent::__construct(MatchResultType::ACCEPTED, $error);

        $this->position = $position;
        $this->asts = $asts;
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
