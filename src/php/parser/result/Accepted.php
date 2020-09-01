<?php


namespace parser\result;


use parser\ast\ASTs;
use parser\error\RawError;

class Accepted extends MatchResult
{
    private int $position;
    private ASTs $asts;

    /**
     * Accepted constructor.
     * @param int $position
     * @param ASTs $asts
     * @param RawError $error
     */
    public function __construct(int $position, ASTs $asts, RawError $error)
    {
        parent::__construct(MatchResultType::ACCEPTED, $error);

        $this->position = $position;
        $this->asts = $asts;
    }


    public function jsonSerialize()
    {
        return get_object_vars($this);
    }

    public function __toString()
    {
        return json_encode($this);
    }

    public static function asAccepted($matchResult): Accepted
    {
        return $matchResult;
    }

    /**
     * @return int
     */
    public function getPosition(): int
    {
        return $this->position;
    }

    /**
     * @return ASTs
     */
    public function getAsts(): ASTs
    {
        return $this->asts;
    }


}
