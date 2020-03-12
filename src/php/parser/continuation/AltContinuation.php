<?php

namespace parser\continuation;


use parser\ast\ASTs;
use parser\expression\Expressions;

class AltContinuation extends Continuation
{
    private Expressions $expressions;
    private int $position;
    private ASTs $asts;

    /**
     * AltContinuation constructor.
     * @param Expressions $expressions
     * @param int $position
     * @param ASTs $asts
     */
    public function __construct(Expressions $expressions, int $position, ASTs $asts)
    {
        parent::__construct(ContinuationType::ALT);

        $this->expressions = $expressions;
        $this->position = $position;
        $this->asts = $asts;
    }

    /**
     * @return Expressions
     */
    public function getExpressions(): Expressions
    {
        return $this->expressions;
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

    public function __toString()
    {
        return json_encode($this);
    }

    public function jsonSerialize()
    {
        return get_object_vars($this);
    }

    public static function asAltContinuation($continuation): AltContinuation
    {
        return $continuation;
    }
}
