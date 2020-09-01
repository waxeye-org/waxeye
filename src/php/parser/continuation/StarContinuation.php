<?php

namespace parser\continuation;

use parser\ast\ASTs;
use parser\expression\Expression;

class StarContinuation extends Continuation
{
    private Expression $expression;
    private int $position;
    private ASTs $asts;

    /**
     * StarContinuation constructor.
     * @param Expression $expression
     * @param int $position
     * @param ASTs $asts
     */
    public function __construct(Expression $expression, int $position, ASTs $asts)
    {
        parent::__construct(ContinuationType::STAR);

        $this->expression = $expression;
        $this->position = $position;
        $this->asts = $asts;
    }

    /**
     * @return Expression
     */
    public function getExpression(): Expression
    {
        return $this->expression;
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

    public function jsonSerialize()
    {
        return get_object_vars($this);
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
