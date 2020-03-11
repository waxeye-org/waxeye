<?php

namespace parser\continuation;


use ast\IASTs;
use parser\expression\Expressions;

class AltContinuation extends Continuation
{
    private Expressions $expression;
    private int $position;
    private IASTs $asts;

    /**
     * AltContinuation constructor.
     * @param Expressions $expression
     * @param int $position
     * @param IASTs $asts
     */
    public function __construct(Expressions $expression, int $position, IASTs $asts)
    {
        parent::__construct(ContinuationType::ALT);

        $this->expression = $expression;
        $this->position = $position;
        $this->asts = $asts;
    }

    /**
     * @return Expressions
     */
    public function getExpression(): Expressions
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
     * @return IASTs
     */
    public function getAsts(): IASTs
    {
        return $this->asts;
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
