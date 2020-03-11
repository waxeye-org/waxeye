<?php


namespace parser\action;


use ast\IASTs;
use parser\continuation\Continuations;
use parser\error\RawError;
use parser\expression\Expression;

class EvalAction extends Action
{
    private Expression $expression;
    private int $position;
    private IASTs $asts;
    private RawError $error;

    /**
     * EvalAction constructor.
     * @param Continuations $continuations
     * @param Expression $expression
     * @param int $position
     * @param IASTs $asts
     * @param RawError $error
     */
    public function __construct(Continuations $continuations, Expression $expression, int $position, IASTs $asts, RawError $error)
    {
        parent::__construct(ActionType::EVAL, $continuations);

        $this->expression = $expression;
        $this->position = $position;
        $this->asts = $asts;
        $this->error = $error;
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
     * @return IASTs
     */
    public function getAsts(): IASTs
    {
        return $this->asts;
    }

    /**
     * @return RawError
     */
    public function getError(): RawError
    {
        return $this->error;
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return get_object_vars($this);
    }
}
