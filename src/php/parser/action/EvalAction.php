<?php


namespace parser\action;


use parser\ast\ASTs;
use parser\continuation\Continuations;
use parser\error\RawError;
use parser\expression\Expression;

class EvalAction extends Action
{
    private Expression $expression;
    private int $position;
    private ASTs $asts;
    private RawError $error;

    /**
     * EvalAction constructor.
     * @param Continuations $continuations
     * @param Expression $expression
     * @param int $position
     * @param ASTs $asts
     * @param RawError $error
     */
    public function __construct(Continuations $continuations, Expression $expression, int $position, ASTs $asts, RawError $error)
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
     * @return ASTs
     */
    public function getAsts(): ASTs
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

    public function __toString()
    {
        return json_encode($this);
    }

    public static function asEvalAction($action): EvalAction
    {
        return $action;
    }
}
