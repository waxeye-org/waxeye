<?php
namespace parser\expression;


abstract class ExpressionWithExpression extends Expression
{
    protected Expression $expression;

    public function __construct(Expression $expression, string $type)
    {
        parent::__construct($type);

        $this->expression = $expression;
    }

    /**
     * @return Expression
     */
    public function getExpression(): Expression
    {
        return $this->expression;
    }
}
