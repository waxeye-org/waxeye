<?php

namespace parser\expression;


use JsonSerializable;

abstract class Expression implements JsonSerializable
{
    protected int $type;

    /**
     * Expression constructor.
     * @param int $type
     */
    protected function __construct(int $type)
    {
        $this->type = $type;
    }

    /**
     * @return int
     */
    public function getType(): int
    {
        return $this->type;
    }

    public static function CharExpression(string $char): CharExpression
    {
        return new CharExpression($char);
    }

    public static function asCharExpression($expression): CharExpression
    {
        return $expression;
    }

    public static function NonTerminalExpression(string $name): NonTerminalExpression
    {
        return new NonTerminalExpression($name);
    }

    public static function asNonTerminalExpression($expression): NonTerminalExpression
    {
        return $expression;
    }

    public static function AltExpression(Expression...$expressions): AltExpression
    {
        return new AltExpression(Expressions::from($expressions));
    }

    public static function asAltExpression($expression): AltExpression
    {
        return $expression;
    }
}
