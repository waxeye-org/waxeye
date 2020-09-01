<?php

namespace parser\expression;


use IntlChar;
use JsonSerializable;

abstract class Expression implements JsonSerializable
{
    protected string $type;

    /**
     * Expression constructor.
     * @param string $type
     */
    protected function __construct(string $type)
    {
        $this->type = $type;
    }

    /**
     * @return string
     */
    public function getType(): string
    {
        return $this->type;
    }

    public static function CharExpression(int $char): CharExpression
    {
        return new CharExpression(IntlChar::chr($char));
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

    public static function AndExpression(Expression $expression): AndExpression
    {
        return new AndExpression($expression);
    }

    public static function AnyCharExpression(): AnyCharExpression
    {
        return new AnyCharExpression();
    }

    public static function NotExpression(Expression $expression): NotExpression
    {
        return new NotExpression($expression);
    }

    public static function SeqExpression(Expression...$expressions): SeqExpression
    {
        return new SeqExpression(Expressions::from($expressions));
    }

    public static function StarExpression(Expression $expression): StarExpression
    {
        return new StarExpression($expression);
    }

    public static function CharClassExpression(array $single, array $min, array $max): CharClassExpression
    {
        return new CharClassExpression($single, $min, $max);
    }

    public static function PlusExpression(Expression $expression): PlusExpression
    {
        return new PlusExpression($expression);
    }

    public static function OptExpression(Expression $expression): OptExpression
    {
        return new OptExpression($expression);
    }

    public static function VoidExpression(Expression $expression): VoidExpression
    {
        return new VoidExpression($expression);
    }
}
