<?php

namespace parser\expression;


class CharClassExpression extends Expression
{
    private array $single;
    private array $min;
    private array $max;

    public function __construct(array $single, array $min, array $max)
    {
        parent::__construct(ExpressionType::CHAR_CLASS);

        $this->single = $single;
        $this->min = $min;
        $this->max = $max;
    }

    /**
     * @return array
     */
    public function getSingle(): array
    {
        return $this->single;
    }

    /**
     * @return array
     */
    public function getMin(): array
    {
        return $this->min;
    }

    /**
     * @return array
     */
    public function getMax(): array
    {
        return $this->max;
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

    public static function asCharClassExpression($expression): CharClassExpression
    {
        return $expression;
    }
}
