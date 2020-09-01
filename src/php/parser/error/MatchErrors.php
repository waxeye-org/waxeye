<?php


namespace parser\error;


use util\ArrayIteratorStr;

class MatchErrors extends ArrayIteratorStr
{
    public static function matchErrors(MatchError $head, MatchErrors $tail): MatchErrors
    {
        $result = new MatchErrors($head);
        foreach ($tail as $matchError) {
            $result[] = $matchError;
        }

        return $result;
    }

    public function __construct(MatchError...$matchErrors)
    {
        parent::__construct($matchErrors);
    }

    public function current(): MatchError
    {
        return parent::current();
    }

    public function offsetGet($index): MatchError
    {
        return parent::offsetGet($index);
    }

}
