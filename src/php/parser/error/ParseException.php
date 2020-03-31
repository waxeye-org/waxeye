<?php


namespace parser\error;


use RuntimeException;

class ParseException extends RuntimeException
{
    private ParseError $parseError;

    /**
     * ParseException constructor.
     * @param ParseError $parseError
     */
    public function __construct(ParseError $parseError)
    {
        parent::__construct($parseError);

        $this->parseError = $parseError;
    }

    /**
     * @return ParseError
     */
    public function getParseError(): ParseError
    {
        return $this->parseError;
    }
}
