<?php


namespace parser;


use ast\IAST;
use JsonSerializable;
use parser\error\ParseError;
use parser\error\ParseErrors;

class ParseResult implements JsonSerializable
{
    private ?IAST $ast;
    private ?ParseErrors $parseErrors;

    /**
     * ParseResult constructor.
     * @param IAST $ast
     * @param ParseErrors $parseErrors
     */
    public function __construct(?IAST $ast, ?ParseErrors $parseErrors)
    {
        $this->ast = $ast;
        $this->parseErrors = $parseErrors;
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return array("ParseResult" => get_object_vars($this));
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
