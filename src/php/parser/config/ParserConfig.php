<?php


namespace parser\config;


use JsonSerializable;

class ParserConfig implements JsonSerializable
{
    private Automata $automata;
    private string $start;

    /**
     * ParserConfig constructor.
     * @param Automata $automata
     * @param string $start
     */
    public function __construct(Automata $automata, string $start)
    {
        $this->automata = $automata;
        $this->start = $start;
    }

    /**
     * @return Automata
     */
    public function getAutomata(): Automata
    {
        return $this->automata;
    }

    /**
     * @return string
     */
    public function getStart(): string
    {
        return $this->start;
    }

    /**
     * @inheritDoc
     */
    public function jsonSerialize()
    {
        return array("ParserConfig" => get_object_vars($this));
    }

    public function __toString()
    {
        return json_encode($this);
    }
}
