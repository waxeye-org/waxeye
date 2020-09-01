<?php


namespace parser\error;


use JsonSerializable;

class RawError implements JsonSerializable
{
    private int $position;
    private array $nonTerminals;
    private MatchErrors $failedChars;
    private string $currentNonTerminal;

    /**
     * RawError constructor.
     * @param int $position
     * @param array $nonTerminals
     * @param MatchErrors $failedChars
     * @param string $currentNonTerminal
     */
    public function __construct(int $position, array $nonTerminals, MatchErrors $failedChars, string $currentNonTerminal)
    {
        $this->position = $position;
        $this->nonTerminals = $nonTerminals;
        $this->failedChars = $failedChars;
        $this->currentNonTerminal = $currentNonTerminal;
    }

    /**
     * @return array
     */
    public function getNonTerminals(): array
    {
        return $this->nonTerminals;
    }

    /**
     * @return MatchErrors
     */
    public function getFailedChars(): MatchErrors
    {
        return $this->failedChars;
    }


    /**
     * @return string
     */
    public function getCurrentNonTerminal(): string
    {
        return $this->currentNonTerminal;
    }

    /**
     * @return int
     */
    public function getPosition(): int
    {
        return $this->position;
    }

    public function toParseError(string $input): ParseError
    {
        $lineCol = $this->getLineAndColumn($this->position, $input);
        $line = $lineCol[0];
        $column = $lineCol[1];

        $uniqueNonTerminals = array();
        $seenNonTerminal = array();

        foreach ($this->nonTerminals as $nonTerminal) {
            if (array_key_exists($nonTerminal, $seenNonTerminal)) {
                continue;
            } else {
                $seenNonTerminal[$nonTerminal] = $nonTerminal;
                $uniqueNonTerminals[] = $nonTerminal;
            }
        }

        return new ParseError($this->position, $line, $column, $uniqueNonTerminals, $this->failedChars);
    }

    private function getLineAndColumn(int $position, string $input): array
    {
        $lineNumber = 1;
        $lineStartPos = 0;
        $newLinePos = strpos($input, "\n");

        while (($newLinePos !== false) && ($newLinePos < $this->position)) {
            $lineNumber++;
            $lineStartPos = $newLinePos + 1;
            $newLinePos = strpos($input, "\n", $lineStartPos);
        }

        return array($lineNumber, $position - $lineStartPos + 1);
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

    public static function updateError(RawError $rawError, int $position, MatchError $matchError): RawError
    {
        if (null === $rawError) {
            return new RawError(0, array(''), new MatchErrors($matchError), '');
        } else {
            if ($position > $rawError->getPosition()) {
                return new RawError($position, array($rawError->getCurrentNonTerminal()), new MatchErrors($matchError), $rawError->getCurrentNonTerminal());
            } elseif ($position === $rawError->getPosition()) {
                return new RawError($position, array_merge(array($rawError->getCurrentNonTerminal()), $rawError->getNonTerminals()), MatchErrors::matchErrors($matchError, $rawError->getFailedChars()), $rawError->getCurrentNonTerminal());
            } else {
                return new RawError($rawError->getPosition(), $rawError->getNonTerminals(), $rawError->getFailedChars(), $rawError->getCurrentNonTerminal());
            }
        }
    }
}
