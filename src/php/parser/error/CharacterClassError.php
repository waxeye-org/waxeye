<?php


namespace parser\error;


use JsonSerializable;
use parser\expression\CharClassExpression;
use RuntimeException;

class CharacterClassError implements MatchError, JsonSerializable
{
    private CharClassExpression $charClass;

    /**
     * CharacterClassError constructor.
     * @param CharClassExpression $charClass
     */
    public function __construct(CharClassExpression $charClass)
    {
        $this->charClass = $charClass;
    }


    public function toGrammarString(): string
    {
        throw new RuntimeException("Class " . self::class . "->toGrammarString not implemented yet!");

        /*
         *   constructor(public charClasses: Array<number|[number, number]>) {}

  public toGrammarString() {
    return `[${
        this.charClasses
            .map((charClass) => {
              return JSON
                  .stringify(
                      typeof charClass === 'number' ?
                          String.fromCodePoint(charClass) :
                          `${String.fromCodePoint(charClass[0])}-${
                              String.fromCodePoint(charClass[1])}`)
                  .slice(1, -1);
            })
            .join('')}]`;
         */
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
}
