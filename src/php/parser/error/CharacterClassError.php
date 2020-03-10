<?php


namespace parser\error;


use RuntimeException;

class CharacterClassError implements MatchError
{
    private array $charClasses;

    /**
     * CharacterClassError constructor.
     * @param array $charClasses
     */
    public function __construct(array $charClasses)
    {
        $this->charClasses = $charClasses;
    }


    public function toGrammarString(): string
    {
        throw new RuntimeException("Class " + self::class + "->toGrammarString not implemented yet!");

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
}
