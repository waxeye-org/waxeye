<?php


use parser\config\Automata;
use parser\config\Automaton;
use parser\config\ParserConfig;
use parser\expression\Expression;
use parser\expression\Expressions;
use parser\NonTerminalMode;
use parser\Parser;

class WaxeyeParser2 extends Parser
{
    public function __construct()
    {
        $a = new Automaton("cwrFile", NonTerminalMode::NORMAL, Expression::AltExpression(Expression::NonTerminalExpression("hdr"), Expression::NonTerminalExpression("trl")));
        $b = new Automaton("hdr", NonTerminalMode::NORMAL, Expression::CharExpression("X"));
        $c = new Automaton("trl", NonTerminalMode::NORMAL, Expression::CharExpression("Y"));

        $automata = new Automata();
        $automata[$a->getKey()] = $a;
        $automata[$b->getKey()] = $b;
        $automata[$c->getKey()] = $c;

        $config = new ParserConfig($automata, $a->getKey());

        printf("%s\n", $config);

        parent::__construct($config);
    }
}
