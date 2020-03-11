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
        $a = new Automaton("A", NonTerminalMode::NORMAL, Expression::AltExpression(Expression::NonTerminalExpression("B"), Expression::NonTerminalExpression("C")));
        $b = new Automaton("B", NonTerminalMode::NORMAL, Expression::CharExpression("X"));
        $c = new Automaton("C", NonTerminalMode::NORMAL, Expression::CharExpression("Y"));

        $automata = new Automata();
        $automata["A"] = $a;
        $automata["B"] = $b;
        $automata["C"] = $c;

        $config = new ParserConfig($automata, "A");

        printf("%s\n", $config);

        parent::__construct($config);
    }
}
