<?php

use parser\config\Automata;
use parser\config\Automaton;
use parser\config\ParserConfig;
use parser\expression\Expression;
use parser\expression\Expressions;
use parser\NonTerminalMode;
use parser\Parser;

class CalcParser extends Parser
{
    public function __construct()
{
        $automata = new Automata();

$automata["calc"] = new Automaton("calc", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::NonTerminalExpression("ws"), Expression::NonTerminalExpression("sum")));
    $automata["sum"] = new Automaton("sum", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::NonTerminalExpression("prod"), Expression::StarExpression(Expression::SeqExpression(Expression::CharClassExpression(array('+', '-'), array(), array()), Expression::NonTerminalExpression("ws"), Expression::NonTerminalExpression("prod")))));
    $automata["prod"] = new Automaton("prod", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::NonTerminalExpression("unary"), Expression::StarExpression(Expression::SeqExpression(Expression::CharClassExpression(array('*', '/'), array(), array()), Expression::NonTerminalExpression("ws"), Expression::NonTerminalExpression("unary")))));
    $automata["unary"] = new Automaton("unary", NonTerminalMode::PRUNING, Expression::AltExpression(Expression::SeqExpression(Expression::CharExpression('-'), Expression::NonTerminalExpression("ws"), Expression::NonTerminalExpression("unary")), Expression::SeqExpression(Expression::VoidExpression(Expression::CharExpression('(')), Expression::NonTerminalExpression("ws"), Expression::NonTerminalExpression("sum"), Expression::VoidExpression(Expression::CharExpression(')')), Expression::NonTerminalExpression("ws")), Expression::NonTerminalExpression("num")));
    $automata["num"] = new Automaton("num", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::PlusExpression(Expression::CharClassExpression(array(), array('0'), array('9'))), Expression::OptExpression(Expression::SeqExpression(Expression::CharExpression('.'), Expression::PlusExpression(Expression::CharClassExpression(array(), array('0'), array('9'))))), Expression::NonTerminalExpression("ws")));
    $automata["ws"] = new Automaton("ws", NonTerminalMode::VOIDING, Expression::StarExpression(Expression::CharClassExpression(array('\r', ' '), array('\t'), array('\n'))));
$config = new ParserConfig($automata, "calc");
parent::__construct($config);
}
}
