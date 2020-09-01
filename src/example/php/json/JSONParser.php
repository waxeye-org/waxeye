<?php

use parser\config\Automata;
use parser\config\Automaton;
use parser\config\ParserConfig;
use parser\expression\Expression;
use parser\expression\Expressions;
use parser\NonTerminalMode;
use parser\Parser;

class JSONParser extends Parser
{
    public function __construct()
    {
        $automata = new Automata();

        $automata["Json"] = new Automaton("Json", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::NonTerminalExpression("Ws"), Expression::NonTerminalExpression("Value")));
        $automata["Value"] = new Automaton("Value", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::AltExpression(Expression::NonTerminalExpression("Object"), Expression::NonTerminalExpression("Array"), Expression::NonTerminalExpression("Number"), Expression::NonTerminalExpression("String"), Expression::NonTerminalExpression("Literal")), Expression::NonTerminalExpression("Ws")));
        $automata["Object"] = new Automaton("Object", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::VoidExpression(Expression::CharExpression(123)), Expression::NonTerminalExpression("Ws"), Expression::OptExpression(Expression::SeqExpression(Expression::NonTerminalExpression("Member"), Expression::StarExpression(Expression::SeqExpression(Expression::NonTerminalExpression("Com"), Expression::NonTerminalExpression("Member"))))), Expression::VoidExpression(Expression::CharExpression(125))));
        $automata["Member"] = new Automaton("Member", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::NonTerminalExpression("String"), Expression::NonTerminalExpression("Ws"), Expression::NonTerminalExpression("Col"), Expression::NonTerminalExpression("Value")));
        $automata["Array"] = new Automaton("Array", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::VoidExpression(Expression::CharExpression(91)), Expression::NonTerminalExpression("Ws"), Expression::OptExpression(Expression::SeqExpression(Expression::NonTerminalExpression("Value"), Expression::StarExpression(Expression::SeqExpression(Expression::NonTerminalExpression("Com"), Expression::NonTerminalExpression("Value"))))), Expression::VoidExpression(Expression::CharExpression(93))));
        $automata["Number"] = new Automaton("Number", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::OptExpression(Expression::CharExpression(45)), Expression::AltExpression(Expression::CharExpression(48), Expression::SeqExpression(Expression::CharClassExpression(array(), array(49), array(57)), Expression::StarExpression(Expression::CharClassExpression(array(), array(48), array(57))))), Expression::OptExpression(Expression::SeqExpression(Expression::CharExpression(46), Expression::PlusExpression(Expression::CharClassExpression(array(), array(48), array(57))))), Expression::OptExpression(Expression::SeqExpression(Expression::CharClassExpression(array(69, 101), array(), array()), Expression::OptExpression(Expression::CharClassExpression(array(43, 45), array(), array())), Expression::PlusExpression(Expression::CharClassExpression(array(), array(48), array(57)))))));
        $automata["String"] = new Automaton("String", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::VoidExpression(Expression::CharExpression(34)), Expression::StarExpression(Expression::AltExpression(Expression::SeqExpression(Expression::VoidExpression(Expression::CharExpression(92)), Expression::NonTerminalExpression("Escaped")), Expression::SeqExpression(Expression::NotExpression(Expression::CharExpression(92)), Expression::NotExpression(Expression::CharExpression(34)), Expression::AnyCharExpression()))), Expression::VoidExpression(Expression::CharExpression(34))));
        $automata["Escaped"] = new Automaton("Escaped", NonTerminalMode::NORMAL, Expression::AltExpression(Expression::SeqExpression(Expression::CharExpression(117), Expression::CharClassExpression(array(), array(48, 65, 97), array(57, 70, 102)), Expression::CharClassExpression(array(), array(48, 65, 97), array(57, 70, 102)), Expression::CharClassExpression(array(), array(48, 65, 97), array(57, 70, 102)), Expression::CharClassExpression(array(), array(48, 65, 97), array(57, 70, 102))), Expression::CharClassExpression(array(34, 47, 92, 98, 102, 110, 114, 116), array(), array())));
        $automata["Literal"] = new Automaton("Literal", NonTerminalMode::NORMAL, Expression::AltExpression(Expression::SeqExpression(Expression::CharExpression(116), Expression::CharExpression(114), Expression::CharExpression(117), Expression::CharExpression(101)), Expression::SeqExpression(Expression::CharExpression(102), Expression::CharExpression(97), Expression::CharExpression(108), Expression::CharExpression(115), Expression::CharExpression(101)), Expression::SeqExpression(Expression::CharExpression(110), Expression::CharExpression(117), Expression::CharExpression(108), Expression::CharExpression(108))));
        $automata["Ws"] = new Automaton("Ws", NonTerminalMode::VOIDING, Expression::StarExpression(Expression::CharClassExpression(array(13, 32), array(9), array(10))));
        $automata["Com"] = new Automaton("Com", NonTerminalMode::VOIDING, Expression::SeqExpression(Expression::CharExpression(44), Expression::NonTerminalExpression("Ws")));
        $automata["Col"] = new Automaton("Col", NonTerminalMode::VOIDING, Expression::SeqExpression(Expression::CharExpression(58), Expression::NonTerminalExpression("Ws")));
        $config = new ParserConfig($automata, "Json");
        parent::__construct($config);
    }
}
