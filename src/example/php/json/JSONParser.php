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
        $automata["Object"] = new Automaton("Object", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::VoidExpression(Expression::CharExpression("{")), Expression::NonTerminalExpression("Ws"), Expression::OptExpression(Expression::SeqExpression(Expression::NonTerminalExpression("Member"), Expression::StarExpression(Expression::SeqExpression(Expression::NonTerminalExpression("Com"), Expression::NonTerminalExpression("Member"))))), Expression::VoidExpression(Expression::CharExpression("}"))));
        $automata["Member"] = new Automaton("Member", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::NonTerminalExpression("String"), Expression::NonTerminalExpression("Ws"), Expression::NonTerminalExpression("Col"), Expression::NonTerminalExpression("Value")));
        $automata["Array"] = new Automaton("Array", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::VoidExpression(Expression::CharExpression("[")), Expression::NonTerminalExpression("Ws"), Expression::OptExpression(Expression::SeqExpression(Expression::NonTerminalExpression("Value"), Expression::StarExpression(Expression::SeqExpression(Expression::NonTerminalExpression("Com"), Expression::NonTerminalExpression("Value"))))), Expression::VoidExpression(Expression::CharExpression("]"))));
        $automata["Number"] = new Automaton("Number", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::OptExpression(Expression::CharExpression("-")), Expression::AltExpression(Expression::CharExpression("0"), Expression::SeqExpression(Expression::CharClassExpression(array(), array("1"), array("9")), Expression::StarExpression(Expression::CharClassExpression(array(), array("0"), array("9"))))), Expression::OptExpression(Expression::SeqExpression(Expression::CharExpression("."), Expression::PlusExpression(Expression::CharClassExpression(array(), array("0"), array("9"))))), Expression::OptExpression(Expression::SeqExpression(Expression::CharClassExpression(array("E", "e"), array(), array()), Expression::OptExpression(Expression::CharClassExpression(array("+", "-"), array(), array())), Expression::PlusExpression(Expression::CharClassExpression(array(), array("0"), array("9")))))));
        $automata["String"] = new Automaton("String", NonTerminalMode::NORMAL, Expression::SeqExpression(Expression::VoidExpression(Expression::CharExpression("\"")), Expression::StarExpression(Expression::AltExpression(Expression::SeqExpression(Expression::VoidExpression(Expression::CharExpression("\\")), Expression::NonTerminalExpression("Escaped")), Expression::SeqExpression(Expression::NotExpression(Expression::CharExpression("\\")), Expression::NotExpression(Expression::CharExpression("\"")), Expression::AnyCharExpression()))), Expression::VoidExpression(Expression::CharExpression("\""))));
        $automata["Escaped"] = new Automaton("Escaped", NonTerminalMode::NORMAL, Expression::AltExpression(Expression::SeqExpression(Expression::CharExpression("u"), Expression::CharClassExpression(array(), array("0", "A", "a"), array("9", "F", "f")), Expression::CharClassExpression(array(), array("0", "A", "a"), array("9", "F", "f")), Expression::CharClassExpression(array(), array("0", "A", "a"), array("9", "F", "f")), Expression::CharClassExpression(array(), array("0", "A", "a"), array("9", "F", "f"))), Expression::CharClassExpression(array("\"", "/", "\\", "b", "f", "n", "r", "t"), array(), array())));
        $automata["Literal"] = new Automaton("Literal", NonTerminalMode::NORMAL, Expression::AltExpression(Expression::SeqExpression(Expression::CharExpression("t"), Expression::CharExpression("r"), Expression::CharExpression("u"), Expression::CharExpression("e")), Expression::SeqExpression(Expression::CharExpression("f"), Expression::CharExpression("a"), Expression::CharExpression("l"), Expression::CharExpression("s"), Expression::CharExpression("e")), Expression::SeqExpression(Expression::CharExpression("n"), Expression::CharExpression("u"), Expression::CharExpression("l"), Expression::CharExpression("l"))));
        $automata["Ws"] = new Automaton("Ws", NonTerminalMode::VOIDING, Expression::StarExpression(Expression::CharClassExpression(array("\r", " "), array("\t"), array("\n"))));
        $automata["Com"] = new Automaton("Com", NonTerminalMode::VOIDING, Expression::SeqExpression(Expression::CharExpression(","), Expression::NonTerminalExpression("Ws")));
        $automata["Col"] = new Automaton("Col", NonTerminalMode::VOIDING, Expression::SeqExpression(Expression::CharExpression(":"), Expression::NonTerminalExpression("Ws")));
        $config = new ParserConfig($automata, "Json");
        parent::__construct($config);
    }
}
