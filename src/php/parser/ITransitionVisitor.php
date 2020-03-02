<?php


namespace parser;


interface ITransitionVisitor
{
    public function visitAutomationTransition(AutomatonTransition $automatonTransition);
    public function visitCharTransition(CharTransition $charTransition);
}
