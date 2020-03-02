<?php


namespace parser;


interface ITransition
{
    public function acceptVisitor(ITransitionVisitor $visitor);
}
