<?php
namespace parser\expression;


interface ExpressionType
{
    const NT = 1;
    const ALT = 2;
    const SEQ = 3;
    const PLUS = 4;
    const STAR = 5;
    const OPT = 6;
    const AND = 7;
    const NOT = 8;
    const VOID = 9;
    const ANY_CHAR = 10;
    const CHAR = 11;
    const CHAR_CLASS = 12;
}
