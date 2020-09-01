<?php
namespace parser\expression;


interface ExpressionType
{
    const NT = "NT";
    const ALT = "ALT";
    const SEQ = "SEQ";
    const PLUS = "PLUS";
    const STAR = "STAR";
    const OPT = "OPT";
    const AND = "AND";
    const NOT = "NOT";
    const VOID = "VOID";
    const ANY_CHAR = "ANY_CHAR";
    const CHAR = "CHAR";
    const CHAR_CLASS = "CHAR_CLASS";
}
