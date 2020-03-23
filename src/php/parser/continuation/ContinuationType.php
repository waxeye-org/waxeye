<?php

namespace parser\continuation;
interface ContinuationType
{
    const SEQ = "CONT_SEQ";
    const ALT = "CONT_ALT";
    const AND = "CONT_AND";
    const NOT = "CONT_NOT";
    const OPT = "CONT_OPT";
    const STAR = "CONT_STAR";
    const PLUS = "CONT_PLUS";
    const VOID = "CONT_VOID";
    const NT = "CONT_NT";
}
