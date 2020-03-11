<?php

namespace parser\continuation;
interface ContinuationType
{
    const SEQ = 1;
    const ALT = 2;
    const AND = 3;
    const NOT = 4;
    const OPT = 5;
    const STAR = 6;
    const PLUS = 7;
    const VOID = 8;
    const NT = 9;
}
