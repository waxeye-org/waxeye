<?php

use parser\error\ParseException;

spl_autoload_register(function ($class_name) {
    include "../../../php/" . $class_name . ".php";
});

include "WaxeyeGrammarParser.php";

$files = array();
$files[] = "../../../../grammars/calc.waxeye";
$files[] = "../../../../grammars/json.waxeye";
$files[] = "../../../../grammars/num.waxeye";
$files[] = "../../../../grammars/regexp.waxeye";
$files[] = "../../../../grammars/templ.waxeye";

$parser = new WaxeyeGrammarParser();

foreach ($files as $file) {
    try {
        $input = file_get_contents($file);
        $result = $parser->parse($input);
        printf("%s: %s\n", $file, $result);
    } catch (ParseException $exception) {
        printf("%s: parse error at position %s (line %s, col %s), expected: %s, read: %s\n", $file, $exception->getParseError()->getPosition(), $exception->getParseError()->getLine(), $exception->getParseError()->getColumn(), $exception->getParseError()->getChars(), $input[$exception->getParseError()->getPosition()]);
    }
}


