<?php

use parser\error\ParseException;

spl_autoload_register(function ($class_name) {
    include "../../../php/" . $class_name . ".php";
});

include "JSONParser.php";

$file = fopen("example.json", "r");
$input = "";
while (($buffer = fgets($file)) != false) {
    $input .= $buffer;
}

$parser = new JSONParser();
try {
    $result = $parser->parse($input);
    file_put_contents("result.json", $result);
} catch (ParseException $exception) {
    printf("parse error at position %s, expected: %s, read: %s\n", $exception->getParseError()->getPosition(), $exception->getParseError()->getChars(), $input[$exception->getParseError()->getPosition()]);
}
