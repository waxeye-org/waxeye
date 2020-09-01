<?php

use parser\error\ParseException;

spl_autoload_register(function ($class_name) {
    include "../../../php/" . $class_name . ".php";
});

include "GenParser.php";

$file = fopen("result.json", "w");
fwrite($file, "[");

$parser = new GenParser();
$automataCount = count($parser->getAutomata());
$percentiles = intval($automataCount / 20);
$i = 0;

try {
    printf("processing ... ");
    $strBuf = "";
    foreach ($parser->getAutomata() as $automaton) {
        $dec = intval(substr($automaton->getKey(), 3));
        $char = IntlChar::chr($dec);
        $result = $parser->parse($char, $automaton->getKey());

        $strBuf .= "{\"input\": \"" . IntlChar::charName($dec) . "\", \"strlen\":" . json_encode(strlen($char)) . ", \"result\": " . $result . "}";

        if ($i < $automataCount - 1) {
            $strBuf .= ",\n";
        } else {
            $strBuf .= "\n";
        }

        if ($i > 0 && $i % $percentiles === 0) {
            echo "\u{25a0}";
            fwrite($file, $strBuf);
            $strBuf = "";
        }

        $i++;
    }
    printf("finished\n");
} catch (ParseException $exception) {
    printf("parse error at position %s, expected: %s, read: %s\n", $exception->getParseError()->getPosition(), $exception->getParseError()->getChars(), $char[$exception->getParseError()->getPosition()]);
}

fwrite($file, "]");
fclose($file);
