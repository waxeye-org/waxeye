<?php
spl_autoload_register(function ($class_name) {
    include "../../php/" . $class_name . ".php";
});

include "calc/CalcParser.php";

$parser = new CalcParser();
$result = $parser->parse("3456*3");
printf("%s\n", $result);
