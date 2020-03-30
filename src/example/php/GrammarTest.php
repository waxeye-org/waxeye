<?php
spl_autoload_register(function ($class_name) {
    include "../../php/" . $class_name . ".php";
});

include "GenParser.php";

$parser = new GenParser();
$result = $parser->parse("3*3");
printf("%s\n", $result);
