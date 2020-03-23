<?php
spl_autoload_register(function ($class_name) {
    include $class_name . ".php";
});
$parser = new GenParser();
$result = $parser->parse("1+2");
printf($result);
