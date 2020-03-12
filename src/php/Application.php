<?php


spl_autoload_register(function ($class_name) {
    include $class_name . ".php";
});

$parser = new WaxeyeParser2();
$result = $parser->parse("X");
printf($result);



//$parser = new WaxeyeParser();
//$result = $parser->parse("HDRPB512590964");//KICK  THE FLAME MUSIKVERLAG, RAJK BARTHEL    01.102019112512513620191125               ");
//printf("%s\n", $result);

