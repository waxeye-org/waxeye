<?php

use parser\ast\AST;
use parser\ast\Char;

spl_autoload_register(function ($class_name) {
    include "../../php/" . $class_name . ".php";
});

include "GenParser.php";

class Calculator
{
    private GenParser $parser;

    public function __construct()
    {
        $this->parser = new GenParser();
    }

    public function calc(string $input): float
    {
        printf("calculating %s\n", $input);
        $ast = $this->parser->parse($input);
        return $this->sum($ast->getChildren()[0]);
    }


    private function sum(AST $ast): float
    {
        $children = $ast->getChildren();
        $value = $this->prod($children[0]);

        for ($i = 1; $i < count($children); $i += 2) {
            $operator = Char::asCharAST($children[$i])->getValue();

            switch ($operator) {
                case "+":
                {
                    $value += $this->prod($children[$i + 1]);
                    break;
                }
                case "-":
                {
                    $value -= $this->prod($children[$i + 1]);
                    break;
                }
                default:
                {
                    throw new RuntimeException("Unsupported operator in $children[$i]");
                }
            }
        }

        return $value;
    }

    private function prod(AST $ast): float
    {
        $children = $ast->getChildren();
        $value = $this->unary($children[0]);

        for ($i = 1; $i < count($children); $i += 2) {
            $operator = Char::asCharAST($children[$i])->getValue();

            switch ($operator) {
                case "*":
                {
                    $value *= $this->unary($children[$i + 1]);
                    break;
                }
                case "/":
                {
                    $value /= $this->unary($children[$i + 1]);
                    break;
                }
                default:
                {
                    throw new RuntimeException("Unsupported operator in $children[$i]");
                }
            }
        }

        return $value;
    }

    private function unary(AST $unary): float
    {
        switch ($unary->getType()) {
            case "num":
            {
                return $unary->getChildrenAsString();
            }
            case "sum":
            {
                return $this->sum($unary);
            }
            default:
            {
                throw new RuntimeException("Not supported type in unary $unary");
            }
        }
    }
}

printf("input: ");
$line = trim(fgets(STDIN));

$calc = new Calculator();
$result = $calc->calc($line);
printf("result: %s\n", $result);
