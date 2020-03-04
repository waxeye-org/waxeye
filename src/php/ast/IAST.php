<?php


namespace ast;


use JsonSerializable;
use SplDoublyLinkedList;

interface IAST extends JsonSerializable
{
    public function getType(): string;

    public function getValue(): string;

    public function getPosition(): int;

    public function getChildren(): SplDoublyLinkedList;
}
