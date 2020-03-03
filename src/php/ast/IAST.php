<?php


namespace ast;


use SplDoublyLinkedList;

interface IAST
{
    public function getType(): string;

    public function getValue(): string;

    public function getPosition(): int;

    public function getChildren(): SplDoublyLinkedList;
}
