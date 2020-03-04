<?php


namespace ast;


use JsonSerializable;

interface IAST extends JsonSerializable
{
    public function getType(): string;

    public function getValue(): string;

    public function getPosition(): int;

    public function getChildren(): IASTs;

    public function setChildren(IASTs $children);
}
