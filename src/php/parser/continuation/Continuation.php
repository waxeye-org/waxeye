<?php

namespace parser\continuation;

use JsonSerializable;

abstract class Continuation implements JsonSerializable
{
    protected string $type;

    /**
     * Continuation constructor.
     * @param string $type
     */
    public function __construct(string $type)
    {
        $this->type = $type;
    }

    /**
     * @return string
     */
    public function getType(): string
    {
        return $this->type;
    }


}
