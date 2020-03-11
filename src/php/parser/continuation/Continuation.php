<?php

namespace parser\continuation;

use JsonSerializable;

abstract class Continuation implements JsonSerializable
{
    private int $type;

    /**
     * Continuation constructor.
     * @param int $type
     */
    public function __construct(int $type)
    {
        $this->type = $type;
    }

    /**
     * @return int
     */
    public function getType(): int
    {
        return $this->type;
    }

    public function jsonSerialize()
    {
        return get_object_vars($this);
    }
}
