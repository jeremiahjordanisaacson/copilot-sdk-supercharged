<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot;

/**
 * SDK Protocol Version
 *
 * This must match the version expected by the copilot-agent-runtime server.
 * Code generated from sdk-protocol-version.json. DO NOT EDIT.
 */
final class SdkProtocolVersion
{
    /**
     * The SDK protocol version number.
     */
    public const VERSION = 2;

    /**
     * Gets the SDK protocol version.
     */
    public static function get(): int
    {
        return self::VERSION;
    }
}
