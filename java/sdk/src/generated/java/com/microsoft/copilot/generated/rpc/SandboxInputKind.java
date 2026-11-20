/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Normalization rule for a sandbox setting.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum SandboxInputKind {
    /** The {@code hostPattern} variant. */
    HOSTPATTERN("hostPattern"),
    /** The {@code proxyUrl} variant. */
    PROXYURL("proxyUrl"),
    /** The {@code proxyResolutionUrl} variant. */
    PROXYRESOLUTIONURL("proxyResolutionUrl");

    private final String value;
    SandboxInputKind(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static SandboxInputKind fromValue(String value) {
        for (SandboxInputKind v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown SandboxInputKind value: " + value);
    }
}
