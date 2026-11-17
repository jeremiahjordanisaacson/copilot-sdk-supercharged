/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import javax.annotation.processing.Generated;

/**
 * How long an instruction update remains authoritative
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum InstructionUpdateLifetime {
    /** The {@code persistent} variant. */
    PERSISTENT("persistent"),
    /** The {@code turn} variant. */
    TURN("turn");

    private final String value;
    InstructionUpdateLifetime(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static InstructionUpdateLifetime fromValue(String value) {
        for (InstructionUpdateLifetime v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown InstructionUpdateLifetime value: " + value);
    }
}
