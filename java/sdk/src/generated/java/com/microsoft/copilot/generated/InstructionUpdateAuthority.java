/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import javax.annotation.processing.Generated;

/**
 * Authority responsible for an instruction update
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum InstructionUpdateAuthority {
    /** The {@code runtime} variant. */
    RUNTIME("runtime"),
    /** The {@code host} variant. */
    HOST("host"),
    /** The {@code managedPolicy} variant. */
    MANAGEDPOLICY("managedPolicy");

    private final String value;
    InstructionUpdateAuthority(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static InstructionUpdateAuthority fromValue(String value) {
        for (InstructionUpdateAuthority v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown InstructionUpdateAuthority value: " + value);
    }
}
