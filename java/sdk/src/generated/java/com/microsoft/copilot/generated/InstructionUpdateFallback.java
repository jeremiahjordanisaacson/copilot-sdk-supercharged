/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import javax.annotation.processing.Generated;

/**
 * Deterministic behavior when the active provider cannot represent the requested update
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum InstructionUpdateFallback {
    /** The {@code rebuildSystemPrompt} variant. */
    REBUILDSYSTEMPROMPT("rebuildSystemPrompt"),
    /** The {@code reject} variant. */
    REJECT("reject");

    private final String value;
    InstructionUpdateFallback(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static InstructionUpdateFallback fromValue(String value) {
        for (InstructionUpdateFallback v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown InstructionUpdateFallback value: " + value);
    }
}
