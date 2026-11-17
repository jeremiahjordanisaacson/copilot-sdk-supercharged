/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import javax.annotation.processing.Generated;

/**
 * Earliest legal insertion boundary for an instruction update
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum InstructionUpdateBoundary {
    /** The {@code afterToolResults} variant. */
    AFTERTOOLRESULTS("afterToolResults"),
    /** The {@code nextModelRequest} variant. */
    NEXTMODELREQUEST("nextModelRequest");

    private final String value;
    InstructionUpdateBoundary(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static InstructionUpdateBoundary fromValue(String value) {
        for (InstructionUpdateBoundary v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown InstructionUpdateBoundary value: " + value);
    }
}
