/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.microsoft.copilot.CopilotExperimental;
import javax.annotation.processing.Generated;

/**
 * An original-host persistence barrier stage.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum SessionPersistenceBarrierPhase {
    /** The {@code prepare} variant. */
    PREPARE("prepare"),
    /** The {@code writeCallbacks} variant. */
    WRITECALLBACKS("writeCallbacks"),
    /** The {@code complete} variant. */
    COMPLETE("complete"),
    /** The {@code tracking} variant. */
    TRACKING("tracking"),
    /** The {@code cancel} variant. */
    CANCEL("cancel");

    private final String value;
    SessionPersistenceBarrierPhase(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static SessionPersistenceBarrierPhase fromValue(String value) {
        for (SessionPersistenceBarrierPhase v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown SessionPersistenceBarrierPhase value: " + value);
    }
}
