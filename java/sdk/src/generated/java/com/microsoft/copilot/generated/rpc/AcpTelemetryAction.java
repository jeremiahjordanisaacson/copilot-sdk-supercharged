/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * The transaction step requested from the embedding host.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum AcpTelemetryAction {
    /** The {@code prepare} variant. */
    PREPARE("prepare"),
    /** The {@code apply} variant. */
    APPLY("apply"),
    /** The {@code commit} variant. */
    COMMIT("commit"),
    /** The {@code rollback} variant. */
    ROLLBACK("rollback");

    private final String value;
    AcpTelemetryAction(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static AcpTelemetryAction fromValue(String value) {
        for (AcpTelemetryAction v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown AcpTelemetryAction value: " + value);
    }
}
