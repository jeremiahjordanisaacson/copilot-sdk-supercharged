/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Representation of content submitted to the runtime redaction filter.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum SecretFilterFormat {
    /** The {@code text} variant. */
    TEXT("text"),
    /** The {@code json} variant. */
    JSON("json");

    private final String value;
    SecretFilterFormat(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static SecretFilterFormat fromValue(String value) {
        for (SecretFilterFormat v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown SecretFilterFormat value: " + value);
    }
}
