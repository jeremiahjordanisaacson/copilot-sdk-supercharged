/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Verbosity for a runtime-owned file logger.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum FileLogLevel {
    /** The {@code none} variant. */
    NONE("none"),
    /** The {@code error} variant. */
    ERROR("error"),
    /** The {@code warning} variant. */
    WARNING("warning"),
    /** The {@code info} variant. */
    INFO("info"),
    /** The {@code debug} variant. */
    DEBUG("debug");

    private final String value;
    FileLogLevel(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static FileLogLevel fromValue(String value) {
        for (FileLogLevel v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown FileLogLevel value: " + value);
    }
}
