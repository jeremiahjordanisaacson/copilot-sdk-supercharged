/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Operation applied by the runtime logger, including its filtering and category formatting.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum FileLogOperation {
    /** The {@code log} variant. */
    LOG("log"),
    /** The {@code debug} variant. */
    DEBUG("debug"),
    /** The {@code info} variant. */
    INFO("info"),
    /** The {@code notice} variant. */
    NOTICE("notice"),
    /** The {@code warning} variant. */
    WARNING("warning"),
    /** The {@code error} variant. */
    ERROR("error"),
    /** The {@code startGroup} variant. */
    STARTGROUP("startGroup"),
    /** The {@code endGroup} variant. */
    ENDGROUP("endGroup");

    private final String value;
    FileLogOperation(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static FileLogOperation fromValue(String value) {
        for (FileLogOperation v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown FileLogOperation value: " + value);
    }
}
