/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Outcome of an explicit cloud memory import.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum MemoryImportStatus {
    /** The {@code imported} variant. */
    IMPORTED("imported"),
    /** The {@code conflict} variant. */
    CONFLICT("conflict"),
    /** The {@code disabled} variant. */
    DISABLED("disabled"),
    /** The {@code unavailable} variant. */
    UNAVAILABLE("unavailable"),
    /** The {@code failed} variant. */
    FAILED("failed");

    private final String value;
    MemoryImportStatus(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static MemoryImportStatus fromValue(String value) {
        for (MemoryImportStatus v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown MemoryImportStatus value: " + value);
    }
}
