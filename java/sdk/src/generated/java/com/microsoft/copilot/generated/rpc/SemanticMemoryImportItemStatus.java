/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Outcome for one validated semantic memory record.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum SemanticMemoryImportItemStatus {
    /** The {@code validated} variant. */
    VALIDATED("validated"),
    /** The {@code imported} variant. */
    IMPORTED("imported"),
    /** The {@code skipped} variant. */
    SKIPPED("skipped"),
    /** The {@code failed} variant. */
    FAILED("failed");

    private final String value;
    SemanticMemoryImportItemStatus(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static SemanticMemoryImportItemStatus fromValue(String value) {
        for (SemanticMemoryImportItemStatus v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown SemanticMemoryImportItemStatus value: " + value);
    }
}
