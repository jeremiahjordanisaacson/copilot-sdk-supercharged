/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Final acceptance-only upload outcome.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum FeedbackUploadStatus {
    /** The {@code accepted} variant. */
    ACCEPTED("accepted"),
    /** The {@code failed} variant. */
    FAILED("failed"),
    /** The {@code canceled} variant. */
    CANCELED("canceled");

    private final String value;
    FeedbackUploadStatus(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static FeedbackUploadStatus fromValue(String value) {
        for (FeedbackUploadStatus v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown FeedbackUploadStatus value: " + value);
    }
}
