/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: session-events.schema.json

package com.microsoft.copilot.generated;

import javax.annotation.processing.Generated;

/**
 * Data loss prevention (DLP) block reported by the model provider's exact error code. Resending the same conversation is blocked again; rewind to the user.message whose messageId equals the error's originatingMessageId.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public enum DlpBlockKind {
    /** The {@code prompt_dlp_blocked} variant. */
    PROMPT_DLP_BLOCKED("prompt_dlp_blocked"),
    /** The {@code inline_restricted} variant. */
    INLINE_RESTRICTED("inline_restricted");

    private final String value;
    DlpBlockKind(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static DlpBlockKind fromValue(String value) {
        for (DlpBlockKind v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown DlpBlockKind value: " + value);
    }
}
