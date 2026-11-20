/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import javax.annotation.processing.Generated;

/**
 * Current phase of the archive upload.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
enum FeedbackUploadPhase {
    /** The {@code created} variant. */
    CREATED("created"),
    /** The {@code packaging} variant. */
    PACKAGING("packaging"),
    /** The {@code creating_job} variant. */
    CREATING_JOB("creating_job"),
    /** The {@code staging_blocks} variant. */
    STAGING_BLOCKS("staging_blocks"),
    /** The {@code completing_job} variant. */
    COMPLETING_JOB("completing_job"),
    /** The {@code canceling} variant. */
    CANCELING("canceling"),
    /** The {@code accepted} variant. */
    ACCEPTED("accepted"),
    /** The {@code failed} variant. */
    FAILED("failed"),
    /** The {@code canceled} variant. */
    CANCELED("canceled");

    private final String value;
    FeedbackUploadPhase(String value) { this.value = value; }
    @com.fasterxml.jackson.annotation.JsonValue
    public String getValue() { return value; }
    @com.fasterxml.jackson.annotation.JsonCreator
    public static FeedbackUploadPhase fromValue(String value) {
        for (FeedbackUploadPhase v : values()) {
            if (v.value.equals(value)) return v;
        }
        throw new IllegalArgumentException("Unknown FeedbackUploadPhase value: " + value);
    }
}
