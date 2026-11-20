/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import javax.annotation.processing.Generated;

/**
 * Connection-owned upload state retained until host shutdown.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record FeedbackUploadSnapshot(
    /** Opaque identifier scoped to the launch-authorized owner. */
    @JsonProperty("operationId") String operationId,
    /** Monotonically increasing revision of the local snapshot. */
    @JsonProperty("revision") Long revision,
    /** Current local upload progress. */
    @JsonProperty("progress") FeedbackUploadProgress progress,
    /** Present exactly when the operation is terminal. */
    @JsonProperty("outcome") FeedbackUploadOutcome outcome
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    FeedbackUploadSnapshot {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    public static final class Builder {
        private Builder() {}
        private String operationId;
        private Long revision;
        private FeedbackUploadProgress progress;
        private FeedbackUploadOutcome outcome;

        /**
         * Sets the {@code operationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder operationId(String value) {
            this.operationId = value;
            return this;
        }

        /**
         * Sets the {@code revision} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder revision(Long value) {
            this.revision = value;
            return this;
        }

        /**
         * Sets the {@code progress} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder progress(FeedbackUploadProgress value) {
            this.progress = value;
            return this;
        }

        /**
         * Sets the {@code outcome} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder outcome(FeedbackUploadOutcome value) {
            this.outcome = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public FeedbackUploadSnapshot build() {
            if (operationId == null) throw new IllegalStateException("Missing required property: operationId");
            if (revision == null) throw new IllegalStateException("Missing required property: revision");
            if (progress == null) throw new IllegalStateException("Missing required property: progress");
            return new FeedbackUploadSnapshot(operationId, revision, progress, outcome);
        }
    }
}
