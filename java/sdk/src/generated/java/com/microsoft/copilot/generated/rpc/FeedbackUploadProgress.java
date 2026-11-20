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
 * Latest local progress snapshot for the saved archive.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record FeedbackUploadProgress(
    /** Current upload phase. */
    @JsonProperty("phase") FeedbackUploadPhase phase,
    /** Bytes acknowledged by the service, excluding repeated part retries. */
    @JsonProperty("uploadedBytes") Long uploadedBytes,
    /** Compressed archive byte count once the file has been hashed. */
    @JsonProperty("totalBytes") Long totalBytes,
    /** True when no further upload work remains. */
    @JsonProperty("terminal") Boolean terminal
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    FeedbackUploadProgress {}

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
        private FeedbackUploadPhase phase;
        private Long uploadedBytes;
        private Long totalBytes;
        private Boolean terminal;

        /**
         * Sets the {@code phase} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder phase(FeedbackUploadPhase value) {
            this.phase = value;
            return this;
        }

        /**
         * Sets the {@code uploadedBytes} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder uploadedBytes(Long value) {
            this.uploadedBytes = value;
            return this;
        }

        /**
         * Sets the {@code totalBytes} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder totalBytes(Long value) {
            this.totalBytes = value;
            return this;
        }

        /**
         * Sets the {@code terminal} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder terminal(Boolean value) {
            this.terminal = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public FeedbackUploadProgress build() {
            if (phase == null) throw new IllegalStateException("Missing required property: phase");
            if (uploadedBytes == null) throw new IllegalStateException("Missing required property: uploadedBytes");
            if (terminal == null) throw new IllegalStateException("Missing required property: terminal");
            return new FeedbackUploadProgress(phase, uploadedBytes, totalBytes, terminal);
        }
    }
}
