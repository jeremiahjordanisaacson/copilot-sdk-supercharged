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
 * Terminal outcome. The saved local file is retained on every outcome.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record FeedbackUploadOutcome(
    /** Acceptance, failure, or local cancellation. */
    @JsonProperty("status") FeedbackUploadStatus status,
    /** Acknowledged archive bytes. */
    @JsonProperty("uploadedBytes") Long uploadedBytes,
    /** Total compressed archive size, or zero when unavailable. */
    @JsonProperty("totalBytes") Long totalBytes,
    /** Fixed safe error text, excluding raw service responses and credentials. */
    @JsonProperty("errorMessage") String errorMessage,
    /** HTTP status associated with a failure when available. */
    @JsonProperty("httpStatus") Long httpStatus
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    FeedbackUploadOutcome {}

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
        private FeedbackUploadStatus status;
        private Long uploadedBytes;
        private Long totalBytes;
        private String errorMessage;
        private Long httpStatus;

        /**
         * Sets the {@code status} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder status(FeedbackUploadStatus value) {
            this.status = value;
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
         * Sets the {@code errorMessage} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder errorMessage(String value) {
            this.errorMessage = value;
            return this;
        }

        /**
         * Sets the {@code httpStatus} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder httpStatus(Long value) {
            this.httpStatus = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public FeedbackUploadOutcome build() {
            if (status == null) throw new IllegalStateException("Missing required property: status");
            if (uploadedBytes == null) throw new IllegalStateException("Missing required property: uploadedBytes");
            if (totalBytes == null) throw new IllegalStateException("Missing required property: totalBytes");
            return new FeedbackUploadOutcome(status, uploadedBytes, totalBytes, errorMessage, httpStatus);
        }
    }
}
