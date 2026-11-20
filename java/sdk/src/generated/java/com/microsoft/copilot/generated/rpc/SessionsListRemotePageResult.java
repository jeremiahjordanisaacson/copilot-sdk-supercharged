/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * One step of a Mission Control task listing: up to one active page and one archived page.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionsListRemotePageResult(
    /** Remote sessions from this step. Tasks can repeat across steps; callers deduplicate by session ID. */
    @JsonProperty("sessions") List<RemoteSessionPageEntry> sessions,
    /** Continuation for the next step. Omitted when the listing is complete or the step failed. */
    @JsonProperty("continuation") String continuation,
    /** Raw Mission Control records accepted this step, before deduplication or filtering. */
    @JsonProperty("recordCount") Long recordCount,
    /** Milliseconds Mission Control asked callers to wait before listing again, from a `Retry-After` response header. */
    @JsonProperty("retryAfterMs") Long retryAfterMs,
    /** Set when the step failed; `sessions` is then empty. */
    @JsonProperty("failure") RemoteSessionPageFailure failure
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionsListRemotePageResult {}

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
        private List<RemoteSessionPageEntry> sessions;
        private String continuation;
        private Long recordCount;
        private Long retryAfterMs;
        private RemoteSessionPageFailure failure;

        /**
         * Sets the {@code sessions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessions(List<RemoteSessionPageEntry> value) {
            this.sessions = value;
            return this;
        }

        /**
         * Sets the {@code continuation} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder continuation(String value) {
            this.continuation = value;
            return this;
        }

        /**
         * Sets the {@code recordCount} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder recordCount(Long value) {
            this.recordCount = value;
            return this;
        }

        /**
         * Sets the {@code retryAfterMs} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder retryAfterMs(Long value) {
            this.retryAfterMs = value;
            return this;
        }

        /**
         * Sets the {@code failure} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder failure(RemoteSessionPageFailure value) {
            this.failure = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionsListRemotePageResult build() {
            if (sessions == null) throw new IllegalStateException("Missing required property: sessions");
            if (recordCount == null) throw new IllegalStateException("Missing required property: recordCount");
            return new SessionsListRemotePageResult(sessions, continuation, recordCount, retryAfterMs, failure);
        }
    }
}
