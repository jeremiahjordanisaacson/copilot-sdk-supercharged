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
 * A read-only host projection for clients with synchronous render paths.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AhpSessionProjectionResult(
    /** Monotonic revision within this attachment. */
    @JsonProperty("revision") Long revision,
    /** Serialized collaboration, approval, and pending-request snapshot. */
    @JsonProperty("snapshotJson") String snapshotJson,
    /** Whether this attachment has closed. */
    @JsonProperty("closed") Boolean closed
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpSessionProjectionResult {}

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
        private Long revision;
        private String snapshotJson;
        private Boolean closed;

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
         * Sets the {@code snapshotJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder snapshotJson(String value) {
            this.snapshotJson = value;
            return this;
        }

        /**
         * Sets the {@code closed} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder closed(Boolean value) {
            this.closed = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpSessionProjectionResult build() {
            if (revision == null) throw new IllegalStateException("Missing required property: revision");
            if (snapshotJson == null) throw new IllegalStateException("Missing required property: snapshotJson");
            if (closed == null) throw new IllegalStateException("Missing required property: closed");
            return new AhpSessionProjectionResult(revision, snapshotJson, closed);
        }
    }
}
