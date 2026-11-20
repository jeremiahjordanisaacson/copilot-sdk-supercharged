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
 * One Mission Control task shaped as remote session metadata, with the host identity hints needed to resolve it.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record RemoteSessionPageEntry(
    /** Remote session metadata for the task. */
    @JsonProperty("metadata") RemoteSessionMetadataValue metadata,
    /** Kind of the task's current environment, when Mission Control reports one. */
    @JsonProperty("environmentKind") String environmentKind,
    /** ID of the task's current environment, when Mission Control reports one. */
    @JsonProperty("environmentId") String environmentId,
    /** Session ID on the host running the task's latest session, when known. */
    @JsonProperty("hostSessionId") String hostSessionId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    RemoteSessionPageEntry {}

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
        private RemoteSessionMetadataValue metadata;
        private String environmentKind;
        private String environmentId;
        private String hostSessionId;

        /**
         * Sets the {@code metadata} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder metadata(RemoteSessionMetadataValue value) {
            this.metadata = value;
            return this;
        }

        /**
         * Sets the {@code environmentKind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder environmentKind(String value) {
            this.environmentKind = value;
            return this;
        }

        /**
         * Sets the {@code environmentId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder environmentId(String value) {
            this.environmentId = value;
            return this;
        }

        /**
         * Sets the {@code hostSessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hostSessionId(String value) {
            this.hostSessionId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteSessionPageEntry build() {
            if (metadata == null) throw new IllegalStateException("Missing required property: metadata");
            return new RemoteSessionPageEntry(metadata, environmentKind, environmentId, hostSessionId);
        }
    }
}
