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
 * A session created at the remote source and its refreshed connection snapshot.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AhpConnectionCreateSessionResult(
    /** Remote AHP session URI. */
    @JsonProperty("uri") String uri,
    /** Connection state after the source refreshed its catalog. */
    @JsonProperty("stateJson") String stateJson
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpConnectionCreateSessionResult {}

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
        private String uri;
        private String stateJson;

        /**
         * Sets the {@code uri} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder uri(String value) {
            this.uri = value;
            return this;
        }

        /**
         * Sets the {@code stateJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder stateJson(String value) {
            this.stateJson = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpConnectionCreateSessionResult build() {
            if (uri == null) throw new IllegalStateException("Missing required property: uri");
            if (stateJson == null) throw new IllegalStateException("Missing required property: stateJson");
            return new AhpConnectionCreateSessionResult(uri, stateJson);
        }
    }
}
