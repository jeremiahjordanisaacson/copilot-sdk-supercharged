/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Opens a diagnostic log before outgoing authentication starts.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AhpConnectionLogsOpenParams(
    /** Absolute destination for the connection log. */
    @JsonProperty("path") String path,
    /** Serialized environment metadata for the log header. */
    @JsonProperty("environmentJson") String environmentJson,
    /** Initial values to redact, in addition to the runtime's live secret filter. */
    @JsonProperty("secretValues") List<String> secretValues
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpConnectionLogsOpenParams {}

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
        private String path;
        private String environmentJson;
        private List<String> secretValues;

        /**
         * Sets the {@code path} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder path(String value) {
            this.path = value;
            return this;
        }

        /**
         * Sets the {@code environmentJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder environmentJson(String value) {
            this.environmentJson = value;
            return this;
        }

        /**
         * Sets the {@code secretValues} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder secretValues(List<String> value) {
            this.secretValues = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpConnectionLogsOpenParams build() {
            if (path == null) throw new IllegalStateException("Missing required property: path");
            if (environmentJson == null) throw new IllegalStateException("Missing required property: environmentJson");
            if (secretValues == null) throw new IllegalStateException("Missing required property: secretValues");
            return new AhpConnectionLogsOpenParams(path, environmentJson, secretValues);
        }
    }
}
