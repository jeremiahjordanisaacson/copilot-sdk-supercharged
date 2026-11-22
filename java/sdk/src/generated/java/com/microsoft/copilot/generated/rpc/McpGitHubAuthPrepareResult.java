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
 * Configuration after the original engine retains approved account-authenticated servers.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpGitHubAuthPrepareResult(
    /** Remaining ordinary MCP configuration. Approval grants never leave the engine. */
    @JsonProperty("additionalConfigJson") String additionalConfigJson
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpGitHubAuthPrepareResult {}

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
        private String additionalConfigJson;

        /**
         * Sets the {@code additionalConfigJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder additionalConfigJson(String value) {
            this.additionalConfigJson = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpGitHubAuthPrepareResult build() {
            if (additionalConfigJson == null) throw new IllegalStateException("Missing required property: additionalConfigJson");
            return new McpGitHubAuthPrepareResult(additionalConfigJson);
        }
    }
}
