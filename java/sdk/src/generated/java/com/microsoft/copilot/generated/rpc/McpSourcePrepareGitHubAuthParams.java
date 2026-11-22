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
 * Explicit host approval for account-authenticated MCP endpoints.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpSourcePrepareGitHubAuthParams(
    /** Explicit server-name and exact-origin approvals from the host command line. */
    @JsonProperty("specifications") List<String> specifications,
    /** Additional MCP configuration before approved servers leave ordinary configuration. */
    @JsonProperty("additionalConfigJson") String additionalConfigJson
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpSourcePrepareGitHubAuthParams {}

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
        private List<String> specifications;
        private String additionalConfigJson;

        /**
         * Sets the {@code specifications} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder specifications(List<String> value) {
            this.specifications = value;
            return this;
        }

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
        public McpSourcePrepareGitHubAuthParams build() {
            if (specifications == null) throw new IllegalStateException("Missing required property: specifications");
            if (additionalConfigJson == null) throw new IllegalStateException("Missing required property: additionalConfigJson");
            return new McpSourcePrepareGitHubAuthParams(specifications, additionalConfigJson);
        }
    }
}
