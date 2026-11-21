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
import javax.annotation.processing.Generated;

/**
 * Inspects extension directories contributed by a changed plugin under the selected session's activation and profile.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionPluginsInspectExtensionContributionsParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Plugin name or marketplace-qualified spec whose in-place change can require extension restarts. */
    @JsonProperty("changedPluginSpec") String changedPluginSpec
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionPluginsInspectExtensionContributionsParams {}

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
        private String sessionId;
        private String changedPluginSpec;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code changedPluginSpec} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder changedPluginSpec(String value) {
            this.changedPluginSpec = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionPluginsInspectExtensionContributionsParams build() {
            if (changedPluginSpec == null) throw new IllegalStateException("Missing required property: changedPluginSpec");
            return new SessionPluginsInspectExtensionContributionsParams(sessionId, changedPluginSpec);
        }
    }
}
