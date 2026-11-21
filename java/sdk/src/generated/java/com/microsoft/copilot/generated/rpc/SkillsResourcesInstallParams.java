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
 * Registers a skill directory or installs a local or HTTPS skill definition.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SkillsResourcesInstallParams(
    /** Host paths and working directory. */
    @JsonProperty("host") SkillResourceContext host,
    /** Local file, directory, or HTTPS source. */
    @JsonProperty("source") String source,
    /** Use the preferred project creation directory instead of personal storage. */
    @JsonProperty("project") Boolean project,
    /** User agent for the existing runtime HTTPS fetcher. */
    @JsonProperty("userAgent") String userAgent
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SkillsResourcesInstallParams {}

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
        private SkillResourceContext host;
        private String source;
        private Boolean project;
        private String userAgent;

        /**
         * Sets the {@code host} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder host(SkillResourceContext value) {
            this.host = value;
            return this;
        }

        /**
         * Sets the {@code source} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder source(String value) {
            this.source = value;
            return this;
        }

        /**
         * Sets the {@code project} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder project(Boolean value) {
            this.project = value;
            return this;
        }

        /**
         * Sets the {@code userAgent} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder userAgent(String value) {
            this.userAgent = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SkillsResourcesInstallParams build() {
            if (host == null) throw new IllegalStateException("Missing required property: host");
            if (source == null) throw new IllegalStateException("Missing required property: source");
            return new SkillsResourcesInstallParams(host, source, project, userAgent);
        }
    }
}
