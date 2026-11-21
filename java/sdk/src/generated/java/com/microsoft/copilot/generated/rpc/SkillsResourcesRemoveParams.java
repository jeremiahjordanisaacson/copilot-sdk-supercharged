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
 * Removes a personal or project skill, or unregisters a custom directory.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SkillsResourcesRemoveParams(
    /** Host paths and working directory. */
    @JsonProperty("host") SkillResourceContext host,
    /** Skill name or registered directory. */
    @JsonProperty("target") String target
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SkillsResourcesRemoveParams {}

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
        private String target;

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
         * Sets the {@code target} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder target(String value) {
            this.target = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SkillsResourcesRemoveParams build() {
            if (host == null) throw new IllegalStateException("Missing required property: host");
            if (target == null) throw new IllegalStateException("Missing required property: target");
            return new SkillsResourcesRemoveParams(host, target);
        }
    }
}
