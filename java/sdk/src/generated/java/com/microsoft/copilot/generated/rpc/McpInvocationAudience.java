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
 * Feature populations for a sessionless capability inspection.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record McpInvocationAudience(
    /** Whether this installation belongs to staff. */
    @JsonProperty("isStaff") Boolean isStaff,
    /** Whether this installation belongs to GitHub staff. */
    @JsonProperty("isStaffGithub") Boolean isStaffGitHub,
    /** Whether this installation belongs to Microsoft staff. */
    @JsonProperty("isStaffMicrosoft") Boolean isStaffMicrosoft,
    /** Whether experimental capabilities are enabled. */
    @JsonProperty("isExperimental") Boolean isExperimental,
    /** Whether team capabilities are enabled. */
    @JsonProperty("isTeam") Boolean isTeam,
    /** Whether streamer mode is active. */
    @JsonProperty("streamerMode") Boolean streamerMode,
    /** First launch time in Unix milliseconds. */
    @JsonProperty("firstLaunchAtMillis") Double firstLaunchAtMillis
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    McpInvocationAudience {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static final class Builder {
        private Builder() {}
        private Boolean isStaff;
        private Boolean isStaffGitHub;
        private Boolean isStaffMicrosoft;
        private Boolean isExperimental;
        private Boolean isTeam;
        private Boolean streamerMode;
        private Double firstLaunchAtMillis;

        /**
         * Sets the {@code isStaff} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder isStaff(Boolean value) {
            this.isStaff = value;
            return this;
        }

        /**
         * Sets the {@code isStaffGithub} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder isStaffGitHub(Boolean value) {
            this.isStaffGitHub = value;
            return this;
        }

        /**
         * Sets the {@code isStaffMicrosoft} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder isStaffMicrosoft(Boolean value) {
            this.isStaffMicrosoft = value;
            return this;
        }

        /**
         * Sets the {@code isExperimental} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder isExperimental(Boolean value) {
            this.isExperimental = value;
            return this;
        }

        /**
         * Sets the {@code isTeam} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder isTeam(Boolean value) {
            this.isTeam = value;
            return this;
        }

        /**
         * Sets the {@code streamerMode} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder streamerMode(Boolean value) {
            this.streamerMode = value;
            return this;
        }

        /**
         * Sets the {@code firstLaunchAtMillis} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder firstLaunchAtMillis(Double value) {
            this.firstLaunchAtMillis = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public McpInvocationAudience build() {
            if (isStaff == null) throw new IllegalStateException("Missing required property: isStaff");
            if (isExperimental == null) throw new IllegalStateException("Missing required property: isExperimental");
            if (isTeam == null) throw new IllegalStateException("Missing required property: isTeam");
            if (streamerMode == null) throw new IllegalStateException("Missing required property: streamerMode");
            return new McpInvocationAudience(isStaff, isStaffGitHub, isStaffMicrosoft, isExperimental, isTeam, streamerMode, firstLaunchAtMillis);
        }
    }
}
