/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Enablement changed and the selected session was reconciled.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonTypeInfo(use = JsonTypeInfo.Id.NONE)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
public record SkillInstallationManagementOutcomeEnabledChanged(
    /** Updated installation summary. */
    @JsonProperty("installation") SkillInstallationSummary installation,
    /** Safe reload or reconciliation diagnostics. */
    @JsonProperty("diagnostics") List<String> diagnostics,
    /** Skill installation management outcome discriminator. */
    @JsonProperty("kind") String kind
) implements SkillInstallationManagementOutcome {
    /**
     * Creates this variant positionally and normalizes its discriminator.
     * Use {@link #builder()} for named construction.
     */
    public SkillInstallationManagementOutcomeEnabledChanged {
        kind = "enabled-changed";
    }

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
        private SkillInstallationSummary installation;
        private List<String> diagnostics;

        /**
         * Sets the {@code installation} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder installation(SkillInstallationSummary value) {
            this.installation = value;
            return this;
        }

        /**
         * Sets the {@code diagnostics} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder diagnostics(List<String> value) {
            this.diagnostics = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SkillInstallationManagementOutcomeEnabledChanged build() {
            if (installation == null) throw new IllegalStateException("Missing required property: installation");
            if (diagnostics == null) throw new IllegalStateException("Missing required property: diagnostics");
            return new SkillInstallationManagementOutcomeEnabledChanged(installation, diagnostics, "enabled-changed");
        }
    }
}
