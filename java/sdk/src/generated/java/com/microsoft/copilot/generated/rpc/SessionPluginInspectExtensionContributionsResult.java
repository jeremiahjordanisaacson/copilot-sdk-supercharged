/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Whether a changed active plugin contributes extension directories, including empty directories.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionPluginInspectExtensionContributionsResult(
    /** True when directories exist or the session has no authoritative activation snapshot. */
    @JsonProperty("shipsExtensions") Boolean shipsExtensions,
    /** Canonical extension discovery diagnostics, in resolver order. */
    @JsonProperty("warnings") List<PluginPolicyDiagnostic> warnings
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionPluginInspectExtensionContributionsResult {}

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
        private Boolean shipsExtensions;
        private List<PluginPolicyDiagnostic> warnings;

        /**
         * Sets the {@code shipsExtensions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder shipsExtensions(Boolean value) {
            this.shipsExtensions = value;
            return this;
        }

        /**
         * Sets the {@code warnings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder warnings(List<PluginPolicyDiagnostic> value) {
            this.warnings = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionPluginInspectExtensionContributionsResult build() {
            if (shipsExtensions == null) throw new IllegalStateException("Missing required property: shipsExtensions");
            if (warnings == null) throw new IllegalStateException("Missing required property: warnings");
            return new SessionPluginInspectExtensionContributionsResult(shipsExtensions, warnings);
        }
    }
}
