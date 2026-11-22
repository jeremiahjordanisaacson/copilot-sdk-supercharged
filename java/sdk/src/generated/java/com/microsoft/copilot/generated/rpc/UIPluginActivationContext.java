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
 * Context used for effective plugin resolution.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIPluginActivationContext(
    /** Working directory requested by the caller. */
    @JsonProperty("requestedWorkingDirectory") String requestedWorkingDirectory,
    /** Working directory used by the resolver. */
    @JsonProperty("resolvedWorkingDirectory") String resolvedWorkingDirectory,
    /** Settings root used by the resolver. */
    @JsonProperty("settingsRoot") String settingsRoot,
    /** Repository root, when present. */
    @JsonProperty("gitRoot") String gitRoot,
    /** Root covered by workspace trust, when present. */
    @JsonProperty("trustRoot") String trustRoot,
    /** Whether repository plugin inputs are trusted. */
    @JsonProperty("trusted") Boolean trusted,
    /** Runtime source of the trust decision. */
    @JsonProperty("trustSource") String trustSource
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIPluginActivationContext {}

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
        private String requestedWorkingDirectory;
        private String resolvedWorkingDirectory;
        private String settingsRoot;
        private String gitRoot;
        private String trustRoot;
        private Boolean trusted;
        private String trustSource;

        /**
         * Sets the {@code requestedWorkingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder requestedWorkingDirectory(String value) {
            this.requestedWorkingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code resolvedWorkingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder resolvedWorkingDirectory(String value) {
            this.resolvedWorkingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code settingsRoot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder settingsRoot(String value) {
            this.settingsRoot = value;
            return this;
        }

        /**
         * Sets the {@code gitRoot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder gitRoot(String value) {
            this.gitRoot = value;
            return this;
        }

        /**
         * Sets the {@code trustRoot} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder trustRoot(String value) {
            this.trustRoot = value;
            return this;
        }

        /**
         * Sets the {@code trusted} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder trusted(Boolean value) {
            this.trusted = value;
            return this;
        }

        /**
         * Sets the {@code trustSource} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder trustSource(String value) {
            this.trustSource = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIPluginActivationContext build() {
            if (requestedWorkingDirectory == null) throw new IllegalStateException("Missing required property: requestedWorkingDirectory");
            if (resolvedWorkingDirectory == null) throw new IllegalStateException("Missing required property: resolvedWorkingDirectory");
            if (settingsRoot == null) throw new IllegalStateException("Missing required property: settingsRoot");
            if (trusted == null) throw new IllegalStateException("Missing required property: trusted");
            if (trustSource == null) throw new IllegalStateException("Missing required property: trustSource");
            return new UIPluginActivationContext(requestedWorkingDirectory, resolvedWorkingDirectory, settingsRoot, gitRoot, trustRoot, trusted, trustSource);
        }
    }
}
