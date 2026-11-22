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
 * Effective plugin activation resolved by the runtime.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIPluginActivationSnapshot(
    /** Active plugin inventory. */
    @JsonProperty("plugins") List<InstalledPlugin> plugins,
    /** Activation decisions for every considered plugin. */
    @JsonProperty("inventory") List<UIPluginActivationDecision> inventory,
    /** Marketplace resolution warnings. */
    @JsonProperty("warnings") List<UIPluginActivationWarning> warnings,
    /** Resolution context. */
    @JsonProperty("context") UIPluginActivationContext context,
    /** Identity of the resolved activation. */
    @JsonProperty("fingerprint") String fingerprint,
    /** Repository-owned live plugin directories, when known. */
    @JsonProperty("repositoryLivePluginPaths") List<String> repositoryLivePluginPaths,
    /** Classification of a failed resolution, when present. */
    @JsonProperty("resolutionFailure") String resolutionFailure
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIPluginActivationSnapshot {}

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
        private List<InstalledPlugin> plugins;
        private List<UIPluginActivationDecision> inventory;
        private List<UIPluginActivationWarning> warnings;
        private UIPluginActivationContext context;
        private String fingerprint;
        private List<String> repositoryLivePluginPaths;
        private String resolutionFailure;

        /**
         * Sets the {@code plugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder plugins(List<InstalledPlugin> value) {
            this.plugins = value;
            return this;
        }

        /**
         * Sets the {@code inventory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder inventory(List<UIPluginActivationDecision> value) {
            this.inventory = value;
            return this;
        }

        /**
         * Sets the {@code warnings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder warnings(List<UIPluginActivationWarning> value) {
            this.warnings = value;
            return this;
        }

        /**
         * Sets the {@code context} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder context(UIPluginActivationContext value) {
            this.context = value;
            return this;
        }

        /**
         * Sets the {@code fingerprint} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder fingerprint(String value) {
            this.fingerprint = value;
            return this;
        }

        /**
         * Sets the {@code repositoryLivePluginPaths} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder repositoryLivePluginPaths(List<String> value) {
            this.repositoryLivePluginPaths = value;
            return this;
        }

        /**
         * Sets the {@code resolutionFailure} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder resolutionFailure(String value) {
            this.resolutionFailure = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIPluginActivationSnapshot build() {
            if (plugins == null) throw new IllegalStateException("Missing required property: plugins");
            if (inventory == null) throw new IllegalStateException("Missing required property: inventory");
            if (warnings == null) throw new IllegalStateException("Missing required property: warnings");
            if (context == null) throw new IllegalStateException("Missing required property: context");
            if (fingerprint == null) throw new IllegalStateException("Missing required property: fingerprint");
            return new UIPluginActivationSnapshot(plugins, inventory, warnings, context, fingerprint, repositoryLivePluginPaths, resolutionFailure);
        }
    }
}
