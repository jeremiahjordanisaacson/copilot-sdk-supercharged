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
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * Runtime-owned plugin discovery and trust policy.
 *
 * @since 1.0.0
 */
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record UIPluginActivationPolicy(
    /** Whether ambient plugin discovery is enabled. */
    @JsonProperty("includeAmbient") Boolean includeAmbient,
    /** Explicit plugin-directory inputs. */
    @JsonProperty("explicitPlugins") List<InstalledPlugin> explicitPlugins,
    /** Trust context used by the policy. */
    @JsonProperty("trust") UIPluginTrustContext trust,
    /** Additional caller-supplied ambient plugin inputs. */
    @JsonProperty("ambientPlugins") List<InstalledPlugin> ambientPlugins,
    /** Host enablement overrides, when supplied. */
    @JsonProperty("baseEnabledPlugins") Map<String, Boolean> baseEnabledPlugins,
    /** Session enablement defaults, when supplied. */
    @JsonProperty("defaultEnabledPlugins") Map<String, Boolean> defaultEnabledPlugins
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public UIPluginActivationPolicy {}

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
        private Boolean includeAmbient;
        private List<InstalledPlugin> explicitPlugins;
        private UIPluginTrustContext trust;
        private List<InstalledPlugin> ambientPlugins;
        private Map<String, Boolean> baseEnabledPlugins;
        private Map<String, Boolean> defaultEnabledPlugins;

        /**
         * Sets the {@code includeAmbient} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includeAmbient(Boolean value) {
            this.includeAmbient = value;
            return this;
        }

        /**
         * Sets the {@code explicitPlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder explicitPlugins(List<InstalledPlugin> value) {
            this.explicitPlugins = value;
            return this;
        }

        /**
         * Sets the {@code trust} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder trust(UIPluginTrustContext value) {
            this.trust = value;
            return this;
        }

        /**
         * Sets the {@code ambientPlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder ambientPlugins(List<InstalledPlugin> value) {
            this.ambientPlugins = value;
            return this;
        }

        /**
         * Sets the {@code baseEnabledPlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder baseEnabledPlugins(Map<String, Boolean> value) {
            this.baseEnabledPlugins = value;
            return this;
        }

        /**
         * Sets the {@code defaultEnabledPlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder defaultEnabledPlugins(Map<String, Boolean> value) {
            this.defaultEnabledPlugins = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UIPluginActivationPolicy build() {
            if (includeAmbient == null) throw new IllegalStateException("Missing required property: includeAmbient");
            if (explicitPlugins == null) throw new IllegalStateException("Missing required property: explicitPlugins");
            if (trust == null) throw new IllegalStateException("Missing required property: trust");
            return new UIPluginActivationPolicy(includeAmbient, explicitPlugins, trust, ambientPlugins, baseEnabledPlugins, defaultEnabledPlugins);
        }
    }
}
