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
 * A user-scoped approval for one installed plugin extension and capability set.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record PermissionsExtensionApproval(
    /** Stable extension identity, such as `plugin:plugin-name:extension-name`. */
    @JsonProperty("extensionName") String extensionName,
    /** Name of the plugin that contributed the extension. */
    @JsonProperty("pluginName") String pluginName,
    /** Exact privileged capabilities the user approved. A later request is covered only when its capabilities are a subset. */
    @JsonProperty("capabilities") List<String> capabilities,
    /** RFC 3339 timestamp when the approval was last granted. */
    @JsonProperty("grantedAt") String grantedAt,
    /** Opaque stable identity of the installed plugin source. Hosts must return it unchanged when revoking. */
    @JsonProperty("installationKey") String installationKey,
    /** Optional human-readable installation source label. */
    @JsonProperty("installationLabel") String installationLabel
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public PermissionsExtensionApproval {}

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
        private String extensionName;
        private String pluginName;
        private List<String> capabilities;
        private String grantedAt;
        private String installationKey;
        private String installationLabel;

        /**
         * Sets the {@code extensionName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder extensionName(String value) {
            this.extensionName = value;
            return this;
        }

        /**
         * Sets the {@code pluginName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder pluginName(String value) {
            this.pluginName = value;
            return this;
        }

        /**
         * Sets the {@code capabilities} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder capabilities(List<String> value) {
            this.capabilities = value;
            return this;
        }

        /**
         * Sets the {@code grantedAt} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder grantedAt(String value) {
            this.grantedAt = value;
            return this;
        }

        /**
         * Sets the {@code installationKey} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder installationKey(String value) {
            this.installationKey = value;
            return this;
        }

        /**
         * Sets the {@code installationLabel} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder installationLabel(String value) {
            this.installationLabel = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PermissionsExtensionApproval build() {
            if (extensionName == null) throw new IllegalStateException("Missing required property: extensionName");
            if (pluginName == null) throw new IllegalStateException("Missing required property: pluginName");
            if (capabilities == null) throw new IllegalStateException("Missing required property: capabilities");
            if (grantedAt == null) throw new IllegalStateException("Missing required property: grantedAt");
            if (installationKey == null) throw new IllegalStateException("Missing required property: installationKey");
            return new PermissionsExtensionApproval(extensionName, pluginName, capabilities, grantedAt, installationKey, installationLabel);
        }
    }
}
