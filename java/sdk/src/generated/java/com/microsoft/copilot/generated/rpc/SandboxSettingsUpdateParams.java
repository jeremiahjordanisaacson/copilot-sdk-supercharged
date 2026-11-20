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
 * A sandbox settings patch and the host context that selects its settings file.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SandboxSettingsUpdateParams(
    /** The configuration directory, home directory, and environment used to resolve settings.json. */
    @JsonProperty("context") UserSettingsHostContext context,
    /** Sandbox settings changes. Filesystem, network, and seatbelt fields merge into userPolicy; other supported fields merge at the sandbox root. */
    @JsonInclude(JsonInclude.Include.ALWAYS)
    @JsonProperty("patch") Object patch
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SandboxSettingsUpdateParams {}

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
        private UserSettingsHostContext context;
        private Object patch;
        private boolean $presence1;

        /**
         * Sets the {@code context} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder context(UserSettingsHostContext value) {
            this.context = value;
            return this;
        }

        /**
         * Sets the {@code patch} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder patch(Object value) {
            this.patch = value;
            this.$presence1 = true;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SandboxSettingsUpdateParams build() {
            if (context == null) throw new IllegalStateException("Missing required property: context");
            if (!$presence1) throw new IllegalStateException("Missing required property: patch");
            return new SandboxSettingsUpdateParams(context, patch);
        }
    }
}
