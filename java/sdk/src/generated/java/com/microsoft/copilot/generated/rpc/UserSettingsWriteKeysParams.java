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
import java.util.Map;
import javax.annotation.processing.Generated;

/**
 * An atomic set of top-level settings updates that preserves unrelated stored values.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record UserSettingsWriteKeysParams(
    /** Host paths and environment that select the target document. */
    @JsonProperty("context") UserSettingsHostContext context,
    /** Top-level values to write. Stored null remains a value, not deletion. */
    @JsonInclude(value = JsonInclude.Include.NON_NULL, content = JsonInclude.Include.ALWAYS)
    @JsonProperty("settings") Map<String, Object> settings,
    /** Top-level keys to remove in the same transaction. */
    @JsonProperty("deleteKeys") List<String> deleteKeys
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    UserSettingsWriteKeysParams {}

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
        private Map<String, Object> settings;
        private List<String> deleteKeys;

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
         * Sets the {@code settings} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder settings(Map<String, Object> value) {
            this.settings = value;
            return this;
        }

        /**
         * Sets the {@code deleteKeys} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder deleteKeys(List<String> value) {
            this.deleteKeys = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public UserSettingsWriteKeysParams build() {
            if (context == null) throw new IllegalStateException("Missing required property: context");
            if (settings == null) throw new IllegalStateException("Missing required property: settings");
            if (deleteKeys == null) throw new IllegalStateException("Missing required property: deleteKeys");
            return new UserSettingsWriteKeysParams(context, settings, deleteKeys);
        }
    }
}
