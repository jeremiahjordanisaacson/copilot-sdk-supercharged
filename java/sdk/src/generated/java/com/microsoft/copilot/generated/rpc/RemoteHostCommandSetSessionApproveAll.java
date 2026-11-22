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
 * Set the remote session's approve-all state.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandSetSessionApproveAll extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "setSessionApproveAll";

    @Override
    public String getType() { return type; }

    /** Whether approve-all is enabled. */
    @JsonProperty("enabled")
    private Boolean enabled;

    public Boolean getEnabled() { return enabled; }
    public void setEnabled(Boolean enabled) { this.enabled = enabled; }


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
        private Boolean enabled;

        /**
         * Sets the {@code enabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder enabled(Boolean value) {
            this.enabled = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandSetSessionApproveAll build() {
            if (enabled == null) throw new IllegalStateException("Missing required property: enabled");
            RemoteHostCommandSetSessionApproveAll instance = new RemoteHostCommandSetSessionApproveAll();
            instance.setEnabled(enabled);
            return instance;
        }
    }
}
