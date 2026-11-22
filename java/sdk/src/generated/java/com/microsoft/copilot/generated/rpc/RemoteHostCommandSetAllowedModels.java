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
 * Set permitted model identifiers.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandSetAllowedModels extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "setAllowedModels";

    @Override
    public String getType() { return type; }

    /** Null restores the host's default model policy. */
    @JsonProperty("allowedModels")
    private List<String> allowedModels;

    public List<String> getAllowedModels() { return allowedModels; }
    public void setAllowedModels(List<String> allowedModels) { this.allowedModels = allowedModels; }


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
        private List<String> allowedModels;

        /**
         * Sets the {@code allowedModels} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder allowedModels(List<String> value) {
            this.allowedModels = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandSetAllowedModels build() {
            RemoteHostCommandSetAllowedModels instance = new RemoteHostCommandSetAllowedModels();
            instance.setAllowedModels(allowedModels);
            return instance;
        }
    }
}
