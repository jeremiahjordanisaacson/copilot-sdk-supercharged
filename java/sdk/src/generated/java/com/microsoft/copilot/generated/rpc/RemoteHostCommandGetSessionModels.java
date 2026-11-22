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
 * Read models through the remote owner.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandGetSessionModels extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "getSessionModels";

    @Override
    public String getType() { return type; }

    /** Whether to bypass the host model cache. */
    @JsonProperty("skipCache")
    private Boolean skipCache;

    public Boolean getSkipCache() { return skipCache; }
    public void setSkipCache(Boolean skipCache) { this.skipCache = skipCache; }


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
        private Boolean skipCache;

        /**
         * Sets the {@code skipCache} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder skipCache(Boolean value) {
            this.skipCache = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandGetSessionModels build() {
            if (skipCache == null) throw new IllegalStateException("Missing required property: skipCache");
            RemoteHostCommandGetSessionModels instance = new RemoteHostCommandGetSessionModels();
            instance.setSkipCache(skipCache);
            return instance;
        }
    }
}
