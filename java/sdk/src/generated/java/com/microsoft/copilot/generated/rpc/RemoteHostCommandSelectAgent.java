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
 * Select a host agent.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandSelectAgent extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "selectAgent";

    @Override
    public String getType() { return type; }

    /** Agent URI; null selects the host default. */
    @JsonProperty("agentUri")
    private String agentUri;

    public String getAgentUri() { return agentUri; }
    public void setAgentUri(String agentUri) { this.agentUri = agentUri; }


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
        private String agentUri;

        /**
         * Sets the {@code agentUri} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder agentUri(String value) {
            this.agentUri = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandSelectAgent build() {
            RemoteHostCommandSelectAgent instance = new RemoteHostCommandSelectAgent();
            instance.setAgentUri(agentUri);
            return instance;
        }
    }
}
