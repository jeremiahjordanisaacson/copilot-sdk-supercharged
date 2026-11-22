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
 * Cancel an existing remote turn.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandCancelTurn extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "cancelTurn";

    @Override
    public String getType() { return type; }

    /** Turn identifier. */
    @JsonProperty("turnId")
    private String turnId;

    public String getTurnId() { return turnId; }
    public void setTurnId(String turnId) { this.turnId = turnId; }


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
        private String turnId;

        /**
         * Sets the {@code turnId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder turnId(String value) {
            this.turnId = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandCancelTurn build() {
            if (turnId == null) throw new IllegalStateException("Missing required property: turnId");
            RemoteHostCommandCancelTurn instance = new RemoteHostCommandCancelTurn();
            instance.setTurnId(turnId);
            return instance;
        }
    }
}
