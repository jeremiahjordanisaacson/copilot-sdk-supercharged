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
 * Remove a queued or steering message.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandRemovePendingMessage extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "removePendingMessage";

    @Override
    public String getType() { return type; }

    /** AHP pending message kind. */
    @JsonProperty("kind")
    private RemoteHostPendingMessageKind kind;

    /** Message identifier. */
    @JsonProperty("id")
    private String id;

    public RemoteHostPendingMessageKind getKind() { return kind; }
    public void setKind(RemoteHostPendingMessageKind kind) { this.kind = kind; }

    public String getId() { return id; }
    public void setId(String id) { this.id = id; }


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
        private RemoteHostPendingMessageKind kind;
        private String id;

        /**
         * Sets the {@code kind} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder kind(RemoteHostPendingMessageKind value) {
            this.kind = value;
            return this;
        }

        /**
         * Sets the {@code id} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder id(String value) {
            this.id = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandRemovePendingMessage build() {
            if (kind == null) throw new IllegalStateException("Missing required property: kind");
            if (id == null) throw new IllegalStateException("Missing required property: id");
            RemoteHostCommandRemovePendingMessage instance = new RemoteHostCommandRemovePendingMessage();
            instance.setKind(kind);
            instance.setId(id);
            return instance;
        }
    }
}
