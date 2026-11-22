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
 * Associate local shell routing with the remote session.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostCommandBindUserRequestedShell extends RemoteHostCommand {

    @JsonProperty("type")
    private final String type = "bindUserRequestedShell";

    @Override
    public String getType() { return type; }

    /** Native session identifier owned by the same runtime. */
    @JsonProperty("nativeSessionId")
    private String nativeSessionId;

    public String getNativeSessionId() { return nativeSessionId; }
    public void setNativeSessionId(String nativeSessionId) { this.nativeSessionId = nativeSessionId; }


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
        private String nativeSessionId;

        /**
         * Sets the {@code nativeSessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder nativeSessionId(String value) {
            this.nativeSessionId = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostCommandBindUserRequestedShell build() {
            if (nativeSessionId == null) throw new IllegalStateException("Missing required property: nativeSessionId");
            RemoteHostCommandBindUserRequestedShell instance = new RemoteHostCommandBindUserRequestedShell();
            instance.setNativeSessionId(nativeSessionId);
            return instance;
        }
    }
}
