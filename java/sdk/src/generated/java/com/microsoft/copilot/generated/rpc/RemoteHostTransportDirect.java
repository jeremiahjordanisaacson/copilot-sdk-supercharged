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
 * Connect directly without sharing a GitHub credential unless explicitly allowed.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class RemoteHostTransportDirect extends RemoteHostTransport {

    @JsonProperty("kind")
    private final String kind = "direct";

    @Override
    public String getKind() { return kind; }

    /** Absolute WebSocket endpoint selected by the application. */
    @JsonProperty("url")
    private String url;

    /** Explicit user permission to share the owner's GitHub credential with this endpoint. */
    @JsonProperty("authenticateWithGitHub")
    private Boolean authenticateWithGitHub;

    public String getUrl() { return url; }
    public void setUrl(String url) { this.url = url; }

    public Boolean getAuthenticateWithGitHub() { return authenticateWithGitHub; }
    public void setAuthenticateWithGitHub(Boolean authenticateWithGitHub) { this.authenticateWithGitHub = authenticateWithGitHub; }


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
        private String url;
        private Boolean authenticateWithGitHub;

        /**
         * Sets the {@code url} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder url(String value) {
            this.url = value;
            return this;
        }

        /**
         * Sets the {@code authenticateWithGitHub} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder authenticateWithGitHub(Boolean value) {
            this.authenticateWithGitHub = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public RemoteHostTransportDirect build() {
            if (url == null) throw new IllegalStateException("Missing required property: url");
            RemoteHostTransportDirect instance = new RemoteHostTransportDirect();
            instance.setUrl(url);
            instance.setAuthenticateWithGitHub(authenticateWithGitHub);
            return instance;
        }
    }
}
