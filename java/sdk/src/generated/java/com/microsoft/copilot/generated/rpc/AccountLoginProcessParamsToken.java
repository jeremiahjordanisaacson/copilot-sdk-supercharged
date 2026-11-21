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
 * Validates and persists a token through the original authentication handle.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class AccountLoginProcessParamsToken extends AccountLoginProcessParams {

    @JsonProperty("operation")
    private final String operation = "token";

    @Override
    public String getOperation() { return operation; }

    /** The opaque handle returned by open. */
    @JsonProperty("ownerId")
    private String ownerId;

    /** The normalized GitHub host. */
    @JsonProperty("host")
    private String host;

    /** The user-supplied token. It is never returned. */
    @JsonProperty("token")
    private String token;

    public String getOwnerId() { return ownerId; }
    public void setOwnerId(String ownerId) { this.ownerId = ownerId; }

    public String getHost() { return host; }
    public void setHost(String host) { this.host = host; }

    public String getToken() { return token; }
    public void setToken(String token) { this.token = token; }


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
        private String ownerId;
        private String host;
        private String token;

        /**
         * Sets the {@code ownerId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder ownerId(String value) {
            this.ownerId = value;
            return this;
        }

        /**
         * Sets the {@code host} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder host(String value) {
            this.host = value;
            return this;
        }

        /**
         * Sets the {@code token} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder token(String value) {
            this.token = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountLoginProcessParamsToken build() {
            if (ownerId == null) throw new IllegalStateException("Missing required property: ownerId");
            if (host == null) throw new IllegalStateException("Missing required property: host");
            if (token == null) throw new IllegalStateException("Missing required property: token");
            AccountLoginProcessParamsToken instance = new AccountLoginProcessParamsToken();
            instance.setOwnerId(ownerId);
            instance.setHost(host);
            instance.setToken(token);
            return instance;
        }
    }
}
