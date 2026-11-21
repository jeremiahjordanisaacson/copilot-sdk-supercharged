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
 * Continues the same pending persistence operation after plaintext consent.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class AccountLoginProcessParamsContinue extends AccountLoginProcessParams {

    @JsonProperty("operation")
    private final String operation = "continue";

    @Override
    public String getOperation() { return operation; }

    /** The opaque handle returned by open. */
    @JsonProperty("ownerId")
    private String ownerId;

    /** Whether the user consented to plaintext credential storage. */
    @JsonProperty("allowPlaintext")
    private Boolean allowPlaintext;

    public String getOwnerId() { return ownerId; }
    public void setOwnerId(String ownerId) { this.ownerId = ownerId; }

    public Boolean getAllowPlaintext() { return allowPlaintext; }
    public void setAllowPlaintext(Boolean allowPlaintext) { this.allowPlaintext = allowPlaintext; }


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
        private Boolean allowPlaintext;

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
         * Sets the {@code allowPlaintext} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder allowPlaintext(Boolean value) {
            this.allowPlaintext = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountLoginProcessParamsContinue build() {
            if (ownerId == null) throw new IllegalStateException("Missing required property: ownerId");
            if (allowPlaintext == null) throw new IllegalStateException("Missing required property: allowPlaintext");
            AccountLoginProcessParamsContinue instance = new AccountLoginProcessParamsContinue();
            instance.setOwnerId(ownerId);
            instance.setAllowPlaintext(allowPlaintext);
            return instance;
        }
    }
}
