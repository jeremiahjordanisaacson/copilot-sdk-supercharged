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
 * Creates a connection-owned authentication handle without starting a login flow.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class AccountLoginProcessParamsOpen extends AccountLoginProcessParams {

    @JsonProperty("operation")
    private final String operation = "open";

    @Override
    public String getOperation() { return operation; }

    /** The original immutable authentication configuration. */
    @JsonProperty("context")
    private ProcessLoginContext context;

    public ProcessLoginContext getContext() { return context; }
    public void setContext(ProcessLoginContext context) { this.context = context; }


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
        private ProcessLoginContext context;

        /**
         * Sets the {@code context} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder context(ProcessLoginContext value) {
            this.context = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountLoginProcessParamsOpen build() {
            if (context == null) throw new IllegalStateException("Missing required property: context");
            AccountLoginProcessParamsOpen instance = new AccountLoginProcessParamsOpen();
            instance.setContext(context);
            return instance;
        }
    }
}
