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
 * Evaluates both organization policy channels against the owner's current credential.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class AccountLoginProcessParamsPolicy extends AccountLoginProcessParams {

    @JsonProperty("operation")
    private final String operation = "policy";

    @Override
    public String getOperation() { return operation; }

    /** The opaque handle returned by open. */
    @JsonProperty("ownerId")
    private String ownerId;

    /** The original host's current policy inputs. */
    @JsonProperty("context")
    private ProcessLoginPolicyContext context;

    public String getOwnerId() { return ownerId; }
    public void setOwnerId(String ownerId) { this.ownerId = ownerId; }

    public ProcessLoginPolicyContext getContext() { return context; }
    public void setContext(ProcessLoginPolicyContext context) { this.context = context; }


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
        private ProcessLoginPolicyContext context;

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
         * Sets the {@code context} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder context(ProcessLoginPolicyContext value) {
            this.context = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountLoginProcessParamsPolicy build() {
            if (ownerId == null) throw new IllegalStateException("Missing required property: ownerId");
            if (context == null) throw new IllegalStateException("Missing required property: context");
            AccountLoginProcessParamsPolicy instance = new AccountLoginProcessParamsPolicy();
            instance.setOwnerId(ownerId);
            instance.setContext(context);
            return instance;
        }
    }
}
