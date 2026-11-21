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
 * A persistence operation completed or requires user input.
 *
 * @since 1.0.0
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@javax.annotation.processing.Generated("copilot-sdk-codegen")
final class AccountLoginProcessResultPersistence extends AccountLoginProcessResult {

    @JsonProperty("kind")
    private final String kind = "persistence";

    @Override
    public String getKind() { return kind; }

    /** The original persistence result. */
    @JsonProperty("result")
    private ProcessLoginPersistence result;

    public ProcessLoginPersistence getResult() { return result; }
    public void setResult(ProcessLoginPersistence result) { this.result = result; }


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
        private ProcessLoginPersistence result;

        /**
         * Sets the {@code result} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder result(ProcessLoginPersistence value) {
            this.result = value;
            return this;
        }

        /**
         * Builds a variant after checking required property presence.
         * @return a new variant
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountLoginProcessResultPersistence build() {
            if (result == null) throw new IllegalStateException("Missing required property: result");
            AccountLoginProcessResultPersistence instance = new AccountLoginProcessResultPersistence();
            instance.setResult(result);
            return instance;
        }
    }
}
