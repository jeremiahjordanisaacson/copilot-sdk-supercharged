/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

// AUTO-GENERATED FILE - DO NOT EDIT
// Generated from: api.schema.json

package com.microsoft.copilot.generated.rpc;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.microsoft.copilot.CopilotExperimental;
import javax.annotation.processing.Generated;

/**
 * Advances a device authorization flow. A plaintext decision is accepted only after the runtime requests consent.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AccountDeviceCodeLoginAdvanceParams(
    /** Identifier returned by deviceCodeLogin.begin. */
    @JsonProperty("flowId") String flowId,
    /** Explicit storage decision after a needs-plaintext-consent result. */
    @JsonProperty("allowPlaintext") Boolean allowPlaintext
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AccountDeviceCodeLoginAdvanceParams {}

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
        private String flowId;
        private Boolean allowPlaintext;

        /**
         * Sets the {@code flowId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder flowId(String value) {
            this.flowId = value;
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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountDeviceCodeLoginAdvanceParams build() {
            if (flowId == null) throw new IllegalStateException("Missing required property: flowId");
            return new AccountDeviceCodeLoginAdvanceParams(flowId, allowPlaintext);
        }
    }
}
