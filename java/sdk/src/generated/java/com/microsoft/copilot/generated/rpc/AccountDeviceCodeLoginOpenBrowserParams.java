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
 * An opaque device authorization identifier owned by this connection.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AccountDeviceCodeLoginOpenBrowserParams(
    /** Identifier returned by deviceCodeLogin.begin. */
    @JsonProperty("flowId") String flowId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AccountDeviceCodeLoginOpenBrowserParams {}

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
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AccountDeviceCodeLoginOpenBrowserParams build() {
            if (flowId == null) throw new IllegalStateException("Missing required property: flowId");
            return new AccountDeviceCodeLoginOpenBrowserParams(flowId);
        }
    }
}
