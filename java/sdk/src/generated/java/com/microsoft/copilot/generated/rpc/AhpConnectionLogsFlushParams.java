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
 * Identifies a diagnostic log owned by the originating SDK connection.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record AhpConnectionLogsFlushParams(
    /** Opaque identifier returned by ahp.connectionLogs.open. */
    @JsonProperty("logId") String logId
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    AhpConnectionLogsFlushParams {}

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
        private String logId;

        /**
         * Sets the {@code logId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder logId(String value) {
            this.logId = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public AhpConnectionLogsFlushParams build() {
            if (logId == null) throw new IllegalStateException("Missing required property: logId");
            return new AhpConnectionLogsFlushParams(logId);
        }
    }
}
