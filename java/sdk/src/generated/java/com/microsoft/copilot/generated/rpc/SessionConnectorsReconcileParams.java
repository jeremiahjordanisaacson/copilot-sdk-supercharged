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
 * Requests authoritative Connector-to-MCP reconciliation for the pinned account.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionConnectorsReconcileParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Opaque account selection ID. It must match the account already pinned to the session, if any. */
    @JsonProperty("accountId") String accountId,
    /** When true, refresh the catalog before reconciling. A disabled Connector API performs no service request. */
    @JsonProperty("refreshCatalog") Boolean refreshCatalog,
    /** Optional Connector name to reinitialize. Requires the targetedReconcile capability. */
    @JsonProperty("forceConnectorName") String forceConnectorName
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionConnectorsReconcileParams {}

    /**
     * Returns a named builder.
     * @return a new builder
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static Builder builder() { return new Builder(); }

    /**
     * Named construction for this record.
     * @since 2.0.0
     */
    @com.microsoft.copilot.CopilotExperimental
    public static final class Builder {
        private Builder() {}
        private String sessionId;
        private String accountId;
        private Boolean refreshCatalog;
        private String forceConnectorName;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code accountId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder accountId(String value) {
            this.accountId = value;
            return this;
        }

        /**
         * Sets the {@code refreshCatalog} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder refreshCatalog(Boolean value) {
            this.refreshCatalog = value;
            return this;
        }

        /**
         * Sets the {@code forceConnectorName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder forceConnectorName(String value) {
            this.forceConnectorName = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionConnectorsReconcileParams build() {
            if (accountId == null) throw new IllegalStateException("Missing required property: accountId");
            return new SessionConnectorsReconcileParams(sessionId, accountId, refreshCatalog, forceConnectorName);
        }
    }
}
