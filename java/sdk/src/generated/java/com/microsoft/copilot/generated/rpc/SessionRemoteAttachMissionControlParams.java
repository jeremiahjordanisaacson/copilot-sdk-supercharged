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
 * Attaches Mission Control polling and steering to this locally retained remote-session facade. Credentials remain with the runtime owner and resolve for each request.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionRemoteAttachMissionControlParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Public facade identifier used for the host's frontend URL registration. */
    @JsonProperty("publicSessionId") String publicSessionId,
    /** Explicit Mission Control endpoint; omission derives it from the identity. */
    @JsonProperty("baseUrl") String baseUrl,
    /** Explicit frontend origin; omission derives it from the identity. */
    @JsonProperty("frontendBaseUrl") String frontendBaseUrl,
    /** Identity whose credential and endpoints the runtime resolves. */
    @JsonProperty("authInfo") AuthIdentity authInfo,
    /** Registered credential owner, when the host uses one. */
    @JsonProperty("authContextId") String authContextId,
    /** Integration identifier sent to Mission Control. */
    @JsonProperty("integrationId") String integrationId,
    /** Cloud task whose events and commands this session follows. */
    @JsonProperty("taskId") String taskId,
    /** Repository owner associated with the remote task. */
    @JsonProperty("repositoryOwner") String repositoryOwner,
    /** Repository name associated with the remote task. */
    @JsonProperty("repositoryName") String repositoryName,
    /** Previously observed events, including events excluded from replay. */
    @JsonProperty("observedEventsJson") String observedEventsJson,
    /** Whether this facade permits steering. */
    @JsonProperty("remoteSteerable") Boolean remoteSteerable,
    /** Poll interval in milliseconds; omission uses the runtime default. */
    @JsonProperty("pollIntervalMs") Long pollIntervalMs,
    /** Whether the attachment provides remote JSON-RPC commands. */
    @JsonProperty("rpcEnabled") Boolean rpcEnabled
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionRemoteAttachMissionControlParams {}

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
        private String publicSessionId;
        private String baseUrl;
        private String frontendBaseUrl;
        private AuthIdentity authInfo;
        private String authContextId;
        private String integrationId;
        private String taskId;
        private String repositoryOwner;
        private String repositoryName;
        private String observedEventsJson;
        private Boolean remoteSteerable;
        private Long pollIntervalMs;
        private Boolean rpcEnabled;

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
         * Sets the {@code publicSessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder publicSessionId(String value) {
            this.publicSessionId = value;
            return this;
        }

        /**
         * Sets the {@code baseUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder baseUrl(String value) {
            this.baseUrl = value;
            return this;
        }

        /**
         * Sets the {@code frontendBaseUrl} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder frontendBaseUrl(String value) {
            this.frontendBaseUrl = value;
            return this;
        }

        /**
         * Sets the {@code authInfo} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder authInfo(AuthIdentity value) {
            this.authInfo = value;
            return this;
        }

        /**
         * Sets the {@code authContextId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder authContextId(String value) {
            this.authContextId = value;
            return this;
        }

        /**
         * Sets the {@code integrationId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder integrationId(String value) {
            this.integrationId = value;
            return this;
        }

        /**
         * Sets the {@code taskId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder taskId(String value) {
            this.taskId = value;
            return this;
        }

        /**
         * Sets the {@code repositoryOwner} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder repositoryOwner(String value) {
            this.repositoryOwner = value;
            return this;
        }

        /**
         * Sets the {@code repositoryName} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder repositoryName(String value) {
            this.repositoryName = value;
            return this;
        }

        /**
         * Sets the {@code observedEventsJson} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder observedEventsJson(String value) {
            this.observedEventsJson = value;
            return this;
        }

        /**
         * Sets the {@code remoteSteerable} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder remoteSteerable(Boolean value) {
            this.remoteSteerable = value;
            return this;
        }

        /**
         * Sets the {@code pollIntervalMs} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder pollIntervalMs(Long value) {
            this.pollIntervalMs = value;
            return this;
        }

        /**
         * Sets the {@code rpcEnabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder rpcEnabled(Boolean value) {
            this.rpcEnabled = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionRemoteAttachMissionControlParams build() {
            if (publicSessionId == null) throw new IllegalStateException("Missing required property: publicSessionId");
            if (authInfo == null) throw new IllegalStateException("Missing required property: authInfo");
            if (integrationId == null) throw new IllegalStateException("Missing required property: integrationId");
            if (taskId == null) throw new IllegalStateException("Missing required property: taskId");
            if (observedEventsJson == null) throw new IllegalStateException("Missing required property: observedEventsJson");
            if (remoteSteerable == null) throw new IllegalStateException("Missing required property: remoteSteerable");
            if (rpcEnabled == null) throw new IllegalStateException("Missing required property: rpcEnabled");
            return new SessionRemoteAttachMissionControlParams(sessionId, publicSessionId, baseUrl, frontendBaseUrl, authInfo, authContextId, integrationId, taskId, repositoryOwner, repositoryName, observedEventsJson, remoteSteerable, pollIntervalMs, rpcEnabled);
        }
    }
}
