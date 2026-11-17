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
 * Starts a supervised AHP host with at least one explicitly selected transport.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record HostStartParams(
    /** Caller-generated UUID identifying this connection-owned listener. */
    @JsonProperty("hostId") String hostId,
    /** Explicit admission policy, required by the runtime before hosting. Optional in the wire DTO only to preserve generated client construction. */
    @JsonProperty("authentication") Object authentication,
    /** Stable application identity selecting its durable host catalog. Defaults to the persisted runtime compute GUID; must agree with githubEnvironment.computeId when both are supplied. */
    @JsonProperty("computeId") String computeId,
    /** Absolute directory on the runtime host to serve through AHP. Defaults to the runtime's configured working directory. Does not change process or session working directories. */
    @JsonProperty("workspace") String workspace,
    /** Enables a local WebSocket listener. */
    @JsonProperty("localServer") HostLocalServerOptions localServer,
    /** Registers a GitHub Mission Control environment and enables its relay transport. */
    @JsonProperty("githubEnvironment") HostGitHubEnvironmentOptions gitHubEnvironment,
    /** Ask the owning SDK application to materialize AHP sessions. */
    @JsonProperty("sessionFactory") Boolean sessionFactory,
    /** Ask the owning application to resume its durable AHP sessions. */
    @JsonProperty("resumeFactory") Boolean resumeFactory,
    /** Whether remote clients may create new sessions. Defaults to denied when omitted. Does not restrict owner work, publication, or existing sessions. */
    @JsonProperty("allowCreatingNewSessions") Boolean allowCreatingNewSessions
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public HostStartParams {}

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
        private String hostId;
        private Object authentication;
        private String computeId;
        private String workspace;
        private HostLocalServerOptions localServer;
        private HostGitHubEnvironmentOptions gitHubEnvironment;
        private Boolean sessionFactory;
        private Boolean resumeFactory;
        private Boolean allowCreatingNewSessions;

        /**
         * Sets the {@code hostId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder hostId(String value) {
            this.hostId = value;
            return this;
        }

        /**
         * Sets the {@code authentication} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder authentication(Object value) {
            this.authentication = value;
            return this;
        }

        /**
         * Sets the {@code computeId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder computeId(String value) {
            this.computeId = value;
            return this;
        }

        /**
         * Sets the {@code workspace} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder workspace(String value) {
            this.workspace = value;
            return this;
        }

        /**
         * Sets the {@code localServer} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder localServer(HostLocalServerOptions value) {
            this.localServer = value;
            return this;
        }

        /**
         * Sets the {@code githubEnvironment} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder gitHubEnvironment(HostGitHubEnvironmentOptions value) {
            this.gitHubEnvironment = value;
            return this;
        }

        /**
         * Sets the {@code sessionFactory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder sessionFactory(Boolean value) {
            this.sessionFactory = value;
            return this;
        }

        /**
         * Sets the {@code resumeFactory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder resumeFactory(Boolean value) {
            this.resumeFactory = value;
            return this;
        }

        /**
         * Sets the {@code allowCreatingNewSessions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder allowCreatingNewSessions(Boolean value) {
            this.allowCreatingNewSessions = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public HostStartParams build() {
            if (hostId == null) throw new IllegalStateException("Missing required property: hostId");
            return new HostStartParams(hostId, authentication, computeId, workspace, localServer, gitHubEnvironment, sessionFactory, resumeFactory, allowCreatingNewSessions);
        }
    }
}
