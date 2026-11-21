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
import java.util.List;
import javax.annotation.processing.Generated;

/**
 * Host launch resources and lifecycle options for the original session's extension service.
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record SessionExtensionsConfigureHostParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** Existing extension host engine. The original session authority must own its session generation. */
    @JsonProperty("hostServerId") Long hostServerId,
    /** Path to the host's extension SDK. */
    @JsonProperty("sdkPath") String sdkPath,
    /** Directory that contains the host's CLI distribution. */
    @JsonProperty("cliDistDir") String cliDistDir,
    /** Executable that launches extension processes. */
    @JsonProperty("launcherProgram") String launcherProgram,
    /** Working directory captured by the host for this configuration. */
    @JsonProperty("workingDirectory") String workingDirectory,
    /** Existing initial startup token, when configuration belongs to startup. */
    @JsonProperty("initialStartupToken") String initialStartupToken,
    /** Opaque token for the original host's eligibility check after native discovery. */
    @JsonProperty("hostValidationToken") String hostValidationToken,
    /** Explicit disabled IDs. When absent, the runtime reads the selected profile. */
    @JsonProperty("disabledIds") List<String> disabledIds,
    /** Explicit source filters. When absent, the runtime uses all allowed sources. */
    @JsonProperty("includeSources") List<String> includeSources,
    /** Host-mounted plugins outside the persisted installation records. */
    @JsonProperty("mountedPlugins") List<?> mountedPlugins,
    /** Whether this configuration permits extension processes to launch. */
    @JsonProperty("launchProcesses") Boolean launchProcesses,
    /** Whether compatible unchanged processes remain active. */
    @JsonProperty("retainProcessesIfUnchanged") Boolean retainProcessesIfUnchanged,
    /** Host gate for extension management tools. */
    @JsonProperty("managementToolsEnabled") Boolean managementToolsEnabled
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    SessionExtensionsConfigureHostParams {}

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
        private String sessionId;
        private Long hostServerId;
        private String sdkPath;
        private String cliDistDir;
        private String launcherProgram;
        private String workingDirectory;
        private String initialStartupToken;
        private String hostValidationToken;
        private List<String> disabledIds;
        private List<String> includeSources;
        private List<?> mountedPlugins;
        private Boolean launchProcesses;
        private Boolean retainProcessesIfUnchanged;
        private Boolean managementToolsEnabled;

        /**
         * Sets the {@code sessionId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sessionId(String value) {
            this.sessionId = value;
            return this;
        }

        /**
         * Sets the {@code hostServerId} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hostServerId(Long value) {
            this.hostServerId = value;
            return this;
        }

        /**
         * Sets the {@code sdkPath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder sdkPath(String value) {
            this.sdkPath = value;
            return this;
        }

        /**
         * Sets the {@code cliDistDir} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder cliDistDir(String value) {
            this.cliDistDir = value;
            return this;
        }

        /**
         * Sets the {@code launcherProgram} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder launcherProgram(String value) {
            this.launcherProgram = value;
            return this;
        }

        /**
         * Sets the {@code workingDirectory} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder workingDirectory(String value) {
            this.workingDirectory = value;
            return this;
        }

        /**
         * Sets the {@code initialStartupToken} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder initialStartupToken(String value) {
            this.initialStartupToken = value;
            return this;
        }

        /**
         * Sets the {@code hostValidationToken} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder hostValidationToken(String value) {
            this.hostValidationToken = value;
            return this;
        }

        /**
         * Sets the {@code disabledIds} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder disabledIds(List<String> value) {
            this.disabledIds = value;
            return this;
        }

        /**
         * Sets the {@code includeSources} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder includeSources(List<String> value) {
            this.includeSources = value;
            return this;
        }

        /**
         * Sets the {@code mountedPlugins} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder mountedPlugins(List<?> value) {
            this.mountedPlugins = value;
            return this;
        }

        /**
         * Sets the {@code launchProcesses} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder launchProcesses(Boolean value) {
            this.launchProcesses = value;
            return this;
        }

        /**
         * Sets the {@code retainProcessesIfUnchanged} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder retainProcessesIfUnchanged(Boolean value) {
            this.retainProcessesIfUnchanged = value;
            return this;
        }

        /**
         * Sets the {@code managementToolsEnabled} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        public Builder managementToolsEnabled(Boolean value) {
            this.managementToolsEnabled = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionExtensionsConfigureHostParams build() {
            if (hostServerId == null) throw new IllegalStateException("Missing required property: hostServerId");
            if (sdkPath == null) throw new IllegalStateException("Missing required property: sdkPath");
            if (cliDistDir == null) throw new IllegalStateException("Missing required property: cliDistDir");
            if (launcherProgram == null) throw new IllegalStateException("Missing required property: launcherProgram");
            if (workingDirectory == null) throw new IllegalStateException("Missing required property: workingDirectory");
            if (launchProcesses == null) throw new IllegalStateException("Missing required property: launchProcesses");
            if (retainProcessesIfUnchanged == null) throw new IllegalStateException("Missing required property: retainProcessesIfUnchanged");
            if (managementToolsEnabled == null) throw new IllegalStateException("Missing required property: managementToolsEnabled");
            return new SessionExtensionsConfigureHostParams(sessionId, hostServerId, sdkPath, cliDistDir, launcherProgram, workingDirectory, initialStartupToken, hostValidationToken, disabledIds, includeSources, mountedPlugins, launchProcesses, retainProcessesIfUnchanged, managementToolsEnabled);
        }
    }
}
