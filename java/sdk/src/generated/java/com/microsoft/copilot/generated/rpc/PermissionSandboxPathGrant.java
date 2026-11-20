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
 * A sandbox filesystem policy edit that would let a blocked operation run inside the sandbox instead of outside it. Offered only on a sandbox escalation request whose denial adding this path lifts, and only when managed policy permits the grant. A host accepts it with session.sandbox.grantPathForRequest, which adds the path to the session's sandbox policy and re-runs the operation sandboxed. A host that persists sandbox settings must save the optional policy spelling when present; otherwise it must escape every literal `$` in the concrete path as `$$` before saving that path.
 *
 * @apiNote This type is experimental and may change in a future version.
 *
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
record PermissionSandboxPathGrant(
    /** Absolute path to add to the sandbox filesystem policy */
    @JsonProperty("path") String path,
    /** The path the sandbox refused, present only when it differs from path. That happens when a write under a read-only folder moves the folder to the read-write paths, when a path that does not exist yet is granted through its nearest existing folder, because the OS sandbox cannot grant a path before it exists, and when either is spelled through a symlink, because a grant covers its path as written, so path is then the resolved location. Hosts should then name path in the offer, since the denial names this one. */
    @JsonProperty("deniedPath") String deniedPath,
    /** Which access the grant confers, and so which policy list the path is added to */
    @JsonProperty("access") PermissionSandboxPathGrantAccess access,
    /** readonlyPaths entries the grant removes, exactly as written in the policy, because a read-only entry for the same location would otherwise keep the path read-only. A host that persists the path must remove these entries from its stored readonlyPaths too. */
    @JsonProperty("removedReadonlyPaths") List<String> removedReadonlyPaths,
    /** Policy spelling to persist for the grant. This can differ from path when a managed allowlist names the same location through an environment expression. Hosts that persist sandbox settings must save this value when present. When it is absent, they must escape every literal `$` in path as `$$` before saving path so the policy grammar does not interpret it as an environment reference. */
    @JsonProperty("persistedPath") String persistedPath
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    PermissionSandboxPathGrant {}

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
        private String path;
        private String deniedPath;
        private PermissionSandboxPathGrantAccess access;
        private List<String> removedReadonlyPaths;
        private String persistedPath;

        /**
         * Sets the {@code path} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder path(String value) {
            this.path = value;
            return this;
        }

        /**
         * Sets the {@code deniedPath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder deniedPath(String value) {
            this.deniedPath = value;
            return this;
        }

        /**
         * Sets the {@code access} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder access(PermissionSandboxPathGrantAccess value) {
            this.access = value;
            return this;
        }

        /**
         * Sets the {@code removedReadonlyPaths} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder removedReadonlyPaths(List<String> value) {
            this.removedReadonlyPaths = value;
            return this;
        }

        /**
         * Sets the {@code persistedPath} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder persistedPath(String value) {
            this.persistedPath = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public PermissionSandboxPathGrant build() {
            if (path == null) throw new IllegalStateException("Missing required property: path");
            if (access == null) throw new IllegalStateException("Missing required property: access");
            return new PermissionSandboxPathGrant(path, deniedPath, access, removedReadonlyPaths, persistedPath);
        }
    }
}
