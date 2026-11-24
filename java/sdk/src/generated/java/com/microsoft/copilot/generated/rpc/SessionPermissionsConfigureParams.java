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
 * Patch of permission policy fields to apply (omit a field to leave it unchanged).
 *
 * @apiNote This method is experimental and may change in a future version.
 * @since 1.0.0
 */
@CopilotExperimental
@javax.annotation.processing.Generated("copilot-sdk-codegen")
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public record SessionPermissionsConfigureParams(
    /** Target session identifier */
    @JsonProperty("sessionId") String sessionId,
    /** If specified, sets whether tool permission requests are auto-approved without prompting. Omit to leave the current value unchanged. */
    @JsonProperty("approveAllToolPermissionRequests") Boolean approveAllToolPermissionRequests,
    /** If specified, sets whether path/URL read permission requests are auto-approved. Omit to leave the current value unchanged. */
    @JsonProperty("approveAllReadPermissionRequests") Boolean approveAllReadPermissionRequests,
    /** If specified, replaces the session's approved/denied permission rules. Omit to leave the current rules unchanged. */
    @JsonProperty("rules") PermissionRulesSet rules,
    /** If specified, replaces the session's path-permission policy. The runtime constructs the appropriate PathManager based on these inputs (rooted at the session's working directory). Omit to leave the current path policy unchanged. */
    @JsonProperty("paths") PermissionPathsConfig paths,
    /** If specified, replaces the session's URL-permission policy. The runtime constructs a fresh DefaultUrlManager based on these inputs. Omit to leave the current URL policy unchanged. */
    @JsonProperty("urls") PermissionUrlsConfig urls,
    /** If specified, replaces the host-supplied GitHub Content Exclusion policies on the session (combined with natively-discovered policies when evaluating tool/file access). Omit to leave the current policies unchanged. */
    @JsonProperty("additionalContentExclusionPolicies") List<PermissionsConfigureAdditionalContentExclusionPolicy> additionalContentExclusionPolicies,
    /** User-authored instructions appended verbatim to the assisted-approval judge's system prompt. Applies to subsequent judge requests in this local session without changing permission mode, pre-judge gates, or the coding agent's prompt. Omit to leave unchanged; an empty or whitespace-only string clears the instructions. Hosts must reapply their configuration after resuming a session. Remote sessions are not supported. */
    @JsonProperty("assistedApprovalInstructions") String assistedApprovalInstructions,
    /** SDK-host advisory JSON for future Assisted Permissions judge requests. The value replaces only its named source; null clears that source. Omit to leave all sources unchanged. The runtime applies strict structural and size limits, renders the source in a runtime-controlled advisory frame, and never treats the value as instructions or authorization. Session-scoped only; hosts must reapply after resume. */
    @JsonProperty("assistedApprovalContext") PermissionsConfigureAssistedApprovalContext assistedApprovalContext,
    /** Optional owner-authenticated per-source update to lifecycle-fenced session authority. Active grants from all sources are rendered together, in source order, to the Assisted Permissions judge, which alone interprets their statements and scope. Only the owning non-extension session connection, bound to the current session generation, may replace or revoke authority. Omit to leave unchanged. Hard runtime controls, mandatory policy, and later human restrictions still take precedence. Session-scoped only; hosts must reapply after resume. */
    @JsonProperty("authenticatedAuthority") AuthenticatedAuthorityUpdate authenticatedAuthority,
    /** Declares whether this connection's registered permission handler applies non-human host policy to the tools and canvases this connection registered. When true, unattended Assisted Autopilot sends permission requests for those tools, and `open_canvas` and `invoke_canvas_action` requests whose target canvas this connection registered, to the handler before runtime Assisted Approval or unattended fallback can settle them. All other requests, including shell commands, file access, URLs, and tools or canvases registered by other connections or extensions, keep runtime settlement. Managed policy and other runtime enforcement still take precedence. Only the session's owning non-extension connection may set this field. The declaration ends when that connection detaches or the session is resumed, so hosts must reapply it after resume. Omit to leave unchanged; false clears it. Without a declaration, registered handlers are not consulted during unattended Assisted Autopilot. Remote sessions are not supported. */
    @JsonProperty("hostPolicyPermissionHandler") Boolean hostPolicyPermissionHandler,
    /** Declares a non-prompting host-policy callback that the runtime may call before the Assisted Permissions judge for eligible path, read, and custom-tool requests. Only the owning non-extension connection may set it. False disables the declaration. Hosts must reapply after resume. Remote sessions are not supported. */
    @JsonProperty("hostPolicyPreCheck") PermissionsConfigureHostPolicyPreCheck hostPolicyPreCheck
) {

    /**
     * Creates a record positionally.
     * Use {@link #builder()} for named construction.
     */
    public SessionPermissionsConfigureParams {}

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
        private Boolean approveAllToolPermissionRequests;
        private Boolean approveAllReadPermissionRequests;
        private PermissionRulesSet rules;
        private PermissionPathsConfig paths;
        private PermissionUrlsConfig urls;
        private List<PermissionsConfigureAdditionalContentExclusionPolicy> additionalContentExclusionPolicies;
        private String assistedApprovalInstructions;
        private PermissionsConfigureAssistedApprovalContext assistedApprovalContext;
        private AuthenticatedAuthorityUpdate authenticatedAuthority;
        private Boolean hostPolicyPermissionHandler;
        private PermissionsConfigureHostPolicyPreCheck hostPolicyPreCheck;

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
         * Sets the {@code approveAllToolPermissionRequests} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder approveAllToolPermissionRequests(Boolean value) {
            this.approveAllToolPermissionRequests = value;
            return this;
        }

        /**
         * Sets the {@code approveAllReadPermissionRequests} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder approveAllReadPermissionRequests(Boolean value) {
            this.approveAllReadPermissionRequests = value;
            return this;
        }

        /**
         * Sets the {@code rules} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder rules(PermissionRulesSet value) {
            this.rules = value;
            return this;
        }

        /**
         * Sets the {@code paths} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder paths(PermissionPathsConfig value) {
            this.paths = value;
            return this;
        }

        /**
         * Sets the {@code urls} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder urls(PermissionUrlsConfig value) {
            this.urls = value;
            return this;
        }

        /**
         * Sets the {@code additionalContentExclusionPolicies} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder additionalContentExclusionPolicies(List<PermissionsConfigureAdditionalContentExclusionPolicy> value) {
            this.additionalContentExclusionPolicies = value;
            return this;
        }

        /**
         * Sets the {@code assistedApprovalInstructions} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder assistedApprovalInstructions(String value) {
            this.assistedApprovalInstructions = value;
            return this;
        }

        /**
         * Sets the {@code assistedApprovalContext} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder assistedApprovalContext(PermissionsConfigureAssistedApprovalContext value) {
            this.assistedApprovalContext = value;
            return this;
        }

        /**
         * Sets the {@code authenticatedAuthority} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder authenticatedAuthority(AuthenticatedAuthorityUpdate value) {
            this.authenticatedAuthority = value;
            return this;
        }

        /**
         * Sets the {@code hostPolicyPermissionHandler} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder hostPolicyPermissionHandler(Boolean value) {
            this.hostPolicyPermissionHandler = value;
            return this;
        }

        /**
         * Sets the {@code hostPolicyPreCheck} property.
         * @param value the property value
         * @return this builder
         * @since 2.0.0
         */
        @com.microsoft.copilot.CopilotExperimental
        public Builder hostPolicyPreCheck(PermissionsConfigureHostPolicyPreCheck value) {
            this.hostPolicyPreCheck = value;
            return this;
        }

        /**
         * Builds a record after checking required property presence.
         * @return a new record
         * @throws IllegalStateException if a required property is missing
         * @since 2.0.0
         */
        public SessionPermissionsConfigureParams build() {
            return new SessionPermissionsConfigureParams(sessionId, approveAllToolPermissionRequests, approveAllReadPermissionRequests, rules, paths, urls, additionalContentExclusionPolicies, assistedApprovalInstructions, assistedApprovalContext, authenticatedAuthority, hostPolicyPermissionHandler, hostPolicyPreCheck);
        }
    }
}
