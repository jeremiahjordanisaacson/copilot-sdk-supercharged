//! Auto-generated from api.schema.json — do not edit manually.

#![allow(clippy::large_enum_variant)]
#![allow(deprecated)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(rustdoc::invalid_html_tags)]

use std::collections::HashMap;

pub(crate) use lifecycle_diagnostics::LifecycleDiagnosticEvent;
use serde::{Deserialize, Serialize};
pub use session_events::{
    AbortReason, AgentModelPolicy, AssistedApprovalAssessment, AutoTier, BlockedPermissionAction,
    ContextTier, IdeConnectedInfo, IdeSelection, IndexedSearchState, ManagedPermissionsContext,
    ManagedSettingsResolvedSource, McpOauthHttpResponse, McpOauthWWWAuthenticateParams,
    McpServerMetadata, McpServerSource, McpServerStatus, McpSkillOrigin, ModelChangeSource,
    ModelProviderKind, ModelProviderRef, OmittedBinaryOmittedReason, PermissionDecisionSource,
    PermissionMode, PermissionPromptRequest, PermissionRule, PermissionSandboxPathGrant,
    PolicyHelperCapabilities, PolicyHelperFailure, ProviderQuotaState, ReasoningSummary,
    RemediationAction, SessionLimitsConfig, SessionMode, ShutdownType, SkillSource, TaskBlocker,
    TaskCompletionOutcome, UsageGetMetricsResult, UserInputImageAttachment,
    UserToolSessionApproval, Verbosity,
};

use super::sdk_api_shared::*;
use crate::generated::{lifecycle_diagnostics, session_events};
use crate::types::{RequestId, SessionEvent, SessionId};

/// The stored sandbox settings to resolve against the defaults.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxApplyDefaultsRequest {
    /// The sandbox settings as stored in the settings file. An empty object resolves to the defaults alone.
    pub stored: serde_json::Value,
}
crate::record_serde::wire_record!(SandboxApplyDefaultsRequest { stored: "stored" });

/// Sandbox configurations to compare under runtime identity rules.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxConfigsEqualParams {
    /// Previous configuration. Absent means no sandbox.
    pub a: Option<serde_json::Value>,
    /// New configuration. Absent means no sandbox.
    pub b: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(SandboxConfigsEqualParams { a: "a" if is_none, b: "b" if is_none });

/// The result of atomically consuming the user's first-enable sandbox marker.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxConsumeOnboardingResult {
    /// True only when this call first records that the user saw sandbox onboarding. False when a previous call or sandbox enable command already recorded the marker.
    pub first_enable: bool,
}
crate::record_serde::wire_record!(SandboxConsumeOnboardingResult {
    first_enable: "firstEnable"
});

/// Possible unconfigured secrets in the sandbox shell environment, sorted by variable name, with no secret values.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxCredentialSuggestionsResult {
    /// Candidates for user review. Empty when sandboxing is disabled or no unconfigured candidates are present.
    pub suggestions: Vec<SandboxCredentialSuggestion>,
}
crate::record_serde::wire_record!(SandboxCredentialSuggestionsResult {
    suggestions: "suggestions"
});

/// Request to disable sandboxing for the current session while resolving an active sandbox-bypass permission prompt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxDisableForSessionRequest {
    /// Optional attribution for the permission decision. A claim that a person decided (`source` `human_response`) counts only from the session's authenticated owning connection or the in-process host.
    pub decision_context: Option<PermissionDecisionContext>,
    /// Identifier of the exact pending sandbox-bypass permission request that authorized the session opt-out.
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(SandboxDisableForSessionRequest { decision_context: "decisionContext" if is_none, request_id: "requestId" });

/// Result of attempting to disable sandboxing for the current session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxDisableForSessionResult {
    /// The authoritative sandbox enabled state after the operation.
    pub enabled: bool,
    /// Whether this call resolved the pending request and applied the session opt-out.
    pub success: bool,
}
crate::record_serde::wire_record!(SandboxDisableForSessionResult {
    enabled: "enabled",
    success: "success"
});

/// The documentation URL a host cites when it declines or downgrades a sandbox setting.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxDocsUrlResult {
    /// Documentation URL describing the sandbox and the settings that control it.
    pub url: String,
}
crate::record_serde::wire_record!(SandboxDocsUrlResult { url: "url" });

/// A sandbox config and the per-feature routing flag to resolve it for.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxEffectiveForOptionalParams {
    /// The routing flag naming the feature being launched, such as `sandboxMcpServers` or `sandboxLspServers`.
    pub routing_flag: String,
    /// The session's resolved `sandbox` slice. Omitted when the host has none, which yields a disabled sandbox rather than an error.
    pub sandbox: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(SandboxEffectiveForOptionalParams { routing_flag: "routingFlag", sandbox: "sandbox" if is_none });

/// The sandbox a named feature actually launches under.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxEffectiveForOptionalResult {
    /// The effective sandbox config for the requested feature, or a disabled sandbox when the feature is routed out of it.
    pub sandbox_json: serde_json::Value,
}
crate::record_serde::wire_record!(SandboxEffectiveForOptionalResult {
    sandbox_json: "sandboxJson"
});

/// One slash command, as typed, to test against the enforcement-block allowlist.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxEnforcementCommandAllowedParams {
    /// The command's arguments, in order. Pass an empty array for a bare command: several entries in the allowlist admit the bare form and refuse every argument.
    pub args: Vec<String>,
    /// The command name as the user typed it, with or without its leading slash.
    pub command_name: String,
}
crate::record_serde::wire_record!(SandboxEnforcementCommandAllowedParams {
    args: "args",
    command_name: "commandName"
});

/// Managed sandbox enforcement state for a session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxEnforcementStatus {
    /// Whether an enforcement failure has permanently blocked the session.
    pub blocked: bool,
    /// The first sandbox enforcement failure that blocked the session.
    pub reason: Option<String>,
    /// Whether the effective managed policy requires an available sandbox backend.
    pub required: bool,
}
crate::record_serde::wire_record!(SandboxEnforcementStatus { blocked: "blocked", reason: "reason" if is_none, required: "required" });

/// One file-picker suggestion offered up for the sandbox filesystem check.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxPickerCandidate {
    /// Whether the suggestion is a directory. A directory the policy does not itself allow may still be needed as a waypoint to an allowed subtree.
    pub is_directory: bool,
    /// Absolute path the suggestion would attach.
    pub path: String,
}
crate::record_serde::wire_record!(SandboxPickerCandidate {
    is_directory: "isDirectory",
    path: "path"
});

/// A batch of file-picker candidates to check against a prepared policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxFilterPickerCandidatesParams {
    /// The candidates to check, in the order the answers are returned.
    #[doc(hidden)]
    pub(crate) candidates: Vec<SandboxPickerCandidate>,
    /// The session's working directory, matching the one the policy was prepared from.
    pub cwd: String,
    /// The policy returned by `sandbox.preparePickerPolicy`. Omitted keeps every candidate.
    pub sandbox: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(SandboxFilterPickerCandidatesParams { candidates: "candidates", cwd: "cwd", sandbox: "sandbox" if is_none });

/// Per-candidate allow decisions, positionally aligned with the request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxFilterPickerCandidatesResult {
    /// One decision per candidate, in the same order. Always the same length as the request's `candidates`.
    pub allowed: Vec<bool>,
}
crate::record_serde::wire_record!(SandboxFilterPickerCandidatesResult { allowed: "allowed" });

/// Remote MCP endpoint and the host's existing proxy secret directory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxGetRemoteMcpEgressRequest {
    /// Existing proxy secret directory on the runtime host.
    pub secret_config_dir: Option<String>,
    /// Target endpoint for the session network policy decision.
    pub url: Option<String>,
}
crate::record_serde::wire_record!(SandboxGetRemoteMcpEgressRequest { secret_config_dir: "secretConfigDir" if is_none, url: "url" if is_none });

/// Session-owned remote MCP egress decision. A denial forbids connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxGetRemoteMcpEgressResult {
    /// Reason a connection is forbidden; absent when allowed.
    pub denied_reason: Option<String>,
    /// Authorized proxy URL; absent for a direct connection or a denial.
    pub proxy_url: Option<String>,
}
crate::record_serde::wire_record!(SandboxGetRemoteMcpEgressResult { denied_reason: "deniedReason" if is_none, proxy_url: "proxyUrl" if is_none });

/// Request to accept the sandbox path grant offered on an active sandbox escalation permission prompt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxGrantPathForRequestRequest {
    /// Optional attribution for the permission decision. A claim that a person decided (`source` `human_response`) counts only from the session's authenticated owning connection or the in-process host.
    pub decision_context: Option<PermissionDecisionContext>,
    /// Identifier of the exact pending sandbox escalation permission request whose sandboxPathGrant to accept.
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(SandboxGrantPathForRequestRequest { decision_context: "decisionContext" if is_none, request_id: "requestId" });

/// Result of accepting a sandbox path grant.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxGrantPathForRequestResult {
    /// Whether this call resolved the pending request and added the path to the session's sandbox policy.
    pub success: bool,
}
crate::record_serde::wire_record!(SandboxGrantPathForRequestResult { success: "success" });

/// Whether this host can run one sandbox policy feature. A session whose effective policy uses an unsupported feature fails each sandboxed command with `reason`, except `filesystem_enumeration`, whose absence degrades sandboxed PowerShell instead of failing it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxHostCapability {
    /// The policy feature, as an extensible string: ignore names you do not recognize. Known values: `network` (sandboxed commands can reach the network; on Linux this needs the tooling for Bubblewrap's private network namespace, such as slirp4netns), `network_filtering` (host rules and the sandbox proxy; on Linux this needs the same tooling as `network`; on Windows it needs Process Security Environment 1.1 host-loopback support or MXC's PSEC 1.0-only proxy-loopback compatibility capability, and a policy that uses it must also set `network.allowLocalNetwork`; compatibility applies only to an explicit identity-less runtime proxy, not general host-loopback access, and other policy restrictions still apply), `denied_paths` (native enforcement of `filesystem.deniedPaths`), `shell` (shell commands inside the sandbox), and `filesystem_enumeration` (enumerate-only filesystem grants: PowerShell's drive roots and `filesystem.enumeratePaths`; on Windows this needs Process Security Environment 1.1 filesystem enumeration support, and without it sandboxed PowerShell still runs but cannot resolve its current location, and `filesystem.enumeratePaths` entries are ignored; other platforms always report it).
    pub name: String,
    /// Human-readable reason and remedy when the feature is unsupported, such as a package to install or an OS update. Present only when `supported` is false.
    pub reason: Option<String>,
    /// Whether this host can run the feature.
    pub supported: bool,
}
crate::record_serde::wire_record!(SandboxHostCapability { name: "name", reason: "reason" if is_none, supported: "supported" });

/// Whether the host running this runtime can run the command sandbox. The runtime checks `supported` once per process. A capability answer can change while the process runs, for example after the user installs a missing package.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxHostSupport {
    /// Sandbox policy features whose availability varies between hosts that can run the backend. Empty when `supported` is false, because no feature can run without a backend. Later runtimes can add entries; ignore an entry whose `name` you do not recognize.
    pub capabilities: Vec<SandboxHostCapability>,
    /// Human-readable reason the sandbox cannot run on this host. Present only when `supported` is false.
    pub reason: Option<String>,
    /// Whether a process-containment backend is usable on this host: Seatbelt on macOS, Bubblewrap on Linux, or ProcessContainer on Windows.
    pub supported: bool,
}
crate::record_serde::wire_record!(SandboxHostSupport { capabilities: "capabilities", reason: "reason" if is_none, supported: "supported" });

/// Request for an admitted host to use its original remote sandbox owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SandboxInvokeOwnedRequest {
    /// Typed operation and its existing request parameters.
    #[doc(hidden)]
    pub(crate) operation: serde_json::Value,
    /// Transport selector; runtime authority and bindings still govern access.
    #[doc(hidden)]
    pub(crate) owner: SandboxTransportOwner,
}

/// Existing result of the sandbox operation on its original transport owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxInvokeOwnedResult {
    /// Operation-specific result, including null for operations without a value.
    pub value: serde_json::Value,
}
crate::record_serde::wire_record!(SandboxInvokeOwnedResult { value: "value" });

/// Effective sandbox and managed policy used for the learning-mode probe.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxLearningModeStartParams {
    /// Effective sandbox configuration.
    pub config: serde_json::Value,
    /// Authoritative managed sandbox floor.
    pub managed_sandbox: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(SandboxLearningModeStartParams { config: "config", managed_sandbox: "managedSandbox" if is_none });

/// How shell commands start under the managed learning policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxLearningModeStartResult {
    /// Explanation when the host cannot apply the managed learning policy.
    pub notice: Option<String>,
    /// Whether the host can allow and record denials.
    pub relaxed: bool,
}
crate::record_serde::wire_record!(SandboxLearningModeStartResult { notice: "notice" if is_none, relaxed: "relaxed" });

/// One sandbox value to normalize before saving it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxNormalizeInputParams {
    /// Runtime rule to apply.
    pub kind: SandboxInputKind,
    /// User-entered value.
    pub value: String,
}
crate::record_serde::wire_record!(SandboxNormalizeInputParams {
    kind: "kind",
    value: "value"
});

/// Normalized value, absent when the runtime rejects a proxy URL.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxNormalizeInputResult {
    /// Accepted value. Invalid host patterns produce a request error.
    pub value: Option<String>,
}
crate::record_serde::wire_record!(SandboxNormalizeInputResult { value: "value" if is_none });

/// The path a user is about to grant.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxPathGrantWarningParams {
    /// Whether the grant is read-only. Omitted means read/write, which gets the stricter advice.
    pub read_only: Option<bool>,
    /// The grant as it will reach the sandbox: unquoted, anchored to the session's working directory and normalized. A relative path can give a false negative.
    pub resolved_path: String,
}
crate::record_serde::wire_record!(SandboxPathGrantWarningParams { read_only: "readOnly" if is_none, resolved_path: "resolvedPath" });

/// Advice for a path grant, when it needs any.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxPathGrantWarningResult {
    /// The user-facing warning. Absent when the grant needs none, so a host can call unconditionally and show only what it gets.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(SandboxPathGrantWarningResult { warning: "warning" if is_none });

/// Policy values a host needs to describe the effective sandbox.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxPolicyPresentationParams {
    /// Whether managed policy allows an individual command bypass.
    pub allow_bypass: Option<bool>,
    /// Effective sandbox configuration, after managed composition.
    pub config: serde_json::Value,
    /// Whether unresolved policy enabled the fail-closed sandbox floor.
    pub enabled_by_undetermined_policy: Option<bool>,
    /// Whether managed policy requires the sandbox.
    pub managed_requires_sandbox: Option<bool>,
    /// Whether the managed policy remains unresolved.
    pub policy_undetermined: Option<bool>,
    /// Explicit sandbox flag, before feature gating.
    pub sandbox_flag: Option<bool>,
    /// Saved user preference before the managed floor applies.
    pub user_sandbox_enabled: Option<bool>,
}
crate::record_serde::wire_record!(SandboxPolicyPresentationParams { allow_bypass: "allowBypass" if is_none, config: "config", enabled_by_undetermined_policy: "enabledByUndeterminedPolicy" if is_none, managed_requires_sandbox: "managedRequiresSandbox" if is_none, policy_undetermined: "policyUndetermined" if is_none, sandbox_flag: "sandboxFlag" if is_none, user_sandbox_enabled: "userSandboxEnabled" if is_none });

/// Authoritative policy labels and opt-out capability for a host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxPolicyPresentationResult {
    /// Notice used when the user declines an advisory managed floor.
    pub advisory_floor_declined_notice: String,
    /// Whether the effective policy permits a session opt-out.
    pub allows_opt_out: bool,
    /// Notice when managed policy overrides an explicit flag.
    pub flag_overridden_by_policy_notice: Option<String>,
    /// Notice when managed policy enables the sandbox.
    pub managed_floor_notice: Option<String>,
}
crate::record_serde::wire_record!(SandboxPolicyPresentationResult { advisory_floor_declined_notice: "advisoryFloorDeclinedNotice", allows_opt_out: "allowsOptOut", flag_overridden_by_policy_notice: "flagOverriddenByPolicyNotice" if is_none, managed_floor_notice: "managedFloorNotice" if is_none });

/// The working directory and sandbox settings to build a file-picker policy from.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxPreparePickerPolicyParams {
    /// The session's working directory. Git discovery and grant canonicalization are resolved against it.
    pub cwd: String,
    /// The session's resolved `sandbox` slice. Omitted, or disabled, yields no policy and every candidate is kept.
    pub sandbox: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(SandboxPreparePickerPolicyParams { cwd: "cwd", sandbox: "sandbox" if is_none });

/// The prepared file-picker policy, or none when the sandbox is disabled.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxPreparePickerPolicyResult {
    /// The prepared policy to pass back to `sandbox.filterPickerCandidates`. Absent when the sandbox is disabled or absent, in which case filtering keeps every candidate.
    pub sandbox_json: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(SandboxPreparePickerPolicyResult { sandbox_json: "sandboxJson" if is_none });

/// Result of creating the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaCreateResult {
    /// Absolute path of the public certificate of the certificate authority, in PEM format.
    pub certificate_path: String,
}
crate::record_serde::wire_record!(SandboxProxyCaCreateResult {
    certificate_path: "certificatePath"
});

/// Selects a Copilot profile and the credential hosts that its sandbox credential-proxy certificate authority must cover. The runtime always adds the hosts from that profile's saved user settings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaProfileRequest {
    /// Copilot configuration directory that owns the certificate authority and saved credential hosts. Omit it to use the runtime's default Copilot home.
    pub config_dir: Option<String>,
    /// The sandbox configuration that the host gives its sessions. The runtime reads the credential hosts from `auth` and `credentials`; it ignores `enabled` and the other fields.
    pub sandbox_config: Option<SandboxConfig>,
}
crate::record_serde::wire_record!(SandboxProxyCaProfileRequest { config_dir: "configDir" if is_none, sandbox_config: "sandboxConfig" if is_none });

/// Identifies the credential hosts that the persistent certificate authority of the sandbox credential proxy must cover. The runtime always adds the hosts from the saved user settings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaRequest {
    /// The sandbox configuration that the host gives its sessions. The runtime reads the credential hosts from `auth` and `credentials`; it ignores `enabled` and the other fields.
    pub sandbox_config: Option<SandboxConfig>,
}
crate::record_serde::wire_record!(SandboxProxyCaRequest { sandbox_config: "sandboxConfig" if is_none });

/// Status of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaStatus {
    /// Whether this process can add the certificate authority to OS trust without credentials from a different user. False where OS trust is unsupported, and on Windows when the process cannot elevate itself to write the machine trust store. When false, do not offer to set up the certificate authority.
    pub can_install: bool,
    /// Human-readable reason for the state. On `installed` or `notInstalled`, present only when the certificate authority must be rotated, and then says why.
    pub detail: Option<String>,
    /// The state of the certificate authority.
    pub state: SandboxProxyCaState,
}
crate::record_serde::wire_record!(SandboxProxyCaStatus { can_install: "canInstall", detail: "detail" if is_none, state: "state" });

/// A certificate file and the credential hosts permitted by the caller for OS trust.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaTrustCertificateRequest {
    /// Additional permitted credential hosts. The runtime always includes the selected profile's saved credential hosts and validates the certificate constraints before it changes OS trust.
    pub allow_hosts: Vec<String>,
    /// Public certificate file to validate and trust. Omit it to trust the certificate stored in the selected Copilot home.
    pub certificate_file: Option<String>,
    /// Copilot configuration directory whose saved credential hosts authorize trust. Omit it to use the default Copilot home.
    pub config_dir: Option<String>,
}
crate::record_serde::wire_record!(SandboxProxyCaTrustCertificateRequest { allow_hosts: "allowHosts", certificate_file: "certificateFile" if is_none, config_dir: "configDir" if is_none });

/// The stored sandbox proxy password, if one is present.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxySecretGetResult {
    /// The stored password. Absent when no entry exists in either the keychain or the on-disk fallback store.
    pub value: Option<String>,
}
crate::record_serde::wire_record!(SandboxProxySecretGetResult { value: "value" if is_none });

/// Where this runtime keeps the sandbox proxy password.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxySecretLocation {
    /// Absolute path of the credential store holding the sandbox proxy password. The proxy password belongs to no MCP server, so it is kept in its own directory rather than in the MCP secret store.
    pub config_dir: String,
    /// Identifier of the proxy password entry within that store. Derived from the configuration directory, so two Copilot homes on one machine never share an entry. Use it to recognize a `${secret:…}` reference in stored settings as this runtime's own proxy password.
    pub secret_id: String,
}
crate::record_serde::wire_record!(SandboxProxySecretLocation {
    config_dir: "configDir",
    secret_id: "secretId"
});

/// The sandbox proxy password to store.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxySecretSaveRequest {
    /// The password to store for the sandbox proxy.
    pub value: String,
}
crate::record_serde::wire_record!(SandboxProxySecretSaveRequest { value: "value" });

/// Where the sandbox proxy password was stored.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxySecretSaveResult {
    /// Whether the OS keychain accepted the value. False means it was written to the on-disk fallback store instead, which is still readable by `sandbox.proxySecret.get`.
    pub stored_in_keychain: bool,
}
crate::record_serde::wire_record!(SandboxProxySecretSaveResult {
    stored_in_keychain: "storedInKeychain"
});

/// Saved preference to adopt after clearing this session's temporary choices.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxResetSessionChoicesRequest {
    /// Host preference before the runtime applies the managed floor.
    pub config: serde_json::Value,
    /// Origin of the saved preference; this field grants no bypass authority.
    pub config_source: SandboxConfigSource,
}
crate::record_serde::wire_record!(SandboxResetSessionChoicesRequest {
    config: "config",
    config_source: "configSource"
});

/// The user's sandbox settings and the managed sandbox floor to raise them to.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxResolveFlooredConfigParams {
    /// The effective managed `sandbox` slice acting as a floor. Omitted when no administrator policy applies, in which case the user's own settings stand unchanged.
    pub managed_sandbox: Option<serde_json::Value>,
    /// The user's own `sandbox` settings slice. Omitted when the user has configured none, which still yields the managed floor rather than a disabled sandbox.
    pub user_sandbox: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(SandboxResolveFlooredConfigParams { managed_sandbox: "managedSandbox" if is_none, user_sandbox: "userSandbox" if is_none });

/// The user's sandbox settings raised to the managed floor.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxResolveFlooredConfigResult {
    /// The floored sandbox config, normalized through the declared sandbox shape so only declared fields in a fixed order are present.
    pub sandbox_json: serde_json::Value,
}
crate::record_serde::wire_record!(SandboxResolveFlooredConfigResult {
    sandbox_json: "sandboxJson"
});

/// Effective session policy and runtime-owned session choices.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxSessionState {
    /// Whether queued sandbox bypass requests may use this session opt-out.
    pub auto_approve_bypass: bool,
    /// Effective sandbox configuration after managed composition.
    pub config: serde_json::Value,
    /// Exact runtime attribution of the effective sandbox choice.
    pub config_source: String,
    /// Whether this session holds an authorized or local session opt-out.
    pub disabled_for_session: bool,
    /// Runtime incarnation of the live session.
    pub incarnation: i64,
    /// Monotonic revision of this live session's sandbox projection.
    pub revision: i64,
    /// Explicit session enable choice, when one exists.
    pub session_override: Option<bool>,
}
crate::record_serde::wire_record!(SandboxSessionState {
    auto_approve_bypass: "autoApproveBypass",
    config: "config",
    config_source: "configSource",
    disabled_for_session: "disabledForSession",
    incarnation: "incarnation",
    revision: "revision",
    session_override: "sessionOverride"
});

/// The host context to read sandbox settings from.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxSettingsLoadRequest {
    /// The configuration directory, home directory, and environment used to resolve settings.json.
    pub context: UserSettingsHostContext,
    /// Reject a settings warning instead of returning recovered settings, for a caller about to edit the document.
    pub reject_warnings: Option<bool>,
}
crate::record_serde::wire_record!(SandboxSettingsLoadRequest { context: "context", reject_warnings: "rejectWarnings" if is_none });

/// The resolved sandbox settings and the user's sparse stored override.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxSettingsLoadResult {
    /// Sandbox settings with all runtime defaults applied.
    pub resolved: serde_json::Value,
    /// The resolved path to the user's settings.json file.
    pub settings_path: String,
    /// The stored sandbox object, or an empty object when the user has no override.
    pub stored: serde_json::Value,
}
crate::record_serde::wire_record!(SandboxSettingsLoadResult {
    resolved: "resolved",
    settings_path: "settingsPath",
    stored: "stored"
});

/// A literal sandbox path grant to save in the user's settings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxSettingsPersistPathGrantRequest {
    /// The configuration directory, home directory, and environment used to resolve settings.json.
    pub context: UserSettingsHostContext,
    /// The path grant to persist, including read-only entries to remove by their exact stored spelling.
    pub grant: PermissionSandboxPathGrant,
}
crate::record_serde::wire_record!(SandboxSettingsPersistPathGrantRequest {
    context: "context",
    grant: "grant"
});

/// A sandbox settings patch and the host context that selects its settings file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxSettingsUpdateRequest {
    /// The configuration directory, home directory, and environment used to resolve settings.json.
    pub context: UserSettingsHostContext,
    /// Sandbox settings changes. Filesystem, network, and seatbelt fields merge into userPolicy; other supported fields merge at the sandbox root.
    pub patch: serde_json::Value,
}
crate::record_serde::wire_record!(SandboxSettingsUpdateRequest {
    context: "context",
    patch: "patch"
});

/// The managed sandbox policy in force, which selects which remedy the notice offers.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxUnsupportedStartupNoticeParams {
    /// True when the sandbox stayed on only because the managed policy could not be determined. Distinguishes a policy that positively enforces the sandbox from one that merely failed to resolve, which is the difference between telling the user to contact an administrator and telling them to wait. Defaults to false.
    pub enabled_by_undetermined_policy: Option<bool>,
    /// The managed `sandbox` slice, when a policy supplies one. The notice treats the sandbox as enforced only when this both enables it and sets `failIfUnavailable`, because a managed sandbox that tolerates being unavailable still leaves the user able to turn it off.
    pub managed_sandbox: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(SandboxUnsupportedStartupNoticeParams { enabled_by_undetermined_policy: "enabledByUndeterminedPolicy" if is_none, managed_sandbox: "managedSandbox" if is_none });

/// The startup warning, absent when the machine can run a sandbox.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxUnsupportedStartupNoticeResult {
    /// The user-facing warning, carrying the platform remedy and the recovery the policy in force actually allows. Absent when this machine supports a sandbox, so a host can call unconditionally and print only what it gets.
    pub notice: Option<String>,
}
crate::record_serde::wire_record!(SandboxUnsupportedStartupNoticeResult { notice: "notice" if is_none });

/// Status of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaRemoveForConfigDirResult {
    /// Whether this process can add the certificate authority to OS trust without credentials from a different user. False where OS trust is unsupported, and on Windows when the process cannot elevate itself to write the machine trust store. When false, do not offer to set up the certificate authority.
    pub can_install: bool,
    /// Human-readable reason for the state. On `installed` or `notInstalled`, present only when the certificate authority must be rotated, and then says why.
    pub detail: Option<String>,
    /// The state of the certificate authority.
    pub state: SandboxProxyCaState,
}
crate::record_serde::wire_record!(SandboxProxyCaRemoveForConfigDirResult { can_install: "canInstall", detail: "detail" if is_none, state: "state" });

/// Status of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaGetStatusResult {
    /// Whether this process can add the certificate authority to OS trust without credentials from a different user. False where OS trust is unsupported, and on Windows when the process cannot elevate itself to write the machine trust store. When false, do not offer to set up the certificate authority.
    pub can_install: bool,
    /// Human-readable reason for the state. On `installed` or `notInstalled`, present only when the certificate authority must be rotated, and then says why.
    pub detail: Option<String>,
    /// The state of the certificate authority.
    pub state: SandboxProxyCaState,
}
crate::record_serde::wire_record!(SandboxProxyCaGetStatusResult { can_install: "canInstall", detail: "detail" if is_none, state: "state" });

/// Status of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaRotateResult {
    /// Whether this process can add the certificate authority to OS trust without credentials from a different user. False where OS trust is unsupported, and on Windows when the process cannot elevate itself to write the machine trust store. When false, do not offer to set up the certificate authority.
    pub can_install: bool,
    /// Human-readable reason for the state. On `installed` or `notInstalled`, present only when the certificate authority must be rotated, and then says why.
    pub detail: Option<String>,
    /// The state of the certificate authority.
    pub state: SandboxProxyCaState,
}
crate::record_serde::wire_record!(SandboxProxyCaRotateResult { can_install: "canInstall", detail: "detail" if is_none, state: "state" });

/// Status of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaTrustResult {
    /// Whether this process can add the certificate authority to OS trust without credentials from a different user. False where OS trust is unsupported, and on Windows when the process cannot elevate itself to write the machine trust store. When false, do not offer to set up the certificate authority.
    pub can_install: bool,
    /// Human-readable reason for the state. On `installed` or `notInstalled`, present only when the certificate authority must be rotated, and then says why.
    pub detail: Option<String>,
    /// The state of the certificate authority.
    pub state: SandboxProxyCaState,
}
crate::record_serde::wire_record!(SandboxProxyCaTrustResult { can_install: "canInstall", detail: "detail" if is_none, state: "state" });

/// Status of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaGetStatusForConfigDirResult {
    /// Whether this process can add the certificate authority to OS trust without credentials from a different user. False where OS trust is unsupported, and on Windows when the process cannot elevate itself to write the machine trust store. When false, do not offer to set up the certificate authority.
    pub can_install: bool,
    /// Human-readable reason for the state. On `installed` or `notInstalled`, present only when the certificate authority must be rotated, and then says why.
    pub detail: Option<String>,
    /// The state of the certificate authority.
    pub state: SandboxProxyCaState,
}
crate::record_serde::wire_record!(SandboxProxyCaGetStatusForConfigDirResult { can_install: "canInstall", detail: "detail" if is_none, state: "state" });

/// Result of creating the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaCreateForConfigDirResult {
    /// Absolute path of the public certificate of the certificate authority, in PEM format.
    pub certificate_path: String,
}
crate::record_serde::wire_record!(SandboxProxyCaCreateForConfigDirResult {
    certificate_path: "certificatePath"
});

/// Status of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaRotateForConfigDirResult {
    /// Whether this process can add the certificate authority to OS trust without credentials from a different user. False where OS trust is unsupported, and on Windows when the process cannot elevate itself to write the machine trust store. When false, do not offer to set up the certificate authority.
    pub can_install: bool,
    /// Human-readable reason for the state. On `installed` or `notInstalled`, present only when the certificate authority must be rotated, and then says why.
    pub detail: Option<String>,
    /// The state of the certificate authority.
    pub state: SandboxProxyCaState,
}
crate::record_serde::wire_record!(SandboxProxyCaRotateForConfigDirResult { can_install: "canInstall", detail: "detail" if is_none, state: "state" });

/// Status of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaTrustForConfigDirResult {
    /// Whether this process can add the certificate authority to OS trust without credentials from a different user. False where OS trust is unsupported, and on Windows when the process cannot elevate itself to write the machine trust store. When false, do not offer to set up the certificate authority.
    pub can_install: bool,
    /// Human-readable reason for the state. On `installed` or `notInstalled`, present only when the certificate authority must be rotated, and then says why.
    pub detail: Option<String>,
    /// The state of the certificate authority.
    pub state: SandboxProxyCaState,
}
crate::record_serde::wire_record!(SandboxProxyCaTrustForConfigDirResult { can_install: "canInstall", detail: "detail" if is_none, state: "state" });

/// Status of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaTrustCertificateResult {
    /// Whether this process can add the certificate authority to OS trust without credentials from a different user. False where OS trust is unsupported, and on Windows when the process cannot elevate itself to write the machine trust store. When false, do not offer to set up the certificate authority.
    pub can_install: bool,
    /// Human-readable reason for the state. On `installed` or `notInstalled`, present only when the certificate authority must be rotated, and then says why.
    pub detail: Option<String>,
    /// The state of the certificate authority.
    pub state: SandboxProxyCaState,
}
crate::record_serde::wire_record!(SandboxProxyCaTrustCertificateResult { can_install: "canInstall", detail: "detail" if is_none, state: "state" });

/// Status of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxyCaRemoveResult {
    /// Whether this process can add the certificate authority to OS trust without credentials from a different user. False where OS trust is unsupported, and on Windows when the process cannot elevate itself to write the machine trust store. When false, do not offer to set up the certificate authority.
    pub can_install: bool,
    /// Human-readable reason for the state. On `installed` or `notInstalled`, present only when the certificate authority must be rotated, and then says why.
    pub detail: Option<String>,
    /// The state of the certificate authority.
    pub state: SandboxProxyCaState,
}
crate::record_serde::wire_record!(SandboxProxyCaRemoveResult { can_install: "canInstall", detail: "detail" if is_none, state: "state" });

/// Where this runtime keeps the sandbox proxy password.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxProxySecretGetLocationResult {
    /// Absolute path of the credential store holding the sandbox proxy password. The proxy password belongs to no MCP server, so it is kept in its own directory rather than in the MCP secret store.
    pub config_dir: String,
    /// Identifier of the proxy password entry within that store. Derived from the configuration directory, so two Copilot homes on one machine never share an entry. Use it to recognize a `${secret:…}` reference in stored settings as this runtime's own proxy password.
    pub secret_id: String,
}
crate::record_serde::wire_record!(SandboxProxySecretGetLocationResult {
    config_dir: "configDir",
    secret_id: "secretId"
});

/// Authoritative policy labels and opt-out capability for a host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxGetPolicyPresentationResult {
    /// Notice used when the user declines an advisory managed floor.
    pub advisory_floor_declined_notice: String,
    /// Whether the effective policy permits a session opt-out.
    pub allows_opt_out: bool,
    /// Notice when managed policy overrides an explicit flag.
    pub flag_overridden_by_policy_notice: Option<String>,
    /// Notice when managed policy enables the sandbox.
    pub managed_floor_notice: Option<String>,
}
crate::record_serde::wire_record!(SandboxGetPolicyPresentationResult { advisory_floor_declined_notice: "advisoryFloorDeclinedNotice", allows_opt_out: "allowsOptOut", flag_overridden_by_policy_notice: "flagOverriddenByPolicyNotice" if is_none, managed_floor_notice: "managedFloorNotice" if is_none });

/// How shell commands start under the managed learning policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxGetLearningModeStartResult {
    /// Explanation when the host cannot apply the managed learning policy.
    pub notice: Option<String>,
    /// Whether the host can allow and record denials.
    pub relaxed: bool,
}
crate::record_serde::wire_record!(SandboxGetLearningModeStartResult { notice: "notice" if is_none, relaxed: "relaxed" });

/// The sandbox a named feature actually launches under.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxResolveForOptionalResult {
    /// The effective sandbox config for the requested feature, or a disabled sandbox when the feature is routed out of it.
    pub sandbox_json: serde_json::Value,
}
crate::record_serde::wire_record!(SandboxResolveForOptionalResult {
    sandbox_json: "sandboxJson"
});

/// The startup warning, absent when the machine can run a sandbox.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxGetUnsupportedStartupNoticeResult {
    /// The user-facing warning, carrying the platform remedy and the recovery the policy in force actually allows. Absent when this machine supports a sandbox, so a host can call unconditionally and print only what it gets.
    pub notice: Option<String>,
}
crate::record_serde::wire_record!(SandboxGetUnsupportedStartupNoticeResult { notice: "notice" if is_none });

/// Advice for a path grant, when it needs any.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SandboxGetPathGrantWarningResult {
    /// The user-facing warning. Absent when the grant needs none, so a host can call unconditionally and show only what it gets.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(SandboxGetPathGrantWarningResult { warning: "warning" if is_none });

/// The documentation URL a host cites when it declines or downgrades a sandbox setting.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxGetDocsUrlResult {
    /// Documentation URL describing the sandbox and the settings that control it.
    pub url: String,
}
crate::record_serde::wire_record!(SandboxGetDocsUrlResult { url: "url" });

/// Whether the host running this runtime can run the command sandbox. The runtime checks `supported` once per process. A capability answer can change while the process runs, for example after the user installs a missing package.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxGetHostSupportResult {
    /// Sandbox policy features whose availability varies between hosts that can run the backend. Empty when `supported` is false, because no feature can run without a backend. Later runtimes can add entries; ignore an entry whose `name` you do not recognize.
    pub capabilities: Vec<SandboxHostCapability>,
    /// Human-readable reason the sandbox cannot run on this host. Present only when `supported` is false.
    pub reason: Option<String>,
    /// Whether a process-containment backend is usable on this host: Seatbelt on macOS, Bubblewrap on Linux, or ProcessContainer on Windows.
    pub supported: bool,
}
crate::record_serde::wire_record!(SandboxGetHostSupportResult { capabilities: "capabilities", reason: "reason" if is_none, supported: "supported" });

/// Extensible identifier of a sandbox policy feature whose availability varies between hosts. A plain string, so an older client decodes a name added by a newer runtime; ignore names you do not recognize. Known values: `network` — sandboxed commands can reach the network (`network.allowOutbound`, on by default); on Linux this needs the tooling for Bubblewrap's private network namespace, such as slirp4netns. `network_filtering` — host rules and the sandbox proxy (`network.allowedHosts`, `network.blockedHosts`, `network.proxy`); on Linux this needs the same tooling as `network`; on Windows it needs Process Security Environment 1.1 host-loopback support or MXC's PSEC 1.0-only proxy-loopback compatibility capability, and a policy that uses it must also set `network.allowLocalNetwork`, because Windows reaches the local proxy only together with private-network access. Compatibility applies only to an explicit identity-less runtime proxy, not general host-loopback access, and other policy restrictions still apply. `denied_paths` — native enforcement of `filesystem.deniedPaths`; on Windows this needs a version whose sandbox contract reports denied-path support. `shell` — shell commands inside the sandbox: bash on macOS and Linux, PowerShell on Windows. `filesystem_enumeration` — enumerate-only filesystem grants, which PowerShell's drive roots and `filesystem.enumeratePaths` use on Windows; this needs a version with Process Security Environment 1.1 filesystem enumeration support. Without it, sandboxed PowerShell still runs, but `Get-Location` may report the drive root, `Set-Location` may fail, and relative paths may resolve against the drive root; the session also receives a `session.warning` with `warningType` `sandbox`. `filesystem.enumeratePaths` entries are ignored. Other platforms always report it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxHostCapabilityName = String;

/// True when the untrusted stale certificate authority was replaced.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxProxyCaRotateIfUntrustedResult = bool;

/// True when the ordinary installation offer was declined.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxProxyCaGetDeclinedResult = bool;

/// True when this process can offer installing the sandbox credential-proxy certificate authority into OS trust.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxProxyCaCheckInstallationSupportResult = bool;

/// True when the sandbox configuration uses the credential proxy and can need certificate-authority setup.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxProxyCaCheckRelevanceResult = bool;

/// The effective sandbox settings, with every unset value resolved to its default.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxSettingsApplyDefaultsResult = HashMap<String, serde_json::Value>;

/// The settings file selected by the supplied configuration directory, home directory, and environment.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxSettingsGetPathResult = String;

/// The runtime-local temporary directory allowed by the default sandbox grants, with Node-style trailing-separator trimming.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxGetTemporaryDirectoryResult = String;

/// Whether both configurations describe the same effective sandbox.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxConfigsEqualResult = bool;

/// True when an OS-level sandbox can run on this host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxCheckPlatformSupportResult = bool;

/// True when the sandbox on this host can reach host loopback directly.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxCheckWindowsHostLoopbackSupportResult = bool;

/// The user-facing refusal, including the platform remedy and the documentation link.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxGetUnsupportedMessageResult = String;

/// The user-facing notice that a saved sandbox setting is inert for this session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxGetUnsupportedSettingDisabledNoticeResult = String;

/// True when the command stays available while enforcement is blocked.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SandboxCheckEnforcementCommandResult = bool;

/// Normalization rule for a sandbox setting.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxInputKind {
    /// Normalizes a credential host pattern.
    #[serde(rename = "hostPattern")]
    HostPattern,
    /// Normalizes a proxy URL for sandbox settings.
    #[serde(rename = "proxyUrl")]
    ProxyUrl,
    /// Normalizes a proxy URL for host proxy resolution.
    #[serde(rename = "proxyResolutionUrl")]
    ProxyResolutionUrl,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxInputKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxInputKind",
            &["hostPattern", "proxyUrl", "proxyResolutionUrl", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::HostPattern,
            1 => Self::ProxyUrl,
            2 => Self::ProxyResolutionUrl,
            _ => Self::Unknown,
        })
    }
}

/// State of the persistent certificate authority of the sandbox credential proxy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxProxyCaState {
    /// This platform has no supported OS trust store. The proxy uses a per-process certificate bundle.
    #[serde(rename = "unsupported")]
    Unsupported,
    /// OS trust does not include the certificate authority, or none is stored.
    #[serde(rename = "notInstalled")]
    NotInstalled,
    /// OS trust includes the stored certificate authority.
    #[serde(rename = "installed")]
    Installed,
    /// The runtime could not read the certificate authority or the OS trust store.
    #[serde(rename = "error")]
    Error,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxProxyCaState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxProxyCaState",
            &[
                "unsupported",
                "notInstalled",
                "installed",
                "error",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Unsupported,
            1 => Self::NotInstalled,
            2 => Self::Installed,
            3 => Self::Error,
            _ => Self::Unknown,
        })
    }
}
