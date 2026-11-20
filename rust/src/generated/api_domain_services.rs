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
    TaskCompletionOutcome, UsageGetMetricsResult, UserToolSessionApproval, Verbosity,
};

use super::sdk_api_shared::*;
use crate::generated::{lifecycle_diagnostics, session_events};
use crate::types::{RequestId, SessionEvent, SessionId};

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AcceptedEnqueueCommandResult {
    /// True because the command was accepted into the local execution queue.
    #[serde(
        deserialize_with = "AcceptedEnqueueCommandResult::deserialize_queued",
        serialize_with = "AcceptedEnqueueCommandResult::serialize_queued"
    )]
    pub queued: bool,
    /// Stable opaque ID of the queued command.
    pub queue_id: String,
}

impl AcceptedEnqueueCommandResult {
    fn deserialize_queued<'de, D>(deserializer: D) -> Result<bool, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = bool::deserialize(deserializer)?;
        if !value {
            return Err(serde::de::Error::custom("expected true"));
        }
        Ok(value)
    }

    fn serialize_queued<S>(value: &bool, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if !*value {
            return Err(serde::ser::Error::custom("expected true"));
        }
        value.serialize(serializer)
    }
}

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UnsupportedEnqueueCommandResult {
    /// False because the target session does not support local command queueing (e.g. remote sessions).
    #[serde(
        deserialize_with = "UnsupportedEnqueueCommandResult::deserialize_queued",
        serialize_with = "UnsupportedEnqueueCommandResult::serialize_queued"
    )]
    pub queued: bool,
    /// Legacy null queue ID accepted for compatibility with older runtimes.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub queue_id: Option<serde_json::Value>,
}

impl UnsupportedEnqueueCommandResult {
    fn deserialize_queued<'de, D>(deserializer: D) -> Result<bool, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = bool::deserialize(deserializer)?;
        if value {
            return Err(serde::de::Error::custom("expected false"));
        }
        Ok(value)
    }

    fn serialize_queued<S>(value: &bool, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if *value {
            return Err(serde::ser::Error::custom("expected false"));
        }
        value.serialize(serializer)
    }
}

/// Parameters for aborting the current turn
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AbortRequest {
    /// Finite reason code describing why the current turn was aborted
    pub reason: Option<AbortReason>,
}
crate::record_serde::wire_record!(AbortRequest { reason: "reason" if is_none });

/// Result of aborting the current turn
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AbortResult {
    /// Error message if the abort failed
    pub error: Option<String>,
    /// Whether the abort completed successfully
    pub success: bool,
}
crate::record_serde::wire_record!(AbortResult { error: "error" if is_none, success: "success" });

/// Canonical directory where custom agents can be discovered or created, with scope, preference, and optional project path.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentDiscoveryPath {
    /// Absolute path of the search/create directory (may not exist on disk yet)
    pub path: String,
    /// Whether this is the canonical directory to create a new agent in its tier. At most one entry per tier is preferred.
    pub preferred_for_creation: bool,
    /// The input project path this directory was derived from (only for project scope)
    pub project_path: Option<String>,
    /// Which tier this directory belongs to
    pub scope: AgentDiscoveryPathScope,
}
crate::record_serde::wire_record!(AgentDiscoveryPath { path: "path", preferred_for_creation: "preferredForCreation", project_path: "projectPath" if is_none, scope: "scope" });

/// Canonical locations where custom agents can be created so the runtime will recognize them.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentDiscoveryPathList {
    /// Canonical agent create/discovery directories, in priority order
    pub paths: Vec<AgentDiscoveryPath>,
}
crate::record_serde::wire_record!(AgentDiscoveryPathList { paths: "paths" });

/// The currently selected custom agent, or null when using the default agent.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentGetCurrentResult {
    /// Currently selected custom agent, or null if using the default agent
    pub agent: Option<AgentInfo>,
}
crate::record_serde::wire_record!(AgentGetCurrentResult { agent: "agent" });

/// Agents available to the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentList {
    /// Available agents
    pub agents: Vec<AgentInfo>,
}
crate::record_serde::wire_record!(AgentList { agents: "agents" });

/// Controls whether built-in agents and authored prompt text are included.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentListRequest {
    /// When true, request the session's configured built-in agents alongside custom agents. Listing applies feature, context, inclusion, exclusion, and user-disabled-agent policy, but does not evaluate transient invocation requirements such as model availability. Built-in metadata may be omitted when the session cannot project it, such as a relay session.
    pub include_built_in_agents: Option<bool>,
    /// When true, request authored base prompt text on each AgentInfo. Prompt text may be omitted when unavailable, such as for agents projected through a relay session.
    pub include_prompt: Option<bool>,
}
crate::record_serde::wire_record!(AgentListRequest { include_built_in_agents: "includeBuiltInAgents" if is_none, include_prompt: "includePrompt" if is_none });

/// A filesystem error, with its original platform code.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistryError {
    /// The platform error code.
    pub code: String,
    /// The filesystem error detail.
    pub message: Option<String>,
    /// The affected path.
    pub path: Option<String>,
}
crate::record_serde::wire_record!(AgentRegistryError { code: "code", message: "message" if is_none, path: "path" if is_none });

/// The resolved registry directory and any creation failure.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistryEnsureDirResult {
    /// The creation failure, if any.
    pub error: Option<AgentRegistryError>,
    /// The resolved directory, also present on failure.
    pub path: String,
}
crate::record_serde::wire_record!(AgentRegistryEnsureDirResult { error: "error" if is_none, path: "path" });

/// Options for local agent discovery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistryListEntriesRequest {
    /// Explicit Copilot home, or absent to use the home directory.
    pub copilot_home: Option<String>,
    /// A publisher to exclude.
    pub exclude_pid: Option<i64>,
    /// The caller's home directory.
    pub home_dir: String,
    /// Override the current epoch time for deterministic discovery.
    pub now_ms: Option<f64>,
    /// Override process liveness checks.
    pub pid_alive_override: Option<bool>,
    /// Maximum heartbeat age in milliseconds.
    pub stale_ms: Option<f64>,
}
crate::record_serde::wire_record!(AgentRegistryListEntriesRequest { copilot_home: "copilotHome" if is_none, exclude_pid: "excludePid" if is_none, home_dir: "homeDir", now_ms: "nowMs" if is_none, pid_alive_override: "pidAliveOverride" if is_none, stale_ms: "staleMs" if is_none });

/// Full registry entry for the spawned child. Lets the controller call `handleLiveTargetSelected(entry)` directly without re-reading the registry (avoids a TOCTOU window).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistryLiveTargetEntry {
    /// Kind of attention required when status === "attention". Meaningful only when status === "attention".
    pub attention_kind: Option<AgentRegistryLiveTargetEntryAttentionKind>,
    /// Git branch of the session (when known)
    pub branch: Option<String>,
    /// Copilot CLI version that wrote the entry
    pub copilot_version: String,
    /// Working directory of the session (when known)
    pub cwd: Option<String>,
    /// Bind host for the entry's JSON-RPC server
    pub host: String,
    /// Process kind tag for the registry entry
    pub kind: AgentRegistryLiveTargetEntryKind,
    /// Wall-clock milliseconds since the watcher last observed this entry (heartbeat freshness)
    pub last_seen_ms: i64,
    /// How the most recent turn ended (clean vs aborted). Lets the renderer distinguish done from done_cancelled.
    pub last_terminal_event: Option<AgentRegistryLiveTargetEntryLastTerminalEvent>,
    /// Model identifier currently selected for the session
    pub model: Option<String>,
    /// Operating-system pid of the process owning this entry
    pub pid: i64,
    /// TCP port the entry's JSON-RPC server is listening on
    pub port: i64,
    /// Registry entry schema version (1 = ui-server, 2 = managed-server)
    pub schema_version: i64,
    /// Session ID of the foreground session for this entry
    pub session_id: Option<SessionId>,
    /// Friendly session name (when set)
    pub session_name: Option<String>,
    /// ISO 8601 timestamp captured at registration
    pub started_at: String,
    /// Coarse lifecycle status of the foreground session
    pub status: Option<AgentRegistryLiveTargetEntryStatus>,
    /// Monotonic per-publisher revision counter incremented on every status update. Lets watchers detect transient flips.
    pub status_revision: Option<i64>,
    /// Connection token (null when the target is unauthenticated)
    #[doc(hidden)]
    pub(crate) token: Option<String>,
}
crate::record_serde::wire_record!(AgentRegistryLiveTargetEntry { attention_kind: "attentionKind" if is_none, branch: "branch" if is_none, copilot_version: "copilotVersion", cwd: "cwd" if is_none, host: "host", kind: "kind", last_seen_ms: "lastSeenMs", last_terminal_event: "lastTerminalEvent" if is_none, model: "model" if is_none, pid: "pid", port: "port", schema_version: "schemaVersion", session_id: "sessionId" if is_none, session_name: "sessionName" if is_none, started_at: "startedAt", status: "status" if is_none, status_revision: "statusRevision" if is_none, token: "token" if is_none });

/// Live agent metadata without connection credentials.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistryListEntriesResult {
    /// The live local agents.
    pub entries: Vec<AgentRegistryLiveTargetEntry>,
    /// The discovery failure, if any.
    pub error: Option<AgentRegistryError>,
}
crate::record_serde::wire_record!(AgentRegistryListEntriesResult { entries: "entries", error: "error" if is_none });

/// The location of a local agent registry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistryLocationRequest {
    /// Explicit Copilot home, or absent to use the home directory.
    pub copilot_home: Option<String>,
    /// The caller's home directory.
    pub home_dir: String,
}
crate::record_serde::wire_record!(AgentRegistryLocationRequest { copilot_home: "copilotHome" if is_none, home_dir: "homeDir" });

/// Canonical process-log discovery outcome; populated from spawnLiveTarget.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistryLogCapture {
    /// Whether a canonical process log was discovered for this managed spawn
    pub enabled: bool,
    /// Why no canonical process log could be opened for this managed spawn (set only when enabled is false)
    pub open_error: Option<String>,
    /// Categorized reason no canonical process log could be opened (set only when enabled is false)
    pub open_error_reason: Option<AgentRegistryLogCaptureOpenErrorReason>,
    /// Absolute path to the managed spawn's process-`<timestamp>`-`<pid>`.log file (only set when enabled)
    pub path: Option<String>,
}
crate::record_serde::wire_record!(AgentRegistryLogCapture { enabled: "enabled", open_error: "openError" if is_none, open_error_reason: "openErrorReason" if is_none, path: "path" if is_none });

/// A local agent entry identified by its publisher.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistryPidRequest {
    /// Explicit Copilot home, or absent to use the home directory.
    pub copilot_home: Option<String>,
    /// The caller's home directory.
    pub home_dir: String,
    /// The publisher's process identifier.
    pub pid: i64,
}
crate::record_serde::wire_record!(AgentRegistryPidRequest { copilot_home: "copilotHome" if is_none, home_dir: "homeDir", pid: "pid" });

/// The outcome of removing a registry entry, with no success payload.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistryRemoveEntryResult {
    /// The operation failure, if any.
    pub error: Option<AgentRegistryError>,
}
crate::record_serde::wire_record!(AgentRegistryRemoveEntryResult { error: "error" if is_none });

/// `child_process.spawn` itself failed before the child entered the registry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistrySpawnError {
    /// Underlying errno code (e.g. ENOENT, EACCES) when available
    pub code: Option<String>,
    /// Discriminator: child_process.spawn itself failed
    pub kind: AgentRegistrySpawnErrorKind,
    /// Human-readable error message
    pub message: String,
}
crate::record_serde::wire_record!(AgentRegistrySpawnError { code: "code" if is_none, kind: "kind", message: "message" });

/// Spawn succeeded but the child did not publish a matching managed-server entry within the timeout.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistrySpawnRegistryTimeout {
    /// Process ID of the orphaned child (so the caller can offer 'kill the pid' guidance)
    pub child_pid: i64,
    /// Discriminator: spawn succeeded but child never registered
    pub kind: AgentRegistrySpawnRegistryTimeoutKind,
    /// Per-spawn log-capture outcome; populated from spawnLiveTarget.
    pub log_capture: Option<AgentRegistryLogCapture>,
}
crate::record_serde::wire_record!(AgentRegistrySpawnRegistryTimeout { child_pid: "childPid", kind: "kind", log_capture: "logCapture" if is_none });

/// Inputs to spawn a managed-server child via the controller's spawn delegate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistrySpawnRequest {
    /// Custom or built-in agent name (e.g. 'explore'). When omitted, the child uses its own default.
    pub agent_name: Option<String>,
    /// Working directory for the spawned child (must be an existing directory)
    pub cwd: String,
    /// Optional first user message. Forwarded to the caller (the CLI's spawn wrapper sends it post-attach via the standard LocalRpcSession.send path).
    pub initial_prompt: Option<String>,
    /// Model identifier to apply to the new session
    pub model: Option<String>,
    /// Friendly session name. Must satisfy validateSessionName: non-empty, no leading/trailing whitespace, <=100 chars, no control chars, no double quotes.
    pub name: Option<String>,
    /// Permission posture for the new session. 'yolo' requires the controller-local session to currently be in allow-all mode.
    pub permission_mode: Option<AgentRegistrySpawnPermissionMode>,
}
crate::record_serde::wire_record!(AgentRegistrySpawnRequest { agent_name: "agentName" if is_none, cwd: "cwd", initial_prompt: "initialPrompt" if is_none, model: "model" if is_none, name: "name" if is_none, permission_mode: "permissionMode" if is_none });

/// Managed-server child was spawned and registered successfully.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistrySpawnSpawned {
    /// Full registry entry for the spawned child. Lets the controller call `handleLiveTargetSelected(entry)` directly without re-reading the registry (avoids a TOCTOU window).
    pub entry: AgentRegistryLiveTargetEntry,
    /// If the delegate attempted to send the initial prompt and failed, the categorized error message.
    pub initial_prompt_error: Option<String>,
    /// Whether the delegate already sent the initial prompt. Always omitted in the current wiring: the controller sends the prompt post-attach via the standard LocalRpcSession.send path.
    pub initial_prompt_sent: Option<bool>,
    /// Discriminator: managed-server child spawned successfully
    pub kind: AgentRegistrySpawnSpawnedKind,
    /// Per-spawn log-capture outcome; populated from spawnLiveTarget.
    pub log_capture: Option<AgentRegistryLogCapture>,
}
crate::record_serde::wire_record!(AgentRegistrySpawnSpawned { entry: "entry", initial_prompt_error: "initialPromptError" if is_none, initial_prompt_sent: "initialPromptSent" if is_none, kind: "kind", log_capture: "logCapture" if is_none });

/// Synchronous pre-validation rejected the spawn request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentRegistrySpawnValidationError {
    /// Which parameter field was invalid. Omitted when the rejection is not field-specific.
    pub field: Option<AgentRegistrySpawnValidationErrorField>,
    /// Discriminator: synchronous pre-validation rejected the request
    pub kind: AgentRegistrySpawnValidationErrorKind,
    /// Human-readable explanation; safe to surface in the UI banner. Never logged to unrestricted telemetry.
    pub message: String,
    /// Categorized reason for the rejection. Low-cardinality enum so telemetry can aggregate by reason without leaking raw paths or agent/model names.
    pub reason: AgentRegistrySpawnValidationErrorReason,
}
crate::record_serde::wire_record!(AgentRegistrySpawnValidationError { field: "field" if is_none, kind: "kind", message: "message", reason: "reason" });

/// Custom agents available to the session after reloading definitions from disk.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentReloadResult {
    /// Reloaded custom agents
    pub agents: Vec<AgentInfo>,
}
crate::record_serde::wire_record!(AgentReloadResult { agents: "agents" });

/// The models a custom agent asks for, and the models actually available.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AgentsCustomAgentInitialModelDecisionParams {
    /// The agent's declared `model:` entry, serialized. A single name or an ordered list of acceptable names.
    pub agent_models_json: String,
    /// The models available to this session, serialized in the shape the model list carries.
    pub available_models_json: String,
}
crate::record_serde::wire_record!(AgentsCustomAgentInitialModelDecisionParams {
    agent_models_json: "agentModelsJson",
    available_models_json: "availableModelsJson"
});

/// The model to switch to, and the warning to show when the agent's preference could not be met.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AgentsCustomAgentInitialModelDecisionResult {
    /// The reasoning effort attached to the selected model preference. Absent when that preference does not specify an effort.
    pub reasoning_effort: Option<String>,
    /// The first available model that matches the agent's preferences. Absent when none of the requested models is available.
    pub target_model: Option<String>,
    /// What to tell the user about an unmet preference. Absent when the preference was met. A warning with no `targetModel` means the agent's models are all unavailable.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(AgentsCustomAgentInitialModelDecisionResult { reasoning_effort: "reasoningEffort" if is_none, target_model: "targetModel" if is_none, warning: "warning" if is_none });

/// Optional project paths to include in agent discovery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentsDiscoverRequest {
    /// Explicit discovery-only agent roots, separate from customAgents and filesystem grants. Available without ambient discovery, subject to managed/source disables. Relative paths resolve against the request's effective working directory when supplied. Null and omission are equivalent; an empty array contributes no explicit roots.
    pub agent_directories: Option<Vec<String>>,
    /// When true, omit the host's agents (the user-level agent directory and all plugin agents), leaving only project and remote agents. For multitenant deployments.
    pub exclude_host_agents: Option<bool>,
    /// Optional list of project directory paths to scan for project-scoped agents. When omitted or empty, no project is scanned: user-level and globally enabled plugin agents are still returned, but agents of a plugin enabled only in a repository's settings are not.
    pub project_paths: Option<Vec<String>>,
}
crate::record_serde::wire_record!(AgentsDiscoverRequest { agent_directories: "agentDirectories" if is_none, exclude_host_agents: "excludeHostAgents" if is_none, project_paths: "projectPaths" if is_none });

/// Name of the custom agent to select for subsequent turns.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentSelectRequest {
    /// Name of the custom agent to select
    pub name: String,
}
crate::record_serde::wire_record!(AgentSelectRequest { name: "name" });

/// The newly selected custom agent.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentSelectResult {
    /// The newly selected custom agent
    pub agent: AgentInfo,
}
crate::record_serde::wire_record!(AgentSelectResult { agent: "agent" });

/// An in-memory authored prompt override for an available agent.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentSetPromptRequest {
    /// Stable effective agent id. Plugin namespace separators are normalized.
    pub id: String,
    /// Replacement authored prompt. Empty text is valid.
    pub prompt: String,
}
crate::record_serde::wire_record!(AgentSetPromptRequest {
    id: "id",
    prompt: "prompt"
});

/// The feature flags to evaluate shipped agents against.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AgentsGetAvailableBuiltinsRequest {
    /// The surface asking, which gates agents that only apply to one client. Omit or pass null to apply no client filter.
    pub context: Option<String>,
    /// Feature flag values keyed by name, evaluated with the runtime's truthiness rules. Omit or pass null for no flags.
    pub feature_flags: Option<HashMap<String, serde_json::Value>>,
    /// Flag overrides keyed by name. A null entry uses the corresponding base flag; false explicitly disables it. Omit or pass null for no overrides.
    pub overrides: Option<HashMap<String, serde_json::Value>>,
}
crate::record_serde::wire_record!(AgentsGetAvailableBuiltinsRequest { context: "context" if is_none, feature_flags: "featureFlags" if is_none, overrides: "overrides" if is_none });

/// A shipped agent, named and described.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct BuiltinAgentSummary {
    /// One-line description of what the agent does.
    pub description: String,
    /// The agent name, as it appears in `getBuiltins`.
    pub name: String,
}
crate::record_serde::wire_record!(BuiltinAgentSummary {
    description: "description",
    name: "name"
});

/// The shipped agents available under the requested flags.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AgentsGetAvailableBuiltinsResult {
    /// Available shipped agents, in the runtime's own order.
    #[doc(hidden)]
    pub(crate) agents: Vec<BuiltinAgentSummary>,
}
crate::record_serde::wire_record!(AgentsGetAvailableBuiltinsResult { agents: "agents" });

/// The shipped agent whose definition to load.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AgentsGetBuiltinDefinitionRequest {
    /// The agent name, which must be one of `getBuiltins`'s `yamlBasedNames`. A name outside that list is special-cased in code and has no definition, and is reported as an error rather than as an empty definition.
    pub name: String,
}
crate::record_serde::wire_record!(AgentsGetBuiltinDefinitionRequest { name: "name" });

/// One shipped agent's definition.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AgentsGetBuiltinDefinitionResult {
    /// The agent's definition, serialized as JSON. It carries the authored keys plus the runtime's projected `__nativeCustomAgent` view of the same agent. It is a string rather than an object because the runtime parses it with the agent schema's tolerant shape, which accepts keys this contract does not name.
    pub definition_json: String,
}
crate::record_serde::wire_record!(AgentsGetBuiltinDefinitionResult {
    definition_json: "definitionJson"
});

/// The shipped agent whose listing entry to load.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AgentsGetBuiltinListingDefinitionRequest {
    /// The agent name, taken from `getAvailableBuiltins`. Unlike `getBuiltinDefinition`, the agent that `getBuiltins` reports as special-cased rather than YAML-based is answered here too, from its in-code definition.
    pub name: String,
}
crate::record_serde::wire_record!(AgentsGetBuiltinListingDefinitionRequest { name: "name" });

/// One shipped agent, projected for a listing.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AgentsGetBuiltinListingDefinitionResult {
    /// The agent projected as a custom agent, serialized as JSON. It is a string rather than an object for the same reason as `getBuiltinDefinition`: the runtime parses the underlying definition with the agent schema's tolerant shape, which accepts keys this contract does not name.
    pub definition_json: String,
}
crate::record_serde::wire_record!(AgentsGetBuiltinListingDefinitionResult {
    definition_json: "definitionJson"
});

/// The agents this runtime ships, named so a consumer can tell them apart from authored ones.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AgentsGetBuiltinsResult {
    /// The subset of `names` a user is allowed to turn off. A shipped agent outside this list is always active and a client should not offer a toggle for it.
    pub disableable_names: Vec<String>,
    /// Every agent name this runtime ships.
    pub names: Vec<String>,
    /// The subset of `names` defined by a shipped YAML definition. The remainder are special-cased in code and have no definition to load.
    pub yaml_based_names: Vec<String>,
}
crate::record_serde::wire_record!(AgentsGetBuiltinsResult {
    disableable_names: "disableableNames",
    names: "names",
    yaml_based_names: "yamlBasedNames"
});

/// Optional project paths to include when enumerating agent discovery directories.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentsGetDiscoveryPathsRequest {
    /// Explicit discovery-only agent roots to enumerate, including configured missing directories. Relative paths resolve against the request's effective working directory when supplied. These are not filesystem grants or preferred creation targets. Null and omission are equivalent.
    pub agent_directories: Option<Vec<String>>,
    /// When true, omit the host's user-level agent directory, leaving only project directories. For multitenant deployments (mirrors `discover`'s `excludeHostAgents`).
    pub exclude_host_agents: Option<bool>,
    /// Optional list of project directory paths. When omitted or empty, only the user-level directory is returned.
    pub project_paths: Option<Vec<String>>,
}
crate::record_serde::wire_record!(AgentsGetDiscoveryPathsRequest { agent_directories: "agentDirectories" if is_none, exclude_host_agents: "excludeHostAgents" if is_none, project_paths: "projectPaths" if is_none });

/// Authenticates pending protected resources with the connection's renewable credential source.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionAuthenticateRequest {
    /// Opaque connection identifier.
    pub connection_id: String,
    /// Additional resources to authenticate. An omitted list is empty.
    pub extra_resources: Option<Vec<String>>,
}
crate::record_serde::wire_record!(AhpConnectionAuthenticateRequest { connection_id: "connectionId", extra_resources: "extraResources" if is_none });

/// Connect directly to an AHP WebSocket endpoint.
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionTransportDirect {
    /// Permit this endpoint to receive the original owner's GitHub credential. Defaults to false.
    pub authenticate_with_git_hub: Option<bool>,
    /// Outgoing AHP transport discriminator.
    pub kind: AhpConnectionTransportDirectKind,
    /// WebSocket URL of the AHP endpoint.
    pub url: String,
}
crate::record_serde::wire_record!(AhpConnectionTransportDirect { authenticate_with_git_hub: "authenticateWithGitHub" if is_none, kind: "kind", url: "url" });

/// Ask Mission Control to allocate the connection.
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionTransportMissionControl {
    /// Mission Control service URL.
    pub base_url: String,
    /// Target environment identifier.
    pub environment_id: String,
    /// Outgoing AHP transport discriminator.
    pub kind: AhpConnectionTransportMissionControlKind,
}
crate::record_serde::wire_record!(AhpConnectionTransportMissionControl {
    base_url: "baseUrl",
    environment_id: "environmentId",
    kind: "kind"
});

/// Creates an inert outgoing connection under the caller's existing runtime auth owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AhpConnectionCreateRequest {
    /// Optional stable client identity. Omission uses Mission Control's assigned identity or the engine default.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_id: Option<String>,
    /// Optional connection-owned diagnostic log created before authentication.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub log_id: Option<String>,
    /// The one transport this connection uses.
    pub transport: AhpConnectionTransport,
}

/// Seeds a model selection on a newly created AHP session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionModelSelection {
    /// Provider-defined model configuration.
    pub config: Option<HashMap<String, serde_json::Value>>,
    /// Host-advertised model identifier.
    pub id: String,
}
crate::record_serde::wire_record!(AhpConnectionModelSelection { config: "config" if is_none, id: "id" });

/// Seeds project provisioning on a newly created AHP session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionProjectOptions {
    /// Optional branch for the session worktree.
    pub base_branch: Option<String>,
    /// Credential-free HTTPS clone URL.
    pub clone_url: String,
}
crate::record_serde::wire_record!(AhpConnectionProjectOptions { base_branch: "baseBranch" if is_none, clone_url: "cloneUrl" });

/// Options understood by the remote AHP session source.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionSessionOptions {
    /// Agent-specific initial configuration.
    pub config: Option<HashMap<String, serde_json::Value>>,
    /// Initial model selection.
    pub model: Option<AhpConnectionModelSelection>,
    /// Optional project to provision before session creation.
    pub project: Option<AhpConnectionProjectOptions>,
    /// Host-advertised agent provider.
    pub provider: Option<String>,
    /// Directory on the remote host. Omission uses its default.
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(AhpConnectionSessionOptions { config: "config" if is_none, model: "model" if is_none, project: "project" if is_none, provider: "provider" if is_none, working_directory: "workingDirectory" if is_none });

/// Creates a session at the remote source, not a local SDK attachment.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionCreateSessionRequest {
    /// Owned outgoing connection identifier.
    pub connection_id: String,
    /// Source-side creation options.
    pub options: Option<AhpConnectionSessionOptions>,
    /// Optional initial turn text.
    pub text: Option<String>,
}
crate::record_serde::wire_record!(AhpConnectionCreateSessionRequest { connection_id: "connectionId", options: "options" if is_none, text: "text" if is_none });

/// A session created at the remote source and its refreshed connection snapshot.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionCreateSessionResult {
    /// Connection state after the source refreshed its catalog.
    pub state_json: String,
    /// Remote AHP session URI.
    pub uri: String,
}
crate::record_serde::wire_record!(AhpConnectionCreateSessionResult {
    state_json: "stateJson",
    uri: "uri"
});

/// Disposes a remote source session for every attached client.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionDisposeSessionRequest {
    /// Owned outgoing connection identifier.
    pub connection_id: String,
    /// Remote AHP session URI.
    pub session_uri: String,
}
crate::record_serde::wire_record!(AhpConnectionDisposeSessionRequest {
    connection_id: "connectionId",
    session_uri: "sessionUri"
});

/// A complete connection snapshot.
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionEventState {
    /// Outgoing AHP event discriminator.
    pub kind: AhpConnectionEventStateKind,
    /// Serialized relay state, ordered by its revision field.
    pub state_json: String,
}
crate::record_serde::wire_record!(AhpConnectionEventState {
    kind: "kind",
    state_json: "stateJson"
});

/// A projected action from an acquired session subscription.
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionEventProjected {
    /// Serialized projected action.
    pub dispatch_json: String,
    /// Outgoing AHP event discriminator.
    pub kind: AhpConnectionEventProjectedKind,
    /// Original AHP session URI.
    pub session_uri: String,
}
crate::record_serde::wire_record!(AhpConnectionEventProjected {
    dispatch_json: "dispatchJson",
    kind: "kind",
    session_uri: "sessionUri"
});

/// Opens a diagnostic log before outgoing authentication starts.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionLogOpenRequest {
    /// Serialized environment metadata for the log header.
    pub environment_json: String,
    /// Absolute destination for the connection log.
    pub path: String,
    /// Initial values to redact, in addition to the runtime's live secret filter.
    pub secret_values: Vec<String>,
}
crate::record_serde::wire_record!(AhpConnectionLogOpenRequest {
    environment_json: "environmentJson",
    path: "path",
    secret_values: "secretValues"
});

/// Identifies a diagnostic log owned by the originating SDK connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionLogRequest {
    /// Opaque identifier returned by ahp.connectionLogs.open.
    pub log_id: String,
}
crate::record_serde::wire_record!(AhpConnectionLogRequest { log_id: "logId" });

/// Appends a redacted diagnostic message to a connection log.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionLogWriteRequest {
    /// Opaque connection-owned log identifier.
    pub log_id: String,
    /// Diagnostic text to redact and append.
    pub message: String,
}
crate::record_serde::wire_record!(AhpConnectionLogWriteRequest {
    log_id: "logId",
    message: "message"
});

/// Events admitted for this connection, in protocol order.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionReadEventsResult {
    /// The next queued events. Empty only after disposal.
    pub events: Vec<AhpConnectionEvent>,
}
crate::record_serde::wire_record!(AhpConnectionReadEventsResult { events: "events" });

/// Selects an outgoing connection owned by this SDK connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionRequest {
    /// Opaque identifier returned by ahp.connections.create.
    pub connection_id: String,
}
crate::record_serde::wire_record!(AhpConnectionRequest {
    connection_id: "connectionId"
});

/// The outcome for one protected resource.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionResourceAuthResult {
    /// Failure detail when the host rejected the credential.
    pub error: Option<String>,
    /// Whether the host accepted the credential.
    pub ok: bool,
    /// Protected resource URI.
    pub resource: String,
}
crate::record_serde::wire_record!(AhpConnectionResourceAuthResult { error: "error" if is_none, ok: "ok", resource: "resource" });

/// Selects a remote source session without opening a session subscription.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionSelectSessionRequest {
    /// Owned outgoing connection identifier.
    pub connection_id: String,
    /// Optional session identifier. Omission selects the host's default session.
    pub session_id: Option<SessionId>,
}
crate::record_serde::wire_record!(AhpConnectionSelectSessionRequest { connection_id: "connectionId", session_id: "sessionId" if is_none });

/// The selected source catalog row.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionSelectSessionResult {
    /// Serialized AHP session summary, or null when no row matches.
    pub summary_json: Option<String>,
}
crate::record_serde::wire_record!(AhpConnectionSelectSessionResult {
    summary_json: "summaryJson"
});

/// The connection's current protocol snapshot and negotiated client identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionStateResult {
    /// Effective client identity, including any Mission Control assignment.
    pub client_id: String,
    /// Opaque connection identifier scoped to the originating SDK connection.
    pub connection_id: String,
    /// Serialized AHP relay state, including its monotonic revision.
    pub state_json: String,
}
crate::record_serde::wire_record!(AhpConnectionStateResult {
    client_id: "clientId",
    connection_id: "connectionId",
    state_json: "stateJson"
});

/// One request-owned admission decision, sent only to its originating SDK connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AhpSessionAdmissionCallbackRequest {
    /// Invocation identity supplied by the same client that owns the send.
    pub admission_id: String,
    /// Recheck a deferred guard, or acknowledge the final irreversible admission.
    #[doc(hidden)]
    pub(crate) phase: AhpSessionAdmissionPhase,
}
crate::record_serde::wire_record!(AhpSessionAdmissionCallbackRequest {
    admission_id: "admissionId",
    phase: "phase"
});

/// A decision for one callback, with JavaScript errors retained by its originating client.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AhpSessionAdmissionCallbackResult {
    /// Whether this invocation may cross the requested admission boundary.
    pub allowed: bool,
}
crate::record_serde::wire_record!(AhpSessionAdmissionCallbackResult { allowed: "allowed" });

/// Attaches a runtime session facade to an existing AHP session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpSessionAttachRequest {
    /// Connection returned by ahp.connections.create, owned by this caller.
    pub connection_id: String,
    /// Controller feature service configuration, without live native handles.
    pub feature_flag_service_json: String,
    /// Serialized session options. The runtime retains its existing auth owner.
    pub options_json: String,
    /// Controller working directory used only by local UI services.
    pub process_working_directory: String,
    /// Session URI advertised by that connection's host.
    pub session_uri: String,
}
crate::record_serde::wire_record!(AhpSessionAttachRequest {
    connection_id: "connectionId",
    feature_flag_service_json: "featureFlagServiceJson",
    options_json: "optionsJson",
    process_working_directory: "processWorkingDirectory",
    session_uri: "sessionUri"
});

/// Host timestamps and title for the attached session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpSessionMetadata {
    /// Host modification time in ISO 8601 format.
    pub modified_time: String,
    /// Runtime identity used by all standard session methods.
    pub session_id: SessionId,
    /// Host creation time in ISO 8601 format.
    pub start_time: String,
    /// Host session title.
    pub summary: Option<String>,
}
crate::record_serde::wire_record!(AhpSessionMetadata { modified_time: "modifiedTime", session_id: "sessionId", start_time: "startTime", summary: "summary" if is_none });

/// A registered runtime session and its independent attachment lease.
/// Read session.remote.readProjection before using host state.
/// The first read waits for subscription readiness and reports initialization errors.
/// Release the lease if initialization fails.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpSessionAttachResult {
    /// Lease for independent facade teardown on this SDK connection.
    pub attachment_id: String,
    /// Host metadata used for the SDK attachment.
    pub metadata: AhpSessionMetadata,
    /// Runtime identity, distinct from the host's AHP URI.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(AhpSessionAttachResult {
    attachment_id: "attachmentId",
    metadata: "metadata",
    session_id: "sessionId"
});

/// Releases this caller's runtime facade without disposing the host's session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpSessionDetachRequest {
    /// Lease returned to this facade by attachSession.
    pub attachment_id: String,
    /// Runtime session identity returned by ahp.connections.attachSession.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(AhpSessionDetachRequest {
    attachment_id: "attachmentId",
    session_id: "sessionId"
});

/// Request for one phase of replacing an attached session's transport subscription.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpSessionLeaseRequest {
    /// Release or restore operation for the caller's attachment lease.
    pub phase: serde_json::Value,
}
crate::record_serde::wire_record!(AhpSessionLeaseRequest { phase: "phase" });

/// Runtime generation that fences the two subscription replacement phases.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpSessionLeaseResult {
    /// Generation released or restored by this operation.
    pub generation: i64,
}
crate::record_serde::wire_record!(AhpSessionLeaseResult {
    generation: "generation"
});

/// Reads the host-owned collaboration and pending-request projection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpSessionProjectionRequest {
    /// Last observed revision. An unchanged projection waits for an update.
    pub after_revision: Option<i64>,
}
crate::record_serde::wire_record!(AhpSessionProjectionRequest { after_revision: "afterRevision" if is_none });

/// A read-only host projection for clients with synchronous render paths.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpSessionProjectionResult {
    /// Whether this attachment has closed.
    pub closed: bool,
    /// Monotonic revision within this attachment.
    pub revision: i64,
    /// Serialized collaboration, approval, and pending-request snapshot.
    pub snapshot_json: String,
}
crate::record_serde::wire_record!(AhpSessionProjectionResult {
    closed: "closed",
    revision: "revision",
    snapshot_json: "snapshotJson"
});

/// Publishes this client's collaboration state through the AHP host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpSessionPublishRequest {
    /// Latest composer text, when the composer changes.
    pub draft_text: Option<String>,
    /// Serialized peer view, when the visible surfaces change.
    pub view_json: Option<String>,
}
crate::record_serde::wire_record!(AhpSessionPublishRequest { draft_text: "draftText" if is_none, view_json: "viewJson" if is_none });

/// One trusted CLI host's message and its invocation-local admission identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct AhpSessionSendAdmissionRequest {
    /// Opaque identity registered by the originating client before this request.
    pub admission_id: String,
    /// Serialized canonical SendRequest, including clientTurnId and all supplied message options.
    /// The runtime validates these options and owns their preparation and routing.
    pub request_json: String,
}
crate::record_serde::wire_record!(AhpSessionSendAdmissionRequest {
    admission_id: "admissionId",
    request_json: "requestJson"
});

/// Request to add a model to the server-owned saved provider configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationAddModelRequest {
    /// Optional replacement API key. Stored in the OS keychain and never returned.
    pub credential: Option<String>,
    /// Model to add to the provider.
    pub model: ProviderModelConfig,
    /// Provider settings to create when this provider is not already saved.
    pub provider: SavedProviderConfigurationProviderInput,
}
crate::record_serde::wire_record!(SavedProviderConfigurationAddModelRequest { credential: "credential" if is_none, model: "model", provider: "provider" });

/// Saves through the exact authenticated local attachment that discovered a model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachedProviderConfigurationAddModelRequest {
    /// Original authenticated local attachment, not a replacement with the same session id.
    pub attach_id: i64,
    /// The ordinary server-owned save request, without a caller-selected destination.
    pub configuration: SavedProviderConfigurationAddModelRequest,
    /// Session whose owning server receives the save.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(AttachedProviderConfigurationAddModelRequest {
    attach_id: "attachId",
    configuration: "configuration",
    session_id: "sessionId"
});

/// Blob attachment with inline base64-encoded data
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentBlob {
    /// Internal: content-addressed id of the session.binary_asset event holding this attachment's model-facing bytes (e.g. "sha256:..."). Absent externally.
    pub asset_id: Option<String>,
    /// Internal: decoded byte length of the attachment's model-facing bytes. Absent externally.
    pub byte_length: Option<i64>,
    /// Base64-encoded content. Present on input and for external consumers; replaced by an internal `assetId` reference in persisted events when interned to a content-addressed asset.
    pub data: Option<String>,
    /// User-facing display name for the attachment
    pub display_name: Option<String>,
    /// MIME type of the inline data
    pub mime_type: String,
    /// Internal: why model-facing bytes are absent from persistence. Absent externally.
    pub omitted_reason: Option<OmittedBinaryOmittedReason>,
    /// Attachment type discriminator
    pub r#type: AttachmentBlobType,
}
crate::record_serde::wire_record!(AttachmentBlob { asset_id: "assetId" if is_none, byte_length: "byteLength" if is_none, data: "data" if is_none, display_name: "displayName" if is_none, mime_type: "mimeType", omitted_reason: "omittedReason" if is_none, r#type: "type" });

/// Directory attachment
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentDirectory {
    /// User-facing display name for the attachment
    pub display_name: String,
    /// Absolute directory path
    pub path: String,
    /// Frozen rendered line this attachment contributed to the `<tagged_files>` prompt block (e.g. "* /path (12 items)"). Captured at send time so resumed history reproduces the exact text the model saw, independent of later filesystem changes.
    pub tagged_files_entry: Option<String>,
    /// Attachment type discriminator
    pub r#type: AttachmentDirectoryType,
}
crate::record_serde::wire_record!(AttachmentDirectory { display_name: "displayName", path: "path", tagged_files_entry: "taggedFilesEntry" if is_none, r#type: "type" });

/// Structured context contributed by an extension. Composer pills displayed in the host are forwarded back through session.send.attachments, then rendered into the model prompt as an <extension_context> XML block.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentExtensionContext {
    /// Provider-local canvas identifier when the push was bound to a canvas instance
    pub canvas_id: Option<String>,
    /// ISO 8601 timestamp captured by the runtime when the push was accepted
    pub captured_at: String,
    /// Owning extension identifier. Runtime-derived from the caller's connection when produced via session.extensions.sendAttachmentsToMessage; preserved verbatim on subsequent transports.
    pub extension_id: String,
    /// Open canvas instance identifier when the push was bound to a canvas instance
    pub instance_id: Option<String>,
    /// Caller-supplied JSON payload
    pub payload: Option<serde_json::Value>,
    /// Human-readable composer pill label
    pub title: String,
    /// Attachment type discriminator
    pub r#type: AttachmentExtensionContextType,
}
crate::record_serde::wire_record!(AttachmentExtensionContext { canvas_id: "canvasId" if is_none, captured_at: "capturedAt", extension_id: "extensionId", instance_id: "instanceId" if is_none, payload: "payload" if is_none, title: "title", r#type: "type" });

/// Optional line range to scope the attachment to a specific section of the file
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentFileLineRange {
    /// End line number (1-based, inclusive)
    pub end: i64,
    /// Start line number (1-based)
    pub start: i64,
}
crate::record_serde::wire_record!(AttachmentFileLineRange {
    end: "end",
    start: "start"
});

/// File attachment
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentFile {
    /// Internal: content-addressed id of the session.binary_asset event holding this attachment's model-facing bytes (e.g. "sha256:..."). Absent externally.
    pub asset_id: Option<String>,
    /// Internal: decoded byte length of the attachment's model-facing bytes. Absent externally.
    pub byte_length: Option<i64>,
    /// User-facing display name for the attachment
    pub display_name: String,
    /// Optional line range to scope the attachment to a specific section of the file
    pub line_range: Option<AttachmentFileLineRange>,
    /// Internal: MIME type of the file's model-facing bytes (post-resize for images). Set when the file's bytes are interned to an asset. Absent externally.
    pub mime_type: Option<String>,
    /// Internal: why model-facing bytes are absent from persistence. Absent externally.
    pub omitted_reason: Option<OmittedBinaryOmittedReason>,
    /// Absolute file path
    pub path: String,
    /// Frozen rendered line this attachment contributed to the `<tagged_files>` prompt block (e.g. "* /path (123 lines)"). Captured at send time so resumed history reproduces the exact text the model saw, independent of later filesystem changes. Present only for attachments routed to `<tagged_files>` (mutually exclusive with assetId, which marks bytes sent natively).
    pub tagged_files_entry: Option<String>,
    /// Attachment type discriminator
    pub r#type: AttachmentFileType,
}
crate::record_serde::wire_record!(AttachmentFile { asset_id: "assetId" if is_none, byte_length: "byteLength" if is_none, display_name: "displayName", line_range: "lineRange" if is_none, mime_type: "mimeType" if is_none, omitted_reason: "omittedReason" if is_none, path: "path", tagged_files_entry: "taggedFilesEntry" if is_none, r#type: "type" });

/// Pointer to a GitHub Actions job.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubActionsJob {
    /// Terminal conclusion of the job when finished (e.g., success, failure, cancelled). Absent for in-progress jobs.
    pub conclusion: Option<String>,
    /// Job id within the workflow run
    pub job_id: i64,
    /// Display name of the job
    pub job_name: String,
    /// Repository the workflow run belongs to
    pub repo: GitHubRepoRef,
    /// Attachment type discriminator
    pub r#type: AttachmentGitHubActionsJobType,
    /// URL to the job on GitHub
    pub url: String,
    /// Display name of the workflow the job ran in
    pub workflow_name: String,
}
crate::record_serde::wire_record!(AttachmentGitHubActionsJob { conclusion: "conclusion" if is_none, job_id: "jobId", job_name: "jobName", repo: "repo", r#type: "type", url: "url", workflow_name: "workflowName" });

/// Pointer to a GitHub commit.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubCommit {
    /// First line of the commit message
    pub message: String,
    /// Full commit SHA
    pub oid: String,
    /// Repository the commit belongs to
    pub repo: GitHubRepoRef,
    /// Attachment type discriminator
    pub r#type: AttachmentGitHubCommitType,
    /// URL to the commit on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(AttachmentGitHubCommit {
    message: "message",
    oid: "oid",
    repo: "repo",
    r#type: "type",
    url: "url"
});

/// Pointer to a file in a GitHub repository at a specific ref.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubFile {
    /// Repository-relative path to the file
    pub path: String,
    /// Git ref the file is read at (branch, tag, or commit SHA)
    pub r#ref: String,
    /// Repository the file lives in
    pub repo: GitHubRepoRef,
    /// Attachment type discriminator
    pub r#type: AttachmentGitHubFileType,
    /// URL to the file on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(AttachmentGitHubFile {
    path: "path",
    r#ref: "ref",
    repo: "repo",
    r#type: "type",
    url: "url"
});

/// One side of a file diff (head or base)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubFileDiffSide {
    /// Repository-relative path to the file
    pub path: String,
    /// Git ref (branch, tag, or commit SHA) the file is read at
    pub r#ref: String,
    /// Repository the file lives in
    pub repo: GitHubRepoRef,
}
crate::record_serde::wire_record!(AttachmentGitHubFileDiffSide {
    path: "path",
    r#ref: "ref",
    repo: "repo"
});

/// Pointer to a single-file diff. At least one of `head` and `base` must be present.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubFileDiff {
    /// File location on the base side of the diff. Absent for additions.
    pub base: Option<AttachmentGitHubFileDiffSide>,
    /// File location on the head side of the diff. Absent for deletions.
    pub head: Option<AttachmentGitHubFileDiffSide>,
    /// Attachment type discriminator
    pub r#type: AttachmentGitHubFileDiffType,
    /// URL to the diff on GitHub (e.g., a commit, compare, or PR-file URL)
    pub url: String,
}
crate::record_serde::wire_record!(AttachmentGitHubFileDiff { base: "base" if is_none, head: "head" if is_none, r#type: "type", url: "url" });

/// GitHub issue, pull request, or discussion reference
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubReference {
    /// Issue, pull request, or discussion number
    pub number: i64,
    /// Type of GitHub reference
    pub reference_type: AttachmentGitHubReferenceType,
    /// Current state of the referenced item (e.g., open, closed, merged)
    pub state: String,
    /// Title of the referenced item
    pub title: String,
    /// Attachment type discriminator
    pub r#type: AttachmentGitHubReferenceType,
    /// URL to the referenced item on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(AttachmentGitHubReference {
    number: "number",
    reference_type: "referenceType",
    state: "state",
    title: "title",
    r#type: "type",
    url: "url"
});

/// Pointer to a GitHub release.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubRelease {
    /// Human-readable release name
    pub name: String,
    /// Repository the release belongs to
    pub repo: GitHubRepoRef,
    /// Git tag the release is anchored to
    pub tag_name: String,
    /// Attachment type discriminator
    pub r#type: AttachmentGitHubReleaseType,
    /// URL to the release on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(AttachmentGitHubRelease {
    name: "name",
    repo: "repo",
    tag_name: "tagName",
    r#type: "type",
    url: "url"
});

/// Pointer to a GitHub repository.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubRepository {
    /// Short description of the repository
    pub description: Option<String>,
    /// Git ref this attachment is anchored at (branch, tag, or commit). When absent the default branch is implied.
    pub r#ref: Option<String>,
    /// Repository pointer
    pub repo: GitHubRepoRef,
    /// Attachment type discriminator
    pub r#type: AttachmentGitHubRepositoryType,
    /// URL to the repository on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(AttachmentGitHubRepository { description: "description" if is_none, r#ref: "ref" if is_none, repo: "repo", r#type: "type", url: "url" });

/// Pointer to a line range inside a file in a GitHub repository.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubSnippet {
    /// Line range the snippet covers
    pub line_range: AttachmentFileLineRange,
    /// Repository-relative path to the file
    pub path: String,
    /// Git ref the file is read at (branch, tag, or commit SHA)
    pub r#ref: String,
    /// Repository the file lives in
    pub repo: GitHubRepoRef,
    /// Attachment type discriminator
    pub r#type: AttachmentGitHubSnippetType,
    /// URL to the snippet on GitHub (with line anchor)
    pub url: String,
}
crate::record_serde::wire_record!(AttachmentGitHubSnippet {
    line_range: "lineRange",
    path: "path",
    r#ref: "ref",
    repo: "repo",
    r#type: "type",
    url: "url"
});

/// One side of a tree comparison (head or base)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubTreeComparisonSide {
    /// Repository the revision belongs to
    pub repo: GitHubRepoRef,
    /// Git revision (branch, tag, or commit SHA)
    pub revision: String,
}
crate::record_serde::wire_record!(AttachmentGitHubTreeComparisonSide {
    repo: "repo",
    revision: "revision"
});

/// Pointer to a comparison between two git revisions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubTreeComparison {
    /// Base side of the comparison
    pub base: AttachmentGitHubTreeComparisonSide,
    /// Head side of the comparison
    pub head: AttachmentGitHubTreeComparisonSide,
    /// Attachment type discriminator
    pub r#type: AttachmentGitHubTreeComparisonType,
    /// URL to the comparison on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(AttachmentGitHubTreeComparison {
    base: "base",
    head: "head",
    r#type: "type",
    url: "url"
});

/// Generic GitHub URL reference.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentGitHubUrl {
    /// Attachment type discriminator
    pub r#type: AttachmentGitHubUrlType,
    /// URL to the GitHub resource
    pub url: String,
}
crate::record_serde::wire_record!(AttachmentGitHubUrl {
    r#type: "type",
    url: "url"
});

/// End position of the selection
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentSelectionDetailsEnd {
    /// End character offset within the line (0-based)
    pub character: i64,
    /// End line number (0-based)
    pub line: i64,
}
crate::record_serde::wire_record!(AttachmentSelectionDetailsEnd {
    character: "character",
    line: "line"
});

/// Start position of the selection
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentSelectionDetailsStart {
    /// Start character offset within the line (0-based)
    pub character: i64,
    /// Start line number (0-based)
    pub line: i64,
}
crate::record_serde::wire_record!(AttachmentSelectionDetailsStart {
    character: "character",
    line: "line"
});

/// Position range of the selection within the file
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentSelectionDetails {
    /// End position of the selection
    pub end: AttachmentSelectionDetailsEnd,
    /// Start position of the selection
    pub start: AttachmentSelectionDetailsStart,
}
crate::record_serde::wire_record!(AttachmentSelectionDetails {
    end: "end",
    start: "start"
});

/// Code selection attachment from an editor
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentSelection {
    /// User-facing display name for the selection
    pub display_name: String,
    /// Absolute path to the file containing the selection
    pub file_path: String,
    /// Position range of the selection within the file
    pub selection: AttachmentSelectionDetails,
    /// The selected text content
    pub text: String,
    /// Attachment type discriminator
    pub r#type: AttachmentSelectionType,
}
crate::record_serde::wire_record!(AttachmentSelection {
    display_name: "displayName",
    file_path: "filePath",
    selection: "selection",
    text: "text",
    r#type: "type"
});

/// An ID-bound reference to an eligible live session. Supplies identity context only, without sending a message, reading history, or granting permission.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AttachmentSessionReference {
    /// User-visible mention label captured when the reference was selected.
    pub display_name: Option<String>,
    /// Character offset of the selected mention in the displayed prompt.
    pub mention_index: Option<i64>,
    /// Stable identity of the selected session, never resolved from its title.
    pub session_id: SessionId,
    /// Attachment type discriminator.
    pub r#type: AttachmentSessionReferenceType,
}
crate::record_serde::wire_record!(AttachmentSessionReference { display_name: "displayName" if is_none, mention_index: "mentionIndex" if is_none, session_id: "sessionId", r#type: "type" });

/// Result of applying the ordinary session-idle Autopilot continuation decision.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutopilotEvaluateIdleContinuationResult {
    /// Applied continuation outcome.
    pub outcome: AutopilotEvaluateIdleContinuationOutcome,
}
crate::record_serde::wire_record!(AutopilotEvaluateIdleContinuationResult { outcome: "outcome" });

/// Canonical runtime state for the session's current autopilot objective.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutopilotObjectiveGetStateResult {
    /// Current objective state, or `null` when the session has no objective.
    pub state: Option<AutopilotObjectiveState>,
}
crate::record_serde::wire_record!(AutopilotObjectiveGetStateResult { state: "state" });

/// Client-resolved preference for retaining autopilot mode after objective completion or pause.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AutopilotObjectiveSetStayInAutopilotRequest {
    /// Whether an objective's requested return to interactive mode is suppressed.
    pub value: bool,
}
crate::record_serde::wire_record!(AutopilotObjectiveSetStayInAutopilotRequest { value: "value" });

/// Lists models eligible for local use before a chat session exists.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlCatalogRequest {
    /// Ask for the staff registry; the server also verifies its own authenticated identity.
    pub include_dev: bool,
    /// An explicit user refresh retries a cached sign-in failure.
    pub retry_mode: Option<AzureMlCatalogRetryMode>,
}
crate::record_serde::wire_record!(AzureMlCatalogRequest { include_dev: "includeDev", retry_mode: "retryMode" if is_none });

/// License published with an eligible Azure ML model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlLicense {
    /// Bounded display label for the model license.
    pub name: String,
}
crate::record_serde::wire_record!(AzureMlLicense { name: "name" });

/// Validated local runtime requirements for an Azure ML model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlProfile {
    /// Sanitized model-picker display label.
    pub display_name: String,
    /// Published download size, when available.
    pub download_bytes: Option<i64>,
    /// Published license, when available.
    pub license: Option<AzureMlLicense>,
    /// Maximum context window accepted by the model.
    pub max_context_window_tokens: i64,
    /// Maximum output tokens produced by the model.
    pub max_output_tokens: i64,
    /// Maximum input tokens accepted by the model.
    pub max_prompt_tokens: i64,
    /// Peak disk use during installation, when available.
    pub peak_disk_bytes: Option<i64>,
    /// Recommended graphics memory, when available.
    pub recommended_vram_bytes: Option<i64>,
    /// Validated local runtime configuration.
    pub serve_options: serde_json::Value,
    /// Whether the model supports tool calls.
    pub supports_tool_calls: bool,
}
crate::record_serde::wire_record!(AzureMlProfile { display_name: "displayName", download_bytes: "downloadBytes" if is_none, license: "license" if is_none, max_context_window_tokens: "maxContextWindowTokens", max_output_tokens: "maxOutputTokens", max_prompt_tokens: "maxPromptTokens", peak_disk_bytes: "peakDiskBytes" if is_none, recommended_vram_bytes: "recommendedVramBytes" if is_none, serve_options: "serveOptions", supports_tool_calls: "supportsToolCalls" });

/// Eligible Azure ML model version and its provider metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlModel {
    /// Provider metadata retained for registration.
    pub metadata: serde_json::Value,
    /// Stable model-family name.
    pub name: String,
    /// Validated Windows ML requirements.
    pub profile: AzureMlProfile,
    /// Registry that owns this model version.
    pub registry: AzureMlRegistry,
    /// Exact published model version.
    pub version: String,
}
crate::record_serde::wire_record!(AzureMlModel {
    metadata: "metadata",
    name: "name",
    profile: "profile",
    registry: "registry",
    version: "version"
});

/// Redacted warning for a skipped model or registry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlCatalogWarning {
    /// Safe reason without signed access details.
    pub kind: String,
    /// Model family, or `catalog` for registry-wide errors.
    pub model_name: String,
    /// Registry associated with the warning.
    pub registry: AzureMlRegistry,
    /// Version, if one was available.
    pub version: Option<String>,
}
crate::record_serde::wire_record!(AzureMlCatalogWarning { kind: "kind", model_name: "modelName", registry: "registry", version: "version" if is_none });

/// Eligible models and redacted validation warnings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlCatalogResult {
    /// Model versions suitable for local use.
    pub models: Vec<AzureMlModel>,
    /// Skipped model and catalog errors without sensitive access details.
    pub warnings: Vec<AzureMlCatalogWarning>,
}
crate::record_serde::wire_record!(AzureMlCatalogResult {
    models: "models",
    warnings: "warnings"
});

/// Selects an inactive Azure ML version for cleanup.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlDiscardRequest {
    /// Version directory under this server's Azure ML library.
    pub destination: String,
    /// Optional connection-owned lease that spans the caller's complete family mutation.
    pub lease_id: Option<String>,
}
crate::record_serde::wire_record!(AzureMlDiscardRequest { destination: "destination", lease_id: "leaseId" if is_none });

/// Selects an active connection-owned download.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlDownloadIdRequest {
    /// Identifier supplied when this connection started the download.
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(AzureMlDownloadIdRequest {
    request_id: "requestId"
});

/// Downloads a model version to its configured local library.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlDownloadRequest {
    /// Version directory under this server's Azure ML library.
    pub destination: String,
    /// Optional connection-owned lease that spans the caller's complete family mutation.
    pub lease_id: Option<String>,
    /// Exact model-family name.
    pub name: String,
    /// Registry that owns the model.
    pub registry: AzureMlRegistry,
    /// Unique request identifier for progress and explicit cancellation.
    pub request_id: RequestId,
    /// Exact published version.
    pub version: String,
}
crate::record_serde::wire_record!(AzureMlDownloadRequest { destination: "destination", lease_id: "leaseId" if is_none, name: "name", registry: "registry", request_id: "requestId", version: "version" });

/// Completed and verified Azure ML model files.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlDownloadResult {
    /// Installed companion file paths.
    pub extra_files: Vec<String>,
    /// Published manifest path.
    pub manifest_path: String,
    /// Maximum supported local concurrency.
    pub max_concurrent: i64,
    /// Installed GGUF path.
    pub primary_path: String,
    /// Bytes received over the network during this run, including discarded bytes.
    pub received_bytes: Option<i64>,
    /// Whether a partial from a prior run continued after the server accepted its range.
    pub resumed: Option<bool>,
    /// Whether all prior verified files were reused.
    pub reused: bool,
    /// Validated local runtime settings.
    pub serve_options: serde_json::Value,
    /// Exact bytes across the model version.
    pub total_bytes: i64,
    /// Transient transfer failures retried during this run.
    pub total_failures: Option<i64>,
}
crate::record_serde::wire_record!(AzureMlDownloadResult { extra_files: "extraFiles", manifest_path: "manifestPath", max_concurrent: "maxConcurrent", primary_path: "primaryPath", received_bytes: "receivedBytes" if is_none, resumed: "resumed" if is_none, reused: "reused", serve_options: "serveOptions", total_bytes: "totalBytes", total_failures: "totalFailures" if is_none });

/// Progress and retry facts for one active download.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlDownloadStatus {
    /// Bytes completed across all files.
    pub completed_bytes: i64,
    /// Configured retry limit, if the transfer is retrying.
    pub max_attempts: Option<i64>,
    /// Current transfer phase.
    pub phase: String,
    /// Next retry attempt, if the transfer is retrying.
    pub retry_attempt: Option<i64>,
    /// Planned delay before retry, if the transfer is retrying.
    pub retry_delay_ms: Option<i64>,
    /// Redacted failure reason, if the transfer is retrying.
    pub retry_reason: Option<String>,
    /// Exact bytes expected across all files.
    pub total_bytes: i64,
}
crate::record_serde::wire_record!(AzureMlDownloadStatus { completed_bytes: "completedBytes", max_attempts: "maxAttempts" if is_none, phase: "phase", retry_attempt: "retryAttempt" if is_none, retry_delay_ms: "retryDelayMs" if is_none, retry_reason: "retryReason" if is_none, total_bytes: "totalBytes" });

/// Opaque handle to a connection-owned family mutation interval.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlFamilyLeaseResult {
    /// Server-generated handle, valid only on the originating SDK connection.
    pub lease_id: String,
}
crate::record_serde::wire_record!(AzureMlFamilyLeaseResult {
    lease_id: "leaseId"
});

/// Releases a family mutation interval owned by this SDK connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlFamilyReleaseRequest {
    /// Server-generated handle from `azureMl.acquireFamily`.
    pub lease_id: String,
    /// Exact model-family name.
    pub name: String,
    /// Registry that owns the acquired family.
    pub registry: AzureMlRegistry,
}
crate::record_serde::wire_record!(AzureMlFamilyReleaseRequest {
    lease_id: "leaseId",
    name: "name",
    registry: "registry"
});

/// Identifies a family in the server's Azure ML model library.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlFamilyRequest {
    /// Exact model-family name.
    pub name: String,
    /// Registry that owns the model family.
    pub registry: AzureMlRegistry,
}
crate::record_serde::wire_record!(AzureMlFamilyRequest {
    name: "name",
    registry: "registry"
});

/// Graphics memory available for an on-device model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlGpuMemory {
    /// Dedicated graphics memory, in bytes.
    pub dedicated_bytes: f64,
    /// Adapter description reported by the host.
    pub description: String,
    /// Shared memory usable by a unified adapter, in bytes.
    pub shared_bytes: f64,
    /// Dedicated and usable shared memory, in bytes.
    pub total_bytes: f64,
}
crate::record_serde::wire_record!(AzureMlGpuMemory {
    dedicated_bytes: "dedicatedBytes",
    description: "description",
    shared_bytes: "sharedBytes",
    total_bytes: "totalBytes"
});

/// Measured graphics capability; missing memory means the adapter is unknown.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlHardwareResult {
    /// Selected hardware adapter, absent when the platform cannot measure it.
    pub gpu_memory: Option<AzureMlGpuMemory>,
}
crate::record_serde::wire_record!(AzureMlHardwareResult { gpu_memory: "gpuMemory" if is_none });

/// Selects an Azure ML model family.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlLatestRequest {
    /// Exact model-family name.
    pub name: String,
    /// Registry that owns the model.
    pub registry: AzureMlRegistry,
}
crate::record_serde::wire_record!(AzureMlLatestRequest {
    name: "name",
    registry: "registry"
});

/// Managed Azure ML registration changes under a connection-owned family lease.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlRegistrationRequest {
    /// Exact prior rows returned by a successful upsert, required for restoration.
    pub before: Option<serde_json::Value>,
    /// Provider and model entries for an upsert.
    pub entries: Option<ModelProviderSaveConfigurationRequest>,
    /// Lease retained through transfer, registration, activation, and compensation.
    pub lease_id: String,
    /// Exact model family associated with the lease.
    pub name: String,
    /// Scoped registration change to perform.
    pub operation: AzureMlRegistrationOperation,
    /// Registry associated with the acquired family lease.
    pub registry: AzureMlRegistry,
}
crate::record_serde::wire_record!(AzureMlRegistrationRequest { before: "before" if is_none, entries: "entries" if is_none, lease_id: "leaseId", name: "name", operation: "operation", registry: "registry" });

/// Observable outcome of a scoped registration change.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlRegistrationResult {
    /// Prior managed rows captured under the write lock, for an upsert.
    pub before: Option<serde_json::Value>,
    /// Refusal reason or write error, absent after success.
    pub detail: Option<String>,
    /// Whether the change succeeded, was refused, or failed.
    pub kind: AzureMlRegistrationKind,
    /// Effective providers file, including the configured environment override.
    pub path: String,
}
crate::record_serde::wire_record!(AzureMlRegistrationResult { before: "before" if is_none, detail: "detail" if is_none, kind: "kind", path: "path" });

/// Selects an immutable model version.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlResolveRequest {
    /// Exact model-family name.
    pub name: String,
    /// Registry that owns the model.
    pub registry: AzureMlRegistry,
    /// Exact published version.
    pub version: String,
}
crate::record_serde::wire_record!(AzureMlResolveRequest {
    name: "name",
    registry: "registry",
    version: "version"
});

/// Resolved artifact facts without signed download URLs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlResolveResult {
    /// Other paths relative to the version directory.
    pub extra_files: Vec<String>,
    /// Maximum supported local concurrency.
    pub max_concurrent: i64,
    /// GGUF path relative to the version directory.
    pub primary_path: String,
    /// Validated local runtime settings.
    pub serve_options: serde_json::Value,
    /// Exact bytes across all files in this version.
    pub total_bytes: i64,
}
crate::record_serde::wire_record!(AzureMlResolveResult {
    extra_files: "extraFiles",
    max_concurrent: "maxConcurrent",
    primary_path: "primaryPath",
    serve_options: "serveOptions",
    total_bytes: "totalBytes"
});

/// The running runtime's complete catalog of well-known built-in model IDs, including supported models and additional IDs with built-in metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct BuiltInModelCatalog {
    /// Built-in model entries.
    pub models: Vec<BuiltInModelCatalogEntry>,
}
crate::record_serde::wire_record!(BuiltInModelCatalog { models: "models" });

/// Per-field telemetry-safety policy for a built-in tool.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct BuiltinToolSafeTelemetryFields {
    /// Whether tool input names may be included in telemetry without obfuscation.
    pub inputs_names: Option<bool>,
    /// Whether the tool name may be included in telemetry without obfuscation.
    pub name: Option<bool>,
}
crate::record_serde::wire_record!(BuiltinToolSafeTelemetryFields { inputs_names: "inputsNames" if is_none, name: "name" if is_none });

/// Existing initial interactive CLI readiness observation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CafeMetricsRecordCliReadyRequest {
    /// Existing startup_duration_ms, without rounding or a new clock. Absent when unavailable.
    pub duration_ms: Option<f64>,
    /// Intent captured before the first interactive mount.
    #[doc(hidden)]
    pub(crate) launch_scenario: CafeMetricsLaunchScenario,
}
crate::record_serde::wire_record!(CafeMetricsRecordCliReadyRequest { duration_ms: "durationMs" if is_none, launch_scenario: "launchScenario" });

/// Terminal result of one resume attempt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CafeMetricsRecordResumeResultRequest {
    /// Terminal outcome.
    #[doc(hidden)]
    pub(crate) outcome: CafeMetricsResumeOutcome,
    /// Bounded result classification.
    #[doc(hidden)]
    pub(crate) reason: CafeMetricsResumeReason,
    /// Whether an event-log recovery projection was used.
    pub recovered: bool,
    /// Last phase reached.
    #[doc(hidden)]
    pub(crate) stage: CafeMetricsResumeStage,
}
crate::record_serde::wire_record!(CafeMetricsRecordResumeResultRequest {
    outcome: "outcome",
    reason: "reason",
    recovered: "recovered",
    stage: "stage"
});

/// Release metadata without credentials or links.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CafeMetricsRelease {
    /// Whether this is a draft.
    pub draft: Option<bool>,
    /// Whether this is a prerelease.
    pub prerelease: bool,
    /// Raw release tag.
    pub tag_name: String,
}
crate::record_serde::wire_record!(CafeMetricsRelease { draft: "draft" if is_none, prerelease: "prerelease", tag_name: "tagName" });

#[derive(Debug, Clone, Default)]
pub struct CafeMetricsResolveAdmissionRequestSuccess {
    pub kind: CafeMetricsResolveAdmissionRequestSuccessKind,
    /// Finite non-negative lookup duration in milliseconds.
    pub lookup_duration_ms: f64,
    /// Release metadata from the lookup shared with auto-update.
    #[doc(hidden)]
    pub(crate) releases: Vec<CafeMetricsRelease>,
}
crate::record_serde::wire_record!(CafeMetricsResolveAdmissionRequestSuccess {
    kind: "kind",
    lookup_duration_ms: "lookupDurationMs",
    releases: "releases"
});

#[derive(Debug, Clone, Default)]
pub struct CafeMetricsResolveAdmissionRequestFailed {
    pub kind: CafeMetricsResolveAdmissionRequestFailedKind,
    /// Finite non-negative lookup duration in milliseconds.
    pub lookup_duration_ms: f64,
}
crate::record_serde::wire_record!(CafeMetricsResolveAdmissionRequestFailed {
    kind: "kind",
    lookup_duration_ms: "lookupDurationMs"
});

#[derive(Debug, Clone, Default)]
pub struct CafeMetricsResolveAdmissionResultApplied {
    pub kind: CafeMetricsResolveAdmissionResultAppliedKind,
}
crate::record_serde::wire_record!(CafeMetricsResolveAdmissionResultApplied { kind: "kind" });

#[derive(Debug, Clone, Default)]
pub struct CafeMetricsResolveAdmissionResultNotStarted {
    pub kind: CafeMetricsResolveAdmissionResultNotStartedKind,
}
crate::record_serde::wire_record!(CafeMetricsResolveAdmissionResultNotStarted { kind: "kind" });

#[derive(Debug, Clone, Default)]
pub struct CafeMetricsResolveAdmissionResultAlreadyResolved {
    pub kind: CafeMetricsResolveAdmissionResultAlreadyResolvedKind,
}
crate::record_serde::wire_record!(CafeMetricsResolveAdmissionResultAlreadyResolved {
    kind: "kind"
});

#[derive(Debug, Clone, Default)]
pub struct CafeMetricsResolveAdmissionResultShutdown {
    pub kind: CafeMetricsResolveAdmissionResultShutdownKind,
}
crate::record_serde::wire_record!(CafeMetricsResolveAdmissionResultShutdown { kind: "kind" });

/// Preliminary flush or final disposal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CafeMetricsShutdownRequest {
    /// Send at most one batch without stopping recording. Omission means disposal.
    pub flush_only: Option<bool>,
}
crate::record_serde::wire_record!(CafeMetricsShutdownRequest { flush_only: "flushOnly" if is_none });

/// Versions and provider configuration for CAFE.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CafeMetricsStartRequest {
    /// Effective comparison version and appVersion tag.
    pub app_version: String,
    /// Raw package build version.
    pub build_version: String,
    /// Whether the host uses a custom model provider.
    pub custom_provider_configured: bool,
    /// Embedded package identity. Omission enables the unbundled development fallback.
    #[doc(hidden)]
    pub(crate) embedded_package_identity: Option<CafeMetricsEmbeddedPackageIdentity>,
}
crate::record_serde::wire_record!(CafeMetricsStartRequest { app_version: "appVersion", build_version: "buildVersion", custom_provider_configured: "customProviderConfigured", embedded_package_identity: "embeddedPackageIdentity" if is_none });

/// CAFE startup result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CafeMetricsStartResult {
    /// Refusal reason, absent on success.
    #[doc(hidden)]
    pub(crate) reason: Option<CafeMetricsStartRefusal>,
    /// Whether this call started CAFE.
    pub started: bool,
}
crate::record_serde::wire_record!(CafeMetricsStartResult { reason: "reason" if is_none, started: "started" });

/// Cancellation result for a user-requested shell command.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CancelUserRequestedShellCommandResult {
    /// Whether an in-flight execution was found and signalled to cancel
    pub cancelled: bool,
}
crate::record_serde::wire_record!(CancelUserRequestedShellCommandResult {
    cancelled: "cancelled"
});

/// Agent Finder's verified materialisation of the plugin. Present only when the candidate carries a complete materialisation reference; the reference itself stays runtime-private.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogAgentPluginMaterializationSource {
    /// Discriminator: the plugin installs from Agent Finder's materialisation.
    #[serde(deserialize_with = "CatalogAgentPluginMaterializationSource::deserialize_kind")]
    pub kind: CatalogAgentPluginMaterializationSourceKind,
}

impl CatalogAgentPluginMaterializationSource {
    fn deserialize_kind<'de, D>(
        deserializer: D,
    ) -> Result<CatalogAgentPluginMaterializationSourceKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "materialization" {
            return Err(serde::de::Error::unknown_variant(
                &value,
                &["materialization"],
            ));
        }
        <CatalogAgentPluginMaterializationSourceKind>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// A plugin marketplace named by verified catalogue install data, installed as `pluginName@name`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogAgentPluginMarketplaceSource {
    /// Discriminator: the plugin installs from a marketplace.
    #[serde(deserialize_with = "CatalogAgentPluginMarketplaceSource::deserialize_kind")]
    pub kind: CatalogAgentPluginMarketplaceSourceKind,
    /// Marketplace name.
    pub name: String,
    /// Canonical lowercase GitHub owner/repository hosting the marketplace.
    pub repository: String,
}

impl CatalogAgentPluginMarketplaceSource {
    fn deserialize_kind<'de, D>(
        deserializer: D,
    ) -> Result<CatalogAgentPluginMarketplaceSourceKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "marketplace" {
            return Err(serde::de::Error::unknown_variant(&value, &["marketplace"]));
        }
        <CatalogAgentPluginMarketplaceSourceKind>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// A GitHub repository holding the plugin, installed directly as `owner/repo[:path]` at the plan's commit, without a marketplace.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogAgentPluginRepositorySource {
    /// Discriminator: the plugin installs directly from a repository.
    #[serde(deserialize_with = "CatalogAgentPluginRepositorySource::deserialize_kind")]
    pub kind: CatalogAgentPluginRepositorySourceKind,
    /// Relative POSIX path of the plugin directory in the repository. Omitted when the plugin is at the repository root.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    /// Canonical lowercase GitHub owner/repository holding the plugin.
    pub repository: String,
}

impl CatalogAgentPluginRepositorySource {
    fn deserialize_kind<'de, D>(
        deserializer: D,
    ) -> Result<CatalogAgentPluginRepositorySourceKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "repository" {
            return Err(serde::de::Error::unknown_variant(&value, &["repository"]));
        }
        <CatalogAgentPluginRepositorySourceKind>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// What installing an Agent Plugin would do, for the user to review and confirm. Provisional: the catalogue install data it is built from is not final.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogAgentPluginInstallPlan {
    /// Canvas IDs the plugin contributes. Non-empty exactly when components include `canvas`.
    pub canvas_ids: Vec<String>,
    /// Exact full commit ID to install, in lowercase hex. Always present when the plan has a repository source; a plan with a marketplace source carries at least one of version and commit.
    pub commit: Option<String>,
    /// Components the plugin contributes, in canonical order. Empty only in a materialisation-only plan, where the verified manifests list them in the plugins.planInstall review.
    pub components: Vec<CatalogAgentPluginComponent>,
    /// Connectors the plugin depends on.
    pub connector_dependencies: Vec<String>,
    /// Extension IDs the plugin contributes. Non-empty exactly when components include `extension`.
    pub extension_ids: Vec<String>,
    /// MCP servers the plugin depends on but does not bundle.
    pub mcp_dependencies: Vec<String>,
    /// The plugin's name, as named in its marketplace or manifest. Omitted from a materialisation-only plan, whose sources are only `materialization`: the verified manifests supply the name in the plugins.planInstall review.
    pub plugin_name: Option<String>,
    /// Every valid install source, in preference order: materialization, then marketplace, then repository. Each kind appears at most once.
    pub sources: Vec<CatalogAgentPluginInstallSource>,
    /// Exact plugin version to install. A plan with a marketplace source carries at least one of version and commit.
    pub version: Option<String>,
}
crate::record_serde::wire_record!(CatalogAgentPluginInstallPlan { canvas_ids: "canvasIds", commit: "commit" if is_none, components: "components", connector_dependencies: "connectorDependencies", extension_ids: "extensionIds", mcp_dependencies: "mcpDependencies", plugin_name: "pluginName" if is_none, sources: "sources", version: "version" if is_none });

/// Where and when an Agent Plugin catalog reference was observed. Discovery provenance deliberately carries no descriptor URL, raw data, candidate handle, or content digest.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogAgentPluginCandidateProvenance {
    /// Host of the catalog authority that advertised the reference, without path, query, or credentials. Inert untrusted data.
    pub authority: String,
    /// Media type naming the plugin's format.
    pub media_type: CatalogAgentPluginMediaType,
    /// ISO 8601 timestamp at which the runtime observed the catalog reference. This is not a retrieval or validation timestamp.
    pub observed_at: String,
}
crate::record_serde::wire_record!(CatalogAgentPluginCandidateProvenance {
    authority: "authority",
    media_type: "mediaType",
    observed_at: "observedAt"
});

/// Syntactically validated GitHub repository provenance declared by catalog metadata. This is a source claim rather than proof that the descriptor URL resolves to the repository.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogPluginRepositorySource {
    /// Case-preserving safe relative POSIX path derived from metadata.repoPath. Absent when the catalog declared sourceSet without repoPath, meaning the plugin is at the repository root; only callers that require agent-plugin-confirmed-installation receive such a source.
    pub path: Option<String>,
    /// Canonical lowercase owner/repository name derived from metadata.sourceSet.
    pub repository: String,
}
crate::record_serde::wire_record!(CatalogPluginRepositorySource { path: "path" if is_none, repository: "repository" });

/// An inert Agent Plugin catalog result. Its canonical catalog identity, declared version, repository source claim, and explicit compatibility tags are safe to correlate, while its descriptor, URL, raw data, materialisation reference, and installed-plugin state remain runtime-private. A handle is minted only under agent-plugin-confirmed-installation, for a candidate with a complete materialisation reference.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogAgentPluginCandidate {
    /// Category slugs reported by the feed, such as `build` or `review`, without repeats. Values are passed through, so clients must tolerate slugs they do not recognise. Inert untrusted text. Omitted when the feed reports none.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub categories: Option<Vec<String>>,
    /// Explicit validated compatibility tags, in canonical order. An empty list means the source declared no recognised compatibility; clients must not infer compatibility from other fields. `canvas-only` requires both `canvas` and `github-copilot`.
    pub compatibility_tags: Vec<CatalogAgentPluginCompatibilityTag>,
    /// Identifiers of the connectors the resource works with, as reported by the feed, without repeats. Inert untrusted text. Omitted when the feed reports none.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub connector_ids: Option<Vec<String>>,
    /// Description taken verbatim from the card. Inert untrusted text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Display name taken verbatim from the card. Inert untrusted text.
    pub display_name: String,
    /// Opaque, runtime-instance scoped, TTL-bound, single-use handle for planning this plugin's installation through plugins.planInstall. Present only when the caller requires the agent-plugin-confirmed-installation capability and the candidate carries a complete Agent Finder materialisation reference, which stays runtime-private. Never logged.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub handle: Option<String>,
    /// ISO 8601 timestamp after which the handle is stale and will be rejected. Present exactly when handle is present.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub handle_expires_at: Option<String>,
    /// HTTPS URL of the resource's icon on raw.githubusercontent.com, as reported by the feed. Untrusted external content: load it without credentials. Omitted when absent or outside the feed's icon host policy.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon_url: Option<String>,
    /// Validated, normalised catalogue resource URN. This identity comes only from the catalog identifier and is never inferred from display text or installed-plugin state.
    pub identity: String,
    /// Whether this plugin can be installed in the selected session. Present only when the caller requires the agent-plugin-confirmed-installation capability.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub installability: Option<CatalogAgentPluginInstallability>,
    /// How many times the resource has been installed, as reported by the feed. Omitted when the feed reports no count or an invalid one.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub install_count: Option<i64>,
    /// Reviewable install plan built only from verified catalogue install data. Present exactly when installability is `installable`; the runtime never derives it from the repository source, display text, or identity.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub install_plan: Option<CatalogAgentPluginInstallPlan>,
    /// Discriminator: this candidate describes an Agent Plugin.
    #[serde(deserialize_with = "CatalogAgentPluginCandidate::deserialize_kind")]
    pub kind: CatalogAgentPluginCandidateKind,
    /// Media type naming the plugin's format.
    pub media_type: CatalogAgentPluginMediaType,
    /// Where the Agent Plugin catalog reference was observed, without its descriptor, URL, raw data, or content digest.
    pub provenance: CatalogAgentPluginCandidateProvenance,
    /// Publisher taken verbatim from the card. Inert untrusted text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub publisher: Option<String>,
    /// Bounded repository provenance declared through the catalog's sourceSet and repoPath metadata. It contains no descriptor URL. Always present, with a path, unless the caller requires agent-plugin-confirmed-installation. Such a caller also receives a repository-root plugin (sourceSet without repoPath) whose source has no path, and a plugin with no usable declaration (neither key, or repoPath alone) and no source; the latter carries a complete materialisation reference and a handle, and the verified descriptor supplies the repository, commit and root when it is planned. Never derived from the catalogue URL.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<CatalogPluginRepositorySource>,
    /// Versioned trust metadata observed from the catalog authority. Optional for protocol-3 compatibility and emitted only when the caller also requires the trust-snapshot capability.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trust: Option<serde_json::Value>,
    /// Optional version declared by the catalog source. Omitted rather than guessed when the source supplies no version.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
}

impl CatalogAgentPluginCandidate {
    fn deserialize_kind<'de, D>(
        deserializer: D,
    ) -> Result<CatalogAgentPluginCandidateKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "plugin" {
            return Err(serde::de::Error::unknown_variant(&value, &["plugin"]));
        }
        <CatalogAgentPluginCandidateKind>::deserialize(serde::de::value::StringDeserializer::<
            D::Error,
        >::new(value))
    }
}

/// Where and when an AI skill catalog reference was observed. Discovery provenance deliberately carries no content digest because search does not establish the exact validated content a later plan will bind.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogAiSkillCandidateProvenance {
    /// Host of the catalog authority that advertised the reference, without path, query, or credentials. Inert untrusted data.
    pub authority: String,
    /// Media type advertised for the referenced AI skill card
    #[serde(deserialize_with = "CatalogAiSkillCandidateProvenance::deserialize_media_type")]
    pub media_type: CatalogAiSkillMediaType,
    /// ISO 8601 timestamp at which the runtime observed the catalog reference. This is not a retrieval or validation timestamp.
    pub observed_at: String,
}

impl CatalogAiSkillCandidateProvenance {
    fn deserialize_media_type<'de, D>(deserializer: D) -> Result<CatalogAiSkillMediaType, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "application/ai-skill" {
            return Err(serde::de::Error::unknown_variant(
                &value,
                &["application/ai-skill"],
            ));
        }
        <CatalogAiSkillMediaType>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// Candidate whose card is retrieved from a URL through the runtime's hardened fetch boundary.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogCandidateSourceUrl {
    /// Discriminator: the card is URL-backed, and carries no embedded data
    pub kind: CatalogCandidateSourceUrlKind,
    /// Card URL as advertised. Inert untrusted data: the runtime retrieves it only through its own hardened boundary, and it is never logged.
    pub url: String,
}
crate::record_serde::wire_record!(CatalogCandidateSourceUrl {
    kind: "kind",
    url: "url"
});

/// Candidate whose card reference arrived inline. The document and its content-derived properties stay behind the runtime boundary.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogCandidateSourceEmbedded {
    /// Discriminator: the card is embedded, and carries no URL
    pub kind: CatalogCandidateSourceEmbeddedKind,
}
crate::record_serde::wire_record!(CatalogCandidateSourceEmbedded { kind: "kind" });

/// An inert AI skill catalog result. Verified Skill candidates may be installable only when the runtime reports installability and the selected session is permitted to plan installation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogAiSkillCandidate {
    /// Category slugs reported by the feed, such as `build` or `review`, without repeats. Values are passed through, so clients must tolerate slugs they do not recognise. Inert untrusted text. Omitted when the feed reports none.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub categories: Option<Vec<String>>,
    /// Identifiers of the connectors the resource works with, as reported by the feed, without repeats. Inert untrusted text. Omitted when the feed reports none.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub connector_ids: Option<Vec<String>>,
    /// Description taken verbatim from the card. Inert untrusted text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Display name taken verbatim from the card. Inert untrusted text.
    pub display_name: String,
    /// Opaque, runtime-instance scoped, TTL-bound, single-use handle for this candidate. Carries no readable information and is rejected when stale, replayed, or presented to a different runtime instance. Never logged.
    pub handle: String,
    /// ISO 8601 timestamp after which the handle is stale and will be rejected.
    pub handle_expires_at: String,
    /// HTTPS URL of the resource's icon on raw.githubusercontent.com, as reported by the feed. Untrusted external content: load it without credentials. Omitted when absent or outside the feed's icon host policy.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon_url: Option<String>,
    /// Whether this AI skill candidate can be planned for verified installation in the selected session.
    pub installability: CatalogAiSkillInstallability,
    /// How many times the resource has been installed, as reported by the feed. Omitted when the feed reports no count or an invalid one.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub install_count: Option<i64>,
    /// Discriminator: this candidate describes an AI skill
    #[serde(deserialize_with = "CatalogAiSkillCandidate::deserialize_kind")]
    pub kind: CatalogAiSkillCandidateKind,
    /// Media type of the underlying AI skill card
    #[serde(deserialize_with = "CatalogAiSkillCandidate::deserialize_media_type")]
    pub media_type: CatalogAiSkillMediaType,
    /// Where the catalog reference was observed, without the card itself or any content digest.
    pub provenance: CatalogAiSkillCandidateProvenance,
    /// Publisher taken verbatim from the card. Inert untrusted text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub publisher: Option<String>,
    /// Where the card came from: exactly one of a URL or embedded data, encoded as a tagged union so neither both nor neither can be represented.
    pub source: CatalogCandidateSource,
    /// Versioned trust metadata observed from the catalog authority. Optional for protocol-3 compatibility with runtimes that predate trust snapshots. A trust-capable runtime emits an explicit snapshot even when the authority omitted or malformed its trust field.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trust: Option<serde_json::Value>,
}

impl CatalogAiSkillCandidate {
    fn deserialize_kind<'de, D>(deserializer: D) -> Result<CatalogAiSkillCandidateKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "ai-skill" {
            return Err(serde::de::Error::unknown_variant(&value, &["ai-skill"]));
        }
        <CatalogAiSkillCandidateKind>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }

    fn deserialize_media_type<'de, D>(deserializer: D) -> Result<CatalogAiSkillMediaType, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "application/ai-skill" {
            return Err(serde::de::Error::unknown_variant(
                &value,
                &["application/ai-skill"],
            ));
        }
        <CatalogAiSkillMediaType>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// Where and when an MCP server catalog reference was observed. Discovery provenance deliberately carries no content digest because search does not establish the exact validated content a later plan will bind.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogMcpServerCandidateProvenance {
    /// Host of the catalog authority that advertised the reference, without path, query, or credentials. Inert untrusted data.
    pub authority: String,
    /// JSON MCP media type advertised for the referenced card.
    pub media_type: McpServerCardMediaType,
    /// ISO 8601 timestamp at which the runtime observed the catalog reference. This is not a retrieval or validation timestamp.
    pub observed_at: String,
}
crate::record_serde::wire_record!(CatalogMcpServerCandidateProvenance {
    authority: "authority",
    media_type: "mediaType",
    observed_at: "observedAt"
});

/// An inert MCP server catalog result. Every free-text field is untrusted external data and must never be treated as an instruction, and the handle is the only way to refer to the candidate in a later operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogMcpServerCandidate {
    /// Category slugs reported by the feed, such as `build` or `review`, without repeats. Values are passed through, so clients must tolerate slugs they do not recognise. Inert untrusted text. Omitted when the feed reports none.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub categories: Option<Vec<String>>,
    /// Identifiers of the connectors the resource works with, as reported by the feed, without repeats. Inert untrusted text. Omitted when the feed reports none.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub connector_ids: Option<Vec<String>>,
    /// Description taken verbatim from the card. Inert untrusted text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Display name taken verbatim from the card. Inert untrusted text.
    pub display_name: String,
    /// Opaque, runtime-instance scoped, TTL-bound, single-use handle for this candidate. Carries no readable information and is rejected when stale, replayed, or presented to a different runtime instance. Never logged.
    pub handle: String,
    /// ISO 8601 timestamp after which the handle is stale and will be rejected.
    pub handle_expires_at: String,
    /// HTTPS URL of the resource's icon on raw.githubusercontent.com, as reported by the feed. Untrusted external content: load it without credentials. Omitted when absent or outside the feed's icon host policy.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon_url: Option<String>,
    /// Whether this MCP server can be planned for installation, and if policy prevents it.
    pub installability: CatalogMcpServerInstallability,
    /// How many times the resource has been installed, as reported by the feed. Omitted when the feed reports no count or an invalid one.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub install_count: Option<i64>,
    /// Discriminator: this candidate describes an MCP server
    #[serde(deserialize_with = "CatalogMcpServerCandidate::deserialize_kind")]
    pub kind: CatalogMcpServerCandidateKind,
    /// JSON MCP media type of the underlying card.
    pub media_type: McpServerCardMediaType,
    /// Where the catalog reference was observed, without the card itself or any content digest.
    pub provenance: CatalogMcpServerCandidateProvenance,
    /// Publisher taken verbatim from the card. Inert untrusted text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub publisher: Option<String>,
    /// Where the card came from: exactly one of a URL or embedded data, encoded as a tagged union so neither both nor neither can be represented.
    pub source: CatalogCandidateSource,
    /// Versioned trust metadata observed from the catalog authority. Optional for protocol-3 compatibility with runtimes that predate trust snapshots. A trust-capable runtime emits an explicit snapshot even when the authority omitted or malformed its trust field.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trust: Option<serde_json::Value>,
}

impl CatalogMcpServerCandidate {
    fn deserialize_kind<'de, D>(deserializer: D) -> Result<CatalogMcpServerCandidateKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "mcp-server" {
            return Err(serde::de::Error::unknown_variant(&value, &["mcp-server"]));
        }
        <CatalogMcpServerCandidateKind>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// An entry whose resource resolves and is presented as the same candidate catalog.search would return.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogFeedAvailableEntry {
    /// The entry's resource, with the same projection, installability, handle and display metadata as a catalog.search candidate.
    pub candidate: CatalogCandidate,
    /// The entry's resource identifier as saved in the feed.
    pub identifier: String,
    /// Discriminator: the entry's resource is available.
    #[serde(deserialize_with = "CatalogFeedAvailableEntry::deserialize_status")]
    pub status: CatalogFeedAvailableStatus,
}

impl CatalogFeedAvailableEntry {
    fn deserialize_status<'de, D>(deserializer: D) -> Result<CatalogFeedAvailableStatus, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "available" {
            return Err(serde::de::Error::unknown_variant(&value, &["available"]));
        }
        <CatalogFeedAvailableStatus>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// An entry whose resource resolves but is not in one of the requested trust tiers.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogFeedFilteredEntry {
    /// The entry's resource identifier as saved in the feed.
    pub identifier: String,
    /// Discriminator: the entry was filtered by trust tier.
    #[serde(deserialize_with = "CatalogFeedFilteredEntry::deserialize_status")]
    pub status: CatalogFeedFilteredStatus,
}

impl CatalogFeedFilteredEntry {
    fn deserialize_status<'de, D>(deserializer: D) -> Result<CatalogFeedFilteredStatus, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "filtered" {
            return Err(serde::de::Error::unknown_variant(&value, &["filtered"]));
        }
        <CatalogFeedFilteredStatus>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// An entry that cannot be presented, kept in its position so clients decide how to render it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogFeedUnavailableEntry {
    /// The entry's resource identifier as saved in the feed.
    pub identifier: String,
    /// Why the entry cannot be presented.
    pub reason: CatalogFeedUnavailableReason,
    /// Discriminator: the entry's resource is unavailable.
    #[serde(deserialize_with = "CatalogFeedUnavailableEntry::deserialize_status")]
    pub status: CatalogFeedUnavailableStatus,
}

impl CatalogFeedUnavailableEntry {
    fn deserialize_status<'de, D>(deserializer: D) -> Result<CatalogFeedUnavailableStatus, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "unavailable" {
            return Err(serde::de::Error::unknown_variant(&value, &["unavailable"]));
        }
        <CatalogFeedUnavailableStatus>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// A generated feed's identity. It has no version; generatedAt is its only freshness signal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogGeneratedFeedDescription {
    /// When the feed was computed, as reported by the authority. Omitted when the authority reports no valid timestamp.
    pub generated_at: Option<String>,
    /// Feed key that was requested.
    pub key: String,
    /// Generation strategy that was requested.
    pub strategy: String,
}
crate::record_serde::wire_record!(CatalogGeneratedFeedDescription { generated_at: "generatedAt" if is_none, key: "key", strategy: "strategy" });

/// Retrieves one generated feed, computed on every request, by strategy and key, such as `telemetry` and `popular`. Requires catalog-feeds and catalog-search-credential-required; add catalog-search-session-bound with policySessionId to retain the selected session's authority for installation, exactly as for catalog.search.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogGetGeneratedFeedRequest {
    /// Protocol version and capabilities the caller requires.
    pub contract: CatalogClientContract,
    /// Feed key within the strategy, such as `popular`.
    pub key: String,
    /// Select an existing attached local session. Requires authenticated, session-bound catalogue support.
    /// The runtime never creates, resumes or reconfigures a session to honour this selector.
    pub policy_session_id: Option<String>,
    /// Generation strategy, such as `telemetry`.
    pub strategy: String,
    /// Accept only resources currently assessed at one of these trust tiers. Entries whose resource is in another tier, or has none, are returned as filtered in their original position. Omit for an unfiltered feed.
    pub trust_tiers: Option<Vec<CatalogTrustTier>>,
}
crate::record_serde::wire_record!(CatalogGetGeneratedFeedRequest { contract: "contract", key: "key", policy_session_id: "policySessionId" if is_none, strategy: "strategy", trust_tiers: "trustTiers" if is_none });

/// A retrieved generated feed. Entries keep the feed's order and are never more than 100.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogGetGeneratedFeedSucceeded {
    /// Feed entries in the feed's order.
    pub entries: Vec<CatalogFeedEntry>,
    /// The feed's own identity and freshness.
    pub feed: CatalogGeneratedFeedDescription,
    /// Discriminator: the feed was retrieved
    pub kind: CatalogGetGeneratedFeedSucceededKind,
    /// Protocol version and capabilities the runtime honoured.
    pub negotiated: CatalogNegotiatedContract,
    /// Runtime-issued identifier for this retrieval, with the same meaning as a catalog.search searchId, so a plan can be tied to the feed that produced its candidate. Derived from no user, account, device or feed data.
    pub search_id: String,
}
crate::record_serde::wire_record!(CatalogGetGeneratedFeedSucceeded {
    entries: "entries",
    feed: "feed",
    kind: "kind",
    negotiated: "negotiated",
    search_id: "searchId"
});

/// Retrieves one saved feed by its opaque ID. Requires catalog-feeds and catalog-search-credential-required; add catalog-search-session-bound with policySessionId to retain the selected session's authority for installation, exactly as for catalog.search.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogGetSavedFeedRequest {
    /// Protocol version and capabilities the caller requires.
    pub contract: CatalogClientContract,
    /// Opaque saved feed ID. Treat it as an indivisible string.
    pub feed_id: String,
    /// Select an existing attached local session. Requires authenticated, session-bound catalogue support.
    /// The runtime never creates, resumes or reconfigures a session to honour this selector.
    pub policy_session_id: Option<String>,
    /// Accept only resources currently assessed at one of these trust tiers. Entries whose resource is in another tier, or has none, are returned as filtered in their original position. Omit for an unfiltered feed.
    pub trust_tiers: Option<Vec<CatalogTrustTier>>,
}
crate::record_serde::wire_record!(CatalogGetSavedFeedRequest { contract: "contract", feed_id: "feedId", policy_session_id: "policySessionId" if is_none, trust_tiers: "trustTiers" if is_none });

/// A saved feed's identity. Its version changes only when the feed's definition is replaced, not when a referenced resource changes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSavedFeedDescription {
    /// Opaque feed ID.
    pub id: String,
    /// Feed name as saved by its creator. Inert untrusted text.
    pub name: String,
    /// Version of the saved feed definition.
    pub version: i64,
}
crate::record_serde::wire_record!(CatalogSavedFeedDescription {
    id: "id",
    name: "name",
    version: "version"
});

/// A retrieved saved feed. Entries keep the feed's order and are never more than 100.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogGetSavedFeedSucceeded {
    /// Feed entries in the feed's order.
    pub entries: Vec<CatalogFeedEntry>,
    /// The feed's own identity and freshness.
    pub feed: CatalogSavedFeedDescription,
    /// Discriminator: the feed was retrieved
    pub kind: CatalogGetSavedFeedSucceededKind,
    /// Protocol version and capabilities the runtime honoured.
    pub negotiated: CatalogNegotiatedContract,
    /// Runtime-issued identifier for this retrieval, with the same meaning as a catalog.search searchId, so a plan can be tied to the feed that produced its candidate. Derived from no user, account, device or feed data.
    pub search_id: String,
}
crate::record_serde::wire_record!(CatalogGetSavedFeedSucceeded {
    entries: "entries",
    feed: "feed",
    kind: "kind",
    negotiated: "negotiated",
    search_id: "searchId"
});

/// An Agent Finder materialisation reference pinning one exact resource revision.
///
/// It carries no origin or URL: the runtime always builds the descriptor URL on its fixed Agent
/// Finder authority. It carries no display data either; every reviewed field comes from the
/// verified descriptor and Skill files.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogMaterializationReference {
    /// Expected `sha256:<lowercase hex>` digest of the descriptor's bundle manifest.
    pub bundle_digest: String,
    /// Expected `sha256:<lowercase hex>` digest of the canonical descriptor.
    pub descriptor_digest: String,
    /// Agent Finder resource identifier, a positive decimal integer.
    pub resource_id: String,
    /// Agent Finder materialisation revision identifier, a positive decimal integer. This is not a Git commit.
    pub revision_id: String,
    /// Materialisation descriptor schema version, currently `1.0.0`.
    pub schema_version: String,
}
crate::record_serde::wire_record!(CatalogMaterializationReference {
    bundle_digest: "bundleDigest",
    descriptor_digest: "descriptorDigest",
    resource_id: "resourceId",
    revision_id: "revisionId",
    schema_version: "schemaVersion"
});

/// Side-effect-free planning of whatever resource a materialisation reference pins, rather than a search candidate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogPlanInstallFromReferenceRequest {
    /// Required authenticated bound catalogue and catalog-reference-installation capabilities, plus the installation capability of each kind the caller can apply.
    pub contract: CatalogClientContract,
    /// Existing local session attached to this connection.
    pub policy_session_id: String,
    /// Materialisation reference to verify and plan.
    pub reference: CatalogMaterializationReference,
}
crate::record_serde::wire_record!(CatalogPlanInstallFromReferenceRequest {
    contract: "contract",
    policy_session_id: "policySessionId",
    reference: "reference"
});

/// Initial-page official-product recommendations for a complete task. The runtime fixes recommendationPolicy to official-product and pageSize to 10; callers cannot override the policy, request pagination or fall back to search.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogRecommendRequest {
    /// Required contract, including catalog-recommend, catalog-search-session-bound and catalog-search-credential-required.
    pub contract: CatalogClientContract,
    /// Existing attached local session whose account, host and connection own the candidate references. Never creates or resumes a session.
    pub policy_session_id: String,
    /// Requested product. Agent Finder resolves aliases and applies its versioned official-product approvals before limiting results.
    pub product: String,
    /// Full original task, including action, account, output and host requirements. Forwarded without trimming or rewriting and excluded from telemetry.
    pub query: String,
}
crate::record_serde::wire_record!(CatalogRecommendRequest {
    contract: "contract",
    policy_session_id: "policySessionId",
    product: "product",
    query: "query"
});

/// The reference pins a Skill. Apply the plan with skills.applyInstall.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogReferenceInstallationOutcomeSkillInstallPlanned {
    /// Reference planning outcome discriminator naming the planned kind.
    pub kind: CatalogReferenceInstallationOutcomeSkillInstallPlannedKind,
    /// Prepared Skill install plan.
    pub plan: SkillInstallPlan,
}

/// The reference pins an Agent Plugin. Apply the plan with plugins.applyInstall.
#[derive(Debug, Clone, Default)]
pub struct CatalogReferenceInstallationOutcomePluginInstallPlanned {
    /// Reference planning outcome discriminator naming the planned kind.
    pub kind: CatalogReferenceInstallationOutcomePluginInstallPlannedKind,
    /// Prepared Agent Plugin install plan.
    pub plan: PluginInstallPlan,
}
crate::record_serde::wire_record!(CatalogReferenceInstallationOutcomePluginInstallPlanned {
    kind: "kind",
    plan: "plan"
});

/// Nothing was planned.
#[derive(Debug, Clone, Default)]
pub struct CatalogReferenceInstallationOutcomeRefused {
    /// Reference planning outcome discriminator naming the planned kind.
    pub kind: CatalogReferenceInstallationOutcomeRefusedKind,
    /// Bounded refusal reason.
    pub reason: CatalogReferenceInstallationFailureReason,
    /// Whole seconds GitHub asked the caller to wait, present only with source-rate-limited.
    pub retry_after_seconds: Option<i64>,
}
crate::record_serde::wire_record!(CatalogReferenceInstallationOutcomeRefused { kind: "kind", reason: "reason", retry_after_seconds: "retryAfterSeconds" if is_none });

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogReferenceInstallationResultOutcome {
    /// Discriminator: handled planning outcome.
    pub kind: CatalogReferenceInstallationResultOutcomeKind,
    /// Capabilities honoured for this request.
    pub negotiated: CatalogNegotiatedContract,
    /// Planning outcome.
    pub outcome: CatalogReferenceInstallationOutcome,
}

/// An explicit numbered-page request. SDK consumers treat the token as opaque. For bound search, the runtime unwraps an expiring owner-bound reference to the private authority token; only the runtime changes the authority token's targetPage. Legacy unbound navigation keeps its authority-issued token semantics. No snapshot stability is promised.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSearchPage {
    /// Requested one-based page. Must not exceed either the token's signed pageCount or the navigation window ceil(1000 / pageSize). Repeat the search without page to discover newly available pages beyond that signed pageCount.
    pub number: i32,
    /// Opaque pagination token from an earlier response, owner-bound when session-bound search was requested. Never decode, modify or log it in an SDK consumer. Expired or foreign bound references require a fresh bound search, not a legacy retry.
    pub token: String,
}
crate::record_serde::wire_record!(CatalogSearchPage {
    number: "number",
    token: "token"
});

/// A bounded catalog search. Both the query length and the result count are capped by the schema so a caller cannot request an unbounded scan.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSearchRequest {
    /// Restrict results to resources in at least one of these server-defined workflow categories, such as `design` or `test`. Requires the `catalog-search-categories` capability. Each value is a lowercase slug; the known set is not exhaustive, and a category the server does not recognise matches nothing. Omit to search every category.
    pub categories: Option<Vec<String>>,
    /// Protocol version and capabilities the caller requires.
    pub contract: CatalogClientContract,
    /// Restrict results to these candidate kinds. Agent Plugins are opt-in and require the `agent-plugin-discovery` capability so protocol-v3 clients generated before that variant cannot receive an unknown result; when omitted, the backwards-compatible MCP server and AI skill kinds are searched.
    pub kinds: Option<Vec<CatalogCandidateKind>>,
    /// Maximum number of candidates to return. Defaults to 10 when omitted.
    pub limit: Option<i32>,
    /// Numbered navigation using metadata from an earlier response. Requires catalog-search-pagination and the same query, kinds, categories and effective limit. Omit for a fresh first-page search.
    pub page: Option<CatalogSearchPage>,
    /// Select an existing attached local session. Requires authenticated, session-bound search.
    /// The runtime never creates, resumes or reconfigures a session to honour this selector.
    pub policy_session_id: Option<String>,
    /// Free-text search query. Persisted as tool input for session continuity, but omitted from telemetry.
    pub query: String,
}
crate::record_serde::wire_record!(CatalogSearchRequest { categories: "categories" if is_none, contract: "contract", kinds: "kinds" if is_none, limit: "limit" if is_none, page: "page" if is_none, policy_session_id: "policySessionId" if is_none, query: "query" });

/// A completed catalog search containing inert candidate summaries. MCP server and AI skill variants carry a single-use handle; the Agent Plugin variant is handleless.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSearchSucceeded {
    /// Matching candidates, never more than the requested limit. All text is inert untrusted data.
    pub candidates: Vec<CatalogCandidate>,
    /// Discriminator: the search completed
    pub kind: CatalogSearchSucceededKind,
    /// Protocol version and capabilities the runtime honoured.
    pub negotiated: CatalogNegotiatedContract,
    /// Navigation metadata for callers explicitly requiring catalog-search-pagination. Omitted when the authority returns no token, including tokenless first-page and continuation responses. Counts are never substituted from candidates.length.
    pub pagination: Option<CatalogSearchPagination>,
    /// Pseudonymous identifier for this search, issued by the runtime or by the catalog authority it queried and never by the caller, so it cannot be forged or replayed to attribute an install to a search that never happened. Always present on a success, so a result set can be tied to the installs it leads to. It identifies a search rather than a person: it is derived from no user, account, device, or query data, and must never be joined with user identity to re-identify anyone.
    pub search_id: String,
    /// Legacy indication that the authority returned a page token. Preserved for compatibility; this is not a has-next-page indicator. Use pagination.hasNextPage when pagination metadata is present.
    pub truncated: bool,
}
crate::record_serde::wire_record!(CatalogSearchSucceeded { candidates: "candidates", kind: "kind", negotiated: "negotiated", pagination: "pagination" if is_none, search_id: "searchId", truncated: "truncated" });

/// The request asked for a candidate kind this runtime does not serve.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogUnsupportedKindError {
    /// Discriminator: an unsupported candidate kind was requested
    pub kind: CatalogUnsupportedKindErrorKind,
    /// Human-readable explanation, safe to surface. Never contains a query, URL, handle, or secret.
    pub message: String,
    /// The kinds from the request that are not supported.
    pub requested_kinds: Vec<CatalogCandidateKind>,
    /// Every candidate kind this runtime can serve.
    pub supported_kinds: Vec<CatalogCandidateKind>,
}
crate::record_serde::wire_record!(CatalogUnsupportedKindError {
    kind: "kind",
    message: "message",
    requested_kinds: "requestedKinds",
    supported_kinds: "supportedKinds"
});

/// The caller cancelled the selection interaction and the retained search state was released.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSelectionCancelled {
    /// Discriminator: selection was cancelled
    pub kind: CatalogSelectionCancelledKind,
    /// The search identifier privately bound to the released selection group.
    pub search_id: String,
}
crate::record_serde::wire_record!(CatalogSelectionCancelled {
    kind: "kind",
    search_id: "searchId"
});

/// The caller explicitly declined every candidate and the retained search state was released.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSelectionDeclined {
    /// Discriminator: the candidates were declined
    pub kind: CatalogSelectionDeclinedKind,
    /// The search identifier privately bound to the released selection group.
    pub search_id: String,
}
crate::record_serde::wire_record!(CatalogSelectionDeclined {
    kind: "kind",
    search_id: "searchId"
});

/// The selection reference belongs to another runtime instance or session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSelectionForeign {
    /// Discriminator for this typed selection rejection
    pub kind: CatalogSelectionForeignKind,
    /// Human-readable explanation safe to surface. Never contains the presented reference or private candidate state.
    pub message: String,
}
crate::record_serde::wire_record!(CatalogSelectionForeign {
    kind: "kind",
    message: "message"
});

/// The selection reference was malformed or unknown.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSelectionInvalid {
    /// Discriminator for this typed selection rejection
    pub kind: CatalogSelectionInvalidKind,
    /// Human-readable explanation safe to surface. Never contains the presented reference or private candidate state.
    pub message: String,
}
crate::record_serde::wire_record!(CatalogSelectionInvalid {
    kind: "kind",
    message: "message"
});

/// The selection group was already terminated or its pending host hand-off was already claimed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSelectionReplayed {
    /// Discriminator for this typed selection rejection
    pub kind: CatalogSelectionReplayedKind,
    /// Human-readable explanation safe to surface. Never contains the presented reference or private candidate state.
    pub message: String,
}
crate::record_serde::wire_record!(CatalogSelectionReplayed {
    kind: "kind",
    message: "message"
});

/// Terminates one retained catalog selection group through an opaque reference previously returned by the model-safe search projection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSelectionRequest {
    /// Protocol version and capabilities the caller requires.
    pub contract: CatalogClientContract,
    /// The terminal outcome declared by the caller. Timed-out means the host's live interaction deadline elapsed; a reference whose runtime TTL elapsed is rejected separately as stale.
    pub outcome: CatalogSelectionDecision,
    /// Opaque runtime-instance scoped reference to one visible candidate. For a non-selected outcome, any candidate reference from the same search closes that search's retained group.
    pub selection_ref: String,
    /// Locally owned root session whose retained search state is being resolved.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(CatalogSelectionRequest {
    contract: "contract",
    outcome: "outcome",
    selection_ref: "selectionRef",
    session_id: "sessionId"
});

/// The chosen candidate was transferred into a fresh bounded single-use handle for a later explicit planning request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSelectionSelected {
    /// Fresh single-use candidate handle accepted by mcp.planInstall. Returned only to the native host and never included in model-tool output.
    pub candidate_handle: String,
    /// Discriminator: one candidate was selected
    pub kind: CatalogSelectionSelectedKind,
    /// The exact search identifier privately bound to the selected candidate.
    pub search_id: String,
}
crate::record_serde::wire_record!(CatalogSelectionSelected {
    candidate_handle: "candidateHandle",
    kind: "kind",
    search_id: "searchId"
});

/// The host declared that its live selection interaction timed out, and the retained search state was released.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSelectionTimedOut {
    /// Discriminator: the host's live interaction timed out
    pub kind: CatalogSelectionTimedOutKind,
    /// The search identifier privately bound to the released selection group.
    pub search_id: String,
}
crate::record_serde::wire_record!(CatalogSelectionTimedOut {
    kind: "kind",
    search_id: "searchId"
});

/// The runtime-enforced selection reference lifetime elapsed before the request arrived.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSelectionStale {
    /// Discriminator for this typed selection rejection
    pub kind: CatalogSelectionStaleKind,
    /// Human-readable explanation safe to surface. Never contains the presented reference or private candidate state.
    pub message: String,
}
crate::record_serde::wire_record!(CatalogSelectionStale {
    kind: "kind",
    message: "message"
});

/// The presented opaque handle was issued for another catalog operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogSelectionWrongKind {
    /// Discriminator for this typed selection rejection
    pub kind: CatalogSelectionWrongKindKind,
    /// Human-readable explanation safe to surface. Never contains the presented reference or private candidate state.
    pub message: String,
}
crate::record_serde::wire_record!(CatalogSelectionWrongKind {
    kind: "kind",
    message: "message"
});

/// Where and when the runtime observed the trust metadata. Observation time is not the authority's evaluation time and must not be used to infer staleness.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogTrustProvenance {
    /// ISO 8601 timestamp with a timezone offset at which the runtime observed the search result carrying this trust field.
    pub observed_at: String,
    /// Bounded authority that supplied the trust field.
    pub source: CatalogTrustSource,
}
crate::record_serde::wire_record!(CatalogTrustProvenance {
    observed_at: "observedAt",
    source: "source"
});

/// Discriminator: the authority omitted trust metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogTrustSnapshotAbsent {
    /// Service-computed exposure eligibility. `unknown` is required while Agent Finder returns no explicit eligibility field.
    pub eligibility: CatalogTrustEligibility,
    /// Bounded source and observation time for this snapshot. This is distinct from evidence used by the authority to calculate trust.
    pub provenance: CatalogTrustProvenance,
    /// Schema version of this runtime-owned snapshot envelope.
    pub schema_version: CatalogTrustSnapshotSchemaVersion,
    /// Discriminator: the authority omitted trust metadata.
    #[serde(deserialize_with = "CatalogTrustSnapshotAbsent::deserialize_status")]
    pub status: CatalogTrustSnapshotAbsentStatus,
}

impl CatalogTrustSnapshotAbsent {
    fn deserialize_status<'de, D>(
        deserializer: D,
    ) -> Result<CatalogTrustSnapshotAbsentStatus, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "absent" {
            return Err(serde::de::Error::unknown_variant(&value, &["absent"]));
        }
        <CatalogTrustSnapshotAbsentStatus>::deserialize(serde::de::value::StringDeserializer::<
            D::Error,
        >::new(value))
    }
}

/// A recognised current Agent Finder T1 or T2 trust tier.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogTrustSnapshotCurrent {
    /// Service-computed exposure eligibility. `unknown` is required while Agent Finder returns no explicit eligibility field.
    pub eligibility: CatalogTrustEligibility,
    /// Bounded source and observation time for this snapshot. This is distinct from evidence used by the authority to calculate trust.
    pub provenance: CatalogTrustProvenance,
    /// Schema version of this runtime-owned snapshot envelope.
    pub schema_version: CatalogTrustSnapshotSchemaVersion,
    /// Discriminator: a recognised current trust tier was observed.
    #[serde(deserialize_with = "CatalogTrustSnapshotCurrent::deserialize_status")]
    pub status: CatalogTrustSnapshotCurrentStatus,
    /// Service-computed T1 or T2 trust tier.
    pub tier: CatalogTrustTier,
}

impl CatalogTrustSnapshotCurrent {
    fn deserialize_status<'de, D>(
        deserializer: D,
    ) -> Result<CatalogTrustSnapshotCurrentStatus, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "current" {
            return Err(serde::de::Error::unknown_variant(&value, &["current"]));
        }
        <CatalogTrustSnapshotCurrentStatus>::deserialize(serde::de::value::StringDeserializer::<
            D::Error,
        >::new(value))
    }
}

/// Discriminator: the authority explicitly reported a downgraded assessment.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogTrustSnapshotDowngraded {
    /// Service-computed exposure eligibility. `unknown` is required while Agent Finder returns no explicit eligibility field.
    pub eligibility: CatalogTrustEligibility,
    /// Bounded source and observation time for this snapshot. This is distinct from evidence used by the authority to calculate trust.
    pub provenance: CatalogTrustProvenance,
    /// Schema version of this runtime-owned snapshot envelope.
    pub schema_version: CatalogTrustSnapshotSchemaVersion,
    /// Discriminator: the authority explicitly reported a downgraded assessment.
    #[serde(deserialize_with = "CatalogTrustSnapshotDowngraded::deserialize_status")]
    pub status: CatalogTrustSnapshotDowngradedStatus,
}

impl CatalogTrustSnapshotDowngraded {
    fn deserialize_status<'de, D>(
        deserializer: D,
    ) -> Result<CatalogTrustSnapshotDowngradedStatus, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "downgraded" {
            return Err(serde::de::Error::unknown_variant(&value, &["downgraded"]));
        }
        <CatalogTrustSnapshotDowngradedStatus>::deserialize(serde::de::value::StringDeserializer::<
            D::Error,
        >::new(value))
    }
}

/// Discriminator: the trust field was empty, unbounded, or had the wrong JSON type.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogTrustSnapshotMalformed {
    /// Service-computed exposure eligibility. `unknown` is required while Agent Finder returns no explicit eligibility field.
    pub eligibility: CatalogTrustEligibility,
    /// Bounded source and observation time for this snapshot. This is distinct from evidence used by the authority to calculate trust.
    pub provenance: CatalogTrustProvenance,
    /// Schema version of this runtime-owned snapshot envelope.
    pub schema_version: CatalogTrustSnapshotSchemaVersion,
    /// Discriminator: the trust field was empty, unbounded, or had the wrong JSON type.
    #[serde(deserialize_with = "CatalogTrustSnapshotMalformed::deserialize_status")]
    pub status: CatalogTrustSnapshotMalformedStatus,
}

impl CatalogTrustSnapshotMalformed {
    fn deserialize_status<'de, D>(
        deserializer: D,
    ) -> Result<CatalogTrustSnapshotMalformedStatus, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "malformed" {
            return Err(serde::de::Error::unknown_variant(&value, &["malformed"]));
        }
        <CatalogTrustSnapshotMalformedStatus>::deserialize(serde::de::value::StringDeserializer::<
            D::Error,
        >::new(value))
    }
}

/// Discriminator: the authority explicitly revoked the assessment.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogTrustSnapshotRevoked {
    /// Service-computed exposure eligibility. `unknown` is required while Agent Finder returns no explicit eligibility field.
    pub eligibility: CatalogTrustEligibility,
    /// Bounded source and observation time for this snapshot. This is distinct from evidence used by the authority to calculate trust.
    pub provenance: CatalogTrustProvenance,
    /// Schema version of this runtime-owned snapshot envelope.
    pub schema_version: CatalogTrustSnapshotSchemaVersion,
    /// Discriminator: the authority explicitly revoked the assessment.
    #[serde(deserialize_with = "CatalogTrustSnapshotRevoked::deserialize_status")]
    pub status: CatalogTrustSnapshotRevokedStatus,
}

impl CatalogTrustSnapshotRevoked {
    fn deserialize_status<'de, D>(
        deserializer: D,
    ) -> Result<CatalogTrustSnapshotRevokedStatus, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "revoked" {
            return Err(serde::de::Error::unknown_variant(&value, &["revoked"]));
        }
        <CatalogTrustSnapshotRevokedStatus>::deserialize(serde::de::value::StringDeserializer::<
            D::Error,
        >::new(value))
    }
}

/// Discriminator: the authority explicitly marked the assessment stale.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogTrustSnapshotStale {
    /// Service-computed exposure eligibility. `unknown` is required while Agent Finder returns no explicit eligibility field.
    pub eligibility: CatalogTrustEligibility,
    /// Bounded source and observation time for this snapshot. This is distinct from evidence used by the authority to calculate trust.
    pub provenance: CatalogTrustProvenance,
    /// Schema version of this runtime-owned snapshot envelope.
    pub schema_version: CatalogTrustSnapshotSchemaVersion,
    /// Discriminator: the authority explicitly marked the assessment stale.
    #[serde(deserialize_with = "CatalogTrustSnapshotStale::deserialize_status")]
    pub status: CatalogTrustSnapshotStaleStatus,
}

impl CatalogTrustSnapshotStale {
    fn deserialize_status<'de, D>(
        deserializer: D,
    ) -> Result<CatalogTrustSnapshotStaleStatus, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "stale" {
            return Err(serde::de::Error::unknown_variant(&value, &["stale"]));
        }
        <CatalogTrustSnapshotStaleStatus>::deserialize(serde::de::value::StringDeserializer::<
            D::Error,
        >::new(value))
    }
}

/// Discriminator: the authority supplied a bounded trust value this runtime does not understand.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CatalogTrustSnapshotUnsupported {
    /// Service-computed exposure eligibility. `unknown` is required while Agent Finder returns no explicit eligibility field.
    pub eligibility: CatalogTrustEligibility,
    /// Bounded source and observation time for this snapshot. This is distinct from evidence used by the authority to calculate trust.
    pub provenance: CatalogTrustProvenance,
    /// Schema version of this runtime-owned snapshot envelope.
    pub schema_version: CatalogTrustSnapshotSchemaVersion,
    /// Discriminator: the authority supplied a bounded trust value this runtime does not understand.
    #[serde(deserialize_with = "CatalogTrustSnapshotUnsupported::deserialize_status")]
    pub status: CatalogTrustSnapshotUnsupportedStatus,
}

impl CatalogTrustSnapshotUnsupported {
    fn deserialize_status<'de, D>(
        deserializer: D,
    ) -> Result<CatalogTrustSnapshotUnsupportedStatus, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "unsupported" {
            return Err(serde::de::Error::unknown_variant(&value, &["unsupported"]));
        }
        <CatalogTrustSnapshotUnsupportedStatus>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// Describes the cloud coding-agent task to start and the credential to start it under. Target it either by repository, supplying both `repositoryOwner` and `repositoryName`, or by `owner` alone for an account-level run.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksCreateParams {
    /// Selects which stored credential to resolve when the host holds more than one. Omit it to resolve the default for the supplied `authInfo`.
    pub auth_context_id: Option<String>,
    /// The credential the task is created under, carried opaquely because its shape is the host's own and the runtime only resolves a token and an API endpoint from it.
    pub auth_info: serde_json::Value,
    /// Explicit Mission Control endpoint selected by the host. Omission derives it from the identity.
    pub base_url: Option<String>,
    /// Name of the embedding client, reported to the service so it can attribute the request.
    pub integration_id: String,
    /// Account the task is created for when it is not tied to a repository. Supply it only when `repositoryOwner` and `repositoryName` are omitted.
    pub owner: Option<String>,
    /// Name of the repository the task runs against. Supply it together with `repositoryOwner`.
    pub repository_name: Option<String>,
    /// Account or organization owning the repository the task runs against. Supply it together with `repositoryName`.
    pub repository_owner: Option<String>,
}
crate::record_serde::wire_record!(CloudTasksCreateParams { auth_context_id: "authContextId" if is_none, auth_info: "authInfo", base_url: "baseUrl" if is_none, integration_id: "integrationId", owner: "owner" if is_none, repository_name: "repositoryName" if is_none, repository_owner: "repositoryOwner" if is_none });

/// The outcome of starting a cloud task. A rejected request and a failed one are reported separately: `reason` and `message` describe a request the service declined, while `statusCode` and `errorMessage` describe the HTTP call itself and appear only when one was actually made.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksCreateResult {
    /// Human-readable explanation of a failed HTTP call, when one failed.
    pub error_message: Option<String>,
    /// The error body the service returned, preserved verbatim so a host can surface the service's own message.
    pub error_payload_json: Option<String>,
    /// The explanation to show for a declined request, when the service supplied one. This describes the rejection itself, unlike `errorMessage`, which describes a failed HTTP call.
    pub message: Option<String>,
    /// Whether the task was created.
    pub ok: bool,
    /// Why the request was declined, when it was. Carried as a free-form string rather than a fixed set because the vocabulary is the service's and grows with it; `policy_blocked`, `validation`, `rate_limited` and `error` are the values seen so far, and a caller should treat an unrecognised one as a generic failure rather than rejecting the response.
    pub reason: Option<String>,
    /// The created task record, as returned by the service. Present only when `ok` is true; a caller parses it itself because the runtime does not narrow the service's own shape.
    pub response_json: Option<String>,
    /// HTTP status code of the response, when one was received.
    pub status_code: Option<i32>,
}
crate::record_serde::wire_record!(CloudTasksCreateResult { error_message: "errorMessage" if is_none, error_payload_json: "errorPayloadJson" if is_none, message: "message" if is_none, ok: "ok", reason: "reason" if is_none, response_json: "responseJson" if is_none, status_code: "statusCode" if is_none });

/// Identifies one cloud coding-agent task to delete, and the credential to delete it under.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksDeleteParams {
    /// Selects which stored credential to resolve when the host holds more than one. Omit it to resolve the default for the supplied `authInfo`.
    pub auth_context_id: Option<String>,
    /// The credential the deletion is performed under, carried opaquely because its shape is the host's own and the runtime only resolves a token and an API endpoint from it. Required: the task endpoint is per-account, so there is no unauthenticated form of this call.
    pub auth_info: serde_json::Value,
    /// Explicit Mission Control endpoint selected by the host. Omission derives it from the identity.
    pub base_url: Option<String>,
    /// Name of the embedding client, reported to Mission Control so the service can attribute the request.
    pub integration_id: String,
    /// Identifier of the cloud task to delete.
    pub task_id: String,
}
crate::record_serde::wire_record!(CloudTasksDeleteParams { auth_context_id: "authContextId" if is_none, auth_info: "authInfo", base_url: "baseUrl" if is_none, integration_id: "integrationId", task_id: "taskId" });

/// The HTTP outcome of deleting a cloud task. The deletion returns no body, so a successful call carries nothing beyond `ok`; a failed one is described by the remaining fields rather than raised as an error, so a caller can surface the service's own explanation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksDeleteResult {
    /// Human-readable failure explanation, when the call failed.
    pub error_message: Option<String>,
    /// The error body the service returned, preserved verbatim so a host can surface the service's own message.
    pub error_payload_json: Option<String>,
    /// Whether the service accepted the deletion.
    pub ok: bool,
    /// HTTP status code of the response, when one was received.
    pub status_code: Option<i32>,
}
crate::record_serde::wire_record!(CloudTasksDeleteResult { error_message: "errorMessage" if is_none, error_payload_json: "errorPayloadJson" if is_none, ok: "ok", status_code: "statusCode" if is_none });

/// Identifies one cloud coding-agent task and the credential to read it under.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksGetParams {
    /// Selects which stored credential to resolve when the host holds more than one. Omit it to resolve the default for the supplied `authInfo`.
    pub auth_context_id: Option<String>,
    /// The credential the read is performed under, carried opaquely because its shape is the host's own and the runtime only resolves a token from it. Required: the task endpoint is per-account, so there is no unauthenticated form of this read.
    pub auth_info: serde_json::Value,
    /// Name of the embedding client, reported to the task API so the service can attribute the request.
    pub integration_id: String,
    /// Identifier of the cloud task to read.
    pub task_id: String,
}
crate::record_serde::wire_record!(CloudTasksGetParams { auth_context_id: "authContextId" if is_none, auth_info: "authInfo", integration_id: "integrationId", task_id: "taskId" });

/// The HTTP outcome of reading a cloud task, rather than the task alone. A 404 arrives here as a non-ok result with `statusCode` 404, not as an error, so a caller can tell an absent task apart from a failed call.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksGetResult {
    /// Categorised failure kind, when the call failed.
    pub error_kind: Option<String>,
    /// Human-readable failure explanation, when the call failed.
    pub error_message: Option<String>,
    /// The error body the API returned, preserved verbatim so a host can surface the service's own message.
    pub error_payload_json: Option<String>,
    /// Whether the API returned the task successfully.
    pub ok: bool,
    /// The task record, as returned by the API. Present only when `ok` is true; a caller parses it itself because the runtime does not narrow the API's own shape.
    pub payload_json: Option<String>,
    /// HTTP status code of the response, when one was received.
    pub status_code: Option<i32>,
}
crate::record_serde::wire_record!(CloudTasksGetResult { error_kind: "errorKind" if is_none, error_message: "errorMessage" if is_none, error_payload_json: "errorPayloadJson" if is_none, ok: "ok", payload_json: "payloadJson" if is_none, status_code: "statusCode" if is_none });

/// Identifies one cloud coding-agent session whose logs are to be read, and the credential to read them under.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksGetSessionLogsParams {
    /// Selects which stored credential to resolve when the host holds more than one. Omit it to resolve the default for the supplied `authInfo`.
    pub auth_context_id: Option<String>,
    /// The credential the read is performed under, carried opaquely because its shape is the host's own and the runtime only resolves a token and an API endpoint from it.
    pub auth_info: serde_json::Value,
    /// Identifier of the cloud session whose logs are read.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(CloudTasksGetSessionLogsParams { auth_context_id: "authContextId" if is_none, auth_info: "authInfo", session_id: "sessionId" });

/// The outcome of reading a cloud session's logs. `resolved` distinguishes "this credential has no Copilot API endpoint" from "the request was made and failed"; every other field describes a request that was actually attempted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksGetSessionLogsResult {
    /// Diagnostics the retrieval produced, in order. Worth logging whatever the outcome, because they explain a partial or empty result that is otherwise reported as success.
    pub debug_messages: Option<Vec<String>>,
    /// Categorised failure kind, when the call failed.
    pub error_kind: Option<String>,
    /// Human-readable failure explanation, when the call failed.
    pub error_message: Option<String>,
    /// The error body the API returned, preserved verbatim so a host can surface the service's own message.
    pub error_payload_json: Option<String>,
    /// A retrieval fault that is not an HTTP status, such as a malformed response body.
    pub error_string: Option<String>,
    /// The logs, as returned by the API. Absent when none were returned; a caller that wants a list should treat absence as empty.
    pub logs_json: Option<String>,
    /// Whether the API returned the logs successfully. Meaningful only when `resolved` is true.
    pub ok: bool,
    /// Whether a Copilot API endpoint could be resolved for the credential at all. When false no request was attempted and every other field is absent or empty, which a caller must not report as a transport failure.
    pub resolved: bool,
    /// HTTP status code of the response, when one was received.
    pub status_code: Option<i32>,
}
crate::record_serde::wire_record!(CloudTasksGetSessionLogsResult { debug_messages: "debugMessages" if is_none, error_kind: "errorKind" if is_none, error_message: "errorMessage" if is_none, error_payload_json: "errorPayloadJson" if is_none, error_string: "errorString" if is_none, logs_json: "logsJson" if is_none, ok: "ok", resolved: "resolved", status_code: "statusCode" if is_none });

/// Identifies one cloud coding-agent task whose recorded session is to be replayed, and the credential to read it under.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksGetSessionReplayParams {
    /// Selects which stored credential to resolve when the host holds more than one. Omit it to resolve the default for the supplied `authInfo`.
    pub auth_context_id: Option<String>,
    /// The credential the read is performed under, carried opaquely because its shape is the host's own and the runtime only resolves a token and an API endpoint from it.
    pub auth_info: serde_json::Value,
    /// Explicit Mission Control endpoint selected by the host. Omission derives it from the identity.
    pub base_url: Option<String>,
    /// Name of the embedding client, reported to the service so it can attribute the request.
    pub integration_id: String,
    /// Identifier of the cloud task whose recorded events are replayed.
    pub task_id: String,
}
crate::record_serde::wire_record!(CloudTasksGetSessionReplayParams { auth_context_id: "authContextId" if is_none, auth_info: "authInfo", base_url: "baseUrl" if is_none, integration_id: "integrationId", task_id: "taskId" });

/// A cloud task's recorded events, ordered for replay. Read `preStart` first: it selects which of the two event lists a caller should use, and they differ deliberately.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksGetSessionReplayResult {
    /// Every recorded event in the same replay order, as a JSON array, whether or not the session has started. This is the list to hand to a subsequent ingress so it can tell which events it has already seen, and unlike `replayEventsJson` it is never emptied.
    pub observed_events_json: String,
    /// True when the task has recorded events but the session has not started yet, so only queued work exists. A started session is anchored by either `session.start` or, for a resumed one, `session.resume`.
    pub pre_start: bool,
    /// The events to replay in order to reconstruct the session, as a JSON array. Empty when `preStart` is true, because a session that has not started has nothing to reconstruct and replaying its pending events would invent a history it never had.
    pub replay_events_json: String,
}
crate::record_serde::wire_record!(CloudTasksGetSessionReplayResult {
    observed_events_json: "observedEventsJson",
    pre_start: "preStart",
    replay_events_json: "replayEventsJson"
});

/// Identifies one cloud coding-agent task whose sharing status is to be read, and the credential to read it under.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksGetSharingStatusParams {
    /// Selects which stored credential to resolve when the host holds more than one. Omit it to resolve the default for the supplied `authInfo`.
    pub auth_context_id: Option<String>,
    /// The credential the read is performed under, carried opaquely because its shape is the host's own and the runtime only resolves a token and an API endpoint from it. The token itself is never carried across the door.
    pub auth_info: serde_json::Value,
    /// Explicit Mission Control endpoint selected by the host. Omission derives it from the identity.
    pub base_url: Option<String>,
    /// Name of the embedding client, reported to the service so it can attribute the request.
    pub integration_id: String,
    /// Identifier of the cloud task whose sharing status is read.
    pub task_id: String,
}
crate::record_serde::wire_record!(CloudTasksGetSharingStatusParams { auth_context_id: "authContextId" if is_none, auth_info: "authInfo", base_url: "baseUrl" if is_none, integration_id: "integrationId", task_id: "taskId" });

/// Identifies one cloud coding-agent task to resolve to an openable session, the credential to resolve it under, and the local candidates the caller has already rejected.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksResolveTaskIdParams {
    /// Whether a task carrying no recorded CLI run still counts as a match. Set it when the caller wants the task itself, not only a run that already produced a session.
    pub allow_empty_cli_task: bool,
    /// Selects which stored credential to resolve when the host holds more than one. Omit it to resolve the default for the supplied `authInfo`.
    pub auth_context_id: Option<String>,
    /// The credential the lookup is performed under, carried opaquely because its shape is the host's own and the runtime only resolves a token and an API endpoint from it. Required: the task endpoint is per-account, so there is no unauthenticated form of this call.
    pub auth_info: serde_json::Value,
    /// Overrides the GitHub API base URL the task lookup goes to. Intended for testing against a non-production service; omit it in normal use.
    pub debug_github_api_url: Option<String>,
    /// Whether the caller can open a cloud session. When false, a task that resolves only to a remote run is reported as unresumable instead, because the caller could not act on it.
    pub has_remote_session_manager: bool,
    /// Local session identifiers the caller has already rejected, so resolution skips them and returns the next candidate. Pass the identifiers accumulated across earlier calls, not only the last one.
    pub ignored_local_session_ids: Vec<String>,
    /// Name of the embedding client, reported to Mission Control so the service can attribute the request.
    pub integration_id: String,
    /// Directory holding persisted session state to scan for a local match. Omit it to use the default session home.
    pub sessions_home: Option<String>,
    /// Identifier of the cloud task to resolve.
    pub task_id: String,
}
crate::record_serde::wire_record!(CloudTasksResolveTaskIdParams { allow_empty_cli_task: "allowEmptyCliTask", auth_context_id: "authContextId" if is_none, auth_info: "authInfo", debug_github_api_url: "debugGithubApiUrl" if is_none, has_remote_session_manager: "hasRemoteSessionManager", ignored_local_session_ids: "ignoredLocalSessionIds", integration_id: "integrationId", sessions_home: "sessionsHome" if is_none, task_id: "taskId" });

/// What a caller should open for the task. The four outcomes are distinct and carry different payloads, so read `kind` before any other field.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksResolveTaskIdResult {
    /// Why the cloud lookup produced less than it could have, for logging. It accompanies a successful outcome as well, so it is not a failure signal.
    pub diagnostic: Option<String>,
    /// Machine-readable classification of the failure, when the service supplied one. The vocabulary is the service's and grows with it, so treat an unrecognized value as a generic failure.
    pub error_kind: Option<String>,
    /// Human-readable explanation of a failed service call. Present only when `kind` is `error`.
    pub error_message: Option<String>,
    /// The error body the service returned, preserved verbatim so a host can surface the service's own message.
    pub error_payload_json: Option<String>,
    /// HTTP status code of the failed call, when one was received.
    pub error_status_code: Option<i32>,
    /// Outcome of the resolution: `local-session` when a persisted session matched, `remote-session` when only a cloud run did, `unresumable-task` when the task exists but cannot be continued, `none` when nothing matched, and `error` when the service call failed.
    pub kind: String,
    /// The cloud session record, serialized, describing the run to open. Present only when `kind` is `remote-session`. It is carried as text because its shape is the service's own and the runtime does not narrow it.
    pub metadata_json: Option<String>,
    /// Identifier of the matched local session. Present only when `kind` is `local-session`.
    pub session_id: Option<SessionId>,
}
crate::record_serde::wire_record!(CloudTasksResolveTaskIdResult { diagnostic: "diagnostic" if is_none, error_kind: "errorKind" if is_none, error_message: "errorMessage" if is_none, error_payload_json: "errorPayloadJson" if is_none, error_status_code: "errorStatusCode" if is_none, kind: "kind", metadata_json: "metadataJson" if is_none, session_id: "sessionId" if is_none });

/// Identifies one cloud coding-agent task whose sharing status is to be changed, the status to set, and the credential to change it under.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksSetSharingStatusParams {
    /// Selects which stored credential to resolve when the host holds more than one. Omit it to resolve the default for the supplied `authInfo`.
    pub auth_context_id: Option<String>,
    /// The credential the change is performed under, carried opaquely because its shape is the host's own and the runtime only resolves a token and an API endpoint from it. The token itself is never carried across the door.
    pub auth_info: serde_json::Value,
    /// Explicit Mission Control endpoint selected by the host. Omission derives it from the identity.
    pub base_url: Option<String>,
    /// Name of the embedding client, reported to the service so it can attribute the request.
    pub integration_id: String,
    /// The sharing status to set, sent to the service verbatim. It is a free-form string rather than a closed set because the service owns these values and may add one; narrowing it here would reject a status the service accepts.
    pub sharing_status: String,
    /// Identifier of the cloud task whose sharing status is changed.
    pub task_id: String,
}
crate::record_serde::wire_record!(CloudTasksSetSharingStatusParams { auth_context_id: "authContextId" if is_none, auth_info: "authInfo", base_url: "baseUrl" if is_none, integration_id: "integrationId", sharing_status: "sharingStatus", task_id: "taskId" });

/// The sharing status a cloud task carries after the call. Read `credentialResolved` first: when it is false no request was attempted at all, which is a different outcome from a request the service declined.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksSharingStatusResult {
    /// Whether a credential could be resolved for the supplied `authInfo` at all. When false no request was attempted and every other field is absent, which a caller must not report as a transport failure: the account simply has no usable credential.
    pub credential_resolved: bool,
    /// Whether the service returned the task successfully. Meaningful only when `credentialResolved` is true.
    pub ok: bool,
    /// The task's sharing status, as the service reported it. Absent when the call did not succeed, and also when the service returned a task that carries no sharing status at all. It is a free-form string rather than a closed set because the service owns these values; a caller that only understands some of them should treat an unrecognized one as unknown rather than as an error.
    pub sharing_status: Option<String>,
}
crate::record_serde::wire_record!(CloudTasksSharingStatusResult { credential_resolved: "credentialResolved", ok: "ok", sharing_status: "sharingStatus" if is_none });

/// Slash commands available in the session, after applying any include/exclude filters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandList {
    /// Commands available in this session
    pub commands: Vec<SlashCommandInfo>,
}
crate::record_serde::wire_record!(CommandList {
    commands: "commands"
});

/// The pending slash-command invocation effect to finalize, plus whether the host applied or cancelled it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsFinalizeInvocationEffectRequest {
    /// The slash-command result object that produced the pending effect, echoed back unchanged.
    pub effect: serde_json::Value,
    /// Whether the host applied or cancelled the pending invocation effect.
    pub outcome: CommandsInvocationEffectOutcome,
}
crate::record_serde::wire_record!(CommandsFinalizeInvocationEffectRequest {
    effect: "effect",
    outcome: "outcome"
});

/// Whether finalizing the invocation effect succeeded, and the failure reason when it did not.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsFinalizeInvocationEffectResult {
    /// Failure reason when the invocation effect could not be finalized.
    pub error: Option<String>,
    /// Whether the pending invocation effect was finalized successfully.
    pub success: bool,
}
crate::record_serde::wire_record!(CommandsFinalizeInvocationEffectResult { error: "error" if is_none, success: "success" });

/// Pending command request ID and an optional error if the client handler failed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsHandlePendingCommandRequest {
    /// Error message if the command handler failed
    pub error: Option<String>,
    /// Request ID from the command invocation event
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(CommandsHandlePendingCommandRequest { error: "error" if is_none, request_id: "requestId" });

/// Indicates whether the pending client-handled command was completed successfully.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsHandlePendingCommandResult {
    /// Whether the command was handled successfully
    pub success: bool,
}
crate::record_serde::wire_record!(CommandsHandlePendingCommandResult { success: "success" });

/// Slash command name and optional raw input string to invoke.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsInvokeRequest {
    /// Raw input after the command name
    pub input: Option<String>,
    /// Command name. Leading slashes are stripped and the name is matched case-insensitively.
    pub name: String,
    /// Optional client surface that initiated the invocation
    pub origin: Option<CommandsInvocationOrigin>,
}
crate::record_serde::wire_record!(CommandsInvokeRequest { input: "input" if is_none, name: "name", origin: "origin" if is_none });

/// Optional filters controlling which command sources to include in the listing.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsListRequest {
    /// Include runtime built-in commands
    pub include_builtins: Option<bool>,
    /// Include commands registered by protocol clients, including SDK clients and extensions
    pub include_client_commands: Option<bool>,
    /// Include enabled user-invocable skills and commands
    pub include_skills: Option<bool>,
}
crate::record_serde::wire_record!(CommandsListRequest { include_builtins: "includeBuiltins" if is_none, include_client_commands: "includeClientCommands" if is_none, include_skills: "includeSkills" if is_none });

/// Host report that its execution of a queued command started or stopped waiting for runtime state. The report is observational and never changes command execution.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsReportQueuedCommandWaitRequest {
    /// Stable identity of the account whose managed policy the host awaits.
    pub account_identity: Option<String>,
    /// Request ID from the `command.queued` event the host is executing.
    pub request_id: RequestId,
    /// Runtime state the host waits for.
    pub requirement: QueuedCommandWaitRequirement,
    /// True when the wait starts and false when it ends.
    pub waiting: bool,
}
crate::record_serde::wire_record!(CommandsReportQueuedCommandWaitRequest {
    account_identity: "accountIdentity",
    request_id: "requestId",
    requirement: "requirement",
    waiting: "waiting"
});

/// Whether the runtime recorded the queued-command wait.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsReportQueuedCommandWaitResult {
    /// False when the queued command is no longer pending or the awaited state is not observed.
    pub recorded: bool,
}
crate::record_serde::wire_record!(CommandsReportQueuedCommandWaitResult {
    recorded: "recorded"
});

/// Queued-command request ID and the result indicating whether the host executed it (and whether to stop processing further queued commands).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsRespondToQueuedCommandRequest {
    /// Request ID from the `command.queued` event the host is responding to.
    pub request_id: RequestId,
    /// Result of the queued command execution.
    pub result: serde_json::Value,
}
crate::record_serde::wire_record!(CommandsRespondToQueuedCommandRequest {
    request_id: "requestId",
    result: "result"
});

/// Indicates whether the queued-command response was matched to a pending request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsRespondToQueuedCommandResult {
    /// Whether a pending queued command with the given request ID was found and resolved. False when the request was already resolved, cancelled, or unknown.
    pub success: bool,
}
crate::record_serde::wire_record!(CommandsRespondToQueuedCommandResult { success: "success" });

/// Characters that, when typed in the composer, should trigger a `completions.request`. Empty when the session has no host-driven completions (e.g. local sessions, or a relay host that does not advertise `completionTriggerCharacters`).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CompletionsGetTriggerCharactersResult {
    /// Trigger characters advertised by the host (e.g. `["@", "#"]`). Empty disables host-driven completions for the session.
    pub trigger_characters: Vec<String>,
}
crate::record_serde::wire_record!(CompletionsGetTriggerCharactersResult {
    trigger_characters: "triggerCharacters"
});

/// Request host-driven completions for the current composer input.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CompletionsRequestRequest {
    /// Cursor offset within `text`, in UTF-16 code units.
    pub offset: i64,
    /// The full composed composer input.
    pub text: String,
}
crate::record_serde::wire_record!(CompletionsRequestRequest {
    offset: "offset",
    text: "text"
});

/// Host-driven completion items for the current composer input. Empty when the host returns no items or does not support completions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CompletionsRequestResult {
    /// Completion items in host-ranked order.
    pub items: Vec<SessionCompletionItem>,
}
crate::record_serde::wire_record!(CompletionsRequestResult { items: "items" });

/// Identity of the integrating host, declared once on the `server.connect` handshake so telemetry from this connection is attributed to a single, consistent surface. All fields are optional; omit them to keep the default attribution.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ConnectClientInfo {
    /// Name of the host editor, e.g. `"vscode"`.
    pub editor_name: Option<String>,
    /// Version of the host editor, e.g. `"1.124.2"`. Ignored unless it looks like a version string.
    pub editor_version: Option<String>,
    /// Name of the Copilot extension within the host, e.g. `"copilot-chat"`.
    pub extension_name: Option<String>,
    /// Version of the Copilot extension within the host, e.g. `"0.54.0"`. Ignored unless it looks like a version string.
    pub extension_version: Option<String>,
}
crate::record_serde::wire_record!(ConnectClientInfo { editor_name: "editorName" if is_none, editor_version: "editorVersion" if is_none, extension_name: "extensionName" if is_none, extension_version: "extensionVersion" if is_none });

/// Pins a Connector operation to one host-owned GitHub account through its opaque selection ID. Provider tokens are never accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorAccountRequest {
    /// Opaque account selection ID previously returned by an account discovery API.
    pub account_id: String,
}
crate::record_serde::wire_record!(ConnectorAccountRequest {
    account_id: "accountId"
});

/// Feature detection and hard polling limits for the EXPERIMENTAL session connector API.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorCapabilities {
    /// Connector API contract version.
    pub api_version: i64,
    /// Current session availability. Disabled availability is reported without making a Connector request.
    pub availability: ConnectorAvailability,
    /// Whether connect and reconnect can return an opaque continuation for bounded consent polling.
    pub consent_continuation: bool,
    /// Maximum accepted wall-clock deadline in milliseconds for one continuation call.
    pub max_deadline_ms: i64,
    /// Maximum accepted polling attempts for one continuation call.
    pub max_poll_attempts: i64,
    /// Maximum accepted delay in milliseconds between polling attempts.
    pub max_poll_interval_ms: i64,
    /// Whether callers select a host-owned GitHub account through an opaque selection ID rather than supplying a provider token.
    pub opaque_account_selection: bool,
    /// Whether getAccount is supported. Absence means false.
    pub session_account_selection: Option<bool>,
    /// Whether reconcile accepts forceConnectorName. Absence means false.
    pub targeted_reconcile: Option<bool>,
}
crate::record_serde::wire_record!(ConnectorCapabilities { api_version: "apiVersion", availability: "availability", consent_continuation: "consentContinuation", max_deadline_ms: "maxDeadlineMs", max_poll_attempts: "maxPollAttempts", max_poll_interval_ms: "maxPollIntervalMs", opaque_account_selection: "opaqueAccountSelection", session_account_selection: "sessionAccountSelection" if is_none, targeted_reconcile: "targetedReconcile" if is_none });

/// Selects one Connector and the pinned host-owned account used for its service and MCP authorization.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorConnectRequest {
    /// Opaque account selection ID. It must match the account already pinned to the session, if any.
    pub account_id: String,
    /// Canonical Connector name from the current catalog.
    pub connector_name: String,
}
crate::record_serde::wire_record!(ConnectorConnectRequest {
    account_id: "accountId",
    connector_name: "connectorName"
});

/// The service is connected and the session MCP graph was reconciled.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorConnectResultConnected {
    /// The service is connected and the session MCP graph was reconciled.
    pub kind: ConnectorConnectResultConnectedKind,
    /// Fresh authoritative Connector state after MCP reconciliation.
    pub status: ConnectorStatus,
}
crate::record_serde::wire_record!(ConnectorConnectResultConnected {
    kind: "kind",
    status: "status"
});

/// Host-owned consent is required before bounded continuation can complete.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorConnectResultConsentRequired {
    /// Validated HTTPS consent URL. The runtime does not open it.
    pub consent_url: String,
    /// Opaque ID accepted by continueConnection.
    pub continuation_id: String,
    /// Host-owned consent is required before bounded continuation can complete.
    pub kind: ConnectorConnectResultConsentRequiredKind,
}
crate::record_serde::wire_record!(ConnectorConnectResultConsentRequired {
    consent_url: "consentUrl",
    continuation_id: "continuationId",
    kind: "kind"
});

/// The service is still completing the connection without a consent URL.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorConnectResultPending {
    /// Opaque ID accepted by continueConnection.
    pub continuation_id: String,
    /// The service is still completing the connection without a consent URL.
    pub kind: ConnectorConnectResultPendingKind,
}
crate::record_serde::wire_record!(ConnectorConnectResultPending {
    continuation_id: "continuationId",
    kind: "kind"
});

/// Explicitly bounded continuation of a pending Connector connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorContinueRequest {
    /// Opaque continuation ID returned by connect, reconnect, or an earlier continuation.
    pub continuation_id: String,
    /// Maximum wall-clock duration in milliseconds for this call. Must be between one and the capability limit.
    pub deadline_ms: i32,
    /// Maximum catalog requests made by this call. Must be between one and the capability limit.
    pub max_attempts: i32,
    /// Delay in milliseconds between attempts. Must not exceed the capability limit.
    pub poll_interval_ms: i32,
}
crate::record_serde::wire_record!(ConnectorContinueRequest {
    continuation_id: "continuationId",
    deadline_ms: "deadlineMs",
    max_attempts: "maxAttempts",
    poll_interval_ms: "pollIntervalMs"
});

/// Authoritative result after disconnect and MCP reconciliation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorDisconnectResult {
    /// Whether the service accepted the idempotent disconnect.
    pub disconnected: bool,
    /// Fresh authoritative session state after removing Connector-owned MCP servers.
    pub status: ConnectorStatus,
}
crate::record_serde::wire_record!(ConnectorDisconnectResult {
    disconnected: "disconnected",
    status: "status"
});

/// Account metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorDiscoveryAuthInfo {
    /// Host.
    pub host: String,
    /// Login.
    pub login: String,
    /// Authentication type.
    pub r#type: AuthInfoType,
}
crate::record_serde::wire_record!(ConnectorDiscoveryAuthInfo {
    host: "host",
    login: "login",
    r#type: "type"
});

/// Eligible account.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorDiscoveryAccount {
    /// Opaque account ID.
    pub account_id: String,
    /// Account metadata.
    pub auth_info: ConnectorDiscoveryAuthInfo,
}
crate::record_serde::wire_record!(ConnectorDiscoveryAccount {
    account_id: "accountId",
    auth_info: "authInfo"
});

/// Eligible accounts.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorDiscoveryAccountList {
    /// Eligible accounts.
    pub accounts: Vec<ConnectorDiscoveryAccount>,
    /// Availability.
    pub availability: ConnectorDiscoveryAvailability,
}
crate::record_serde::wire_record!(ConnectorDiscoveryAccountList {
    accounts: "accounts",
    availability: "availability"
});

/// Selected account.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorDiscoveryAccountRequest {
    /// Opaque account ID.
    pub account_id: String,
}
crate::record_serde::wire_record!(ConnectorDiscoveryAccountRequest {
    account_id: "accountId"
});

/// Feature availability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorDiscoveryCapabilities {
    /// API version.
    pub api_version: i64,
    /// Availability.
    pub availability: ConnectorDiscoveryAvailability,
    /// Whether results are cached.
    pub conditional_cache: bool,
    /// Whether accounts are selected by opaque ID.
    pub opaque_account_selection: bool,
}
crate::record_serde::wire_record!(ConnectorDiscoveryCapabilities {
    api_version: "apiVersion",
    availability: "availability",
    conditional_cache: "conditionalCache",
    opaque_account_selection: "opaqueAccountSelection"
});

/// Entry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorDiscoveryCatalogEntry {
    /// Description.
    pub description: Option<String>,
    /// Display name.
    pub display_name: String,
    /// Logo.
    pub logo: Option<String>,
    /// Name.
    pub name: String,
    /// Release tag.
    pub release_tag: Option<String>,
    /// Status.
    pub status: ConnectorCatalogStatus,
    /// Tier.
    pub tier: Option<String>,
}
crate::record_serde::wire_record!(ConnectorDiscoveryCatalogEntry { description: "description" if is_none, display_name: "displayName", logo: "logo" if is_none, name: "name", release_tag: "releaseTag" if is_none, status: "status", tier: "tier" if is_none });

/// Entries for the selected account.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorDiscoveryCatalogResult {
    /// Opaque account ID.
    pub account_id: String,
    /// Entries.
    pub connectors: Vec<ConnectorDiscoveryCatalogEntry>,
    /// Refresh time in Unix epoch milliseconds.
    pub refreshed_at_ms: i64,
    /// Revision.
    pub revision: i64,
}
crate::record_serde::wire_record!(ConnectorDiscoveryCatalogResult {
    account_id: "accountId",
    connectors: "connectors",
    refreshed_at_ms: "refreshedAtMs",
    revision: "revision"
});

/// Requests authoritative Connector-to-MCP reconciliation for the pinned account.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorReconcileRequest {
    /// Opaque account selection ID. It must match the account already pinned to the session, if any.
    pub account_id: String,
    /// Optional Connector name to reinitialize. Requires the targetedReconcile capability.
    pub force_connector_name: Option<String>,
    /// When true, refresh the catalog before reconciling. A disabled Connector API performs no service request.
    pub refresh_catalog: Option<bool>,
}
crate::record_serde::wire_record!(ConnectorReconcileRequest { account_id: "accountId", force_connector_name: "forceConnectorName" if is_none, refresh_catalog: "refreshCatalog" if is_none });

/// Remote session connection parameters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectRemoteSessionParams {
    /// Session ID to connect to.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(ConnectRemoteSessionParams {
    session_id: "sessionId"
});

/// The host application's Microsoft Entra registration, shared by its sessions. This identifies the application requesting tokens, not their target resource.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EntraAuthConfig {
    /// Application (client) ID of the host's Microsoft Entra registration. Must be nonblank. The runtime does not supply a default application.
    pub client_id: String,
    /// Application name reported by OneAuth to Microsoft Auth Telemetry System (MATS). Omit or leave blank to disable MATS telemetry without disabling authentication. OneAuth normalizes the name and may group applications into shared event tables.
    pub mats_app_name: Option<String>,
    /// Native OneAuth application descriptor ID. Omit or leave blank to use clientId. This does not change the OAuth client ID or isolate accounts from other applications.
    pub one_auth_application_id: Option<String>,
}
crate::record_serde::wire_record!(EntraAuthConfig { client_id: "clientId", mats_app_name: "matsAppName" if is_none, one_auth_application_id: "oneAuthApplicationId" if is_none });

/// Connection-level opt-ins for the `server.connect` handshake. Transport authentication is consumed by the native protocol boundary before dispatch.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ConnectRequest {
    /// Identity of the integrating host. Optional; omit it to keep the default attribution.
    #[doc(hidden)]
    pub(crate) client_info: Option<ConnectClientInfo>,
    /// Opt this connection in to GitHub telemetry forwarding for its lifetime. When set, the runtime forwards this host's telemetry across all its sessions, its sessionless events, and explicitly process-wide events over the `gitHubTelemetry.event` notification. Connections intentionally sharing one server receive that server's events; independently embedded runtime hosts do not receive each other's host-owned events. Regular events are also written to the runtime's normal GitHub/CTS path (dual-write); host-only compatibility events are forward-only and intentionally skip that path. Intended for first-party hosts that re-emit the events into their own telemetry stores. Both unrestricted and restricted events are forwarded, each tagged with a `restricted` discriminator; a backstop drops restricted events when restricted telemetry is disabled — using the process-global gate for ordinary events and an explicit session-scoped decision for host-only events.
    pub enable_git_hub_telemetry_forwarding: Option<bool>,
    /// Entra application used by this client's authentication flows. Required to enable built-in Microsoft Entra sign-in and token renewal; other authentication methods remain available.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub entra: Option<EntraAuthConfig>,
    /// Opt this connection in to host-owned Entra identity and token acquisition through entraToken.getToken. Requires entra.clientId and an explicit true acknowledgement in the response. SDKs set this internally when a callback is configured.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub entra_token_provider: Option<bool>,
    /// Task kinds this connection can decode when observing session tasks. Omit to retain agent and shell compatibility.
    pub supported_task_kinds: Option<Vec<TaskKind>>,
    /// Connection token; required when the server was started with COPILOT_CONNECTION_TOKEN
    pub token: Option<String>,
}
crate::record_serde::wire_record!(ConnectRequest { client_info: "clientInfo" if is_none, enable_git_hub_telemetry_forwarding: "enableGitHubTelemetryForwarding" if is_none, entra: "entra" if is_none, entra_token_provider: "entraTokenProvider" if is_none, supported_task_kinds: "supportedTaskKinds" if is_none, token: "token" if is_none });

/// Handshake result reporting the server's protocol version and package version on success.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct ConnectResult {
    /// Acknowledges that this connection's host-owned Entra token provider is enabled. Clients requesting it must reject a missing or false acknowledgement.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This type is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub entra_token_provider: Option<bool>,
    /// Always true on success
    pub ok: bool,
    /// Server protocol version number
    pub protocol_version: i64,
    /// Task kinds the server may return to this connection.
    pub task_kinds: Option<Vec<TaskKind>>,
    /// Server package version
    pub version: String,
}
crate::record_serde::wire_record!(ConnectResult { entra_token_provider: "entraTokenProvider" if is_none, ok: "ok", protocol_version: "protocolVersion", task_kinds: "taskKinds" if is_none, version: "version" });

/// Local file system absolute paths within the session working directory to check against its content-exclusion policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ContentExclusionCheckPathsRequest {
    /// Local file system absolute paths within the session working directory to check. Results are returned in the same order, including duplicates.
    pub paths: Vec<String>,
}
crate::record_serde::wire_record!(ContentExclusionCheckPathsRequest { paths: "paths" });

/// Batch content-exclusion result. Callers must fail closed when policy evaluation is unavailable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ContentExclusionCheckPathsResult {
    /// Whether the session's policy service was available for the complete batch. When false, checks is empty and callers must treat every requested path as excluded.
    pub available: bool,
    /// Per-path decisions in request order. Empty when available is false.
    pub checks: Vec<ContentExclusionPathCheck>,
}
crate::record_serde::wire_record!(ContentExclusionCheckPathsResult {
    available: "available",
    checks: "checks"
});

/// Candidate content-exclusion policies to validate, as the raw JSON text a host received. The text is supplied unparsed so that a syntax error and a schema violation are both reported by the runtime's own validator rather than by the caller's JSON parser.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ContentExclusionValidatePoliciesRequest {
    /// Unparsed JSON text expected to hold an array of content-exclusion policies.
    pub policies_json: String,
}
crate::record_serde::wire_record!(ContentExclusionValidatePoliciesRequest {
    policies_json: "policiesJson"
});

/// The accepted policies, parsed and normalized. A rejected input is reported as an error rather than in this result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ContentExclusionValidatePoliciesResult {
    /// Validated policy objects in input order. Raw JSON preserves every field returned by the runtime's policy parser.
    pub policies: Vec<serde_json::Value>,
}
crate::record_serde::wire_record!(ContentExclusionValidatePoliciesResult {
    policies: "policies"
});

/// JSON-RPC error.data for application code -32004. Carries no credentials or provider error details.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CredentialContextErrorData {
    /// Stable discriminator. Consumers use this value instead of the error message.
    pub code: CredentialContextErrorCode,
}
crate::record_serde::wire_record!(CredentialContextErrorData { code: "code" });

/// Result of one customization reload component.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CustomizationReloadOutcome {
    /// Reason for a skipped component or description of a failure, when available
    pub detail: Option<String>,
    /// Whether the component reloaded, was skipped, or failed
    pub status: CustomizationReloadStatus,
    /// Component whose reload was attempted or skipped
    pub subsystem: CustomizationReloadSubsystem,
}
crate::record_serde::wire_record!(CustomizationReloadOutcome { detail: "detail" if is_none, status: "status", subsystem: "subsystem" });

/// Results of reloading discovered session customizations. Inspect outcomes for reloaded, skipped, or failed subsystems; a rejection may follow partial mutation. Changes to the model-facing prompt and tools apply on the next turn.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CustomizationsReloadResult {
    /// Errors from any component that could not be refreshed
    pub errors: Vec<String>,
    /// Outcome of each component in reload order; a skipped component was not configured or loaded
    pub outcomes: Vec<CustomizationReloadOutcome>,
    /// Warnings from skill discovery
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(CustomizationsReloadResult {
    errors: "errors",
    outcomes: "outcomes",
    warnings: "warnings"
});

#[derive(Debug, Clone, Default)]
pub struct DebugCollectLogsDestinationArchive {
    /// Destination variant discriminator.
    pub kind: DebugCollectLogsDestinationArchiveKind,
    /// When true, create the archive atomically without overwriting an existing file by appending ` (N)` before the extension as needed. Defaults to false.
    pub no_overwrite: Option<bool>,
    /// Absolute or server-relative path for the .tgz archive to create.
    pub output_path: String,
}
crate::record_serde::wire_record!(DebugCollectLogsDestinationArchive { kind: "kind", no_overwrite: "noOverwrite" if is_none, output_path: "outputPath" });

#[derive(Debug, Clone, Default)]
pub struct DebugCollectLogsDestinationDirectory {
    /// Destination variant discriminator.
    pub kind: DebugCollectLogsDestinationDirectoryKind,
    /// Directory where files should be staged. The directory is created if needed.
    pub output_directory: String,
}
crate::record_serde::wire_record!(DebugCollectLogsDestinationDirectory {
    kind: "kind",
    output_directory: "outputDirectory"
});

/// A caller-provided server-local file or directory to include in the debug bundle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DebugCollectLogsEntry {
    /// Relative path to use inside the staged bundle/archive.
    pub bundle_path: String,
    /// Kind of source path to include.
    pub kind: DebugCollectLogsEntryKind,
    /// Server-local source path to read.
    pub path: String,
    /// How text content from this entry should be redacted. Defaults to plain-text. With none, no redaction is applied; the caller must ensure any necessary redaction is performed before this call.
    pub redaction: Option<DebugCollectLogsRedaction>,
    /// When true, collection fails if this entry cannot be read. Defaults to false, which records the entry in `skippedEntries`.
    pub required: Option<bool>,
}
crate::record_serde::wire_record!(DebugCollectLogsEntry { bundle_path: "bundlePath", kind: "kind", path: "path", redaction: "redaction" if is_none, required: "required" if is_none });

/// Built-in session diagnostics to include in the bundle. Omitted fields default to true.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DebugCollectLogsInclude {
    /// Server-local path to the current process log. When set, it is included as `process.log` and its directory is searched for prior logs from the same session.
    pub current_process_log_path: Option<String>,
    /// Include the session event log (`events.jsonl`). Defaults to true.
    pub events: Option<bool>,
    /// Server-local path to the session's events.jsonl file. Internal callers normally omit this and let the runtime derive it from the session.
    pub events_path: Option<String>,
    /// Maximum number of previous process logs to include. Defaults to 5.
    pub previous_process_log_limit: Option<i64>,
    /// Server-local process log directory to search when `currentProcessLogPath` is unavailable, useful for collecting logs for inactive sessions.
    pub process_log_directory: Option<String>,
    /// Include process logs for the session. Defaults to true.
    pub process_logs: Option<bool>,
    /// Include interactive shell logs written under the session's `shell-logs` directory. Defaults to true.
    pub shell_logs: Option<bool>,
}
crate::record_serde::wire_record!(DebugCollectLogsInclude { current_process_log_path: "currentProcessLogPath" if is_none, events: "events" if is_none, events_path: "eventsPath" if is_none, previous_process_log_limit: "previousProcessLogLimit" if is_none, process_log_directory: "processLogDirectory" if is_none, process_logs: "processLogs" if is_none, shell_logs: "shellLogs" if is_none });

/// Options for collecting a session debug bundle with configurable redaction.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DebugCollectLogsRequest {
    /// Caller-provided server-local files or directories to include in addition to the runtime's built-in session diagnostics. This lets host applications add their own diagnostics without changing the API shape.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_entries: Option<Vec<DebugCollectLogsEntry>>,
    /// Where the bundle should be written. Use `archive` to produce a .tgz, or `directory` to stage files for caller-managed upload/post-processing.
    pub destination: DebugCollectLogsDestination,
    /// Which built-in session diagnostics to include. Omitted fields default to true.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include: Option<DebugCollectLogsInclude>,
}

/// Result of collecting a session debug bundle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DebugCollectLogsResult {
    /// Files included in the bundle.
    pub entries: Vec<DebugCollectLogsCollectedEntry>,
    /// Destination kind that was written.
    pub kind: DebugCollectLogsResultKind,
    /// Actual archive path or staging directory path written. This may differ from the requested path when no-overwrite suffixing or fallback-to-temp-directory was needed.
    pub path: String,
    /// Optional files or directories that could not be included.
    pub skipped_entries: Option<Vec<DebugCollectLogsSkippedEntry>>,
}
crate::record_serde::wire_record!(DebugCollectLogsResult { entries: "entries", kind: "kind", path: "path", skipped_entries: "skippedEntries" if is_none });

/// Answer to a boolean question.
#[derive(Debug, Clone, Default)]
pub struct DecisionAnswerBoolean {
    /// Answered question kind discriminator.
    pub kind: DecisionAnswerBooleanKind,
    /// Name of the question this answers.
    pub name: String,
    /// Probability, from 0 to 1, that the answer is yes.
    pub probability: f64,
}
crate::record_serde::wire_record!(DecisionAnswerBoolean {
    kind: "kind",
    name: "name",
    probability: "probability"
});

/// Probability of one choice option.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DecisionOptionProbability {
    /// Probability from 0 to 1.
    pub probability: f64,
    /// Option value.
    pub value: String,
}
crate::record_serde::wire_record!(DecisionOptionProbability {
    probability: "probability",
    value: "value"
});

/// Answer to a choice question.
#[derive(Debug, Clone, Default)]
pub struct DecisionAnswerChoice {
    /// How peaked the distribution is: `(p_max - 1/n) / (1 - 1/n)`. Not a probability of being correct.
    pub confidence: f64,
    /// Answered question kind discriminator.
    pub kind: DecisionAnswerChoiceKind,
    /// Name of the question this answers.
    pub name: String,
    /// Probability of every option, in request order, summing to 1.
    pub probabilities: Vec<DecisionOptionProbability>,
    /// True when another option has exactly the same probability as `value`. Check this, not only `confidence`.
    pub tied: bool,
    /// Value of the most probable option. On a tie, the tied option listed first.
    pub value: String,
    /// The backend's own confidence, which may use a different formula. Diagnostic only.
    pub vendor_confidence: Option<f64>,
}
crate::record_serde::wire_record!(DecisionAnswerChoice { confidence: "confidence", kind: "kind", name: "name", probabilities: "probabilities", tied: "tied", value: "value", vendor_confidence: "vendorConfidence" if is_none });

/// Probability of one score level.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DecisionLevelProbability {
    /// Level label.
    pub label: String,
    /// Probability from 0 to 1.
    pub probability: f64,
}
crate::record_serde::wire_record!(DecisionLevelProbability {
    label: "label",
    probability: "probability"
});

/// Answer to a score question.
#[derive(Debug, Clone, Default)]
pub struct DecisionAnswerScore {
    /// How peaked the distribution is: `(p_max - 1/n) / (1 - 1/n)`. Not a probability of being correct.
    pub confidence: f64,
    /// Answered question kind discriminator.
    pub kind: DecisionAnswerScoreKind,
    /// Label of the most probable level. On a tie, the lower level.
    pub level: String,
    /// Name of the question this answers.
    pub name: String,
    /// Probability of every level, from lowest to highest, summing to 1.
    pub probabilities: Vec<DecisionLevelProbability>,
    /// Expected level position: the sum of each level's zero-based position times its probability.
    pub score: f64,
    /// The backend's own confidence, which may use a different formula. Diagnostic only.
    pub vendor_confidence: Option<f64>,
}
crate::record_serde::wire_record!(DecisionAnswerScore { confidence: "confidence", kind: "kind", level: "level", name: "name", probabilities: "probabilities", score: "score", vendor_confidence: "vendorConfidence" if is_none });

/// The model declined to answer this question. Other answers in the request are unaffected.
#[derive(Debug, Clone, Default)]
pub struct DecisionAnswerRefused {
    /// Answered question kind discriminator.
    pub kind: DecisionAnswerRefusedKind,
    /// Name of the question that was refused.
    pub name: String,
}
crate::record_serde::wire_record!(DecisionAnswerRefused {
    kind: "kind",
    name: "name"
});

/// Plain-text context.
#[derive(Debug, Clone, Default)]
pub struct DecisionContextText {
    /// Context format discriminator.
    pub kind: DecisionContextTextKind,
    /// The text the questions are about.
    pub text: String,
}
crate::record_serde::wire_record!(DecisionContextText {
    kind: "kind",
    text: "text"
});

/// Structured context: a JSON object or an array of objects.
#[derive(Debug, Clone, Default)]
pub struct DecisionContextJson {
    /// Context format discriminator.
    pub kind: DecisionContextJsonKind,
    /// A JSON object, or an array of JSON objects. Other JSON values are rejected with `invalidRequest`.
    pub value: serde_json::Value,
}
crate::record_serde::wire_record!(DecisionContextJson {
    kind: "kind",
    value: "value"
});

/// One rule or limit a request broke.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DecisionViolation {
    /// The value the request had, when numeric.
    pub actual: Option<i64>,
    /// The limit it broke, when numeric.
    pub limit: Option<i64>,
    /// Where in the request, for example `questions[2].options`.
    pub path: String,
    /// Stable rule identifier, for example `minOptions` or `requestBytes`.
    pub rule: String,
}
crate::record_serde::wire_record!(DecisionViolation { actual: "actual" if is_none, limit: "limit" if is_none, path: "path", rule: "rule" });

/// `data` of a JSON-RPC error returned by a `decisions.*` method.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DecisionErrorData {
    /// Error category to branch on.
    pub kind: DecisionErrorKind,
    /// How long to wait before retrying, for `rateLimited`.
    pub retry_after_ms: Option<i64>,
    /// HTTP status the backend returned, when the error came from it.
    pub status_code: Option<i32>,
    /// Every limit or rule the request broke, for `invalidRequest` and `tooLarge`.
    pub violations: Option<Vec<DecisionViolation>>,
}
crate::record_serde::wire_record!(DecisionErrorData { kind: "kind", retry_after_ms: "retryAfterMs" if is_none, status_code: "statusCode" if is_none, violations: "violations" if is_none });

/// One level of a score question's scale.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DecisionLevel {
    /// When this level applies.
    pub description: String,
    /// Short, unique name of the level, such as `Low`.
    pub label: String,
}
crate::record_serde::wire_record!(DecisionLevel {
    description: "description",
    label: "label"
});

/// One candidate answer to a choice question.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DecisionOption {
    /// When this option applies.
    pub description: String,
    /// Identifier returned when the model picks this option. Letters, digits and `._:/@-`, 1-64 characters. The model reads it, so it must not hold anything the caller would not put in the context.
    pub value: String,
}
crate::record_serde::wire_record!(DecisionOption {
    description: "description",
    value: "value"
});

/// A yes/no question, answered with the probability that the answer is yes.
#[derive(Debug, Clone, Default)]
pub struct DecisionQuestionBoolean {
    /// The yes/no question.
    pub instructions: String,
    /// Question kind discriminator.
    pub kind: DecisionQuestionBooleanKind,
    /// Caller-chosen name, unique within the request, that identifies this question's answer. Never sent to the model.
    pub name: String,
    /// Optional description of when the answer is no.
    pub when_false: Option<String>,
    /// Optional description of when the answer is yes.
    pub when_true: Option<String>,
}
crate::record_serde::wire_record!(DecisionQuestionBoolean { instructions: "instructions", kind: "kind", name: "name", when_false: "whenFalse" if is_none, when_true: "whenTrue" if is_none });

/// A pick-one question, answered with one of the supplied options. The model always picks one; ask a separate boolean question when none may fit.
#[derive(Debug, Clone, Default)]
pub struct DecisionQuestionChoice {
    /// The question whose answer is one of `options`.
    pub instructions: String,
    /// Question kind discriminator.
    pub kind: DecisionQuestionChoiceKind,
    /// Caller-chosen name, unique within the request, that identifies this question's answer. Never sent to the model.
    pub name: String,
    /// Candidate answers. Values must be unique identifiers; the model reads both value and description.
    pub options: Vec<DecisionOption>,
}
crate::record_serde::wire_record!(DecisionQuestionChoice {
    instructions: "instructions",
    kind: "kind",
    name: "name",
    options: "options"
});

/// A rating question, answered on the supplied ordered scale.
#[derive(Debug, Clone, Default)]
pub struct DecisionQuestionScore {
    /// The question whose answer is a rating on `levels`.
    pub instructions: String,
    /// Question kind discriminator.
    pub kind: DecisionQuestionScoreKind,
    /// Scale levels from lowest to highest. A level's zero-based position is its numeric value.
    pub levels: Vec<DecisionLevel>,
    /// Caller-chosen name, unique within the request, that identifies this question's answer. Never sent to the model.
    pub name: String,
}
crate::record_serde::wire_record!(DecisionQuestionScore {
    instructions: "instructions",
    kind: "kind",
    levels: "levels",
    name: "name"
});

/// One request to a decision model: shared context plus the questions to answer about it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DecisionsAskRequest {
    /// Shared context every question sees. The runtime never truncates it; an oversized request fails with `tooLarge`.
    pub context: DecisionContext,
    /// GitHub token to ask as. When provided, it is used instead of the current account or `selectionId`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub git_hub_token: Option<String>,
    /// Decision model id from `models.list` `decisionModels`. When omitted, the runtime uses its default decision model for the account.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    /// Static identifier of the calling feature, used only for telemetry and never sent to the model, for example `vscode.chat.route`. Unrecognized values are reported as `other`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub purpose: Option<String>,
    /// Questions to answer about the context. Names must be unique; answers come back in the same order.
    pub questions: Vec<DecisionQuestion>,
    /// Opaque account identifier returned by `account.getAllUsers`. When omitted, the current account is used.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub selection_id: Option<String>,
    /// End-to-end deadline in milliseconds, covering credential resolution, queueing, the model call and parsing. Defaults to 5000; allowed range 250-30000.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeout_ms: Option<i32>,
}

/// Token usage the backend reported for one request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DecisionUsage {
    /// Input tokens served from the backend's prompt cache, when reported.
    pub cached_input_tokens: Option<i64>,
    /// Input tokens written to the backend's prompt cache, when reported.
    pub cache_write_tokens: Option<i64>,
    /// Input tokens, including cached and cache-write tokens.
    pub input_tokens: i64,
    /// Output tokens, including reasoning tokens.
    pub output_tokens: i64,
    /// Reasoning tokens, when reported.
    pub reasoning_tokens: Option<i64>,
}
crate::record_serde::wire_record!(DecisionUsage { cached_input_tokens: "cachedInputTokens" if is_none, cache_write_tokens: "cacheWriteTokens" if is_none, input_tokens: "inputTokens", output_tokens: "outputTokens", reasoning_tokens: "reasoningTokens" if is_none });

/// The decision model's answers.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DecisionsAskResult {
    /// One answer per question, in request order.
    pub answers: Vec<DecisionAnswer>,
    /// Milliseconds from receiving the request to producing the result.
    pub latency_ms: i64,
    /// Decision model id that answered; valid as `model` in a later request.
    pub model: String,
    /// Model name the backend reported, when it differs from `model`. Informational only.
    pub upstream_model: Option<String>,
    /// Token usage, when the backend reports it.
    pub usage: Option<DecisionUsage>,
}
crate::record_serde::wire_record!(DecisionsAskResult { answers: "answers", latency_ms: "latencyMs", model: "model", upstream_model: "upstreamModel" if is_none, usage: "usage" if is_none });

/// Extensions discovered from persisted Copilot home state and their effective loading mode. Launch-scoped additional plugins are not included.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiscoveredExtensions {
    /// Discovered user and enabled installed-plugin extensions from persisted Copilot home state
    pub extensions: Vec<DiscoveredExtension>,
    /// Effective extension loading mode. Defaults to load_and_augment when unset.
    pub mode: DiscoveredExtensionMode,
}
crate::record_serde::wire_record!(DiscoveredExtensions {
    extensions: "extensions",
    mode: "mode"
});

/// Source-qualified extension identifiers to persistently disable for future sessions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiscoveredExtensionsDisableRequest {
    /// Source-qualified user or plugin extension IDs to disable
    pub ids: Vec<String>,
}
crate::record_serde::wire_record!(DiscoveredExtensionsDisableRequest { ids: "ids" });

/// Source-qualified extension identifiers to persistently enable for future sessions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiscoveredExtensionsEnableRequest {
    /// Source-qualified user or plugin extension IDs to enable
    pub ids: Vec<String>,
}
crate::record_serde::wire_record!(DiscoveredExtensionsEnableRequest { ids: "ids" });

/// One server-discovered hook action from user, repository, plugin, or managed-policy configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiscoveredHook {
    /// File URI of the document defining this action on the selected runtime host, not the command or script it executes. Inline hooks point to their containing settings document or plugin manifest. Omitted for sources without a backing file, such as registry policy.
    pub definition_file_uri: Option<String>,
    /// Durable content hash used by hook enablement. Identical actions may intentionally share this key. Omitted when changing the user's disabled-hooks setting cannot change the action's current server-discovered state, including managed-policy hooks, session-start prompt actions, actions suppressed by disable-all settings, and projectless plugin actions that require project-directory expansion.
    pub disable_key: Option<String>,
    /// Whether this action is enabled under the server-side discovery settings. Concrete sessions may differ because they can add session-specific directories, plugins, or trust. False when its disable key is present in the user's disabled-hooks setting or disable-all settings suppress the action.
    pub enabled: bool,
    /// Hook event that invokes this action.
    pub hook_type: HookType,
    /// Deterministic identifier for this server-discovered action row. It remains stable while the project, origin, source, event, action content, and duplicate ordinal are unchanged. This is row identity, not the key persisted in disabledHooks.
    pub id: String,
    /// Configuration tier that contributed this hook action.
    pub origin: HookOrigin,
    /// Opaque identity of the plugin owning this action. Matches plugins.list().plugins[].id and MCP effectiveSource.plugin.id on the same runtime host. Compare the complete value without parsing it. Omitted for non-plugin hooks.
    pub plugin_id: Option<String>,
    /// Input project path for which this server-side action was resolved. Set on every row returned for project-scoped discovery, including repeated user and policy actions.
    pub project_path: Option<String>,
    /// Human-readable source label, such as a hook file path, settings source, or plugin name.
    pub source: Option<String>,
}
crate::record_serde::wire_record!(DiscoveredHook { definition_file_uri: "definitionFileUri" if is_none, disable_key: "disableKey" if is_none, enabled: "enabled", hook_type: "hookType", id: "id", origin: "origin", plugin_id: "pluginId" if is_none, project_path: "projectPath" if is_none, source: "source" if is_none });

/// Models offered for agent conversations by one provider instance. Adapters exclude known-incompatible models, but retain candidates with unknown capabilities. Listing does not guarantee compatibility.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct DiscoveredModelList {
    /// Provider-native models in provider order.
    pub models: Vec<DiscoveredModel>,
    /// Typed operation outcome.
    pub outcome: ModelProviderOperationOutcome,
}
crate::record_serde::wire_record!(DiscoveredModelList {
    models: "models",
    outcome: "outcome"
});

/// Slash-prefixed command string to enqueue for FIFO processing.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EnqueueCommandParams {
    /// Slash-prefixed command string to enqueue, e.g. '/compact' or '/model gpt-4'. Queued FIFO with any in-flight items; if the session is idle, processing kicks off immediately.
    pub command: String,
    /// Optional user-facing text for the queue row. The command string is shown when omitted.
    pub display_text: Option<String>,
}
crate::record_serde::wire_record!(EnqueueCommandParams { command: "command", display_text: "displayText" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EntraTokenAcquireRequestIdentity {
    /// Host account to revalidate. When supplied, the response must identify this exact account.
    pub account_id: Option<String>,
    /// Effective application client ID. Treat it as opaque.
    pub client_id: String,
    /// Entra identity or resource-token acquisition discriminator.
    pub kind: EntraTokenAcquireRequestIdentityKind,
    /// Interaction policy. Only a fresh explicit sign-in may select an unpinned account.
    pub reason: EntraTokenAcquireReason,
}
crate::record_serde::wire_record!(EntraTokenAcquireRequestIdentity { account_id: "accountId" if is_none, client_id: "clientId", kind: "kind", reason: "reason" });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EntraTokenAcquireRequestToken {
    /// Pinned opaque host account ID. The response must identify this exact account.
    pub account_id: String,
    /// Target resource audience, when known.
    pub audience: Option<String>,
    /// Effective application client ID, including any resource-specific override.
    pub client_id: String,
    /// Entra identity or resource-token acquisition discriminator.
    pub kind: EntraTokenAcquireRequestTokenKind,
    /// Interaction policy. Renewal must remain silent.
    pub reason: EntraTokenAcquireReason,
    /// Scopes required for this resource token.
    pub scopes: Vec<String>,
    /// Bypass cached access tokens without changing account selection or interaction policy.
    pub skip_cached_access_token: bool,
    /// Target Entra tenant, when different from the host's default.
    pub tenant: Option<String>,
}
crate::record_serde::wire_record!(EntraTokenAcquireRequestToken { account_id: "accountId", audience: "audience" if is_none, client_id: "clientId", kind: "kind", reason: "reason", scopes: "scopes", skip_cached_access_token: "skipCachedAccessToken", tenant: "tenant" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EntraTokenAcquireResultIdentity {
    /// Stable nonblank opaque account ID owned by the host.
    pub account_id: String,
    /// Entra acquisition response variant discriminator.
    pub kind: EntraTokenAcquireResultIdentityKind,
    /// Optional display name; not an account identifier or credential.
    pub username: Option<String>,
}
crate::record_serde::wire_record!(EntraTokenAcquireResultIdentity { account_id: "accountId", kind: "kind", username: "username" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EntraTokenAcquireResultToken {
    /// Nonblank resource access token. The host owns its caching and renewal.
    pub access_token: String,
    /// Exact host account ID supplied in the request.
    pub account_id: String,
    /// Entra acquisition response variant discriminator.
    pub kind: EntraTokenAcquireResultTokenKind,
}
crate::record_serde::wire_record!(EntraTokenAcquireResultToken {
    access_token: "accessToken",
    account_id: "accountId",
    kind: "kind"
});

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EntraTokenAcquireResultUnavailable {
    /// Entra acquisition response variant discriminator.
    pub kind: EntraTokenAcquireResultUnavailableKind,
}
crate::record_serde::wire_record!(EntraTokenAcquireResultUnavailable { kind: "kind" });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EntraTokenAcquireResultCancelled {
    /// Entra acquisition response variant discriminator.
    pub kind: EntraTokenAcquireResultCancelledKind,
}
crate::record_serde::wire_record!(EntraTokenAcquireResultCancelled { kind: "kind" });

/// Parameters for one connection-owned Entra acquisition callback.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EntraTokenGetTokenRequest {
    /// Typed identity or resource-token acquisition request.
    pub request: EntraTokenAcquireRequest,
}

/// Hosting capabilities and session capacity advertised by an environment.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EnvironmentCapabilities {
    /// Advertised Agent Host Protocol version.
    pub ahp_version: Option<String>,
    /// Current session count, when advertised.
    pub current_sessions: Option<i64>,
    /// Feature identifiers advertised by the environment.
    pub features: Vec<String>,
    /// Maximum session capacity, when advertised.
    pub max_sessions: Option<i64>,
}
crate::record_serde::wire_record!(EnvironmentCapabilities { ahp_version: "ahpVersion" if is_none, current_sessions: "currentSessions" if is_none, features: "features", max_sessions: "maxSessions" if is_none });

/// Identify a user-managed Mission Control environment to delete.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EnvironmentsDeleteRequest {
    /// User-managed environment to delete. GitHub-managed environments cannot be deleted.
    pub environment_id: String,
}
crate::record_serde::wire_record!(EnvironmentsDeleteRequest {
    environment_id: "environmentId"
});

/// Acknowledgement that the requested environment was deleted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EnvironmentsDeleteResult {}

/// Identify a Mission Control environment to retrieve.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EnvironmentsGetRequest {
    /// Identifier assigned by Mission Control.
    pub environment_id: String,
}
crate::record_serde::wire_record!(EnvironmentsGetRequest {
    environment_id: "environmentId"
});

/// Safe discovery information. Host-side relay bootstrap credentials are never included.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct GitHubEnvironment {
    /// Hosting capabilities advertised by the environment.
    pub capabilities: Option<EnvironmentCapabilities>,
    /// Identifier assigned by Mission Control.
    pub id: String,
    /// Compute kind reported by Mission Control.
    pub kind: EnvironmentKind,
    /// Discovery labels attached to the environment.
    pub labels: Option<HashMap<String, String>>,
    /// Timestamp of the last heartbeat received by Mission Control.
    pub last_heartbeat_at: Option<String>,
    /// Human-readable environment name.
    pub name: String,
    /// Organization identifier, when the environment belongs to an organization.
    pub org_id: Option<String>,
    /// Identifier of the environment owner.
    pub owner_id: Option<String>,
    /// Owner category reported by Mission Control.
    pub owner_type: Option<String>,
    /// Open-ended operational status vocabulary.
    pub status: String,
}
crate::record_serde::wire_record!(GitHubEnvironment { capabilities: "capabilities" if is_none, id: "id", kind: "kind", labels: "labels" if is_none, last_heartbeat_at: "lastHeartbeatAt" if is_none, name: "name", org_id: "orgId" if is_none, owner_id: "ownerId" if is_none, owner_type: "ownerType" if is_none, status: "status" });

/// Safe discovery information for the requested environment.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EnvironmentsGetResult {
    /// The requested environment without relay bootstrap credentials.
    pub environment: GitHubEnvironment,
}
crate::record_serde::wire_record!(EnvironmentsGetResult {
    environment: "environment"
});

/// Optional discovery filters supported by GitHub Mission Control.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EnvironmentsListRequest {
    /// Restrict discovery to this compute kind.
    pub kind: Option<EnvironmentKind>,
    /// Operational status, such as online, offline, degraded, waking, or draining.
    pub status: Option<String>,
}
crate::record_serde::wire_record!(EnvironmentsListRequest { kind: "kind" if is_none, status: "status" if is_none });

/// Environments visible to the authenticated caller and matching the supplied filters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EnvironmentsListResult {
    /// Safe discovery records without relay bootstrap credentials.
    pub environments: Vec<GitHubEnvironment>,
    /// Server-requested minimum delay before automatically listing again, in milliseconds.
    pub retry_after_ms: Option<i64>,
}
crate::record_serde::wire_record!(EnvironmentsListResult { environments: "environments", retry_after_ms: "retryAfterMs" if is_none });

/// Cursor, batch size, and optional long-poll/filter parameters for reading session events.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EventLogReadRequest {
    /// Optional non-empty list of subagent identifiers. When provided, only events owned by one of these agents are returned; ownership recognizes the event envelope's agentId plus legacy data.agentId and data.parentToolCallId markers. This filter takes precedence over agentScope.
    pub agent_ids: Option<Vec<String>>,
    /// Agent-scope filter: 'primary' returns only main-agent events plus events whose type starts with 'subagent.' (matching the typed-subscription default behavior); 'all' returns events from all agents (matching wildcard-subscription behavior). Default is 'all' to preserve wildcard semantics for catch-up callers.
    pub agent_scope: Option<EventsAgentScope>,
    /// Opaque cursor returned by a previous read in the same view. Omit on the first call to start from the beginning of the selected view.
    pub cursor: Option<String>,
    /// Direction to page through the session's persisted event history. 'forward' (default) pages from the cursor toward newer events (or from the start of history when no cursor is given). 'backward' enables tail-first reads: with no cursor it returns the NEWEST `max` events, and the returned cursor pages toward OLDER events on subsequent backward reads. Events within a returned batch are always in chronological (oldest-to-newest) order, even for a backward read. Backward reads cover PERSISTED history only; ephemeral events are never returned by a backward read. `direction` selects the INITIAL read only: the returned cursor is self-describing, so a continuation read pages in the cursor's own direction regardless of the `direction` passed alongside it — a forward cursor always pages forward and a backward cursor always pages backward. Pass the direction that matches the cursor to avoid confusion.
    pub direction: Option<EventsReadDirection>,
    /// When false, skip ephemeral events entirely and return only durable (persisted) events. History-backfill callers that discard ephemerals anyway should set this so the read is bounded by the durable log length instead of racing the ephemeral ring on a busy session. Defaults to true (ephemerals are interleaved with durable events in creation order). Ignored by backward reads, which always cover persisted history only.
    pub include_ephemeral: Option<bool>,
    /// Maximum number of events to return in this batch (1–1000, default 200).
    pub max: Option<i64>,
    /// Either '*' to receive all event types, or a non-empty list of event types to receive
    pub types: Option<serde_json::Value>,
    /// Event stream to read. 'history' (default) returns original events and lazily follows predecessor journals, excluding checkpoint copies. 'replay' reads only the active replay head, including checkpoint state, without opening predecessor journals. Use replay for current state and fast transcript previews; use history to reconstruct the original visible transcript. Cursors must remain within the view that produced them.
    pub view: Option<EventsReadView>,
    /// Milliseconds to wait for new events when the cursor is at the tail of history. 0 (default) returns immediately even if no events are available. Capped at 30000ms. Ephemeral events that arrive during the wait are delivered in this batch but are NOT replayable on a subsequent read (use a non-zero waitMs in your next call to capture future ephemerals as they happen). This applies to forward reads only: a backward read always returns immediately and ignores `waitMs`, because backward paging covers persisted history only while new events append at the tail (the opposite end from a backward page), so no blocking or ephemeral delivery can occur.
    pub wait_ms: Option<i32>,
}
crate::record_serde::wire_record!(EventLogReadRequest { agent_ids: "agentIds" if is_none, agent_scope: "agentScope" if is_none, cursor: "cursor" if is_none, direction: "direction" if is_none, include_ephemeral: "includeEphemeral" if is_none, max: "max" if is_none, types: "types" if is_none, view: "view" if is_none, wait_ms: "waitMs" if is_none });

/// Indicates whether the operation succeeded.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EventLogReleaseInterestResult {
    /// Whether the operation succeeded
    pub success: bool,
}
crate::record_serde::wire_record!(EventLogReleaseInterestResult { success: "success" });

/// Snapshot of the current tail cursor without returning any events. Use this when a consumer wants to subscribe to live events going forward without first paginating through the entire persisted history (which would happen if `read` were called without a cursor on a long-lived session).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EventLogTailResult {
    /// Opaque cursor pointing at the current tail of the session's persisted-events history. Pass back to `read` to receive only events that arrive AFTER this snapshot. When the session has no events, this returns the same sentinel as an unset cursor (i.e. equivalent to omitting the cursor on a first read).
    pub cursor: String,
}
crate::record_serde::wire_record!(EventLogTailResult { cursor: "cursor" });

/// Batch of session events returned by a read, with cursor and continuation metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct EventsReadResult {
    /// Opaque cursor for the next read. Pass back unchanged in the next read.cursor to continue from where this read left off. Always present, even when no events were returned. For a backward read this cursor pages toward OLDER events; keep passing `direction: backward` with it (the cursor is also self-describing, so backward paging continues correctly).
    pub cursor: String,
    /// Cursor status: 'ok' means the cursor was applied successfully. For session.eventLog.read, 'expired' means the cursor referred to an event that no longer exists in active history and the read fell back to a boundary of the remaining history: the beginning for a forward read or the newest window for a backward read. That fallback may overlap already rendered events, so active-session consumers should reset, rebase, or deduplicate before continuing. sessions.readPersistedEvents has stricter snapshot semantics: 'expired' returns an empty terminal page and never switches to a replacement journal generation. Other persisted-read I/O failures are RPC errors with diagnostics, not cursor expiry.
    pub cursor_status: EventsCursorStatus,
    /// Session events for this batch, merged into a single stream in creation order: durable (persisted) events and ephemeral events interleave exactly as they were emitted. Set `includeEphemeral: false` to receive only durable events. Ephemeral events are never replayable once pruned from the in-memory ring, so a consumer that needs them should keep reading with a non-zero `waitMs`. For a backward (tail-first) read, the returned window contains persisted events only, still in chronological (oldest-to-newest) append order.
    pub events: Vec<SessionEvent>,
    /// True when more events are available in the read's direction. For a backward read, true means older persisted events remain before the returned window. A persisted-event page may contain fewer than `max` events because of its byte budget while still reporting hasMore true; continue according to this flag rather than the event count.
    pub has_more: bool,
}
crate::record_serde::wire_record!(EventsReadResult {
    cursor: "cursor",
    cursor_status: "cursorStatus",
    events: "events",
    has_more: "hasMore"
});

/// Slash command name and argument string to execute synchronously.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExecuteCommandParams {
    /// Argument string to pass to the command (empty string if none).
    pub args: String,
    /// Name of the slash command to invoke (without the leading '/').
    pub command_name: String,
}
crate::record_serde::wire_record!(ExecuteCommandParams {
    args: "args",
    command_name: "commandName"
});

/// Error message produced while executing the command, if any.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExecuteCommandResult {
    /// Error message produced while executing the command, if any. Omitted when the handler succeeded.
    pub error: Option<String>,
}
crate::record_serde::wire_record!(ExecuteCommandResult { error: "error" if is_none });

/// Expanded external tool result payload
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExternalToolTextResultForLlm {
    /// Base64-encoded binary results returned to the model
    pub binary_results_for_llm: Option<Vec<ExternalToolTextResultForLlmBinaryResultsForLlm>>,
    /// Structured content blocks from the tool
    pub contents: Option<Vec<serde_json::Value>>,
    /// Optional error message for failed executions
    pub error: Option<String>,
    /// Execution outcome classification. Optional for back-compat; normalized to 'success' (or 'failure' when error is present) when missing or unrecognized.
    pub result_type: Option<String>,
    /// Detailed log content for timeline display
    pub session_log: Option<String>,
    /// When true, bypass spill-to-file processing for this result. False or omitted uses the session's normal large-output policy.
    pub skip_large_output_processing: Option<bool>,
    /// Text result returned to the model
    pub text_result_for_llm: String,
    /// Tool references returned by a tool-search override: names of deferred tools to surface to the model. When set, the tool result is materialized as `tool_reference` content blocks (rather than plain text) so the model knows which deferred tools are now available.
    pub tool_references: Option<Vec<String>>,
    /// Optional tool-specific telemetry
    pub tool_telemetry: Option<HashMap<String, serde_json::Value>>,
}
crate::record_serde::wire_record!(ExternalToolTextResultForLlm { binary_results_for_llm: "binaryResultsForLlm" if is_none, contents: "contents" if is_none, error: "error" if is_none, result_type: "resultType" if is_none, session_log: "sessionLog" if is_none, skip_large_output_processing: "skipLargeOutputProcessing" if is_none, text_result_for_llm: "textResultForLlm", tool_references: "toolReferences" if is_none, tool_telemetry: "toolTelemetry" if is_none });

/// Audio content block with base64-encoded data
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExternalToolTextResultForLlmContentAudio {
    /// Base64-encoded audio data
    pub data: String,
    /// MIME type of the audio (e.g., audio/wav, audio/mpeg)
    pub mime_type: String,
    /// Content block type discriminator
    pub r#type: ExternalToolTextResultForLlmContentAudioType,
}
crate::record_serde::wire_record!(ExternalToolTextResultForLlmContentAudio {
    data: "data",
    mime_type: "mimeType",
    r#type: "type"
});

/// Image content block with base64-encoded data
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExternalToolTextResultForLlmContentImage {
    /// Base64-encoded image data
    pub data: String,
    /// MIME type of the image (e.g., image/png, image/jpeg)
    pub mime_type: String,
    /// Content block type discriminator
    pub r#type: ExternalToolTextResultForLlmContentImageType,
}
crate::record_serde::wire_record!(ExternalToolTextResultForLlmContentImage {
    data: "data",
    mime_type: "mimeType",
    r#type: "type"
});

/// Embedded resource content block with inline text or binary data
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExternalToolTextResultForLlmContentResource {
    /// The embedded resource contents, either text or base64-encoded binary
    pub resource: serde_json::Value,
    /// Content block type discriminator
    pub r#type: ExternalToolTextResultForLlmContentResourceType,
}
crate::record_serde::wire_record!(ExternalToolTextResultForLlmContentResource {
    resource: "resource",
    r#type: "type"
});

/// Icon image for a resource
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExternalToolTextResultForLlmContentResourceLinkIcon {
    /// MIME type of the icon image
    pub mime_type: Option<String>,
    /// Available icon sizes (e.g., ['16x16', '32x32'])
    pub sizes: Option<Vec<String>>,
    /// URL or path to the icon image
    pub src: String,
    /// Theme variant this icon is intended for
    pub theme: Option<ExternalToolTextResultForLlmContentResourceLinkIconTheme>,
}
crate::record_serde::wire_record!(ExternalToolTextResultForLlmContentResourceLinkIcon { mime_type: "mimeType" if is_none, sizes: "sizes" if is_none, src: "src", theme: "theme" if is_none });

/// Resource link content block referencing an external resource
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExternalToolTextResultForLlmContentResourceLink {
    /// Human-readable description of the resource
    pub description: Option<String>,
    /// Icons associated with this resource
    pub icons: Option<Vec<ExternalToolTextResultForLlmContentResourceLinkIcon>>,
    /// MIME type of the resource content
    pub mime_type: Option<String>,
    /// Resource name identifier
    pub name: String,
    /// Size of the resource in bytes
    pub size: Option<i64>,
    /// Human-readable display title for the resource
    pub title: Option<String>,
    /// Content block type discriminator
    pub r#type: ExternalToolTextResultForLlmContentResourceLinkType,
    /// URI identifying the resource
    pub uri: String,
}
crate::record_serde::wire_record!(ExternalToolTextResultForLlmContentResourceLink { description: "description" if is_none, icons: "icons" if is_none, mime_type: "mimeType" if is_none, name: "name", size: "size" if is_none, title: "title" if is_none, r#type: "type", uri: "uri" });

/// Shell command exit metadata with optional output preview
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExternalToolTextResultForLlmContentShellExit {
    /// Working directory where the shell command was executed
    pub cwd: Option<String>,
    /// Exit code from the completed shell command
    pub exit_code: i64,
    /// Path reported in the shell session's filesystem namespace when shell output exceeded the configured large-output threshold.
    pub output_file_path: Option<String>,
    /// Output associated with this shell command, if available. May be partial, truncated, or a preview; not guaranteed to be full output.
    pub output_preview: Option<String>,
    /// Whether outputPreview is known to be incomplete or truncated
    pub output_truncated: Option<bool>,
    /// Shell id, as assigned by Copilot runtime
    pub shell_id: String,
    /// Content block type discriminator
    pub r#type: ExternalToolTextResultForLlmContentShellExitType,
}
crate::record_serde::wire_record!(ExternalToolTextResultForLlmContentShellExit { cwd: "cwd" if is_none, exit_code: "exitCode", output_file_path: "outputFilePath" if is_none, output_preview: "outputPreview" if is_none, output_truncated: "outputTruncated" if is_none, shell_id: "shellId", r#type: "type" });

/// Terminal/shell output content block with optional exit code and working directory
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExternalToolTextResultForLlmContentTerminal {
    /// Working directory where the command was executed
    pub cwd: Option<String>,
    /// Process exit code, if the command has completed
    pub exit_code: Option<i64>,
    /// Terminal/shell output text
    pub text: String,
    /// Content block type discriminator
    pub r#type: ExternalToolTextResultForLlmContentTerminalType,
}
crate::record_serde::wire_record!(ExternalToolTextResultForLlmContentTerminal { cwd: "cwd" if is_none, exit_code: "exitCode" if is_none, text: "text", r#type: "type" });

/// Plain text content block
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExternalToolTextResultForLlmContentText {
    /// The text content
    pub text: String,
    /// Content block type discriminator
    pub r#type: ExternalToolTextResultForLlmContentTextType,
}
crate::record_serde::wire_record!(ExternalToolTextResultForLlmContentText {
    text: "text",
    r#type: "type"
});

/// Acknowledges a local cancellation request, not remote erasure.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FeedbackUploadCancelResult {
    /// Whether cancellation was requested before the operation became terminal.
    pub cancel_requested: bool,
}
crate::record_serde::wire_record!(FeedbackUploadCancelResult {
    cancel_requested: "cancelRequested"
});

/// Identifies the upload owned by this connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FeedbackUploadOperationRequest {
    /// Opaque operation identifier returned by start; never grants ownership itself.
    pub operation_id: String,
}
crate::record_serde::wire_record!(FeedbackUploadOperationRequest {
    operation_id: "operationId"
});

/// Terminal outcome. The saved local file is retained on every outcome.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FeedbackUploadOutcome {
    /// Fixed safe error text, excluding raw service responses and credentials.
    pub error_message: Option<String>,
    /// HTTP status associated with a failure when available.
    pub http_status: Option<i32>,
    /// Acceptance, failure, or local cancellation.
    #[doc(hidden)]
    pub(crate) status: FeedbackUploadStatus,
    /// Total compressed archive size, or zero when unavailable.
    pub total_bytes: i64,
    /// Acknowledged archive bytes.
    pub uploaded_bytes: i64,
}
crate::record_serde::wire_record!(FeedbackUploadOutcome { error_message: "errorMessage" if is_none, http_status: "httpStatus" if is_none, status: "status", total_bytes: "totalBytes", uploaded_bytes: "uploadedBytes" });

/// Latest local progress snapshot for the saved archive.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FeedbackUploadProgress {
    /// Current upload phase.
    #[doc(hidden)]
    pub(crate) phase: FeedbackUploadPhase,
    /// True when no further upload work remains.
    pub terminal: bool,
    /// Compressed archive byte count once the file has been hashed.
    pub total_bytes: Option<i64>,
    /// Bytes acknowledged by the service, excluding repeated part retries.
    pub uploaded_bytes: i64,
}
crate::record_serde::wire_record!(FeedbackUploadProgress { phase: "phase", terminal: "terminal", total_bytes: "totalBytes" if is_none, uploaded_bytes: "uploadedBytes" });

/// Read local progress, optionally waiting for a changed revision or terminal outcome. Does not poll the service.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FeedbackUploadReadRequest {
    /// When supplied, wait until this revision changes or the operation settles.
    pub after_revision: Option<i64>,
    /// Opaque operation identifier returned by start.
    pub operation_id: String,
}
crate::record_serde::wire_record!(FeedbackUploadReadRequest { after_revision: "afterRevision" if is_none, operation_id: "operationId" });

/// Connection-owned upload state retained until host shutdown.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FeedbackUploadSnapshot {
    /// Opaque identifier scoped to the launch-authorized owner.
    pub operation_id: String,
    /// Present exactly when the operation is terminal.
    #[doc(hidden)]
    pub(crate) outcome: Option<FeedbackUploadOutcome>,
    /// Current local upload progress.
    #[doc(hidden)]
    pub(crate) progress: FeedbackUploadProgress,
    /// Monotonically increasing revision of the local snapshot.
    pub revision: i64,
}
crate::record_serde::wire_record!(FeedbackUploadSnapshot { operation_id: "operationId", outcome: "outcome" if is_none, progress: "progress", revision: "revision" });

/// Fresh consent to upload the one archive and account authorized at host launch.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FeedbackUploadStartRequest {
    /// True only after the user has reviewed and affirmatively accepted the upload disclosure.
    pub affirmative_consent: bool,
}
crate::record_serde::wire_record!(FeedbackUploadStartRequest {
    affirmative_consent: "affirmativeConsent"
});

/// Result of disposing a fetch handle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchCancelResult {
    /// True if this connection owned a live handle. False for unknown, disposed, or foreign handles.
    pub canceled: bool,
}
crate::record_serde::wire_record!(FetchCancelResult {
    canceled: "canceled"
});

/// The connection-owned HTTP request handle allocated by fetch.create.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchCreateResult {
    /// Opaque handle. Other connections cannot start, read, or cancel it.
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(FetchCreateResult {
    request_id: "requestId"
});

/// JSON-RPC error.data for file creation, writes, completion, or incomplete-file cleanup failures (-32603).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchFileErrorData {
    /// Always fileSystem; these errors do not describe an HTTP transport failure.
    pub kind: FetchFileErrorKind,
    /// The failed filesystem operation.
    pub operation: FetchFileOperation,
    /// Platform error number, when supplied by the operating system.
    pub os_error_code: Option<i64>,
}
crate::record_serde::wire_record!(FetchFileErrorData { kind: "kind", operation: "operation", os_error_code: "osErrorCode" if is_none });

/// One HTTP header entry. Repeated names retain their value order.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchHeader {
    /// HTTP header name.
    pub name: String,
    /// HTTP header value.
    pub value: String,
}
crate::record_serde::wire_record!(FetchHeader {
    name: "name",
    value: "value"
});

/// Per-request proxy environment. An empty object disables ambient proxies.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchProxyEnv {
    /// Proxy for HTTP targets.
    pub http_proxy: Option<String>,
    /// Proxy for HTTPS targets.
    pub https_proxy: Option<String>,
    /// Comma-separated proxy bypass patterns.
    pub no_proxy: Option<String>,
}
crate::record_serde::wire_record!(FetchProxyEnv { http_proxy: "httpProxy" if is_none, https_proxy: "httpsProxy" if is_none, no_proxy: "noProxy" if is_none });

/// One candidate route, in the order the fetch engine tries them.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchProxyRoute {
    /// Proxy URL for this candidate, without user information; the runtime removes any proxy credentials. Absent means a direct connection.
    pub proxy_url: Option<String>,
}
crate::record_serde::wire_record!(FetchProxyRoute { proxy_url: "proxyUrl" if is_none });

/// A response byte range, or EOF. EOF and transport failures dispose the handle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchReadResult {
    /// Response bytes encoded as standard base64. Empty at EOF.
    pub body: String,
    /// True at EOF. Do not read the handle again after EOF.
    pub done: bool,
    /// At EOF, whether TLS did not attest to the end of a close-delimited event stream.
    pub ended_at_unverified_eof: bool,
}
crate::record_serde::wire_record!(FetchReadResult {
    body: "body",
    done: "done",
    ended_at_unverified_eof: "endedAtUnverifiedEof"
});

/// A fetch handle owned by the SDK connection that created it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchRequestId {
    /// Opaque handle. Other connections cannot start, read, or cancel it.
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(FetchRequestId {
    request_id: "requestId"
});

/// A target URL whose proxy route the shared fetch engine should report.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchResolveProxyRequest {
    /// Absolute http or https target URL.
    pub url: String,
}
crate::record_serde::wire_record!(FetchResolveProxyRequest { url: "url" });

/// The proxy routes the shared fetch engine would use for the target, without network work to the target.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchResolveProxyResult {
    /// Candidate routes in fallback order. Never empty.
    pub routes: Vec<FetchProxyRoute>,
    /// Where the decision came from: environment, bypass, client, none, os_static, pac, wpad, or unavailable. Routing provenance only; it does not show that the caller can authenticate to the proxy.
    pub source: String,
}
crate::record_serde::wire_record!(FetchResolveProxyResult {
    routes: "routes",
    source: "source"
});

/// An HTTP request through the shared network transport.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchSendParams {
    /// Permit loopback DNS pins. Defaults to false. This is not a general URL admission policy.
    pub allow_localhost: Option<bool>,
    /// Request bytes encoded as standard base64. Omission sends no body.
    pub body: Option<String>,
    /// Maximum decoded response bytes. Only text mode defaults to 1048576; file and stream modes have no default limit.
    /// Explicit zero permits only an empty body. Exceeding the limit fails without truncation.
    pub body_limit_bytes: Option<i64>,
    /// An existing connection-owned credential capability. Each same-origin hop resolves fresh credentials.
    /// Conflicting headers fail. Cross-origin redirects drop credentials but retain the original authority.
    /// HTTPS downgrades fail. Omission adds no service credentials.
    pub credential_context_id: Option<String>,
    /// Destination on the runtime host, required only for file mode. Must not already exist.
    /// The caller owns completed files. Failed or canceled requests remove their incomplete files.
    pub destination_path: Option<String>,
    /// Disable idle connection reuse. Defaults to false.
    pub disable_connection_reuse: Option<bool>,
    /// Header entries in input order. Repeated names are not combined.
    pub headers: Option<Vec<FetchHeader>>,
    /// HTTP method. Defaults to GET.
    pub method: Option<String>,
    /// DNS address pins. Internal pins bypass proxies; public pins yield to configured proxies.
    pub pinned_addresses: Option<Vec<String>>,
    /// Proxy overrides. Omission reads the process proxy environment per request.
    pub proxy_env: Option<FetchProxyEnv>,
    /// Force proxy Negotiate authentication when supported. Defaults to false; automatic proxy negotiation remains enabled.
    pub proxy_negotiate_auth: Option<bool>,
    /// Optional proxy Kerberos service principal override.
    pub proxy_negotiate_spn: Option<String>,
    /// Read timeout in milliseconds. Defaults to 700000. Explicit zero is preserved.
    pub read_timeout_ms: Option<i64>,
    /// Redirect policy. Defaults to follow.
    pub redirect: Option<FetchRedirect>,
    /// Connection-owned handle returned by fetch.create. Each handle can start only once.
    pub request_id: RequestId,
    /// Response consumption mode. Stream returns headers immediately; text and file consume the complete body.
    pub response_mode: FetchResponseMode,
    /// Total request timeout in milliseconds, including body consumption. Omission sets no total deadline.
    pub timeout_ms: Option<i64>,
    /// Absolute target URL.
    pub url: String,
}
crate::record_serde::wire_record!(FetchSendParams { allow_localhost: "allowLocalhost" if is_none, body: "body" if is_none, body_limit_bytes: "bodyLimitBytes" if is_none, credential_context_id: "credentialContextId" if is_none, destination_path: "destinationPath" if is_none, disable_connection_reuse: "disableConnectionReuse" if is_none, headers: "headers" if is_none, method: "method" if is_none, pinned_addresses: "pinnedAddresses" if is_none, proxy_env: "proxyEnv" if is_none, proxy_negotiate_auth: "proxyNegotiateAuth" if is_none, proxy_negotiate_spn: "proxyNegotiateSpn" if is_none, read_timeout_ms: "readTimeoutMs" if is_none, redirect: "redirect" if is_none, request_id: "requestId", response_mode: "responseMode", timeout_ms: "timeoutMs" if is_none, url: "url" });

/// Headers are available; fetch.read consumes the body and fetch.cancel discards it.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FetchSendResultStream {
    /// Headers including repeated names in value order. Distinct-name order is transport-dependent.
    pub headers: Vec<FetchHeader>,
    /// Selected HTTP response representation.
    #[serde(
        deserialize_with = "FetchSendResultStream::deserialize_response_mode",
        serialize_with = "FetchSendResultStream::serialize_response_mode"
    )]
    pub response_mode: FetchResponseMode,
    /// HTTP status code, including non-success statuses.
    pub status: i32,
    /// HTTP reason phrase.
    pub status_text: String,
    /// Final URL after redirects.
    pub url: String,
}

impl Default for FetchSendResultStream {
    fn default() -> Self {
        Self {
            headers: Default::default(),
            response_mode: FetchResponseMode::Stream,
            status: Default::default(),
            status_text: Default::default(),
            url: Default::default(),
        }
    }
}

impl FetchSendResultStream {
    fn deserialize_response_mode<'de, D>(deserializer: D) -> Result<FetchResponseMode, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "stream" {
            return Err(serde::de::Error::unknown_variant(&value, &["stream"]));
        }
        <FetchResponseMode>::deserialize(serde::de::value::StringDeserializer::<D::Error>::new(
            value,
        ))
    }

    fn serialize_response_mode<S>(
        value: &FetchResponseMode,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if *value != FetchResponseMode::Stream {
            return Err(serde::ser::Error::custom(
                "invalid responseMode for FetchSendResultStream",
            ));
        }
        serializer.serialize_str("stream")
    }
}

/// Complete response text with replacement for malformed UTF-8 and the initial UTF-8 BOM omitted.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FetchSendResultText {
    /// Complete text decoded as WHATWG Response.text(), independent of the Content-Type charset.
    pub body_text: String,
    /// Whether TLS did not attest to the end of a close-delimited event stream.
    pub ended_at_unverified_eof: bool,
    /// Headers including repeated names in value order. Distinct-name order is transport-dependent.
    pub headers: Vec<FetchHeader>,
    /// Selected HTTP response representation.
    #[serde(
        deserialize_with = "FetchSendResultText::deserialize_response_mode",
        serialize_with = "FetchSendResultText::serialize_response_mode"
    )]
    pub response_mode: FetchResponseMode,
    /// HTTP status code, including non-success statuses.
    pub status: i32,
    /// HTTP reason phrase.
    pub status_text: String,
    /// Final URL after redirects.
    pub url: String,
}

impl Default for FetchSendResultText {
    fn default() -> Self {
        Self {
            body_text: Default::default(),
            ended_at_unverified_eof: Default::default(),
            headers: Default::default(),
            response_mode: FetchResponseMode::Text,
            status: Default::default(),
            status_text: Default::default(),
            url: Default::default(),
        }
    }
}

impl FetchSendResultText {
    fn deserialize_response_mode<'de, D>(deserializer: D) -> Result<FetchResponseMode, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "text" {
            return Err(serde::de::Error::unknown_variant(&value, &["text"]));
        }
        <FetchResponseMode>::deserialize(serde::de::value::StringDeserializer::<D::Error>::new(
            value,
        ))
    }

    fn serialize_response_mode<S>(
        value: &FetchResponseMode,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if *value != FetchResponseMode::Text {
            return Err(serde::ser::Error::custom(
                "invalid responseMode for FetchSendResultText",
            ));
        }
        serializer.serialize_str("text")
    }
}

/// Completed caller-owned file. Cancellation and disconnect do not remove it.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FetchSendResultFile {
    /// Whether TLS did not attest to the end of a close-delimited event stream.
    pub ended_at_unverified_eof: bool,
    /// Headers including repeated names in value order. Distinct-name order is transport-dependent.
    pub headers: Vec<FetchHeader>,
    /// Caller-specified path on the runtime host. Remote clients do not receive the file bytes.
    pub path: String,
    /// Selected HTTP response representation.
    #[serde(
        deserialize_with = "FetchSendResultFile::deserialize_response_mode",
        serialize_with = "FetchSendResultFile::serialize_response_mode"
    )]
    pub response_mode: FetchResponseMode,
    /// Number of decoded response bytes written.
    pub size_bytes: i64,
    /// HTTP status code, including non-success statuses.
    pub status: i32,
    /// HTTP reason phrase.
    pub status_text: String,
    /// Final URL after redirects.
    pub url: String,
}

impl Default for FetchSendResultFile {
    fn default() -> Self {
        Self {
            ended_at_unverified_eof: Default::default(),
            headers: Default::default(),
            path: Default::default(),
            response_mode: FetchResponseMode::File,
            size_bytes: Default::default(),
            status: Default::default(),
            status_text: Default::default(),
            url: Default::default(),
        }
    }
}

impl FetchSendResultFile {
    fn deserialize_response_mode<'de, D>(deserializer: D) -> Result<FetchResponseMode, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "file" {
            return Err(serde::de::Error::unknown_variant(&value, &["file"]));
        }
        <FetchResponseMode>::deserialize(serde::de::value::StringDeserializer::<D::Error>::new(
            value,
        ))
    }

    fn serialize_response_mode<S>(
        value: &FetchResponseMode,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if *value != FetchResponseMode::File {
            return Err(serde::ser::Error::custom(
                "invalid responseMode for FetchSendResultFile",
            ));
        }
        serializer.serialize_str("file")
    }
}

/// JSON-RPC error.data for fetch.send/read transport failures (-32603). The transport-code token remains unchanged.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FetchTransportErrorData {
    /// The engine transport code, when available. An unclassified HTTP client failure uses "unknown".
    pub code: Option<String>,
    /// The shared engine's failure category.
    pub kind: FetchTransportErrorKind,
    /// The engine's network-error classification. This does not select a retry policy.
    pub network_error: bool,
    /// The engine's permanent-request-failure classification. This does not select a retry policy.
    pub permanent_request_failure: bool,
}
crate::record_serde::wire_record!(FetchTransportErrorData { code: "code" if is_none, kind: "kind", network_error: "networkError", permanent_request_failure: "permanentRequestFailure" });

/// Folder path to add to trusted folders.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FolderTrustAddParams {
    /// Folder path to mark as trusted
    pub path: String,
}
crate::record_serde::wire_record!(FolderTrustAddParams { path: "path" });

/// Folder path to check for trust.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FolderTrustCheckParams {
    /// Folder path to check
    pub path: String,
}
crate::record_serde::wire_record!(FolderTrustCheckParams { path: "path" });

/// Folder trust check result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct FolderTrustCheckResult {
    /// Whether the folder is trusted
    pub trusted: bool,
}
crate::record_serde::wire_record!(FolderTrustCheckResult { trusted: "trusted" });

/// A folder to check against a persisted trusted-folders list, and the configuration directory holding that list.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FolderTrustIsTrustedParams {
    /// Configuration directory to read the trusted-folders list from. Omit it to read the default location; supply it when the host has redirected its configuration, or the check answers from a list the host is not using.
    pub config_dir: Option<String>,
    /// Folder to check. A folder counts as trusted when it is itself listed, when an ancestor is listed, or when its git root is listed.
    pub path: String,
}
crate::record_serde::wire_record!(FolderTrustIsTrustedParams { config_dir: "configDir" if is_none, path: "path" });

/// Repository and branch that own a canonical Forge proposal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ForgeProposalScope {
    /// Branch name, or the existing unknown sentinel.
    pub branch_name: String,
    /// Working tree root, in the existing persisted path format.
    pub git_root_path: String,
    /// Repository name, absent when the checkout has no known remote.
    pub repo_name: Option<String>,
    /// Repository owner, absent when the checkout has no known remote.
    pub repo_owner: Option<String>,
}
crate::record_serde::wire_record!(ForgeProposalScope { branch_name: "branch_name", git_root_path: "git_root_path", repo_name: "repo_name" if is_none, repo_owner: "repo_owner" if is_none });

/// Fails stale generations in one canonical proposal scope.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ForgeProposalFailStaleRequest {
    /// Repository and branch to inspect.
    pub scope: ForgeProposalScope,
    /// Generation age in milliseconds. Defaults to fifteen minutes.
    pub timeout_ms: Option<f64>,
}
crate::record_serde::wire_record!(ForgeProposalFailStaleRequest { scope: "scope", timeout_ms: "timeoutMs" if is_none });

/// Number of generations that the canonical store marked failed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ForgeProposalFailStaleResult {
    /// Number of changed proposal rows.
    pub count: i64,
}
crate::record_serde::wire_record!(ForgeProposalFailStaleResult { count: "count" });

/// Reconciles a generated artifact with the configured canonical store.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ForgeProposalReconcileRequest {
    /// Content-addressed artifact identity.
    pub fingerprint: String,
    /// Ordered restoration metadata.
    pub manifest: Vec<ForgeProposalManifestEntry>,
    /// Existing generation identifier, when generation started earlier.
    pub proposal_id: Option<String>,
    /// Repository and branch that own the proposal.
    pub scope: ForgeProposalScope,
    /// File-change counts.
    pub summary: ForgeProposalSummary,
    /// Whether a new generation may start when no existing generation completes.
    pub trigger_mode: ForgeProposalTriggerMode,
    /// Original workspace capture for an on-demand generation.
    pub workspace_before_by_path: Option<HashMap<String, String>>,
}
crate::record_serde::wire_record!(ForgeProposalReconcileRequest { fingerprint: "fingerprint", manifest: "manifest", proposal_id: "proposalId" if is_none, scope: "scope", summary: "summary", trigger_mode: "triggerMode", workspace_before_by_path: "workspaceBeforeByPath" if is_none });

/// Existing ready proposal or completed generation; null for an ineligible automatic generation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ForgeProposalReconcileResult {
    /// Canonical proposal selected or completed by reconciliation.
    pub proposal: Option<ForgeProposalRecord>,
}
crate::record_serde::wire_record!(ForgeProposalReconcileResult {
    proposal: "proposal"
});

/// Proposal identifier whose before-generation capture is requested.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ForgeProposalWorkspaceBeforeRequest {
    /// Canonical proposal identifier.
    pub id: String,
}
crate::record_serde::wire_record!(ForgeProposalWorkspaceBeforeRequest { id: "id" });

/// Stored before-generation capture, or null when no capture exists.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ForgeProposalWorkspaceBeforeResult {
    /// Original path-to-content mapping; an empty map remains distinct from no capture.
    pub contents: Option<HashMap<String, String>>,
}
crate::record_serde::wire_record!(ForgeProposalWorkspaceBeforeResult {
    contents: "contents"
});

/// Selects the configuration directory whose machine-wide state to read.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GlobalStateLoadForConfigDirRequest {
    /// Copilot configuration directory to read the state document from, taking precedence over the server's own `COPILOT_HOME` and default home. Omit it, or pass an empty string, to read the directory the server resolved for itself.
    pub config_dir: Option<String>,
}
crate::record_serde::wire_record!(GlobalStateLoadForConfigDirRequest { config_dir: "configDir" if is_none });

/// An account the host has signed in to, identified by the server it lives on and the login it uses there. The same person can appear more than once when they use both github.com and an Enterprise server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct LoggedInUser {
    /// Source account this account was derived from, when one was recorded.
    pub derived_from: Option<String>,
    /// Host the account belongs to, such as `github.com` or an Enterprise server.
    pub host: String,
    /// Account kind, when the host recorded one. Consumers must tolerate new strings.
    pub kind: Option<String>,
    /// Account login on that host.
    pub login: String,
}
crate::record_serde::wire_record!(LoggedInUser { derived_from: "derivedFrom" if is_none, host: "host", kind: "kind" if is_none, login: "login" });

/// The host's machine-wide state. Every field is optional because a fresh install has recorded nothing yet, so a reader must treat an absent field as `not yet`, never as a negative answer. Stored credentials are deliberately absent from this shape.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GlobalStateLoadResult {
    /// Whether the app tip has been shown.
    pub app_tip_shown: Option<bool>,
    /// Terminals the user has already been asked to set up, so the host does not ask twice.
    pub asked_setup_terminals: Option<Vec<String>>,
    /// When the Auto-feedback hint was last shown, as an ISO 8601 timestamp. It enforces the once-per-day cap for non-staff users across restarts.
    pub auto_feedback_last_prompted_at: Option<String>,
    /// When the host first ran on this machine.
    pub first_launch_at: Option<String>,
    /// Plugins installed on this machine.
    pub installed_plugins: Option<Vec<InstalledPlugin>>,
    /// Account used for the most recent sign-in.
    #[doc(hidden)]
    pub(crate) last_logged_in_user: Option<LoggedInUser>,
    /// Every account the host has signed in to on this machine.
    #[doc(hidden)]
    pub(crate) logged_in_users: Option<Vec<LoggedInUser>>,
    /// Whether the one-off cleanup of stored reasoning summaries has run.
    pub reasoning_summaries_cleanup_done: Option<bool>,
    /// Models the user selected recently, most recent first.
    pub recent_model_ids: Option<Vec<String>>,
    /// Whether the user declined to trust the sandbox credential proxy CA.
    pub sandbox_credential_proxy_ca_declined: Option<bool>,
    /// Whether the sandbox onboarding has been shown.
    pub sandbox_onboarding_shown: Option<bool>,
    /// Whether the user is a GitHub or Microsoft staff member, which unlocks internal-only behavior.
    pub staff: Option<bool>,
    /// Whether the user was recognized as GitHub staff.
    pub staff_github: Option<bool>,
    /// When the staff-only log level migration last ran.
    pub staff_log_level_migration_at: Option<String>,
    /// Whether the user was recognized as Microsoft staff.
    pub staff_microsoft: Option<bool>,
    /// When the staff-only model reset last ran.
    pub staff_model_reset_at: Option<String>,
    /// When the staff-only update channel migration last ran.
    pub staff_update_channel_migration_at: Option<String>,
    /// Folders where the user declined the init prompt, so it stays hidden there.
    pub suppress_init_folders: Option<Vec<String>>,
    /// Folders the user has marked as trusted.
    pub trusted_folders: Option<Vec<String>>,
}
crate::record_serde::wire_record!(GlobalStateLoadResult { app_tip_shown: "appTipShown" if is_none, asked_setup_terminals: "askedSetupTerminals" if is_none, auto_feedback_last_prompted_at: "autoFeedbackLastPromptedAt" if is_none, first_launch_at: "firstLaunchAt" if is_none, installed_plugins: "installedPlugins" if is_none, last_logged_in_user: "lastLoggedInUser" if is_none, logged_in_users: "loggedInUsers" if is_none, reasoning_summaries_cleanup_done: "reasoningSummariesCleanupDone" if is_none, recent_model_ids: "recentModelIds" if is_none, sandbox_credential_proxy_ca_declined: "sandboxCredentialProxyCaDeclined" if is_none, sandbox_onboarding_shown: "sandboxOnboardingShown" if is_none, staff: "staff" if is_none, staff_github: "staffGithub" if is_none, staff_log_level_migration_at: "staffLogLevelMigrationAt" if is_none, staff_microsoft: "staffMicrosoft" if is_none, staff_model_reset_at: "staffModelResetAt" if is_none, staff_update_channel_migration_at: "staffUpdateChannelMigrationAt" if is_none, suppress_init_folders: "suppressInitFolders" if is_none, trusted_folders: "trustedFolders" if is_none });

/// A single top-level key to record in the host's machine-wide state. The write replaces only that key and leaves the rest of the document untouched, so two writers recording different one-off flags do not overwrite each other. The stored credential keys cannot be written through this method.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GlobalStateWriteKeyRequest {
    /// Copilot configuration directory to write the state document in, taking precedence over the server's own `COPILOT_HOME` and default home. Omit it, or pass an empty string, to write the directory the server resolved for itself. Mirrors `globalState.loadForConfigDir`, so a caller can read and write the same directory.
    pub config_dir: Option<String>,
    /// Top-level key to write, named as it appears in the result of `globalState.load`. It must be one of the writable keys that `globalState.writeKey` lists.
    pub key: String,
    /// Value to store for the key. Omit it, or pass null, to remove the key instead.
    pub value: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(GlobalStateWriteKeyRequest { config_dir: "configDir" if is_none, key: "key", value: "value" if is_none });

/// Pending external tool call request ID, with the tool result or an error describing why it failed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HandlePendingToolCallRequest {
    /// Error message if the tool call failed
    pub error: Option<String>,
    /// Request ID of the pending tool call
    pub request_id: RequestId,
    /// Tool call result (string or expanded result object)
    pub result: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(HandlePendingToolCallRequest { error: "error" if is_none, request_id: "requestId", result: "result" if is_none });

/// Indicates whether the external tool call result was handled successfully.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HandlePendingToolCallResult {
    /// Whether the tool call result was handled successfully
    pub success: bool,
}
crate::record_serde::wire_record!(HandlePendingToolCallResult { success: "success" });

/// Indicates whether an in-progress manual compaction was aborted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryAbortManualCompactionResult {
    /// Whether an in-progress manual compaction was aborted. False when no manual compaction was running, when its abort controller was already aborted, or when the session is remote.
    pub aborted: bool,
}
crate::record_serde::wire_record!(HistoryAbortManualCompactionResult { aborted: "aborted" });

/// Indicates whether an in-progress background compaction was cancelled.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryCancelBackgroundCompactionResult {
    /// Whether an in-progress background compaction was cancelled. False when no compaction was running, when the session is remote, or when the underlying processor was unavailable.
    pub cancelled: bool,
}
crate::record_serde::wire_record!(HistoryCancelBackgroundCompactionResult {
    cancelled: "cancelled"
});

/// Parameters for clearing the conversation and seeding the window that replaces it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryClearContextRequest {
    /// First user message of the fresh context window. Required: a cleared window holding only system and developer messages is not a conversation a model can answer, so every clear seeds the window it creates. Delivered by the enclosing turn driver once the agentic loop exits, which is why the call must be made from inside a tool handler.
    pub prompt: String,
}
crate::record_serde::wire_record!(HistoryClearContextRequest { prompt: "prompt" });

/// What a successful clear removed. A clear that could not be applied rejects instead of reporting a count.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryClearContextResult {
    /// Number of non-system, non-developer messages that were removed from the conversation. Zero only when the window already held no conversation.
    pub messages_cleared: i64,
}
crate::record_serde::wire_record!(HistoryClearContextResult {
    messages_cleared: "messagesCleared"
});

/// Optional compaction parameters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryCompactRequest {
    /// Optional user-provided instructions to focus the compaction summary
    pub custom_instructions: Option<String>,
    /// Context window token limit this compaction is targeting, recorded as the `tokenLimit` on the persisted `session.compaction_start` / `session.compaction_complete` events. Set it when the compaction targets a window other than the compacting model's own, e.g. switching to a model with a smaller context window: the compaction still runs on the current model, so the limit that motivated it would otherwise be lost. When absent, the events record the compacting model's own resolved limit. Attribution metadata only - it does not change how much the compaction removes.
    pub token_limit: Option<i64>,
    /// What initiated this compaction request, recorded as the `trigger` on the persisted `session.compaction_start` / `session.compaction_complete` events. When absent, the compaction is persisted without trigger attribution (initiator unknown).
    pub trigger: Option<HistoryCompactRequestTrigger>,
}
crate::record_serde::wire_record!(HistoryCompactRequest { custom_instructions: "customInstructions" if is_none, token_limit: "tokenLimit" if is_none, trigger: "trigger" if is_none });

/// Compaction outcome with the number of tokens and messages removed, summary text, and the resulting context window breakdown.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryCompactResult {
    /// Post-compaction context window usage breakdown
    pub context_window: Option<HistoryCompactContextWindow>,
    /// Number of messages removed during compaction
    pub messages_removed: i64,
    /// Whether compaction completed successfully
    pub success: bool,
    /// Summary text produced by compaction. Omitted when compaction did not produce a summary (e.g. failure path).
    pub summary_content: Option<String>,
    /// Number of tokens freed by compaction
    pub tokens_removed: i64,
}
crate::record_serde::wire_record!(HistoryCompactResult { context_window: "contextWindow" if is_none, messages_removed: "messagesRemoved", success: "success", summary_content: "summaryContent" if is_none, tokens_removed: "tokensRemoved" });

/// Rewind points and file-change-tracking availability for the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryListRewindPointsResult {
    /// Whether this session captured file changes from its first turn.
    pub file_change_tracking_enabled: bool,
    /// Root user turns in chronological order. Empty when `unavailableReason` is set.
    pub points: Vec<HistoryRewindPoint>,
    /// Why the listed points could not be produced, when applicable; the points list is empty whenever it is set. `unsupported-remote-session` is permanent for the session and comes with `fileChangeTrackingEnabled: false`. `session-busy` is transient and only ever reported by a session that *is* tracking (`fileChangeTrackingEnabled: true`), because the file-change captures cannot be read while work that may still mutate them is in flight; the same request succeeds once the session settles, so a client that wants points should retry rather than treat it as a failure. It is never `file-change-tracking-disabled`: an untracked local session still lists conversation-only points and reports that through `fileChangeTrackingEnabled: false`.
    pub unavailable_reason: Option<HistoryRewindUnavailableReason>,
}
crate::record_serde::wire_record!(HistoryListRewindPointsResult { file_change_tracking_enabled: "fileChangeTrackingEnabled", points: "points", unavailable_reason: "unavailableReason" if is_none });

/// Event boundary to preview for conversation-and-files rewind.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryPreviewRewindRequest {
    /// ID of the user.message event that begins the discarded suffix.
    pub event_id: String,
}
crate::record_serde::wire_record!(HistoryPreviewRewindRequest {
    event_id: "eventId"
});

/// Files and aggregate changes for a prospective rewind.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryPreviewRewindResult {
    /// Whether file restore is available for this session. This is authoritative: switch on it and read `reason` only when it is false.
    pub available: bool,
    /// Number of unique files in the preview.
    pub file_count: i64,
    /// Files ordered by path.
    pub files: Vec<HistoryRewindFilePreview>,
    /// Why file restore is unavailable, when applicable. Populated only when `available` is false and never set when `available` is true.
    pub reason: Option<HistoryRewindUnavailableReason>,
}
crate::record_serde::wire_record!(HistoryPreviewRewindResult { available: "available", file_count: "fileCount", files: "files", reason: "reason" if is_none });

/// Boundary and mode for rewinding session history.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryRewindRequest {
    /// ID of the user.message event that begins the discarded suffix.
    pub event_id: String,
    /// Whether to rewind only conversation history or also restore captured files.
    pub mode: HistoryRewindMode,
}
crate::record_serde::wire_record!(HistoryRewindRequest {
    event_id: "eventId",
    mode: "mode"
});

/// Structured outcome of a rewind request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryRewindResult {
    /// Failure detail. Set only for the failure and partial-failure outcomes (`files-rolled-back`, `rollback-incomplete`, `truncation-failed`, `checkpoint-cleanup-failed`, `snapshot-prune-failed`); omitted for `success` and for the unavailable outcomes (`session-busy`, `file-change-tracking-disabled`, `unsupported-remote-session`).
    pub error: Option<String>,
    /// Number of persisted events removed by conversation truncation. Present only when truncation succeeded (outcomes `success`, `checkpoint-cleanup-failed`, and `snapshot-prune-failed`); omitted for every unavailable outcome (`session-busy`, `file-change-tracking-disabled`, `unsupported-remote-session`) and for `truncation-failed`, `files-rolled-back`, and `rollback-incomplete`.
    pub events_removed: Option<i64>,
    /// Overall rewind outcome. This discriminates the result: it governs which of the remaining fields are populated, so consumers must switch on it before reading `eventsRemoved`, `restoredFiles`, `skippedFiles`, or `error`. See each field for the outcomes that populate it.
    pub outcome: HistoryRewindOutcome,
    /// Absolute paths restored to their captured preimages. Always empty for conversation-only rewinds and for the unavailable outcomes (`session-busy`, `file-change-tracking-disabled`, `unsupported-remote-session`); only conversation-and-files outcomes that reached the file-restore stage populate it.
    pub restored_files: Vec<String>,
    /// Captured files intentionally left unchanged. Always empty for conversation-only rewinds and for the unavailable outcomes (`session-busy`, `file-change-tracking-disabled`, `unsupported-remote-session`); only conversation-and-files outcomes that reached the file-restore stage populate it.
    pub skipped_files: Vec<HistorySkippedFileRestore>,
}
crate::record_serde::wire_record!(HistoryRewindResult { error: "error" if is_none, events_removed: "eventsRemoved" if is_none, outcome: "outcome", restored_files: "restoredFiles", skipped_files: "skippedFiles" });

/// Compaction settings for this session. Each call replaces the previous settings: an omitted field returns to its default. Settings live in memory until the session ends; re-apply them after resuming a session in a new process.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistorySetCompactionConfigRequest {
    /// Model that summarizes this session's conversation when it compacts,
    /// instead of the session's own model. A Copilot session can name another
    /// Copilot model it may use. A session on a custom provider can name
    /// another model that provider serves: for a single `provider` config, a
    /// model name it accepts, sent with default limits for that name; for a
    /// named provider, another of that provider's model selection IDs. An Azure
    /// provider, a model from a different provider, or a model the session's
    /// catalog or allowed-model policy rejects leaves compaction on the
    /// session's model. Takes precedence over any runtime experiment that picks
    /// a compaction model. Blank values are ignored.
    pub model: Option<String>,
    /// Summarization instruction sent as the final user message of a
    /// compaction request, replacing the built-in prompt entirely; the caller
    /// owns the summary's format. `/compact` focus instructions and a note that
    /// tools are unavailable are appended to it. Blank values are ignored.
    pub prompt: Option<String>,
    /// Reasoning effort for compaction requests: one of `none`, `minimal`,
    /// `low`, `medium`, `high`, `xhigh` or `max`. Applies when the model that
    /// summarizes supports it; otherwise the request keeps its usual effort.
    /// Takes precedence over any runtime experiment that adjusts compaction
    /// reasoning effort. Any other value is rejected.
    pub reasoning_effort: Option<String>,
}
crate::record_serde::wire_record!(HistorySetCompactionConfigRequest { model: "model" if is_none, prompt: "prompt" if is_none, reasoning_effort: "reasoningEffort" if is_none });

/// Markdown summary of the conversation context (empty when not available).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistorySummarizeForHandoffResult {
    /// Markdown summary of the conversation context produced by an LLM. Empty string when there are no messages or when the session does not support local summarization.
    pub summary: String,
}
crate::record_serde::wire_record!(HistorySummarizeForHandoffResult { summary: "summary" });

/// Identifier of the event to truncate to; this event and all later events are removed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryTruncateRequest {
    /// Event ID to truncate to. This event and all events after it are removed from the session.
    pub event_id: String,
}
crate::record_serde::wire_record!(HistoryTruncateRequest {
    event_id: "eventId"
});

/// Number of events that were removed by the truncation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HistoryTruncateResult {
    /// Failure detail when checkpointCleanupFailed is true.
    pub checkpoint_cleanup_error: Option<String>,
    /// True when conversation truncation succeeded but post-truncation workspace checkpoint cleanup failed. History is already truncated; callers may still prune snapshots but should report a checkpoint-cleanup rather than a truncation failure.
    pub checkpoint_cleanup_failed: Option<bool>,
    /// Number of events that were removed
    pub events_removed: i64,
}
crate::record_serde::wire_record!(HistoryTruncateResult { checkpoint_cleanup_error: "checkpointCleanupError" if is_none, checkpoint_cleanup_failed: "checkpointCleanupFailed" if is_none, events_removed: "eventsRemoved" });

/// Runtime-owned wire payload for a server-to-client hook callback invocation.
#[derive(Debug, Clone, Default)]
pub(crate) struct HookInvokeRequest {
    pub hook_type: HookType,
    pub input: serde_json::Value,
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(HookInvokeRequest {
    hook_type: "hookType",
    input: "input",
    session_id: "sessionId"
});

/// Optional output returned by an SDK callback hook.
#[derive(Debug, Clone, Default)]
pub(crate) struct HookInvokeResponse {
    pub output: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(HookInvokeResponse { output: "output" if is_none });

/// Optional project paths and host-exclusion behavior for server-scoped hook discovery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HooksDiscoverRequest {
    /// When true, omit host-owned user and plugin hook rows and their diagnostics. Managed-policy hooks and trusted repository hooks remain visible, and host disabledHooks still contribute to each remaining row's effective enabled state. This filters sources rather than simulating a host with no settings.
    pub exclude_host_hooks: Option<bool>,
    /// Explicit discovery-only file-hook roots, separate from callback hooks and filesystem grants. Available without ambient discovery or repository file-hook enablement, subject to managed/hook disables. Relative paths resolve against the request's effective working directory when supplied. Null and omission are equivalent; an empty array contributes no explicit roots.
    pub hook_directories: Option<Vec<String>>,
    /// Optional project directory paths whose trusted repository and project-expanded plugin hooks should be discovered. When omitted or empty, user, managed-policy, and globally enabled installed or explicit plugin hooks are returned without project expansion.
    pub project_paths: Option<Vec<String>>,
}
crate::record_serde::wire_record!(HooksDiscoverRequest { exclude_host_hooks: "excludeHostHooks" if is_none, hook_directories: "hookDirectories" if is_none, project_paths: "projectPaths" if is_none });

/// Server-discovered hook actions and partial-load diagnostics from user, repository, plugin, and managed-policy sources. Concrete sessions may include additional session-specific hook sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HooksDiscoverResult {
    /// Errors for hook sources or actions that could not be loaded, making the result partially incomplete. Other valid actions are still returned. Project-resolution and repository-settings errors are prefixed with their project path.
    pub errors: Vec<String>,
    /// All discovered hook actions. Byte-identical actions remain separate rows even when they share a disable key.
    pub hooks: Vec<DiscoveredHook>,
    /// Non-fatal source-loading warnings. Discovery remains complete for the affected source, although the source had a recoverable issue. Repository-settings warnings are prefixed with their project path when attribution is available.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(HooksDiscoverResult {
    errors: "errors",
    hooks: "hooks",
    warnings: "warnings"
});

/// A decrypted credential presented by a supervised hosting participant.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostAuthenticateRequest {
    /// Unique validation attempt, scoped to this host.
    pub request_id: RequestId,
    /// Decrypted admission token, never an inference credential.
    pub token: String,
}
crate::record_serde::wire_record!(HostAuthenticateRequest {
    request_id: "requestId",
    token: "token"
});

/// Notifies the owning application that validation ended.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostAuthenticationCancelledNotification {
    /// Owning listener UUID.
    pub host_id: String,
    /// Validation attempt whose signal must be aborted.
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(HostAuthenticationCancelledNotification {
    host_id: "hostId",
    request_id: "requestId"
});

/// A validated credential with a required, future RFC3339 expiration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostAuthenticationResult {
    /// Required future expiration in RFC3339 format.
    pub expires_at: String,
    /// Stable identity, required to match the configured owner exactly.
    pub identity: String,
}
crate::record_serde::wire_record!(HostAuthenticationResult {
    expires_at: "expiresAt",
    identity: "identity"
});

/// Cancels a pending validation on the supervised participant's connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostCancelAuthenticationRequest {
    /// Validation attempt to cancel.
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(HostCancelAuthenticationRequest {
    request_id: "requestId"
});

/// GitHub Mission Control registration options. The compute ID is application-owned and stable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostGitHubEnvironmentOptions {
    /// Stable application installation identity, reused across host restarts.
    pub compute_id: String,
    /// Human-readable environment display name.
    pub name: String,
    /// Require sealed, connection-bound authentication (default true), independent of listener address. False permits unsealed direct credentials and unbound sealed relay credentials; use only when the caller fully controls and trusts the direct transport. Relay token encryption remains mandatory.
    pub require_connection_binding: Option<bool>,
}
crate::record_serde::wire_record!(HostGitHubEnvironmentOptions { compute_id: "computeId", name: "name", require_connection_binding: "requireConnectionBinding" if is_none });

/// Normalized local WebSocket listener settings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostLocalServerConfiguration {
    /// Hostname or IP address to bind.
    pub hostname: String,
    /// Port to bind, with zero requesting OS allocation.
    pub port: i32,
    /// Whether the listener requires token authentication.
    pub require_connection_token: bool,
    /// Secret connection token, absent when authentication is disabled.
    pub token: Option<String>,
}
crate::record_serde::wire_record!(HostLocalServerConfiguration { hostname: "hostname", port: "port", require_connection_token: "requireConnectionToken", token: "token" if is_none });

/// Normalized listener settings delivered only to the supervised hosting participant.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostConfiguration {
    /// Initial remote creation policy, independent of application session factories. Defaults to denied when omitted.
    pub allow_creating_new_sessions: Option<bool>,
    /// Explicit host-wide admission policy. Always supplied by the runtime after validating host.start.
    pub authentication: Option<serde_json::Value>,
    /// Requested GitHub Mission Control registration.
    pub github_environment: Option<HostGitHubEnvironmentOptions>,
    /// Normalized local listener settings, absent for relay-only hosts.
    pub local_server: Option<HostLocalServerConfiguration>,
    /// Whether app-owned durable sessions are resumed by the owning application.
    pub resume_factory: bool,
    /// Whether session materialization is delegated to the owning application.
    pub session_factory: bool,
}
crate::record_serde::wire_record!(HostConfiguration { allow_creating_new_sessions: "allowCreatingNewSessions" if is_none, authentication: "authentication" if is_none, github_environment: "githubEnvironment" if is_none, local_server: "localServer" if is_none, resume_factory: "resumeFactory", session_factory: "sessionFactory" });

/// Private host protocol for one runtime-owned credential transaction. Only the admitted in-process CLI host can use this protocol.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostCredentialLifecycleRequest {
    /// Credential lifecycle transition or host-owned credential observation.
    pub action: HostCredentialLifecycleAction,
    /// Whether the original connection owns a renewable SDK callback rather than a static token context.
    pub callback: Option<bool>,
    /// Original callback connection. The runtime verifies that it belongs to this host's engine.
    pub connection_id: Option<i64>,
    /// Registration retained by a resident ambient-auth rollback.
    pub previous_registration_id: Option<String>,
    /// Existing credential registration, never a bearer token.
    pub registration_id: Option<String>,
    /// Whether begin resumes an existing session.
    pub resume: Option<bool>,
    /// Session resolved by the original host lifecycle.
    pub session_id: Option<SessionId>,
    /// Opaque runtime transaction capability returned by begin.
    pub transaction_id: Option<String>,
}
crate::record_serde::wire_record!(HostCredentialLifecycleRequest { action: "action", callback: "callback" if is_none, connection_id: "connectionId" if is_none, previous_registration_id: "previousRegistrationId" if is_none, registration_id: "registrationId" if is_none, resume: "resume" if is_none, session_id: "sessionId" if is_none, transaction_id: "transactionId" if is_none });

/// Projection of the authoritative runtime credential transaction.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostCredentialLifecycleResult {
    /// Pinned identity resolved through the original credential owner.
    pub auth_info: Option<serde_json::Value>,
    /// Whether the runtime transaction bound its registration.
    pub committed: Option<bool>,
    /// Current registration when one exists.
    pub registration_id: Option<String>,
    /// Whether rollback restored the displaced mapping.
    pub restored: Option<bool>,
    /// Whether resume retains the session's existing registration.
    pub reusing_session_registration: Option<bool>,
    /// Opaque runtime transaction capability.
    pub transaction_id: Option<String>,
}
crate::record_serde::wire_record!(HostCredentialLifecycleResult { auth_info: "authInfo" if is_none, committed: "committed" if is_none, registration_id: "registrationId" if is_none, restored: "restored" if is_none, reusing_session_registration: "reusingSessionRegistration" if is_none, transaction_id: "transactionId" if is_none });

/// Cached error-reporting policy for the runtime. This data does not grant ownership or confirm network delivery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostDiagnostics {
    /// Whether host error reporting awaits policy, is enabled, or is disabled.
    pub error_reporting: HostErrorReportingStatus,
}
crate::record_serde::wire_record!(HostDiagnostics {
    error_reporting: "errorReporting"
});

/// Stops a connection-owned listener and joins its teardown.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostDisposeRequest {
    /// Listener UUID. Unknown or successfully stopped IDs are harmless.
    pub host_id: String,
}
crate::record_serde::wire_record!(HostDisposeRequest { host_id: "hostId" });

/// Empty acknowledgement for a completed host lifecycle operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct HostEmptyResult {}

/// Private credentials delivered only to a runtime-owned Mission Control hosting participant.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostEnvironmentCredentials {
    /// GitHub API base URL for the authenticated service.
    pub github_api_url: String,
    /// Hostname of the authenticated GitHub service.
    pub github_host: String,
    /// Mission Control API origin for environment registration and management.
    pub mission_control_url: String,
    /// Current bearer token for the authenticated GitHub identity.
    pub token: String,
}
crate::record_serde::wire_record!(HostEnvironmentCredentials {
    github_api_url: "githubApiUrl",
    github_host: "githubHost",
    mission_control_url: "missionControlUrl",
    token: "token"
});

/// Reports a supervised listener's hosting-task termination and cleanup outcome.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostExitedNotification {
    /// Explicit startup or teardown failure, when present.
    pub error: Option<String>,
    /// Process exit status when available; absent for in-process listener tasks.
    pub exit_code: Option<i64>,
    /// Listener UUID.
    pub host_id: String,
    /// Cause of termination.
    pub reason: HostExitReason,
}
crate::record_serde::wire_record!(HostExitedNotification { error: "error" if is_none, exit_code: "exitCode" if is_none, host_id: "hostId", reason: "reason" });

/// Reads a running host's catalog or the runtime's saved default catalog.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostListSessionsRequest {
    /// Listener UUID returned by host.start; the caller must own it. Omit to read
    /// the saved default compute-identity catalog without starting a listener.
    pub host_id: Option<String>,
}
crate::record_serde::wire_record!(HostListSessionsRequest { host_id: "hostId" if is_none });

/// Metadata advertised by the AHP host, not a guarantee that history can be resumed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostSessionSummary {
    /// Optional activity description advertised by the host.
    pub activity: Option<String>,
    /// Session creation time as an ISO 8601 timestamp.
    pub created_at: String,
    /// Last modification time as an ISO 8601 timestamp.
    pub modified_at: String,
    /// Stable AHP resource URI identifying the session on this host.
    pub resource: String,
    /// Unsigned 32-bit AHP session status bitset (0..=4294967295); unknown bits must be preserved.
    pub status: i64,
    /// Authoritative title advertised by the host.
    pub title: String,
}
crate::record_serde::wire_record!(HostSessionSummary { activity: "activity" if is_none, created_at: "createdAt", modified_at: "modifiedAt", resource: "resource", status: "status", title: "title" });

/// The complete live and dormant advertised catalog, without subscribing to sessions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostListSessionsResult {
    /// Complete advertised session summaries, in the host's catalog order.
    pub sessions: Vec<HostSessionSummary>,
}
crate::record_serde::wire_record!(HostListSessionsResult {
    sessions: "sessions"
});

/// Local WebSocket transport options.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostLocalServerOptions {
    /// Listener hostname. Defaults to 127.0.0.1; explicit non-loopback binds are allowed.
    pub hostname: Option<String>,
    /// Listener port. Omitted or zero requests an OS-allocated port.
    pub port: Option<i32>,
    /// Require token authentication (default true). Cannot be false with a token.
    pub require_connection_token: Option<bool>,
    /// Nonempty connection token. Generated randomly when required and omitted.
    pub token: Option<String>,
}
crate::record_serde::wire_record!(HostLocalServerOptions { hostname: "hostname" if is_none, port: "port" if is_none, require_connection_token: "requireConnectionToken" if is_none, token: "token" if is_none });

/// Publishes a resident session attached to the listener's owning connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostPublishSessionRequest {
    /// Listener UUID returned by host.start.
    pub host_id: String,
    /// Canonical runtime session ID attached to the listener's owning connection.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(HostPublishSessionRequest {
    host_id: "hostId",
    session_id: "sessionId"
});

/// The existing runtime identity and its resource on the listener.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostPublishSessionResult {
    /// Canonical runtime ID of the published session.
    pub session_id: SessionId,
    /// AHP resource URI for the session on this listener.
    pub session_uri: String,
}
crate::record_serde::wire_record!(HostPublishSessionResult {
    session_id: "sessionId",
    session_uri: "sessionUri"
});

/// Readiness reported by the supervised hosting participant on its own SDK connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostReadyRequest {
    /// Actual bound WebSocket URL.
    pub address: Option<String>,
    /// Registered environment ID, reported only once the relay transport is connected.
    pub environment_id: Option<String>,
    /// Configured secret token, absent when authentication is disabled.
    pub token: Option<String>,
}
crate::record_serde::wire_record!(HostReadyRequest { address: "address" if is_none, environment_id: "environmentId" if is_none, token: "token" if is_none });

/// Listener-scoped registration, not a copy or durable adoption of a session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostRegisterSessionRequest {
    /// Additional directories already granted to the resident session.
    pub additional_directories: Option<Vec<String>>,
    /// Session creation time in milliseconds since the Unix epoch, when available.
    pub created_at_unix_ms: Option<i64>,
    /// Last session modification time in milliseconds since the Unix epoch, when available.
    pub modified_at_unix_ms: Option<i64>,
    /// Canonical ID of the existing resident runtime session.
    pub session_id: SessionId,
    /// Current display title of the resident session, when available.
    pub title: Option<String>,
    /// Absolute working directory of the resident session.
    pub working_directory: String,
}
crate::record_serde::wire_record!(HostRegisterSessionRequest { additional_directories: "additionalDirectories" if is_none, created_at_unix_ms: "createdAtUnixMs" if is_none, modified_at_unix_ms: "modifiedAtUnixMs" if is_none, session_id: "sessionId", title: "title" if is_none, working_directory: "workingDirectory" });

/// Application callback routed over its existing SDK connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostSessionCreateCallback {
    /// Trusted admission identity, separate from inference credentials. Always supplied by the runtime for admitted handoffs.
    pub authentication: Option<serde_json::Value>,
    /// Host-selected SDK creation or resume settings, without executable callbacks or tools.
    pub config: HashMap<String, serde_json::Value>,
    /// Unique identity of the session participation being requested.
    pub handoff_id: String,
    /// Listener UUID identifying the owning application's host.
    pub host_id: String,
    /// Resume an app-owned durable session instead of creating a new session.
    pub resume: Option<bool>,
}
crate::record_serde::wire_record!(HostSessionCreateCallback { authentication: "authentication" if is_none, config: "config", handoff_id: "handoffId", host_id: "hostId", resume: "resume" if is_none });

/// One application-owned session handoff, requested by the supervised hosting participant.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostSessionCreateRequest {
    /// Trusted admission identity supplied by the hosting participant. Required by the runtime before a handoff is admitted.
    pub authentication: Option<serde_json::Value>,
    /// Host-selected SDK creation or resume settings, without executable callbacks or tools.
    pub config: HashMap<String, serde_json::Value>,
    /// Unique identity for this participation, independent of the session lifetime.
    pub handoff_id: String,
    /// For an explicit publication, borrow an existing owning-connection resident
    /// instead of invoking the resume factory. Cold sessions still use the factory.
    pub prefer_resident: Option<bool>,
    /// Resume an app-owned durable session instead of creating a new session.
    pub resume: Option<bool>,
}
crate::record_serde::wire_record!(HostSessionCreateRequest { authentication: "authentication" if is_none, config: "config", handoff_id: "handoffId", prefer_resident: "preferResident" if is_none, resume: "resume" if is_none });

/// The resident session the application has materialized on its own connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostSessionCreateResult {
    /// Runtime session UUID materialized on the application's SDK connection.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(HostSessionCreateResult {
    session_id: "sessionId"
});

/// Releases the original application session object retained for one handoff.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostSessionReleasedNotification {
    /// Identity of the handoff retaining the original application session object.
    pub handoff_id: String,
    /// Listener UUID whose application session participation ended.
    pub host_id: String,
}
crate::record_serde::wire_record!(HostSessionReleasedNotification {
    handoff_id: "handoffId",
    host_id: "hostId"
});

/// Ends one participation, not the application's session lifetime.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostSessionReleaseRequest {
    /// Identity of the participation to release without destroying the session.
    pub handoff_id: String,
}
crate::record_serde::wire_record!(HostSessionReleaseRequest {
    handoff_id: "handoffId"
});

/// Changes remote session creation admission without disconnecting clients.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostSetSessionCreationPolicyRequest {
    /// After acknowledgement, subsequent remote creation admissions use this policy. Already-admitted requests may finish.
    pub allow_creating_new_sessions: bool,
    /// Listener UUID owned by the calling SDK connection.
    pub host_id: String,
}
crate::record_serde::wire_record!(HostSetSessionCreationPolicyRequest {
    allow_creating_new_sessions: "allowCreatingNewSessions",
    host_id: "hostId"
});

/// Starts a supervised AHP host with at least one explicitly selected transport.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostStartRequest {
    /// Whether remote clients may create new sessions. Defaults to denied when omitted. Does not restrict owner work, publication, or existing sessions.
    pub allow_creating_new_sessions: Option<bool>,
    /// Explicit admission policy, required by the runtime before hosting. Optional in the wire DTO only to preserve generated client construction.
    pub authentication: Option<serde_json::Value>,
    /// Stable application identity selecting its durable host catalog. Defaults to the persisted runtime compute GUID; must agree with githubEnvironment.computeId when both are supplied.
    pub compute_id: Option<String>,
    /// Registers a GitHub Mission Control environment and enables its relay transport.
    pub github_environment: Option<HostGitHubEnvironmentOptions>,
    /// Caller-generated UUID identifying this connection-owned listener.
    pub host_id: String,
    /// Enables a local WebSocket listener.
    pub local_server: Option<HostLocalServerOptions>,
    /// Ask the owning application to resume its durable AHP sessions.
    pub resume_factory: Option<bool>,
    /// Ask the owning SDK application to materialize AHP sessions.
    pub session_factory: Option<bool>,
    /// Absolute directory on the runtime host to serve through AHP. Defaults to the runtime's configured working directory. Does not change process or session working directories.
    pub workspace: Option<String>,
}
crate::record_serde::wire_record!(HostStartRequest { allow_creating_new_sessions: "allowCreatingNewSessions" if is_none, authentication: "authentication" if is_none, compute_id: "computeId" if is_none, github_environment: "githubEnvironment" if is_none, host_id: "hostId", local_server: "localServer" if is_none, resume_factory: "resumeFactory" if is_none, session_factory: "sessionFactory" if is_none, workspace: "workspace" if is_none });

/// Listener readiness, returned only after binding and the supervised participant's SDK handshake.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostStartResult {
    /// GitHub Mission Control environment ID, present when its relay transport is ready.
    pub environment_id: Option<String>,
    /// Caller-generated listener UUID.
    pub host_id: String,
    /// Separate host process ID, when provided by a legacy runtime. Absent for in-process listeners.
    pub pid: Option<i64>,
    /// Secret connection token, absent when authentication is disabled.
    pub token: Option<String>,
    /// Actual bound WebSocket URL, including the allocated port.
    pub url: Option<String>,
}
crate::record_serde::wire_record!(HostStartResult { environment_id: "environmentId" if is_none, host_id: "hostId", pid: "pid" if is_none, token: "token" if is_none, url: "url" if is_none });

/// Withdraws an application publication without deleting its underlying local session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostUnpublishSessionRequest {
    /// Listener UUID returned by host.start; the caller must own the live listener.
    pub host_id: String,
    /// Canonical session ID in the durable catalog, whether resident or dormant.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(HostUnpublishSessionRequest {
    host_id: "hostId",
    session_id: "sessionId"
});

/// Publication withdrawal outcome; local history, worktrees and ongoing work are preserved.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostUnpublishSessionResult {
    /// True when a publication was removed; false when it was already absent.
    pub unpublished: bool,
}
crate::record_serde::wire_record!(HostUnpublishSessionResult {
    unpublished: "unpublished"
});

/// Removes a durable application publication and revokes its AHP attachments.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostUnregisterSessionRequest {
    /// Canonical ID of the application publication to withdraw.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(HostUnregisterSessionRequest {
    session_id: "sessionId"
});

/// Application-owned token validation callback.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostValidateAuthenticationRequest {
    /// Owning listener UUID.
    pub host_id: String,
    /// Unique validation attempt.
    pub request_id: RequestId,
    /// Decrypted admission token.
    pub token: String,
}
crate::record_serde::wire_record!(HostValidateAuthenticationRequest {
    host_id: "hostId",
    request_id: "requestId",
    token: "token"
});

/// Live IDE state. This state never restores a connection from session history.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct IdeConnectionState {
    /// Currently available IDE, absent during an outage.
    pub connected_ide: Option<IdeConnectedInfo>,
    /// IDE whose tool definitions remain registered, including during an outage.
    pub retained_ide: Option<IdeConnectedInfo>,
    /// Latest observed selection, absent after disconnect.
    pub selection: Option<IdeSelection>,
}
crate::record_serde::wire_record!(IdeConnectionState { connected_ide: "connectedIde" if is_none, retained_ide: "retainedIde" if is_none, selection: "selection" if is_none });

/// Source descriptor for a direct GitHub plugin install, with `owner/repo`, optional ref or full commit SHA, and optional subpath.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstalledPluginSourceGitHub {
    /// Optional repository-relative path to the plugin.
    pub path: Option<String>,
    /// Optional Git ref to resolve.
    pub r#ref: Option<String>,
    /// GitHub repository in `owner/repo` form.
    pub repo: String,
    /// Optional full 40-character hexadecimal commit SHA.
    pub sha: Option<String>,
    /// Constant value. Always "github".
    pub source: InstalledPluginSourceGitHubSource,
}
crate::record_serde::wire_record!(InstalledPluginSourceGitHub { path: "path" if is_none, r#ref: "ref" if is_none, repo: "repo", sha: "sha" if is_none, source: "source" });

/// Source descriptor for a direct local plugin install, with a local filesystem path.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstalledPluginSourceLocal {
    /// Local filesystem path to the plugin.
    pub path: String,
    /// Constant value. Always "local".
    pub source: InstalledPluginSourceLocalSource,
}
crate::record_serde::wire_record!(InstalledPluginSourceLocal {
    path: "path",
    source: "source"
});

/// Source descriptor for a direct URL plugin install, with URL, optional ref or full commit SHA, and optional subpath.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstalledPluginSourceUrl {
    /// Optional source-relative path to the plugin.
    pub path: Option<String>,
    /// Optional Git ref to resolve.
    pub r#ref: Option<String>,
    /// Optional full 40-character hexadecimal commit SHA.
    pub sha: Option<String>,
    /// Constant value. Always "url".
    pub source: InstalledPluginSourceUrlSource,
    /// URL of the plugin source.
    pub url: String,
}
crate::record_serde::wire_record!(InstalledPluginSourceUrl { path: "path" if is_none, r#ref: "ref" if is_none, sha: "sha" if is_none, source: "source", url: "url" });

/// Canonical file or directory where custom instructions can be discovered or created, with location, kind, preference, and project path.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstructionDiscoveryPath {
    /// Whether the target is a single file or a directory of instruction files
    pub kind: InstructionDiscoveryPathKind,
    /// Which tier this target belongs to
    pub location: InstructionDiscoveryPathLocation,
    /// Absolute path of the file or directory (may not exist on disk yet)
    pub path: String,
    /// Whether this is the canonical target to create new instructions in its tier. At most one entry per tier is preferred.
    pub preferred_for_creation: bool,
    /// The input project path this target was derived from (only for repository targets)
    pub project_path: Option<String>,
}
crate::record_serde::wire_record!(InstructionDiscoveryPath { kind: "kind", location: "location", path: "path", preferred_for_creation: "preferredForCreation", project_path: "projectPath" if is_none });

/// Canonical files and directories where custom instructions can be created so the runtime will recognize them.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstructionDiscoveryPathList {
    /// Canonical instruction create/discovery files and directories, in priority order
    pub paths: Vec<InstructionDiscoveryPath>,
}
crate::record_serde::wire_record!(InstructionDiscoveryPathList { paths: "paths" });

/// Explicit discovery arguments for one location, supplied by the caller rather than derived from a session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct InstructionsDiscoverRepoSourcesRequest {
    /// Extra directories to scan alongside instruction directories configured in the runtime environment.
    pub additional_dirs: Option<Vec<String>>,
    /// Copilot configuration directory for user-level sources. An omitted or empty value uses the server's configuration home.
    pub config_dir: Option<String>,
    /// Working directory whose instruction sources should also be included. Omission adds no separate working-directory scope.
    pub cwd_path: Option<String>,
    /// When true, also discover instruction files nested below the location.
    pub enable_child_instructions: Option<bool>,
    /// When true, include the sources a command-line host reads. Hosts that are not the CLI omit it.
    pub is_cli: Option<bool>,
    /// Repository location to discover. An empty string selects no repository scope; cwdPath can still supply working-directory sources.
    pub location: String,
}
crate::record_serde::wire_record!(InstructionsDiscoverRepoSourcesRequest { additional_dirs: "additionalDirs" if is_none, config_dir: "configDir" if is_none, cwd_path: "cwdPath" if is_none, enable_child_instructions: "enableChildInstructions" if is_none, is_cli: "isCli" if is_none, location: "location" });

/// Optional project paths to include in instruction discovery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstructionsDiscoverRequest {
    /// When true, omit the host's instruction sources (user/home-level files and plugin rules), leaving only repository and working-directory sources. For multitenant deployments.
    pub exclude_host_instructions: Option<bool>,
    /// Explicit discovery-only instruction roots using existing SDK instruction formats, not filesystem grants. Available without ambient discovery, subject to instruction/managed disables. Relative paths resolve against the request's effective working directory when supplied. Null and omission are equivalent; an empty array contributes no explicit roots.
    pub instruction_directories: Option<Vec<String>>,
    /// Optional list of project directory paths to scan for repository/working-directory instruction sources. When omitted or empty, only user-level and plugin instruction sources are returned (no project scan).
    pub project_paths: Option<Vec<String>>,
}
crate::record_serde::wire_record!(InstructionsDiscoverRequest { exclude_host_instructions: "excludeHostInstructions" if is_none, instruction_directories: "instructionDirectories" if is_none, project_paths: "projectPaths" if is_none });

/// Optional project paths to include when enumerating instruction discovery targets.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstructionsGetDiscoveryPathsRequest {
    /// When true, omit the host's user-level instruction targets, leaving only repository targets. For multitenant deployments (mirrors `discover`'s `excludeHostInstructions`).
    pub exclude_host_instructions: Option<bool>,
    /// Explicit discovery-only instruction roots to enumerate, including configured missing directories. Relative paths resolve against the request's effective working directory when supplied. These are not filesystem grants or preferred creation targets. Null and omission are equivalent.
    pub instruction_directories: Option<Vec<String>>,
    /// Optional list of project directory paths. When omitted or empty, only the user-level targets are returned.
    pub project_paths: Option<Vec<String>>,
}
crate::record_serde::wire_record!(InstructionsGetDiscoveryPathsRequest { exclude_host_instructions: "excludeHostInstructions" if is_none, instruction_directories: "instructionDirectories" if is_none, project_paths: "projectPaths" if is_none });

/// Instruction sources loaded for the session, in merge order.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstructionsGetSourcesResult {
    /// Instruction sources for the session
    pub sources: Vec<InstructionSource>,
}
crate::record_serde::wire_record!(InstructionsGetSourcesResult { sources: "sources" });

/// The prompt the init command runs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct InstructionsInitPromptResult {
    /// Prompt text authoring repository instructions.
    pub prompt: String,
}
crate::record_serde::wire_record!(InstructionsInitPromptResult { prompt: "prompt" });

/// Parameters for interrupting the main agent turn.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InterruptMainTurnRequest {
    /// When true, the user's queued prompts are preserved and run as the next turn once the interrupted turn unwinds; when false (the default), the queue is cleared like a plain abort.
    pub flush_queued: Option<bool>,
}
crate::record_serde::wire_record!(InterruptMainTurnRequest { flush_queued: "flushQueued" if is_none });

/// Result of interrupting the main agent turn.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InterruptMainTurnResult {
    /// Whether an in-flight main agent turn was interrupted. False when the main loop was not processing.
    pub interrupted: bool,
}
crate::record_serde::wire_record!(InterruptMainTurnResult {
    interrupted: "interrupted"
});

/// A JSON Schema output contract. OpenAI receives the name, description, schema and strict setting; Anthropic receives the schema in output_config.format and always uses its native strict enforcement.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct JsonSchemaResponseFormat {
    /// Optional description passed to OpenAI providers.
    pub description: Option<String>,
    /// Name of the output schema, subject to the provider's naming restrictions.
    pub name: String,
    /// JSON Schema passed unchanged to the inference provider. Schemas larger than 32 MiB when JSON-encoded are rejected before admission, using the runtime's existing request-size ceiling. This is not a guarantee that the entire model request fits. Supported keywords and schema restrictions are determined by the provider.
    pub schema: serde_json::Value,
    /// Optional strict enforcement setting for OpenAI providers. Omitted uses the provider default. Anthropic always enforces its supported schema subset.
    pub strict: Option<bool>,
}
crate::record_serde::wire_record!(JsonSchemaResponseFormat { description: "description" if is_none, name: "name", schema: "schema", strict: "strict" if is_none });

/// Cursor-based request for experimental runtime lifecycle diagnostic events. The default max is 100 (maximum 500); the default waitMs is zero (maximum 30000).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct LifecycleDiagnosticsReadRequest {
    /// Opaque cursor returned by an earlier lifecycle diagnostics read. Omit to start at the oldest retained event.
    pub cursor: Option<String>,
    /// Maximum number of events to return, from 1 through 500. Omit for 100.
    pub max: Option<i64>,
    /// Maximum time in milliseconds to wait for a new event, from 0 through 30000.
    pub wait_ms: Option<i32>,
}
crate::record_serde::wire_record!(LifecycleDiagnosticsReadRequest { cursor: "cursor" if is_none, max: "max" if is_none, wait_ms: "waitMs" if is_none });

/// One cursor-addressed page of retained experimental runtime lifecycle diagnostic events.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct LifecycleDiagnosticsReadResult {
    /// Opaque cursor for the next independent lifecycle diagnostics read.
    pub cursor: String,
    /// Whether the requested cursor remained within the retained activation window.
    pub cursor_status: DiagnosticCursorStatus,
    /// Number of events lost before this page when known. Omitted when an activation change makes the count unknowable.
    pub dropped_count: Option<i64>,
    /// Retained lifecycle diagnostic events beginning at the requested cursor.
    pub events: Vec<LifecycleDiagnosticEvent>,
    /// Whether additional retained lifecycle diagnostic events follow this page.
    pub has_more: bool,
}
crate::record_serde::wire_record!(LifecycleDiagnosticsReadResult { cursor: "cursor", cursor_status: "cursorStatus", dropped_count: "droppedCount" if is_none, events: "events", has_more: "hasMore" });

/// A request body chunk or cancellation signal.
#[derive(Debug, Clone, Default)]
pub struct LlmInferenceHttpRequestChunkRequest {
    /// Identity of the agent invocation (one agentic loop) this body chunk belongs to, matching the `agentInvocationId` semantics on httpRequestStart. Carried per chunk so a persistent transport can attribute successive turns correctly: when a WebSocket connection is reused across turns, the httpRequestStart identity reflects only the turn that opened the connection, so each later turn stamps its own invocation id here. Absent when the runtime has no invocation context for the request, or on the plain-HTTP transport where every request has its own httpRequestStart.
    pub agent_invocation_id: Option<String>,
    /// When true, `data` is base64-encoded bytes. When absent or false, `data` is UTF-8 text.
    pub binary: Option<bool>,
    /// When true, the runtime is cancelling the in-flight request (e.g. upstream consumer aborted). `data` is ignored. Implies end-of-request.
    pub cancel: Option<bool>,
    /// Optional human-readable reason for the cancellation, propagated for logging.
    pub cancel_reason: Option<String>,
    /// Body byte range. UTF-8 text when `binary` is absent or false; base64-encoded bytes when `binary` is true. May be empty.
    pub data: String,
    /// When true, this is the final body chunk for the request. The SDK may rely on having received an end-marked chunk before treating the request body as complete.
    pub end: Option<bool>,
    /// Matches the requestId from the originating httpRequestStart frame.
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(LlmInferenceHttpRequestChunkRequest { agent_invocation_id: "agentInvocationId" if is_none, binary: "binary" if is_none, cancel: "cancel" if is_none, cancel_reason: "cancelReason" if is_none, data: "data", end: "end" if is_none, request_id: "requestId" });

/// Acknowledgement. The SDK is free to ignore the ack and treat chunk delivery as fire-and-forget.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LlmInferenceHttpRequestChunkResult {}

/// The head of an outbound model-layer HTTP request.
#[derive(Debug, Clone, Default)]
pub struct LlmInferenceHttpRequestStartRequest {
    /// Stable identity of the agent trajectory that issued this request. Present when the request originates from an agent turn; absent for requests outside any agent context. This is the same identity used by lifecycle and bridged session events and remains constant across turns and retries.
    pub agent_id: Option<String>,
    /// Identity of the agent invocation (one agentic loop) that issued this request. It remains fixed across physical retries within the invocation and is distinct from the stable trajectory `agentId`. A caller-supplied invocation id always takes precedence (this covers auxiliary calls that have no model call id). Otherwise, first-party CAPI requests fall back to the runtime's agent task id — the same value the runtime emits as the `X-Agent-Task-Id` header — while custom-provider requests fall back to the model call id.
    pub agent_invocation_id: Option<String>,
    /// HTTP request headers, preserving multiple values per name.
    pub headers: HashMap<String, Vec<String>>,
    /// Coarse classification of the interaction that produced this request. Open string for forward-compatibility; known values include `conversation-agent`, `conversation-subagent`, `conversation-sampling`, `conversation-background`, `conversation-compaction`, and `conversation-user`. Absent when the runtime did not classify the request. Comes from the runtime's per-request agent context independently of transport; on the CAPI transport the runtime derives the upstream `X-Interaction-Type` header from this same context.
    pub interaction_type: Option<String>,
    /// HTTP method, e.g. GET, POST.
    pub method: String,
    /// Stable identity of the immediate parent trajectory. Present for child trajectories such as subagents and conversation-sampling requests; absent for root-agent and non-agent requests.
    pub parent_agent_id: Option<String>,
    /// Opaque runtime-minted id, unique per in-flight request. The SDK uses this to correlate httpRequestChunk frames and to address its httpResponseStart / httpResponseChunk replies back to the runtime.
    pub request_id: RequestId,
    /// Id of the runtime session that triggered this request, when one is in scope. Absent for requests issued outside any session (e.g. startup model-catalog or capability resolution). This is a payload field — not a dispatch key — because the client-global API is registered process-wide rather than per session.
    pub session_id: Option<SessionId>,
    /// Transport the runtime would otherwise use for this request. `http` (the default when absent) covers plain HTTP and SSE responses; `websocket` indicates a full-duplex message channel where each body chunk maps to one WebSocket message and the `binary` flag distinguishes text from binary frames. The SDK consumer uses this to decide whether to service the request with an HTTP client or a WebSocket client. It is the one piece of request metadata the consumer cannot reliably infer from the URL or headers alone.
    pub transport: Option<LlmInferenceHttpRequestStartTransport>,
    /// Absolute request URL.
    pub url: String,
}
crate::record_serde::wire_record!(LlmInferenceHttpRequestStartRequest { agent_id: "agentId" if is_none, agent_invocation_id: "agentInvocationId" if is_none, headers: "headers", interaction_type: "interactionType" if is_none, method: "method", parent_agent_id: "parentAgentId" if is_none, request_id: "requestId", session_id: "sessionId" if is_none, transport: "transport" if is_none, url: "url" });

/// Acknowledgement. Returning successfully simply means the SDK accepted the start frame; it does not imply the request will succeed.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LlmInferenceHttpRequestStartResult {}

/// Set to terminate the response with a transport-level failure. Implies end-of-stream; any further chunks for this requestId are ignored.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LlmInferenceHttpResponseChunkError {
    /// Optional machine-readable error code.
    pub code: Option<String>,
    /// Human-readable failure description.
    pub message: String,
}
crate::record_serde::wire_record!(LlmInferenceHttpResponseChunkError { code: "code" if is_none, message: "message" });

/// A response body chunk or terminal error.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LlmInferenceHttpResponseChunkRequest {
    /// When true, `data` is base64-encoded bytes. When absent or false, `data` is UTF-8 text.
    pub binary: Option<bool>,
    /// Body byte range. UTF-8 text when `binary` is absent or false; base64-encoded bytes when `binary` is true. May be empty (e.g. when the response body is empty: send a single chunk with empty data and end=true).
    pub data: String,
    /// When true, this is the final body chunk for the response. The runtime treats the response body as complete after receiving an end-marked chunk.
    pub end: Option<bool>,
    /// Set to terminate the response with a transport-level failure. Implies end-of-stream; any further chunks for this requestId are ignored.
    pub error: Option<LlmInferenceHttpResponseChunkError>,
    /// Matches the requestId from the originating httpRequestStart frame.
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(LlmInferenceHttpResponseChunkRequest { binary: "binary" if is_none, data: "data", end: "end" if is_none, error: "error" if is_none, request_id: "requestId" });

/// Whether the chunk was accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LlmInferenceHttpResponseChunkResult {
    /// True when the chunk was matched to a pending request; false when unknown.
    pub accepted: bool,
}
crate::record_serde::wire_record!(LlmInferenceHttpResponseChunkResult {
    accepted: "accepted"
});

/// Response head.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LlmInferenceHttpResponseStartRequest {
    /// HTTP response headers, preserving multiple values per name.
    pub headers: HashMap<String, Vec<String>>,
    /// Matches the requestId from the originating httpRequestStart frame.
    pub request_id: RequestId,
    /// HTTP status code.
    pub status: i64,
    /// Optional HTTP status reason phrase.
    pub status_text: Option<String>,
}
crate::record_serde::wire_record!(LlmInferenceHttpResponseStartRequest { headers: "headers", request_id: "requestId", status: "status", status_text: "statusText" if is_none });

/// Whether the start frame was accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LlmInferenceHttpResponseStartResult {
    /// True when the response start was matched to a pending request; false when unknown.
    pub accepted: bool,
}
crate::record_serde::wire_record!(LlmInferenceHttpResponseStartResult {
    accepted: "accepted"
});

/// Indicates whether the calling client was registered as the LLM inference provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LlmInferenceSetProviderResult {
    /// Whether the provider was set successfully
    pub success: bool,
}
crate::record_serde::wire_record!(LlmInferenceSetProviderResult { success: "success" });

/// Automatic registration identity and concurrency.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelAutoRegistration {
    /// Corresponding Copilot model identifier.
    pub capi_id: String,
    /// Maximum simultaneous inference requests.
    pub max_concurrent: f64,
}
crate::record_serde::wire_record!(LocalModelAutoRegistration {
    capi_id: "capiId",
    max_concurrent: "maxConcurrent"
});

/// Exact registry identity of a downloadable model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelAzureMlIdentity {
    /// Exact model family name.
    pub name: String,
    /// Azure ML registry name.
    pub registry: String,
    /// Exact model asset version.
    pub version: String,
}
crate::record_serde::wire_record!(LocalModelAzureMlIdentity {
    name: "name",
    registry: "registry",
    version: "version"
});

/// Published token limits of a local model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelLimits {
    /// Maximum context tokens.
    pub max_context_window_tokens: f64,
    /// Maximum output tokens.
    pub max_output_tokens: f64,
    /// Maximum prompt tokens.
    pub max_prompt_tokens: f64,
}
crate::record_serde::wire_record!(LocalModelLimits {
    max_context_window_tokens: "maxContextWindowTokens",
    max_output_tokens: "maxOutputTokens",
    max_prompt_tokens: "maxPromptTokens"
});

/// Catalog source and its display order.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelSource {
    /// Stable source identifier.
    pub id: String,
    /// Source display name.
    pub label: String,
    /// Display order among catalog sources.
    pub ordering: f64,
}
crate::record_serde::wire_record!(LocalModelSource {
    id: "id",
    label: "label",
    ordering: "ordering"
});

/// Runtime catalog entry, including installation and inference metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelDescriptor {
    /// Published download size, when known.
    pub approximate_bytes: Option<f64>,
    /// Recommended graphics memory, when published.
    pub approximate_vram_bytes: Option<f64>,
    /// Automatic registration metadata, when applicable.
    pub auto_local: Option<LocalModelAutoRegistration>,
    /// Exact registry asset identity.
    pub azure_ml: LocalModelAzureMlIdentity,
    /// Model display name.
    pub display_name: String,
    /// Default primary filename.
    pub file_name: String,
    /// Catalog identifier, distinct from the inference selection identifier.
    pub id: String,
    /// Published license name.
    pub license: String,
    /// Catalog lifecycle category.
    pub lifecycle: Option<String>,
    /// Published token limits.
    pub limits: LocalModelLimits,
    /// Registry metadata, preserved without mutation.
    pub metadata: serde_json::Value,
    /// Published peak installation size, when known.
    pub peak_disk_bytes: Option<f64>,
    /// Model publisher display name.
    pub provider: String,
    /// Stable BYOK provider name.
    pub provider_name: String,
    /// Inference runtime name.
    pub runtime: String,
    /// Whether acquisition installs the inference runtime.
    pub runtime_provisioned: Option<bool>,
    /// Provider-qualified selection identifier.
    pub selection_id: Option<String>,
    /// Published inference options, preserved without mutation.
    pub serve_options: serde_json::Value,
    /// Source and display order.
    pub source: LocalModelSource,
    /// Registry display label.
    pub source_label: String,
    /// Whether the model supports tool calls.
    pub supports_tool_calls: bool,
}
crate::record_serde::wire_record!(LocalModelDescriptor { approximate_bytes: "approximateBytes" if is_none, approximate_vram_bytes: "approximateVramBytes" if is_none, auto_local: "autoLocal" if is_none, azure_ml: "azureMl", display_name: "displayName", file_name: "fileName", id: "id", license: "license", lifecycle: "lifecycle" if is_none, limits: "limits", metadata: "metadata", peak_disk_bytes: "peakDiskBytes" if is_none, provider: "provider", provider_name: "providerName", runtime: "runtime", runtime_provisioned: "runtimeProvisioned" if is_none, selection_id: "selectionId" if is_none, serve_options: "serveOptions", source: "source", source_label: "sourceLabel", supports_tool_calls: "supportsToolCalls" });

/// Available space could not be determined.
#[derive(Debug, Clone, Default)]
pub struct LocalModelDiskRequirementUnknown {
    /// Local disk requirement evaluation.
    pub kind: LocalModelDiskRequirementUnknownKind,
}
crate::record_serde::wire_record!(LocalModelDiskRequirementUnknown { kind: "kind" });

/// Known disk space meets the installation requirement.
#[derive(Debug, Clone, Default)]
pub struct LocalModelDiskRequirementOk {
    /// Available bytes on the destination filesystem.
    pub free_bytes: f64,
    /// Local disk requirement evaluation.
    pub kind: LocalModelDiskRequirementOkKind,
}
crate::record_serde::wire_record!(LocalModelDiskRequirementOk {
    free_bytes: "freeBytes",
    kind: "kind"
});

/// Known disk space does not meet the installation requirement.
#[derive(Debug, Clone, Default)]
pub struct LocalModelDiskRequirementInsufficient {
    /// Destination used for the disk evaluation.
    pub directory: String,
    /// Available bytes on the destination filesystem.
    pub free_bytes: f64,
    /// Local disk requirement evaluation.
    pub kind: LocalModelDiskRequirementInsufficientKind,
    /// Required bytes, including the safety allowance.
    pub required_bytes: f64,
}
crate::record_serde::wire_record!(LocalModelDiskRequirementInsufficient {
    directory: "directory",
    free_bytes: "freeBytes",
    kind: "kind",
    required_bytes: "requiredBytes"
});

/// Current transfer progress.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelDownloadProgress {
    /// Bytes received so far.
    pub received_bytes: f64,
    /// Inference engine installation, when applicable.
    pub stage: Option<String>,
    /// Total transfer bytes, when known.
    pub total_bytes: Option<f64>,
}
crate::record_serde::wire_record!(LocalModelDownloadProgress { received_bytes: "receivedBytes", stage: "stage" if is_none, total_bytes: "totalBytes" if is_none });

/// Graphics memory could not be determined.
#[derive(Debug, Clone, Default)]
pub struct LocalModelGpuRequirementUnknown {
    /// Graphics memory requirement evaluation.
    pub kind: LocalModelGpuRequirementUnknownKind,
}
crate::record_serde::wire_record!(LocalModelGpuRequirementUnknown { kind: "kind" });

/// The catalog publishes no graphics memory requirement.
#[derive(Debug, Clone, Default)]
pub struct LocalModelGpuRequirementUnspecified {
    /// Graphics memory requirement evaluation.
    pub kind: LocalModelGpuRequirementUnspecifiedKind,
}
crate::record_serde::wire_record!(LocalModelGpuRequirementUnspecified { kind: "kind" });

/// Detected graphics memory meets the published requirement.
#[derive(Debug, Clone, Default)]
pub struct LocalModelGpuRequirementOk {
    /// Available graphics memory.
    pub available_bytes: f64,
    /// Graphics adapter description.
    pub description: String,
    /// Graphics memory requirement evaluation.
    pub kind: LocalModelGpuRequirementOkKind,
    /// Published recommended graphics memory.
    pub required_bytes: f64,
}
crate::record_serde::wire_record!(LocalModelGpuRequirementOk {
    available_bytes: "availableBytes",
    description: "description",
    kind: "kind",
    required_bytes: "requiredBytes"
});

/// Detected graphics memory falls below the published requirement.
#[derive(Debug, Clone, Default)]
pub struct LocalModelGpuRequirementInsufficient {
    /// Available graphics memory.
    pub available_bytes: f64,
    /// Graphics adapter description.
    pub description: String,
    /// Graphics memory requirement evaluation.
    pub kind: LocalModelGpuRequirementInsufficientKind,
    /// Published recommended graphics memory.
    pub required_bytes: f64,
}
crate::record_serde::wire_record!(LocalModelGpuRequirementInsufficient {
    available_bytes: "availableBytes",
    description: "description",
    kind: "kind",
    required_bytes: "requiredBytes"
});

/// No active installation exists.
#[derive(Debug, Clone, Default)]
pub struct LocalModelStatusNotDownloaded {
    /// Effective local installation or transfer state.
    pub kind: LocalModelStatusNotDownloadedKind,
}
crate::record_serde::wire_record!(LocalModelStatusNotDownloaded { kind: "kind" });

/// A valid activation references the local model files.
#[derive(Debug, Clone, Default)]
pub struct LocalModelStatusDownloaded {
    /// Effective local installation or transfer state.
    pub kind: LocalModelStatusDownloadedKind,
    /// Local file size, when known.
    pub size_bytes: Option<f64>,
    /// Whether the catalog offers another version.
    pub update_available: Option<bool>,
}
crate::record_serde::wire_record!(LocalModelStatusDownloaded { kind: "kind", size_bytes: "sizeBytes" if is_none, update_available: "updateAvailable" if is_none });

/// A transfer or inference engine installation is active.
#[derive(Debug, Clone, Default)]
pub struct LocalModelStatusDownloading {
    /// Effective local installation or transfer state.
    pub kind: LocalModelStatusDownloadingKind,
    /// Bytes received so far.
    pub received_bytes: f64,
    /// Inference engine installation, when applicable.
    pub stage: Option<String>,
    /// Total transfer bytes, when known.
    pub total_bytes: Option<f64>,
}
crate::record_serde::wire_record!(LocalModelStatusDownloading { kind: "kind", received_bytes: "receivedBytes", stage: "stage" if is_none, total_bytes: "totalBytes" if is_none });

/// One local model and its current files, progress, and requirements.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LocalModelMachineRow {
    /// Resolved catalog model.
    pub descriptor: LocalModelDescriptor,
    /// Disk availability and required space.
    pub disk: LocalModelDiskRequirement,
    /// Whether known disk facts permit installation.
    pub disk_met: bool,
    /// Remaining peak space with the existing safety allowance.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disk_required_bytes: Option<f64>,
    /// Graphics memory availability and required space.
    pub gpu: LocalModelGpuRequirement,
    /// Published recommended graphics memory.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub gpu_required_bytes: Option<f64>,
    /// Bytes in inactive version directories.
    pub leftover_bytes: f64,
    /// Partial bytes in the selected version directory.
    pub staged_bytes: f64,
    /// Effective installation or download state.
    pub status: LocalModelStatus,
}

/// Detected files and resource requirements.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelMachineView {
    /// Detected standalone llama-server executable, if available.
    pub llama_server_path: Option<String>,
    /// Models directory under the connection's configuration root.
    pub models_directory: String,
    /// Catalog models with their current local state.
    pub rows: Vec<LocalModelMachineRow>,
}
crate::record_serde::wire_record!(LocalModelMachineView { llama_server_path: "llamaServerPath" if is_none, models_directory: "modelsDirectory", rows: "rows" });

/// A notice that remains visible until dismissed or replaced.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelNotice {
    /// Catalog identifier associated with this notice.
    pub model_id: String,
    /// Notice text.
    pub text: String,
    /// Information, warning, or error.
    pub tone: String,
}
crate::record_serde::wire_record!(LocalModelNotice {
    model_id: "modelId",
    text: "text",
    tone: "tone"
});

/// Host preferences for its connection-owned manager, not authorization claims.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsConfigureRequest {
    /// Whether the host currently displays the downloadable catalog.
    pub catalog_enabled: bool,
    /// Host diagnostic log path, used only in failure guidance.
    pub diagnostic_log_path: Option<String>,
    /// Whether the host currently offers local model management.
    pub enabled: bool,
    /// Request the internal registry only when runtime-owned staff admission also permits it.
    pub include_dev: bool,
    /// Attached source session for live registration and selection. Its lifetime does not change.
    pub session_id: Option<SessionId>,
    /// Suppress internal model facts while the host displays shared content.
    pub streamer_mode: bool,
}
crate::record_serde::wire_record!(LocalModelsConfigureRequest { catalog_enabled: "catalogEnabled", diagnostic_log_path: "diagnosticLogPath" if is_none, enabled: "enabled", include_dev: "includeDev", session_id: "sessionId" if is_none, streamer_mode: "streamerMode" });

/// A message for a host's activity view.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelTimelineMessage {
    /// Whether the text uses Markdown.
    pub markdown: Option<bool>,
    /// Message text.
    pub text: String,
    /// Information, warning, or error.
    pub r#type: LocalModelTimelineMessageType,
}
crate::record_serde::wire_record!(LocalModelTimelineMessage { markdown: "markdown" if is_none, text: "text", r#type: "type" });

/// A transient presentation update. Logs and telemetry remain runtime-owned.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsEvent {
    /// Message absent for a model-list refresh.
    pub message: Option<LocalModelTimelineMessage>,
    /// Whether current internal-registry access is necessary to display this notification.
    pub requires_dev_access: bool,
    /// Monotonic manager revision associated with this update.
    pub revision: i64,
    /// Host notification or model-list refresh.
    pub r#type: LocalModelsEventType,
}
crate::record_serde::wire_record!(LocalModelsEvent { message: "message" if is_none, requires_dev_access: "requiresDevAccess", revision: "revision", r#type: "type" });

/// Identifier of a job or notice owned by this connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsModelIdRequest {
    /// Catalog identifier. Identifiers from another connection confer no access.
    pub model_id: String,
}
crate::record_serde::wire_record!(LocalModelsModelIdRequest {
    model_id: "modelId"
});

/// A model returned by this connection's catalog.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsModelRequest {
    /// Model whose acquisition or registration the runtime manages.
    pub model: LocalModelDescriptor,
}
crate::record_serde::wire_record!(LocalModelsModelRequest { model: "model" });

/// Catalog refresh intent.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsRefreshRequest {
    /// Use the background catalog policy instead of an explicit user refresh.
    pub background: bool,
}
crate::record_serde::wire_record!(LocalModelsRefreshRequest {
    background: "background"
});

/// Setup intent for one catalog model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsSetupRequest {
    /// Download, update, or repair intent.
    pub intent: LocalModelsSetupRequestIntent,
    /// Requested catalog model. The runtime validates its identity and resolves current artifact access.
    pub model: LocalModelDescriptor,
}
crate::record_serde::wire_record!(LocalModelsSetupRequest {
    intent: "intent",
    model: "model"
});

/// Connection-owned local model state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsView {
    /// Catalog error, when the refresh failed.
    pub catalog_error: Option<String>,
    /// Whether a catalog refresh is active.
    pub catalog_loading: bool,
    /// Active download progress by catalog identifier.
    pub downloads: HashMap<String, LocalModelDownloadProgress>,
    /// Whether the host offers local model management.
    pub enabled: bool,
    /// Whether internal registry models must remain hidden.
    pub hide_dev_models: bool,
    /// Whether the downloadable catalog must remain hidden.
    pub hide_win_ml_models: bool,
    /// Local files, runtime detection, and resource requirements.
    pub machine: LocalModelMachineView,
    /// Persistent presentation notices by catalog identifier.
    pub notices: HashMap<String, LocalModelNotice>,
    /// Active setup or removal stage by catalog identifier.
    pub operations: HashMap<String, LocalModelOperation>,
    /// Inference runtime prerequisite guidance.
    pub prerequisite_notice: String,
}
crate::record_serde::wire_record!(LocalModelsView { catalog_error: "catalogError" if is_none, catalog_loading: "catalogLoading", downloads: "downloads", enabled: "enabled", hide_dev_models: "hideDevModels", hide_win_ml_models: "hideWinMlModels", machine: "machine", notices: "notices", operations: "operations", prerequisite_notice: "prerequisiteNotice" });

/// Snapshot and transient notifications for one connection-owned manager.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsSnapshot {
    /// Transient host notifications since the acknowledged revision.
    pub events: Vec<LocalModelsEvent>,
    /// Revision to acknowledge in the next watch call.
    pub revision: i64,
    /// Current catalog, machine state, active operations, progress, and notices.
    pub state: LocalModelsView,
}
crate::record_serde::wire_record!(LocalModelsSnapshot {
    events: "events",
    revision: "revision",
    state: "state"
});

/// Acknowledged position in this connection's manager updates.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsWatchRequest {
    /// Last consumed revision. The call waits when no newer update exists.
    pub after_revision: i64,
}
crate::record_serde::wire_record!(LocalModelsWatchRequest {
    after_revision: "afterRevision"
});

/// Which stored location permissions were dropped because their directory is gone.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct LocationPermissionsCleanupResult {
    /// Location keys whose grants were removed. Empty when every stored location still exists.
    pub removed_location_keys: Vec<String>,
}
crate::record_serde::wire_record!(LocationPermissionsCleanupResult {
    removed_location_keys: "removedLocationKeys"
});

/// Message text, optional severity level, persistence flag, optional follow-up URL, and optional tip.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LogRequest {
    /// When true, the message is transient and not persisted to the session event log on disk
    pub ephemeral: Option<bool>,
    /// Log severity level. Determines how the message is displayed in the timeline. Defaults to "info".
    pub level: Option<SessionLogLevel>,
    /// Human-readable message
    pub message: String,
    /// Optional actionable tip displayed alongside the message. Only honored on `level: "info"`.
    pub tip: Option<String>,
    /// Domain category for this log entry (e.g., "mcp", "subscription", "policy", "model"). Maps to `infoType`/`warningType`/`errorType` on the emitted event. Defaults to "notification".
    pub r#type: Option<String>,
    /// Optional URL the user can open in their browser for more details
    pub url: Option<String>,
}
crate::record_serde::wire_record!(LogRequest { ephemeral: "ephemeral" if is_none, level: "level" if is_none, message: "message", tip: "tip" if is_none, r#type: "type" if is_none, url: "url" if is_none });

/// Identifier of the session event that was emitted for the log message.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LogResult {
    /// The unique identifier of the emitted session event
    pub event_id: String,
}
crate::record_serde::wire_record!(LogResult {
    event_id: "eventId"
});

/// Runtime-owned configuration inventory and configuration diagnostics.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspConfigurationSnapshot {
    /// Effective servers in merge order.
    pub configs: Vec<LspServerSummary>,
    /// Effective plugin entries.
    pub plugin_servers: Vec<LspServerSummary>,
    /// Plugin configuration diagnostics.
    pub plugin_warnings: Vec<String>,
    /// Nearest project configuration file path.
    pub project_config_path: Option<String>,
    /// Failure to inspect project configuration.
    pub project_inspection_error: Option<String>,
    /// Project files rejected by the loader.
    pub project_problems: Vec<LspConfigurationProblem>,
    /// Project entries, including disabled entries absent from the effective set.
    pub project_servers: Vec<LspServerSummary>,
    /// User configuration read or validation failure.
    pub user_config_error: Option<String>,
    /// User configuration file path.
    pub user_config_path: String,
    /// Identifiers disabled through the user toggle list.
    pub user_disabled_servers: Vec<String>,
    /// User entries, including entries disabled by project configuration.
    pub user_servers: Vec<LspServerSummary>,
    /// Configuration loader diagnostics.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(LspConfigurationSnapshot { configs: "configs", plugin_servers: "pluginServers", plugin_warnings: "pluginWarnings", project_config_path: "projectConfigPath" if is_none, project_inspection_error: "projectInspectionError" if is_none, project_problems: "projectProblems", project_servers: "projectServers", user_config_error: "userConfigError" if is_none, user_config_path: "userConfigPath", user_disabled_servers: "userDisabledServers", user_servers: "userServers", warnings: "warnings" });

/// Parameters for a process-wide, read-only language-server inventory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspGlobalConfigurationRequest {
    /// Absolute working directory used for project and plugin discovery.
    pub working_directory: String,
}
crate::record_serde::wire_record!(LspGlobalConfigurationRequest {
    working_directory: "workingDirectory"
});

/// Parameters for (re)loading the merged LSP configuration set.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspInitializeRequest {
    /// Force re-initialization even when LSP configs were already loaded for the working directory.
    pub force: Option<bool>,
    /// Git root used as the boundary when traversing for project-level LSP configs (supports monorepos).
    pub git_root: Option<String>,
    /// Working directory used to load project-level LSP configs. Defaults to the session working directory when omitted.
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(LspInitializeRequest { force: "force" if is_none, git_root: "gitRoot" if is_none, working_directory: "workingDirectory" if is_none });

/// A server selected for a startup probe.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspProbeRequest {
    /// Configured server identifier.
    pub server_id: String,
}
crate::record_serde::wire_record!(LspProbeRequest {
    server_id: "serverId"
});

/// Result of a language-server startup probe.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspProbeResult {
    /// Effective server identifiers, used when the requested server is absent.
    pub available_servers: Vec<String>,
    /// Child exit observed during the startup window.
    pub immediate_exit: Option<LspProbeExitStatus>,
    /// Child process identifier, or null when no process identifier exists.
    pub pid: Option<i64>,
    /// Whether the runtime confined the probe in the session sandbox.
    pub sandboxed: bool,
    /// Time spent on process startup, in milliseconds.
    pub spawn_time_ms: i64,
    /// Runtime selection outcome: started, disabled, missing, or unavailable.
    pub status: String,
}
crate::record_serde::wire_record!(LspProbeResult { available_servers: "availableServers", immediate_exit: "immediateExit" if is_none, pid: "pid", sandboxed: "sandboxed", spawn_time_ms: "spawnTimeMs", status: "status" });

/// A persistent user-level server toggle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspSetEnabledRequest {
    /// True to remove the user-level disable override.
    pub enabled: bool,
    /// Configured server identifier.
    pub server_id: String,
}
crate::record_serde::wire_record!(LspSetEnabledRequest {
    enabled: "enabled",
    server_id: "serverId"
});

/// The servers retained by the session after warmup.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspWarmupResult {
    /// Individual server startup failures.
    pub errors: Vec<LspWarmupError>,
    /// Successfully acquired server identifiers.
    pub servers: Vec<String>,
}
crate::record_serde::wire_record!(LspWarmupResult {
    errors: "errors",
    servers: "servers"
});

/// Result of registering a new marketplace.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MarketplaceAddResult {
    /// Final name of the marketplace as resolved from its manifest
    pub name: String,
}
crate::record_serde::wire_record!(MarketplaceAddResult { name: "name" });

/// Plugins advertised by the marketplace.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MarketplaceBrowseResult {
    /// Plugins advertised by the marketplace
    pub plugins: Vec<MarketplacePluginInfo>,
}
crate::record_serde::wire_record!(MarketplaceBrowseResult { plugins: "plugins" });

/// All registered marketplaces, including built-in defaults.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MarketplaceListResult {
    /// Registered marketplaces
    pub marketplaces: Vec<MarketplaceInfo>,
}
crate::record_serde::wire_record!(MarketplaceListResult {
    marketplaces: "marketplaces"
});

/// The retired marketplace migration result for the runtime's configured profile.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MarketplaceMigrationResult {
    /// The marketplace registered by this call. Present only when status is migrated.
    pub marketplace: Option<String>,
    /// Whether the migration changed the profile or why it left the profile unchanged.
    pub status: MarketplaceMigrationStatus,
}
crate::record_serde::wire_record!(MarketplaceMigrationResult { marketplace: "marketplace" if is_none, status: "status" });

/// Result of refreshing one or more marketplace catalogs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MarketplaceRefreshResult {
    /// Per-marketplace refresh results in deterministic order.
    pub results: Vec<MarketplaceRefreshEntry>,
}
crate::record_serde::wire_record!(MarketplaceRefreshResult { results: "results" });

/// Outcome of the remove attempt, including dependent-plugin info when applicable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MarketplaceRemoveResult {
    /// Names of installed plugins that prevented removal. Populated only when `removed=false`.
    pub dependent_plugins: Option<Vec<String>>,
    /// True when the marketplace was actually removed. False when removal was skipped because the marketplace has dependent plugins and `force` was not set.
    pub removed: bool,
}
crate::record_serde::wire_record!(MarketplaceRemoveResult { dependent_plugins: "dependentPlugins" if is_none, removed: "removed" });

/// Metadata controlling an MCP task's lifetime.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpTaskMetadata {
    /// Task time-to-live.
    pub ttl: Option<i64>,
}
crate::record_serde::wire_record!(McpTaskMetadata { ttl: "ttl" if is_none });

/// A memory the agent stored after validation, secret filtering and approval.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MemoryStoreRequest {
    /// Target session identifier
    pub session_id: SessionId,
    /// Short topic the memory is about.
    pub subject: String,
    /// The fact to remember.
    pub fact: String,
    /// Sources supporting the fact, such as file paths with line numbers or a quotation of the user.
    pub citations: String,
    /// Why the agent considers the fact worth remembering.
    pub reason: String,
}
crate::record_serde::wire_record!(MemoryStoreRequest {
    session_id: "sessionId",
    subject: "subject",
    fact: "fact",
    citations: "citations",
    reason: "reason"
});

/// The six normalized `/context` header buckets, computed from the same tokenization as `entries` so the two never disagree. Convenience rollups: `freeSpace` and `buffer` describe window capacity rather than occupied context, so the values do not sum to `totalTokens`.
#[derive(Debug, Clone, Default)]
pub struct MetadataContextAttributionResultContextAttributionCategories {
    /// Overlapping output reservation plus post-blocking-threshold buffer.
    pub buffer: i64,
    /// Custom-instructions tokens (0 when none are configured).
    pub custom_instructions: i64,
    /// Remaining unused window capacity (clamped at 0).
    pub free_space: i64,
    /// MCP tool-definition tokens.
    pub mcp_tools: i64,
    /// Conversation (user/assistant/tool) message tokens.
    pub messages: i64,
    /// System prompt tokens, excluding custom instructions.
    pub system_prompt: i64,
    /// Non-MCP tool-definition tokens.
    pub system_tools: i64,
}
crate::record_serde::wire_record!(
    MetadataContextAttributionResultContextAttributionCategories {
        buffer: "buffer",
        custom_instructions: "customInstructions",
        free_space: "freeSpace",
        mcp_tools: "mcpTools",
        messages: "messages",
        system_prompt: "systemPrompt",
        system_tools: "systemTools"
    }
);

/// Successful compaction history for the session.
#[derive(Debug, Clone, Default)]
pub struct MetadataContextAttributionResultContextAttributionCompactions {
    /// Number of successful compactions in this session.
    pub count: i64,
}
crate::record_serde::wire_record!(
    MetadataContextAttributionResultContextAttributionCompactions { count: "count" }
);

#[derive(Debug, Clone, Default)]
pub struct MetadataContextAttributionResultContextAttributionEntriesItem {
    /// Supplementary per-entry metadata (e.g. `messageCount`, `role`, `evictable`, `pluginSource`). Values are stringified; parse as needed and ignore unrecognized keys.
    pub attributes: Option<HashMap<String, String>>,
    /// Identifier for this entry, formed by joining its `kind` and source name (e.g. `tool:bash`, `skill:tmux`, `toolDefinition:bash`); unique within the snapshot. Use it to match the same entry across snapshots, to correlate with other APIs (skill/agent/MCP registries), and as the `parentId` target for nesting. Distinct from the human-facing `label`.
    pub id: String,
    /// Source category for this entry. Not a closed set — tolerate unknown values. Known values today: `skill`, `subagent`, `mcpServer`, `tool`, `system`, `toolDefinition`, `plugin`.
    pub kind: String,
    /// Human-readable display label, e.g. `bash` or `skill: tmux`. Presentation-only; may be localized/reformatted without notice — do not key off it.
    pub label: String,
    /// Optional `id` of the parent entry: e.g. a `plugin` entry parenting its `skill`/`mcpServer` entries, or the `system` entry parenting `toolDefinition` entries. Omitted for top-level entries.
    pub parent_id: Option<String>,
    /// Token count currently in context attributable to this entry.
    pub tokens: i64,
}
crate::record_serde::wire_record!(MetadataContextAttributionResultContextAttributionEntriesItem { attributes: "attributes" if is_none, id: "id", kind: "kind", label: "label", parent_id: "parentId" if is_none, tokens: "tokens" });

/// Per-source token attribution snapshot for the current context window. The heaviest individual messages are available separately via `metadata.getContextHeaviestMessages`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MetadataContextAttributionResultContextAttribution {
    /// Output reservation overlapping the displayed prompt allowance plus the tokens past the effective input budget's buffer-exhaustion blocking threshold. Mirrors `SessionContextInfo.bufferTokens`.
    pub buffer_tokens: i64,
    /// The six normalized `/context` header buckets, computed from the same tokenization as `entries` so the two never disagree. Convenience rollups: `freeSpace` and `buffer` describe window capacity rather than occupied context, so the values do not sum to `totalTokens`.
    pub categories: MetadataContextAttributionResultContextAttributionCategories,
    /// Successful compaction history for the session.
    pub compactions: MetadataContextAttributionResultContextAttributionCompactions,
    /// Token count at which background compaction starts. Mirrors `SessionContextInfo.compactionThreshold`.
    pub compaction_threshold: i64,
    /// Flat list of per-source attribution entries. Group by `kind` and render unrecognized kinds generically. Nesting and rollups are expressed via `parentId`.
    pub entries: Vec<MetadataContextAttributionResultContextAttributionEntriesItem>,
    /// Advertised prompt allowance for the selected context tier: the denominator for context-usage displays and capacity for `categories.freeSpace` and `categories.buffer`. Mirrors `SessionContextInfo.limit`.
    pub limit: i64,
    /// The concrete model id the entire breakdown was tokenized against (feeds the per-model token multiplier). Under `Auto` (Free/Student) this is the resolved model, not the literal `auto` sentinel, so totals are not undercounted. A single-model approximation of a potentially multi-model Auto session.
    pub model_id: String,
    /// How `modelId` was chosen. Not a closed set — tolerate unknown values. Known values today: `autoResolved` (the model Auto resolved to), `selected` (the user's explicitly selected model), `default` (a fallback before any model is known).
    pub model_source: String,
    /// Effective input budget after reserving requested output against the combined context ceiling. Mirrors `SessionContextInfo.promptTokenLimit`.
    pub prompt_token_limit: i64,
    /// Total token count of the current context window the entries are measured against (system message + conversation messages + tool definitions — the same total reported by /context). Divide an entry's `tokens` by this to derive its share.
    pub total_tokens: i64,
}

/// Per-source attribution breakdown for the session's current context window, or null if uninitialized.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataContextAttributionResult {
    /// Per-source context-window attribution, or null if the session has not yet been initialized (no system prompt or tool metadata cached).
    pub context_attribution: Option<MetadataContextAttributionResultContextAttribution>,
}
crate::record_serde::wire_record!(MetadataContextAttributionResult {
    context_attribution: "contextAttribution"
});

/// Parameters for the heaviest-messages query.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataContextHeaviestMessagesRequest {
    /// Maximum number of messages to return, most-expensive first. Omit for the server default.
    pub limit: Option<i64>,
}
crate::record_serde::wire_record!(MetadataContextHeaviestMessagesRequest { limit: "limit" if is_none });

/// The heaviest individual messages in the session's context window, most-expensive first.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataContextHeaviestMessagesResult {
    /// Heaviest messages, most-expensive first.
    pub messages: Vec<ContextHeaviestMessage>,
    /// Total token count of the current context window, so callers can compute each message's share without a second call.
    pub total_tokens: i64,
}
crate::record_serde::wire_record!(MetadataContextHeaviestMessagesResult {
    messages: "messages",
    total_tokens: "totalTokens"
});

/// Model identifier and token limits used to compute the context-info breakdown.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataContextInfoRequest {
    /// Requested output allowance to reserve against the combined context ceiling. Pass 0 to resolve the session's request cap, falling back to the model's advertised output limit.
    pub output_token_limit: i64,
    /// Advertised prompt allowance. Pass 0 to resolve the selected model and context tier from the session.
    pub prompt_token_limit: i64,
    /// Provider owning selectedModel. Omit to use the current provider for the current model, or deterministic legacy lookup for a different model.
    pub provider_id: Option<String>,
    /// Whether a zero outputTokenLimit may resolve a live provider's request cap. Defaults to true. Set false for a cached-only measurement that must not start a managed provider; capacity values use advertised limits, while occupied token counts are unchanged.
    pub resolve_output_token_limit: Option<bool>,
    /// Model identifier used for tokenization. Omit to use the session default. Used both for token counting and to compute display values.
    pub selected_model: Option<String>,
}
crate::record_serde::wire_record!(MetadataContextInfoRequest { output_token_limit: "outputTokenLimit", prompt_token_limit: "promptTokenLimit", provider_id: "providerId" if is_none, resolve_output_token_limit: "resolveOutputTokenLimit" if is_none, selected_model: "selectedModel" if is_none });

/// Token-usage breakdown for the session's current context window
#[derive(Debug, Clone, Default)]
pub struct MetadataContextInfoResultContextInfo {
    /// Output reservation overlapping the displayed prompt allowance plus tokens after the effective input budget's buffer-exhaustion blocking threshold (default 95%).
    pub buffer_tokens: i64,
    /// Token count at which background compaction starts (configurable percentage of promptTokenLimit)
    pub compaction_threshold: i64,
    /// Tokens consumed by user/assistant/tool messages
    pub conversation_tokens: i64,
    /// Provider-owned model display label, or the Auto routing label. Presentation only; modelName remains the tokenization identity.
    pub display_model_name: Option<String>,
    /// Advertised prompt allowance for the selected context tier, without adding output tokens. The denominator for context-usage displays.
    pub limit: i64,
    /// Tokens consumed by MCP tool definitions (subset of toolDefinitionsTokens, excludes deferred tools)
    pub mcp_tools_tokens: i64,
    /// The model used for token counting
    pub model_name: String,
    /// Effective input budget: the selected tier's prompt allowance bounded by the combined context ceiling minus the requested output allowance. Uses DEFAULT_TOKEN_LIMIT when limits are unspecified.
    pub prompt_token_limit: i64,
    /// Provider whose catalog supplies this context window. Omitted when provider identity is unavailable.
    pub provider: Option<ModelProviderRef>,
    /// Tokens consumed by the system prompt
    pub system_tokens: i64,
    /// Tokens consumed by tool definitions sent to the model (excludes deferred tools)
    pub tool_definitions_tokens: i64,
    /// Sum of system, conversation and tool-definition tokens
    pub total_tokens: i64,
}
crate::record_serde::wire_record!(MetadataContextInfoResultContextInfo { buffer_tokens: "bufferTokens", compaction_threshold: "compactionThreshold", conversation_tokens: "conversationTokens", display_model_name: "displayModelName" if is_none, limit: "limit", mcp_tools_tokens: "mcpToolsTokens", model_name: "modelName", prompt_token_limit: "promptTokenLimit", provider: "provider" if is_none, system_tokens: "systemTokens", tool_definitions_tokens: "toolDefinitionsTokens", total_tokens: "totalTokens" });

/// Token breakdown for the session's current context window, or null if uninitialized.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataContextInfoResult {
    /// Token breakdown for the current context window, or null if the session has not yet been initialized (no system prompt or tool metadata cached).
    pub context_info: Option<MetadataContextInfoResultContextInfo>,
}
crate::record_serde::wire_record!(MetadataContextInfoResult {
    context_info: "contextInfo"
});

/// Working directory context options. Git access follows the original session host's authority.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataGetWorkingDirectoryContextRequest {
    /// Requested working directory, echoed without normalization.
    pub cwd: String,
    /// Resolve the default branch when true; defaults to false.
    pub include_default_branch: Option<bool>,
}
crate::record_serde::wire_record!(MetadataGetWorkingDirectoryContextRequest { cwd: "cwd", include_default_branch: "includeDefaultBranch" if is_none });

/// Session host's working directory and optional repository context.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataGetWorkingDirectoryContextResult {
    /// Merge-base commit, when available.
    pub base_commit: Option<String>,
    /// Current branch, when available.
    pub branch: Option<String>,
    /// Requested working directory.
    pub cwd: String,
    /// Default branch, only when requested and available.
    pub default_branch: Option<String>,
    /// Repository working tree root.
    pub git_root: Option<String>,
    /// Current HEAD commit, when available.
    pub head_commit: Option<String>,
    /// Recognized repository host platform.
    pub host_type: Option<SessionWorkingDirectoryContextHostType>,
    /// Repository identifier from the existing Git owner.
    pub repository: Option<String>,
    /// Repository remote host.
    pub repository_host: Option<String>,
}
crate::record_serde::wire_record!(MetadataGetWorkingDirectoryContextResult { base_commit: "baseCommit" if is_none, branch: "branch" if is_none, cwd: "cwd", default_branch: "defaultBranch" if is_none, git_root: "gitRoot" if is_none, head_commit: "headCommit" if is_none, host_type: "hostType" if is_none, repository: "repository" if is_none, repository_host: "repositoryHost" if is_none });

/// Indicates whether the local session is currently processing a turn or background continuation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataIsProcessingResult {
    /// Whether the session is currently processing user/agent messages. False for non-local sessions (which don't run a local agentic loop). Reflects an in-flight turn or background continuation.
    pub processing: bool,
}
crate::record_serde::wire_record!(MetadataIsProcessingResult {
    processing: "processing"
});

/// Model identifier to use when re-tokenizing the session's existing messages.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataRecomputeContextTokensRequest {
    /// Model identifier used for tokenization. The runtime token-counts both chat-context and system-context messages against this model.
    pub model_id: String,
}
crate::record_serde::wire_record!(MetadataRecomputeContextTokensRequest {
    model_id: "modelId"
});

/// Re-tokenize the session's existing messages against `modelId` and return the token totals. Useful for hosts that want an initial estimate of context usage on session resume, before the next agent turn fires `session.usage_info` events. Returns zeros for an empty session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataRecomputeContextTokensResult {
    /// Tokens contributed by user/assistant/tool messages (excludes system/developer prompts).
    pub messages_token_count: i64,
    /// Tokens contributed by system/developer prompt snapshots.
    pub system_token_count: i64,
    /// Sum of tokens across chat-context and system-context messages currently held by the session.
    pub total_tokens: i64,
}
crate::record_serde::wire_record!(MetadataRecomputeContextTokensResult {
    messages_token_count: "messagesTokenCount",
    system_token_count: "systemTokenCount",
    total_tokens: "totalTokens"
});

/// Updated working directory and git context. Emitted as the new payload of `session.context_changed`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionWorkingDirectoryContext {
    /// Merge-base commit SHA (fork point from the remote default branch)
    pub base_commit: Option<String>,
    /// Current git branch name
    pub branch: Option<String>,
    /// Current working directory path
    pub cwd: String,
    /// Root directory of the git repository, resolved via git rev-parse
    pub git_root: Option<String>,
    /// Head commit of the current git branch
    pub head_commit: Option<String>,
    /// Hosting platform type of the repository
    pub host_type: Option<SessionWorkingDirectoryContextHostType>,
    /// Repository identifier derived from the git remote URL ("owner/name" for GitHub, "org/project/repo" for Azure DevOps)
    pub repository: Option<String>,
    /// Raw host string from the git remote URL (e.g. "github.com", "dev.azure.com")
    pub repository_host: Option<String>,
}
crate::record_serde::wire_record!(SessionWorkingDirectoryContext { base_commit: "baseCommit" if is_none, branch: "branch" if is_none, cwd: "cwd", git_root: "gitRoot" if is_none, head_commit: "headCommit" if is_none, host_type: "hostType" if is_none, repository: "repository" if is_none, repository_host: "repositoryHost" if is_none });

/// Updated working-directory/git context to record on the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataRecordContextChangeRequest {
    /// Updated working directory and git context. Emitted as the new payload of `session.context_changed`.
    pub context: SessionWorkingDirectoryContext,
}
crate::record_serde::wire_record!(MetadataRecordContextChangeRequest { context: "context" });

/// Notify the session that its working directory context has changed. Emits a `session.context_changed` event so consumers (telemetry, OTel tracker, ACP, the timeline UI) can react. Use this when the host has detected a cwd/branch/repo change outside the session's normal lifecycle (e.g., after a shell command in interactive mode). For a local session, a report whose `cwd` diverges from the session's current working directory is ignored (the call still succeeds but records nothing and emits no event); move a local session's working directory via `metadata.setWorkingDirectory` instead.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MetadataRecordContextChangeResult {}

/// Absolute path to set as the session's new working directory. For local sessions the path must be absolute and exist on disk: it is validated before any session state changes, and a failing validation rejects the call with nothing mutated, persisted, or emitted. Remote sessions record the path as-is.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataSetWorkingDirectoryRequest {
    /// Absolute path to set as the session's working directory. The runtime updates the session's recorded cwd so subsequent operations (shell tools, file lookups, telemetry) anchor to it.
    pub working_directory: String,
}
crate::record_serde::wire_record!(MetadataSetWorkingDirectoryRequest {
    working_directory: "workingDirectory"
});

/// Update the session's working directory. Used by the host when the user explicitly changes cwd (e.g., the `/cd` slash command). The host is responsible for any related side-effects (file index, etc.); it does NOT change the process working directory (a session's cwd is per-session, not process-global). For local sessions the runtime validates the target first (an absolute path that exists on disk) and re-bases the permission primary directory; a rejected validation fails the call before anything is mutated, persisted, or emitted. Location-scoped permission rules are then re-keyed to the new directory (best-effort). Remote sessions only record the path.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataSetWorkingDirectoryResult {
    /// Working directory after the update
    pub working_directory: String,
}
crate::record_serde::wire_record!(MetadataSetWorkingDirectoryResult {
    working_directory: "workingDirectory"
});

/// Atomic patch for client-owned session metadata. Operations apply in clear, remove, then set order. The resulting bag must satisfy the ClientMetadata entry and serialized-size limits. Local storage coordinates concurrent runtime processes; custom SessionFs providers must serialize writers that access the same session from multiple processes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MetadataUpdateClientMetadataRequest {
    /// Remove every existing client metadata entry before applying remove and set. Defaults to false.
    pub clear: Option<bool>,
    /// Case-sensitive keys to remove. Missing keys are ignored. Each key must be non-empty, at most 256 UTF-8 bytes, and outside the reserved `copilot/` and `github/` namespaces.
    pub remove: Option<Vec<String>>,
    /// String entries to add or replace. Set wins when a key also appears in remove. Each key must be non-empty, at most 256 UTF-8 bytes, and outside the reserved `copilot/` and `github/` namespaces. Each value may contain at most 16 KiB of UTF-8 data.
    pub set: Option<HashMap<String, String>>,
}
crate::record_serde::wire_record!(MetadataUpdateClientMetadataRequest { clear: "clear" if is_none, remove: "remove" if is_none, set: "set" if is_none });

/// Agent interaction mode to apply to the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModeSetRequest {
    /// Explicit response to a model-switch compaction preflight.
    pub compaction_decision: Option<String>,
    /// Mode the session must currently be in for the change to apply. When set and the session is in a different mode the request is a no-op and reports status 'unchanged'.
    pub expected_mode: Option<SessionMode>,
    /// Session whose plan-mode base state should be inherited.
    pub inherit_plan_base_from_session_id: Option<String>,
    /// The session mode the agent is operating in
    pub mode: SessionMode,
    /// Whether the selected plan model should be persisted.
    pub persist_plan_selection: Option<bool>,
    /// Settings context used when persisting the selected plan model.
    pub picker_settings_context: Option<ModelPickerSettingsContext>,
    /// Context tier to use with the dedicated plan model.
    pub plan_context_tier: Option<String>,
    /// Action to perform when leaving plan mode.
    pub plan_exit_action: Option<String>,
    /// Dedicated model to use in plan mode, when configured.
    pub plan_model: Option<String>,
    /// Whether a dedicated plan model is configured.
    pub plan_model_configured: Option<bool>,
    /// Provider owning planModel. Omit for legacy bare-model selection.
    pub plan_model_provider_id: Option<String>,
    /// Reasoning effort to use with the dedicated plan model.
    pub plan_reasoning_effort: Option<String>,
    /// Whether leaving plan mode should restore the session's previous model.
    pub restore_plan_model: Option<bool>,
}
crate::record_serde::wire_record!(ModeSetRequest { compaction_decision: "compactionDecision" if is_none, expected_mode: "expectedMode" if is_none, inherit_plan_base_from_session_id: "inheritPlanBaseFromSessionId" if is_none, mode: "mode", persist_plan_selection: "persistPlanSelection" if is_none, picker_settings_context: "pickerSettingsContext" if is_none, plan_context_tier: "planContextTier" if is_none, plan_exit_action: "planExitAction" if is_none, plan_model: "planModel" if is_none, plan_model_configured: "planModelConfigured" if is_none, plan_model_provider_id: "planModelProviderId" if is_none, plan_reasoning_effort: "planReasoningEffort" if is_none, restore_plan_model: "restorePlanModel" if is_none });

/// Outcome of a session mode change, including any model switch it triggered and follow-up the host must perform.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModeSetResult {
    /// Whether the host should arm an interactive continuation after the mode change.
    pub arm_interactive_continuation: Option<bool>,
    /// Compaction confirmation required before the mode change can complete.
    pub confirmation: Option<ModelSwitchConfirmation>,
    /// Whether the host must defer implementing the requested mode change.
    pub defer_implementation: Option<bool>,
    /// Deprecation warnings associated with the model selected by the mode change.
    pub deprecation_warnings: Option<Vec<String>>,
    /// User-facing outcome message for the model switch triggered by the mode change.
    pub message: Option<String>,
    /// Whether the requested mode was applied to the session. False only when an 'expectedMode' precondition did not hold, in which case any model change reported alongside it was still applied.
    pub mode_applied: Option<bool>,
    /// Whether applying the mode changed the active model.
    pub model_changed: bool,
    /// Lifecycle status of the requested mode change.
    pub status: String,
    /// User-facing warning produced while applying the mode change.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(ModeSetResult { arm_interactive_continuation: "armInteractiveContinuation" if is_none, confirmation: "confirmation" if is_none, defer_implementation: "deferImplementation" if is_none, deprecation_warnings: "deprecationWarnings" if is_none, message: "message" if is_none, mode_applied: "modeApplied" if is_none, model_changed: "modelChanged", status: "status", warning: "warning" if is_none });

/// Result of moving in-flight MCP loading to the background.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct MoveMcpLoadingToBackgroundResult {
    /// Whether an in-flight MCP load was moved to the background, releasing turns that were waiting on it. False when no MCP load was in flight or the waiting turns had already been released.
    pub moved_to_background: bool,
}
crate::record_serde::wire_record!(MoveMcpLoadingToBackgroundResult {
    moved_to_background: "movedToBackground"
});

/// The session's friendly name, or null when not yet set.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct NameGetResult {
    /// The session name (user-set or auto-generated), or null if not yet set
    pub name: Option<String>,
}
crate::record_serde::wire_record!(NameGetResult { name: "name" });

/// Auto-generated session summary to apply as the session's name when no user-set name exists.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct NameSetAutoRequest {
    /// Auto-generated session summary. Empty/whitespace-only values are ignored; values are trimmed before persisting.
    pub summary: String,
}
crate::record_serde::wire_record!(NameSetAutoRequest { summary: "summary" });

/// Indicates whether the auto-generated summary was applied as the session's name.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct NameSetAutoResult {
    /// Whether the auto-generated summary was persisted. False if the session already has a user-set name, the summary normalized to empty, the normalized summary already is the session's name, or the session does not have a workspace.
    pub applied: bool,
}
crate::record_serde::wire_record!(NameSetAutoResult { applied: "applied" });

/// New friendly name to apply to the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct NameSetRequest {
    /// New session name (1–100 characters, trimmed of leading/trailing whitespace)
    pub name: String,
}
crate::record_serde::wire_record!(NameSetRequest { name: "name" });

/// Registers the Basic authentication callback, which then supplies the credentials for proxies that challenge with Basic authentication.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NetworkAuthenticateBasicOptions {}

/// Requests credentials for a proxy's Basic authentication challenge.
#[derive(Debug, Clone, Default)]
pub struct NetworkAuthenticateBasicRequest {
    /// True when the proxy rejected the credentials the connection sent last, including ones the runtime remembered from an earlier answer: answer with different credentials, for example by asking the user, or with none. False when the connection has not sent credentials yet.
    pub previous_rejected: bool,
    /// Proxy that challenged, as `http://host:port` or `https://host:port`.
    pub proxy: String,
    /// Realm the proxy named in its challenge, if any. The proxy supplies it, so treat it as untrusted text.
    pub realm: Option<String>,
}
crate::record_serde::wire_record!(NetworkAuthenticateBasicRequest { previous_rejected: "previousRejected", proxy: "proxy", realm: "realm" if is_none });

/// Credentials for a proxy's Basic authentication challenge.
#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NetworkAuthenticateBasicResult {
    /// Password; omitted for an empty password. Must not contain control characters. Never log or persist it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub password: Option<String>,
    /// User name; omitted or empty when no credentials are available, which fails the request with the proxy's challenge. Must not contain a colon or control characters. Never log or persist it with the password.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub username: Option<String>,
}

impl std::fmt::Debug for NetworkAuthenticateBasicResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NetworkAuthenticateBasicResult")
            .field("password", &self.password.as_ref().map(|_| "<redacted>"))
            .field("username", &self.username)
            .finish()
    }
}

/// Registers the Negotiate authentication callback, which then supplies the Kerberos tokens instead of native credential lookup.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NetworkAuthenticateNegotiateOptions {}

/// Requests a fresh initial SPNEGO token for a Kerberos service principal.
#[derive(Debug, Clone, Default)]
pub struct NetworkAuthenticateNegotiateRequest {
    /// Service principal in the platform-independent `service/host` form, optionally followed by `@REALM`.
    pub spn: String,
}
crate::record_serde::wire_record!(NetworkAuthenticateNegotiateRequest { spn: "spn" });

/// Client credentials for a single Negotiate authentication exchange.
#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NetworkAuthenticateNegotiateResult {
    /// Base64 initial SPNEGO token without the Negotiate prefix; omitted when unavailable. Never log or persist it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub token: Option<String>,
}

impl std::fmt::Debug for NetworkAuthenticateNegotiateResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NetworkAuthenticateNegotiateResult")
            .field("token", &self.token.as_ref().map(|_| "<redacted>"))
            .finish()
    }
}

/// Registers the proxy resolution callback, which then decides the proxies for every request the runtime would otherwise route by ambient proxy settings, or leaves a request to that routing.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NetworkResolveProxyOptions {}

/// Requests the proxies for a request target.
#[derive(Debug, Clone, Default)]
pub struct NetworkResolveProxyRequest {
    /// Request target as a PAC script sees it: without credentials or fragment, and for `https` and `wss` also without path and query.
    pub url: String,
}
crate::record_serde::wire_record!(NetworkResolveProxyRequest { url: "url" });

/// Proxies for a request target, in the order to try them.
#[derive(Debug, Clone, Default)]
pub struct NetworkResolveProxyResult {
    /// PAC-style proxy list such as `PROXY proxy.example:8080; HTTPS proxy.example:443; DIRECT`, as returned by a PAC script or Electron's `session.resolveProxy`; `DIRECT` connects directly. Omitted, null or empty leaves the request to the runtime's own proxy resolution, as if no callback were registered: proxy environment variables, `NO_PROXY`, then the operating system's proxy settings and PAC. Entry types the runtime doesn't support, such as SOCKS and QUIC, are skipped.
    pub proxy: Option<String>,
}
crate::record_serde::wire_record!(NetworkResolveProxyResult { proxy: "proxy" if is_none });

/// Registers the calling SDK connection's network callbacks, replacing any previous registration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct NetworkSetProviderRequest {
    /// Present when the client answers `network.authenticateBasic`.
    pub authenticate_basic: Option<NetworkAuthenticateBasicOptions>,
    /// Present when the client answers `network.authenticateNegotiate`.
    pub authenticate_negotiate: Option<NetworkAuthenticateNegotiateOptions>,
    /// Present when the client answers `network.resolveProxy`.
    pub resolve_proxy: Option<NetworkResolveProxyOptions>,
    /// Traffic the callbacks serve; defaults to `connection`.
    pub scope: Option<NetworkScope>,
}
crate::record_serde::wire_record!(NetworkSetProviderRequest { authenticate_basic: "authenticateBasic" if is_none, authenticate_negotiate: "authenticateNegotiate" if is_none, resolve_proxy: "resolveProxy" if is_none, scope: "scope" if is_none });

/// External tool calls still waiting for a result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PendingExternalToolRequestList {
    /// External tool calls of the session and its sub-agents that session.tools.handlePendingToolCall would still accept, in the order they were requested. Includes calls that were pending when the session was last suspended and resumed with pending work continued, so a host that did not observe the original external_tool.requested event can answer them.
    pub items: Vec<PendingExternalToolRequest>,
}
crate::record_serde::wire_record!(PendingExternalToolRequestList { items: "items" });

/// List of pending permission requests reconstructed from event history.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PendingPermissionRequestList {
    /// Pending permission prompts reconstructed from the session's event history. Equivalent to the set of `permission.requested` events that have not yet been followed by a matching `permission.completed` event. Used by clients (e.g. the CLI) to hydrate UI for prompts that were emitted before the client attached to the session.
    pub items: Vec<PendingPermissionRequest>,
}
crate::record_serde::wire_record!(PendingPermissionRequestList { items: "items" });

/// A complete JSON snapshot to write under the runtime's per-path file lock.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PersistenceWriteRawJsonLockedRequest {
    /// The serialized JSON document to write. It replaces the file's contents entirely; nothing is merged.
    pub data_json: String,
    /// Comment lines to emit above the document, for a format that tolerates them. Omit for strict JSON.
    pub file_header: Option<String>,
    /// Absolute path of the file to replace. Parent directories are created when missing.
    pub path: String,
}
crate::record_serde::wire_record!(PersistenceWriteRawJsonLockedRequest { data_json: "dataJson", file_header: "fileHeader" if is_none, path: "path" });

/// The outcome of a locked snapshot write.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PersistenceWriteRawJsonLockedResult {
    /// Why the write failed. Absent when `success` is true. A lock the runtime could not acquire in time reports here rather than raising.
    pub error: Option<String>,
    /// Whether the file now holds the requested contents.
    pub success: bool,
}
crate::record_serde::wire_record!(PersistenceWriteRawJsonLockedResult { error: "error" if is_none, success: "success" });

/// Optional message to echo back to the caller.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PingRequest {
    /// Optional message to echo back
    pub message: Option<String>,
}
crate::record_serde::wire_record!(PingRequest { message: "message" if is_none });

/// Server liveness response, including the echoed message, current server timestamp, and protocol version.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PingResult {
    /// Echoed message (or default greeting)
    pub message: String,
    /// Server protocol version number
    pub protocol_version: i64,
    /// ISO 8601 timestamp when the server handled the ping
    pub timestamp: String,
}
crate::record_serde::wire_record!(PingResult {
    message: "message",
    protocol_version: "protocolVersion",
    timestamp: "timestamp"
});

/// Existence, contents, and resolved path of the session plan file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PlanReadResult {
    /// The content of the plan file, or null if it does not exist
    pub content: Option<String>,
    /// Whether the plan file exists in the workspace
    pub exists: bool,
    /// Absolute file path of the plan file, or null if workspace is not enabled
    pub path: Option<String>,
}
crate::record_serde::wire_record!(PlanReadResult {
    content: "content",
    exists: "exists",
    path: "path"
});

/// Todo rows read from the session SQL database. Empty when no session database is available.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PlanReadSqlTodosResult {
    /// Rows from the session SQL todos table, ordered by creation time with insertion order used to break ties when available and id used for WITHOUT ROWID tables.
    pub rows: Vec<PlanSqlTodosRow>,
}
crate::record_serde::wire_record!(PlanReadSqlTodosResult { rows: "rows" });

/// Todo rows + dependency edges read from the session SQL database.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PlanReadSqlTodosWithDependenciesResult {
    /// Edges from the session SQL todo_deps table. Empty when no database, no todo_deps table, or the SELECT failed. Read independently from `rows`, so a broken todo_deps table does not affect the rows result and vice versa.
    pub dependencies: Vec<PlanSqlTodoDependency>,
    /// Rows from the session SQL todos table, ordered by creation time with insertion order used to break ties when available and id used for WITHOUT ROWID tables. Empty when no database, no todos table, or the SELECT failed.
    pub rows: Vec<PlanSqlTodosRow>,
}
crate::record_serde::wire_record!(PlanReadSqlTodosWithDependenciesResult {
    dependencies: "dependencies",
    rows: "rows"
});

/// Replacement contents to write to the session plan file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PlanUpdateRequest {
    /// The new content for the plan file
    pub content: String,
}
crate::record_serde::wire_record!(PlanUpdateRequest { content: "content" });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProtocolStaticSectionOverride {
    /// Declarative operation applied to the section.
    pub action: ProtocolStaticSectionAction,
    /// Optional content used by replace, append, and prepend operations.
    pub content: Option<String>,
}
crate::record_serde::wire_record!(ProtocolStaticSectionOverride { action: "action", content: "content" if is_none });

/// The `status.get` response, including optional cached error-reporting policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProtocolStatusResponse {
    /// Cached host policy. This field does not grant ownership or confirm network delivery.
    pub host_diagnostics: Option<HostDiagnostics>,
    /// The runtime SDK protocol version.
    pub protocol_version: i64,
    /// The runtime package version.
    pub version: String,
}
crate::record_serde::wire_record!(ProtocolStatusResponse { host_diagnostics: "hostDiagnostics" if is_none, protocol_version: "protocolVersion", version: "version" });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProtocolSystemMessageAppendConfig {
    /// Text appended to the standard system prompt.
    pub content: Option<String>,
    /// Append-mode discriminator. Omission also selects append mode.
    pub mode: Option<ProtocolAppendMode>,
}
crate::record_serde::wire_record!(ProtocolSystemMessageAppendConfig { content: "content" if is_none, mode: "mode" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProtocolSystemMessageCustomizeConfig {
    /// Text appended after the customized sections.
    pub content: Option<String>,
    /// Customize-mode discriminator.
    pub mode: ProtocolCustomizeMode,
    /// Named standard-prompt section overrides.
    pub sections: Option<HashMap<String, serde_json::Value>>,
}
crate::record_serde::wire_record!(ProtocolSystemMessageCustomizeConfig { content: "content" if is_none, mode: "mode", sections: "sections" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SystemMessageBlock {
    /// Whether providers with explicit prompt caching should place a cache breakpoint after this block.
    pub cache_breakpoint: Option<bool>,
    /// Text content for this system-message block.
    pub content: String,
    /// Whether the block is static and may be cached independently of dynamic prompt content.
    pub is_static: Option<bool>,
}
crate::record_serde::wire_record!(SystemMessageBlock { cache_breakpoint: "cacheBreakpoint" if is_none, content: "content", is_static: "isStatic" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProtocolSystemMessageReplaceConfig {
    /// Complete replacement system-message text.
    pub content: String,
    /// Optional structured blocks corresponding to the replacement content.
    pub content_blocks: Option<Vec<SystemMessageBlock>>,
    /// Replace-mode discriminator.
    pub mode: ProtocolReplaceMode,
}
crate::record_serde::wire_record!(ProtocolSystemMessageReplaceConfig { content: "content", content_blocks: "contentBlocks" if is_none, mode: "mode" });

/// Blob attachment with inline base64-encoded data
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentBlob {
    /// Base64-encoded content
    pub data: String,
    /// User-facing display name for the attachment
    pub display_name: Option<String>,
    /// MIME type of the inline data
    pub mime_type: String,
    /// Attachment type discriminator
    pub r#type: PushAttachmentBlobType,
}
crate::record_serde::wire_record!(PushAttachmentBlob { data: "data", display_name: "displayName" if is_none, mime_type: "mimeType", r#type: "type" });

/// Directory attachment
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentDirectory {
    /// User-facing display name for the attachment
    pub display_name: String,
    /// Absolute directory path
    pub path: String,
    /// Attachment type discriminator
    pub r#type: PushAttachmentDirectoryType,
}
crate::record_serde::wire_record!(PushAttachmentDirectory {
    display_name: "displayName",
    path: "path",
    r#type: "type"
});

/// Optional line range to scope the attachment to a specific section of the file
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentFileLineRange {
    /// End line number (1-based, inclusive)
    pub end: i64,
    /// Start line number (1-based)
    pub start: i64,
}
crate::record_serde::wire_record!(PushAttachmentFileLineRange {
    end: "end",
    start: "start"
});

/// File attachment
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentFile {
    /// User-facing display name for the attachment
    pub display_name: String,
    /// Optional line range to scope the attachment to a specific section of the file
    pub line_range: Option<PushAttachmentFileLineRange>,
    /// Absolute file path
    pub path: String,
    /// Attachment type discriminator
    pub r#type: PushAttachmentFileType,
}
crate::record_serde::wire_record!(PushAttachmentFile { display_name: "displayName", line_range: "lineRange" if is_none, path: "path", r#type: "type" });

/// Pointer to a GitHub repository.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushGitHubRepoRef {
    /// Numeric GitHub repository id
    pub id: Option<i64>,
    /// Repository name (without owner)
    pub name: String,
    /// Repository owner login (user or organization)
    pub owner: String,
}
crate::record_serde::wire_record!(PushGitHubRepoRef { id: "id" if is_none, name: "name", owner: "owner" });

/// Pointer to a GitHub Actions job.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubActionsJob {
    /// Terminal conclusion of the job when finished (e.g., success, failure, cancelled). Absent for in-progress jobs.
    pub conclusion: Option<String>,
    /// Job id within the workflow run
    pub job_id: i64,
    /// Display name of the job
    pub job_name: String,
    /// Repository the workflow run belongs to
    pub repo: PushGitHubRepoRef,
    /// Attachment type discriminator
    pub r#type: PushAttachmentGitHubActionsJobType,
    /// URL to the job on GitHub
    pub url: String,
    /// Display name of the workflow the job ran in
    pub workflow_name: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubActionsJob { conclusion: "conclusion" if is_none, job_id: "jobId", job_name: "jobName", repo: "repo", r#type: "type", url: "url", workflow_name: "workflowName" });

/// Pointer to a GitHub commit.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubCommit {
    /// First line of the commit message
    pub message: String,
    /// Full commit SHA
    pub oid: String,
    /// Repository the commit belongs to
    pub repo: PushGitHubRepoRef,
    /// Attachment type discriminator
    pub r#type: PushAttachmentGitHubCommitType,
    /// URL to the commit on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubCommit {
    message: "message",
    oid: "oid",
    repo: "repo",
    r#type: "type",
    url: "url"
});

/// Pointer to a file in a GitHub repository at a specific ref.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubFile {
    /// Repository-relative path to the file
    pub path: String,
    /// Git ref the file is read at (branch, tag, or commit SHA)
    pub r#ref: String,
    /// Repository the file lives in
    pub repo: PushGitHubRepoRef,
    /// Attachment type discriminator
    pub r#type: PushAttachmentGitHubFileType,
    /// URL to the file on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubFile {
    path: "path",
    r#ref: "ref",
    repo: "repo",
    r#type: "type",
    url: "url"
});

/// One side of a file diff (head or base)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubFileDiffSide {
    /// Repository-relative path to the file
    pub path: String,
    /// Git ref (branch, tag, or commit SHA) the file is read at
    pub r#ref: String,
    /// Repository the file lives in
    pub repo: PushGitHubRepoRef,
}
crate::record_serde::wire_record!(PushAttachmentGitHubFileDiffSide {
    path: "path",
    r#ref: "ref",
    repo: "repo"
});

/// Pointer to a single-file diff. At least one of `head` and `base` must be present.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubFileDiff {
    /// File location on the base side of the diff. Absent for additions.
    pub base: Option<PushAttachmentGitHubFileDiffSide>,
    /// File location on the head side of the diff. Absent for deletions.
    pub head: Option<PushAttachmentGitHubFileDiffSide>,
    /// Attachment type discriminator
    pub r#type: PushAttachmentGitHubFileDiffType,
    /// URL to the diff on GitHub (e.g., a commit, compare, or PR-file URL)
    pub url: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubFileDiff { base: "base" if is_none, head: "head" if is_none, r#type: "type", url: "url" });

/// GitHub issue, pull request, or discussion reference
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubReference {
    /// Issue, pull request, or discussion number
    pub number: i64,
    /// Type of GitHub reference
    pub reference_type: PushAttachmentGitHubReferenceType,
    /// Current state of the referenced item (e.g., open, closed, merged)
    pub state: String,
    /// Title of the referenced item
    pub title: String,
    /// Attachment type discriminator
    pub r#type: PushAttachmentGitHubReferenceType,
    /// URL to the referenced item on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubReference {
    number: "number",
    reference_type: "referenceType",
    state: "state",
    title: "title",
    r#type: "type",
    url: "url"
});

/// Pointer to a GitHub release.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubRelease {
    /// Human-readable release name
    pub name: String,
    /// Repository the release belongs to
    pub repo: PushGitHubRepoRef,
    /// Git tag the release is anchored to
    pub tag_name: String,
    /// Attachment type discriminator
    pub r#type: PushAttachmentGitHubReleaseType,
    /// URL to the release on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubRelease {
    name: "name",
    repo: "repo",
    tag_name: "tagName",
    r#type: "type",
    url: "url"
});

/// Pointer to a GitHub repository.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubRepository {
    /// Short description of the repository
    pub description: Option<String>,
    /// Git ref this attachment is anchored at (branch, tag, or commit). When absent the default branch is implied.
    pub r#ref: Option<String>,
    /// Repository pointer
    pub repo: PushGitHubRepoRef,
    /// Attachment type discriminator
    pub r#type: PushAttachmentGitHubRepositoryType,
    /// URL to the repository on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubRepository { description: "description" if is_none, r#ref: "ref" if is_none, repo: "repo", r#type: "type", url: "url" });

/// Pointer to a line range inside a file in a GitHub repository.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubSnippet {
    /// Line range the snippet covers
    pub line_range: PushAttachmentFileLineRange,
    /// Repository-relative path to the file
    pub path: String,
    /// Git ref the file is read at (branch, tag, or commit SHA)
    pub r#ref: String,
    /// Repository the file lives in
    pub repo: PushGitHubRepoRef,
    /// Attachment type discriminator
    pub r#type: PushAttachmentGitHubSnippetType,
    /// URL to the snippet on GitHub (with line anchor)
    pub url: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubSnippet {
    line_range: "lineRange",
    path: "path",
    r#ref: "ref",
    repo: "repo",
    r#type: "type",
    url: "url"
});

/// One side of a tree comparison (head or base)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubTreeComparisonSide {
    /// Repository the revision belongs to
    pub repo: PushGitHubRepoRef,
    /// Git revision (branch, tag, or commit SHA)
    pub revision: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubTreeComparisonSide {
    repo: "repo",
    revision: "revision"
});

/// Pointer to a comparison between two git revisions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubTreeComparison {
    /// Base side of the comparison
    pub base: PushAttachmentGitHubTreeComparisonSide,
    /// Head side of the comparison
    pub head: PushAttachmentGitHubTreeComparisonSide,
    /// Attachment type discriminator
    pub r#type: PushAttachmentGitHubTreeComparisonType,
    /// URL to the comparison on GitHub
    pub url: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubTreeComparison {
    base: "base",
    head: "head",
    r#type: "type",
    url: "url"
});

/// Generic GitHub URL reference.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentGitHubUrl {
    /// Attachment type discriminator
    pub r#type: PushAttachmentGitHubUrlType,
    /// URL to the GitHub resource
    pub url: String,
}
crate::record_serde::wire_record!(PushAttachmentGitHubUrl {
    r#type: "type",
    url: "url"
});

/// End position of the selection
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentSelectionDetailsEnd {
    /// End character offset within the line (0-based)
    pub character: i64,
    /// End line number (0-based)
    pub line: i64,
}
crate::record_serde::wire_record!(PushAttachmentSelectionDetailsEnd {
    character: "character",
    line: "line"
});

/// Start position of the selection
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentSelectionDetailsStart {
    /// Start character offset within the line (0-based)
    pub character: i64,
    /// Start line number (0-based)
    pub line: i64,
}
crate::record_serde::wire_record!(PushAttachmentSelectionDetailsStart {
    character: "character",
    line: "line"
});

/// Position range of the selection within the file
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentSelectionDetails {
    /// End position of the selection
    pub end: PushAttachmentSelectionDetailsEnd,
    /// Start position of the selection
    pub start: PushAttachmentSelectionDetailsStart,
}
crate::record_serde::wire_record!(PushAttachmentSelectionDetails {
    end: "end",
    start: "start"
});

/// Code selection attachment from an editor
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PushAttachmentSelection {
    /// User-facing display name for the selection
    pub display_name: String,
    /// Absolute path to the file containing the selection
    pub file_path: String,
    /// Position range of the selection within the file
    pub selection: PushAttachmentSelectionDetails,
    /// The selected text content
    pub text: String,
    /// Attachment type discriminator
    pub r#type: PushAttachmentSelectionType,
}
crate::record_serde::wire_record!(PushAttachmentSelection {
    display_name: "displayName",
    file_path: "filePath",
    selection: "selection",
    text: "text",
    r#type: "type"
});

/// Append to one pending steering message without changing its identity or delivery position.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueAppendSteeringRequest {
    /// Mode captured at submission. Only steering messages in the same mode may be combined.
    pub agent_mode: SendAgentMode,
    /// Attachments to add after the message's existing attachments. An empty list preserves the existing attachments.
    pub attachments: Vec<serde_json::Value>,
    /// Display text to append to the existing preview after a blank line.
    pub display_prompt: String,
    /// Expected current prompt, including any previous appends. The runtime applies plan-mode normalization before comparing and refuses a changed message.
    pub expected_prompt: String,
    /// Message identity returned by send, not the queue item id. Only unclaimed user steering messages are eligible.
    pub message_id: String,
    /// Text to append after a blank line.
    pub prompt: String,
}
crate::record_serde::wire_record!(QueueAppendSteeringRequest {
    agent_mode: "agentMode",
    attachments: "attachments",
    display_prompt: "displayPrompt",
    expected_prompt: "expectedPrompt",
    message_id: "messageId",
    prompt: "prompt"
});

/// Inputs for starting a deferred-idle drain.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueBeginDeferredIdleDrainRequest {
    /// Whether the host still has active background work.
    pub active_background_work: bool,
}
crate::record_serde::wire_record!(QueueBeginDeferredIdleDrainRequest {
    active_background_work: "activeBackgroundWork"
});

/// Whether a deferred-idle drain should run.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueBeginDeferredIdleDrainResult {
    /// True when the host should run finishDeferredIdleDrain asynchronously.
    pub should_drain: bool,
}
crate::record_serde::wire_record!(QueueBeginDeferredIdleDrainResult {
    should_drain: "shouldDrain"
});

/// Internal filter for consuming queued system notifications.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueConsumeSystemNotificationsRequest {
    /// Opaque runtime-owned filter object.
    pub filter: serde_json::Value,
}
crate::record_serde::wire_record!(QueueConsumeSystemNotificationsRequest { filter: "filter" });

/// Queued-command response indicating the host executed the command, with an optional flag to stop queue processing.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueuedCommandHandled {
    /// The host actually executed the queued command.
    pub handled: bool,
    /// When true, the runtime will not process subsequent queued commands until a new request comes in.
    pub stop_processing_queue: Option<bool>,
}
crate::record_serde::wire_record!(QueuedCommandHandled { handled: "handled", stop_processing_queue: "stopProcessingQueue" if is_none });

/// Queued-command response indicating the host did not execute the command and the queue may continue.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueuedCommandNotHandled {
    /// The host did not execute the queued command. Unblocks the queue without claiming the command was processed (e.g. when the handler threw before completing).
    pub handled: bool,
}
crate::record_serde::wire_record!(QueuedCommandNotHandled { handled: "handled" });

/// Inputs for marking session.idle deferred in native state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueDeferSessionIdleRequest {
    /// Whether the deferred idle was caused by an aborted foreground turn.
    pub aborted: bool,
}
crate::record_serde::wire_record!(QueueDeferSessionIdleRequest { aborted: "aborted" });

/// Parameters for duplicating a queued item.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueDuplicateAtRequest {
    /// Stable opaque ID of the queued item to duplicate.
    pub id: String,
}
crate::record_serde::wire_record!(QueueDuplicateAtRequest { id: "id" });

/// Result of duplicating a queued item.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueDuplicateAtResult {
    /// Fresh stable opaque id assigned to the duplicate.
    pub id: String,
}
crate::record_serde::wire_record!(QueueDuplicateAtResult { id: "id" });

/// Result of enqueueing the resume-pending wake item.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueEnqueueResumePendingResult {
    /// True when a wake item was newly queued.
    pub queued: bool,
}
crate::record_serde::wire_record!(QueueEnqueueResumePendingResult { queued: "queued" });

/// Inputs for completing a deferred-idle drain.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueFinishDeferredIdleDrainRequest {
    /// Whether the host still has active background work.
    pub active_background_work: bool,
    /// Whether native queued work remains.
    pub has_pending: bool,
}
crate::record_serde::wire_record!(QueueFinishDeferredIdleDrainRequest {
    active_background_work: "activeBackgroundWork",
    has_pending: "hasPending"
});

/// Action selected by the native deferred-idle drain.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueFinishDeferredIdleDrainResult {
    /// Whether the deferred idle was caused by an aborted foreground turn.
    pub aborted: bool,
    /// One of none, processQueue, or emitSessionIdle.
    pub action: String,
}
crate::record_serde::wire_record!(QueueFinishDeferredIdleDrainResult {
    aborted: "aborted",
    action: "action"
});

/// Whether the native queue has pending work.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueHasPendingResult {
    /// True when queued or immediate native work is pending.
    pub has_pending: bool,
}
crate::record_serde::wire_record!(QueueHasPendingResult {
    has_pending: "hasPending"
});

/// Serializable message fields accepted by queue.insertAt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueInsertMessage {
    /// Optional explicit agent mode. When omitted, the session's current mode is assigned.
    pub agent_mode: Option<SendAgentMode>,
    /// Optional attachments for the message.
    pub attachments: Option<Vec<serde_json::Value>>,
    /// Whether the message is billable.
    pub billable: Option<bool>,
    /// Accepted for internal SendOptions compatibility but ignored; delivery is derived from current session activity.
    pub delivery: Option<String>,
    /// Optional user-facing display text.
    pub display_prompt: Option<String>,
    /// Accepted for SendOptions compatibility but ignored; inserted items always use queued delivery semantics.
    pub mode: Option<SendMode>,
    /// Accepted for SendOptions compatibility but ignored; the requested public position controls placement.
    pub prepend: Option<bool>,
    /// The user message text.
    pub prompt: String,
    /// Per-turn request headers.
    pub request_headers: Option<HashMap<String, String>>,
    /// Required tool name for the turn, when any.
    pub required_tool: Option<String>,
    /// Optional provenance source. `system` is rejected: it would hide the inserted row from `pendingItems` and make it unaddressable while still executing, so inserted items must stay visible.
    pub source: Option<String>,
    /// Accepted for SendOptions compatibility but ignored; insertion scheduling is controlled by the queue drain state.
    pub wait: Option<bool>,
}
crate::record_serde::wire_record!(QueueInsertMessage { agent_mode: "agentMode" if is_none, attachments: "attachments" if is_none, billable: "billable" if is_none, delivery: "delivery" if is_none, display_prompt: "displayPrompt" if is_none, mode: "mode" if is_none, prepend: "prepend" if is_none, prompt: "prompt", request_headers: "requestHeaders" if is_none, required_tool: "requiredTool" if is_none, source: "source" if is_none, wait: "wait" if is_none });

/// Parameters for inserting a queued message at a public visible position.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueInsertAtRequest {
    /// Queued message contents and delivery metadata.
    pub message: QueueInsertMessage,
    /// Zero-based position in the public visible queue. Values outside the queue clamp to an end.
    pub position: i64,
}
crate::record_serde::wire_record!(QueueInsertAtRequest {
    message: "message",
    position: "position"
});

/// Result of inserting a queued message.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueInsertAtResult {
    /// Fresh stable opaque id assigned to the inserted item.
    pub id: String,
}
crate::record_serde::wire_record!(QueueInsertAtResult { id: "id" });

/// Parameters for moving a queued item by stable id.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueMoveItemRequest {
    /// Stable opaque queued-item id.
    pub id: String,
    /// Zero-based target position in the public visible queue. Values outside the queue clamp to an end.
    pub to_position: i64,
}
crate::record_serde::wire_record!(QueueMoveItemRequest {
    id: "id",
    to_position: "toPosition"
});

/// Result of moving a queued item.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueMoveItemResult {
    /// True when the item changed position; false when it was already at the requested position.
    pub changed: bool,
}
crate::record_serde::wire_record!(QueueMoveItemResult { changed: "changed" });

/// Snapshot of the session's pending queued items and immediate-steering messages.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueuePendingItemsResult {
    /// How many leading entries of `steeringMessages` have already been folded into the running turn (and so have an emitted `user.message`), as opposed to still waiting for one. Absent for hosts that do not distinguish the two.
    pub in_flight_steering_count: Option<i64>,
    /// Pending queued items in submission order. Includes user messages, queued slash commands, and queued model changes; omits internal system items.
    pub items: Vec<QueuePendingItems>,
    /// Display text for messages currently in the immediate steering queue (interjections sent during a running turn).
    pub steering_messages: Vec<String>,
    /// ID of the running turn's user message while the model has not answered it, so `withdrawMessage` can still take it back once nothing sent after it is pending. A message leaves `items` when its turn starts, before its `user.message` is recorded; this tells that message apart from one that was removed. Absent when no turn prompt can be taken back.
    pub withdrawable_turn_message_id: Option<String>,
}
crate::record_serde::wire_record!(QueuePendingItemsResult { in_flight_steering_count: "inFlightSteeringCount" if is_none, items: "items", steering_messages: "steeringMessages", withdrawable_turn_message_id: "withdrawableTurnMessageId" if is_none });

/// Parameters for removing a queued item by stable id.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueRemoveAtRequest {
    /// Stable opaque ID of the queued item to remove.
    pub id: String,
}
crate::record_serde::wire_record!(QueueRemoveAtRequest { id: "id" });

/// Result of removing a queued item.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueRemoveAtResult {
    /// True when the addressed item was removed.
    pub removed: bool,
}
crate::record_serde::wire_record!(QueueRemoveAtResult { removed: "removed" });

/// Indicates whether a user-facing pending item was removed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueRemoveMostRecentResult {
    /// True if a user-facing pending item was removed (LIFO across both queues); false when no removable items remained.
    pub removed: bool,
}
crate::record_serde::wire_record!(QueueRemoveMostRecentResult { removed: "removed" });

/// Parameters for steering a queued message into a live turn.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueSendNowRequest {
    /// Stable opaque ID of the queued item to steer into the live turn.
    pub id: String,
}
crate::record_serde::wire_record!(QueueSendNowRequest { id: "id" });

/// Result of trying to steer a queued message into a live turn.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueSendNowResult {
    /// True when the item was accepted into the steering lane; false when no main turn was live.
    pub steered: bool,
}
crate::record_serde::wire_record!(QueueSendNowResult { steered: "steered" });

/// Parameters for acquiring or releasing the queued-lane drain pause. Acquisition is exclusive and non-idempotent: `paused: true` against an already-paused session fails with `queue_already_paused`. The pause is never released automatically — it is not tied to the caller's lifetime, so a client that exits without sending `paused: false` leaves the lane frozen. Release is unowned: `paused: false` clears the pause for any caller, including one that never acquired it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueSetDrainPausedRequest {
    /// Whether queued-lane draining should be paused.
    pub paused: bool,
}
crate::record_serde::wire_record!(QueueSetDrainPausedRequest { paused: "paused" });

/// Internal snapshot of native queue state for local session orchestration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueSnapshotResult {
    /// Queue item identifier of a model switch that has been dequeued but not yet applied.
    pub in_flight_model_change_id: Option<String>,
    /// Insertion orders for queued items, aligned with `items`.
    pub item_orders: Option<Vec<i64>>,
    /// User-facing pending items in FIFO order.
    pub items: Vec<QueuePendingItems>,
    /// Insertion orders for immediate steering messages, aligned with `steeringMessages`.
    pub steering_message_orders: Option<Vec<i64>>,
    /// Immediate steering messages waiting for an active turn.
    pub steering_messages: Vec<String>,
}
crate::record_serde::wire_record!(QueueSnapshotResult { in_flight_model_change_id: "inFlightModelChangeId" if is_none, item_orders: "itemOrders" if is_none, items: "items", steering_message_orders: "steeringMessageOrders" if is_none, steering_messages: "steeringMessages" });

/// Parameters for editing a single queued message.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueUpdateTextRequest {
    /// Optional replacement prompt displayed to the user.
    pub display_prompt: Option<String>,
    /// Stable opaque ID of the queued item to edit.
    pub id: String,
    /// Replacement prompt sent to the model.
    pub prompt: String,
}
crate::record_serde::wire_record!(QueueUpdateTextRequest { display_prompt: "displayPrompt" if is_none, id: "id", prompt: "prompt" });

/// Result of editing a queued message.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueUpdateTextResult {
    /// True when the stored text changed.
    pub updated: bool,
}
crate::record_serde::wire_record!(QueueUpdateTextResult { updated: "updated" });

/// Conditional withdrawal of a single user message, from its queue or from the running turn it started.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueWithdrawMessageRequest {
    /// The prompt originally sent. A message edited since submission is not withdrawn, so an obsolete draft cannot replace the edit.
    pub expected_prompt: String,
    /// Message identity returned by send, not the queue item id. Batch messages are not eligible.
    pub message_id: String,
}
crate::record_serde::wire_record!(QueueWithdrawMessageRequest {
    expected_prompt: "expectedPrompt",
    message_id: "messageId"
});

/// Result of withdrawing a user message.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct QueueWithdrawMessageResult {
    /// True when the running turn was interrupted to withdraw the message. With removed false, the turn was interrupted but its events could not be removed, for example because the model answered first, so the message stays in the interrupted turn.
    pub interrupted: bool,
    /// True when the message left the queue or, for a running turn, history.
    pub removed: bool,
}
crate::record_serde::wire_record!(QueueWithdrawMessageResult {
    interrupted: "interrupted",
    removed: "removed"
});

/// Event type to register consumer interest for, used by runtime gating logic.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RegisterEventInterestParams {
    /// The event type the consumer wants the runtime to treat as 'observed' for behavior-switching gating. Some runtime code paths inspect whether any consumer is interested in a specific event type and choose a different implementation accordingly (e.g. `mcp.oauth_required`: when interest is registered the runtime delegates interactive OAuth token acquisition to the consumer via `mcp.oauth_required` events; when no interest is registered the runtime still attempts non-interactive reconnect from cached or refreshable tokens, and only marks the server `needs-auth` if usable credentials are unavailable — it does not open a browser or start interactive OAuth without a consumer). SDK clients that long-poll events do NOT automatically appear as listeners to these gating checks — they must explicitly call `registerInterest` for each event type they want the runtime to count as having a consumer. Multiple registrations for the same event type from the same or different consumers are tracked independently and must each be released. See: `mcp.oauth_required`, `sampling.requested`, `auto_mode_switch.requested`, `session_limits_exhausted.requested`, `user_input.requested`, `elicitation.requested`, `command.queued`, `exit_plan_mode.requested`.
    pub event_type: String,
}
crate::record_serde::wire_record!(RegisterEventInterestParams {
    event_type: "eventType"
});

/// Opaque handle representing an event-type interest registration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RegisterEventInterestResult {
    /// Opaque handle for this registration. Pass to releaseInterest to release. Each call to registerInterest produces a fresh handle, even when the same eventType is registered multiple times.
    pub handle: String,
}
crate::record_serde::wire_record!(RegisterEventInterestResult { handle: "handle" });

/// Opaque handle previously returned by `registerInterest` to release.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ReleaseEventInterestParams {
    /// Handle returned by a previous `registerInterest` call. Idempotent: releasing an unknown or already-released handle is a no-op (returns success). When the last outstanding handle for an event type is released, the runtime reverts to its 'no consumer' code path for that event type.
    pub handle: String,
}
crate::record_serde::wire_record!(ReleaseEventInterestParams { handle: "handle" });

/// One atomic edit of an allowed top-level repository setting.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct RepoSettingsEditRequest {
    /// One canonical top-level repository setting that the allowlist permits.
    pub key: String,
    /// Set a scalar, add or remove a string list item, or remove the setting. The handler rejects a missing or incompatible value.
    #[doc(hidden)]
    pub(crate) operation: SettingsEditOperation,
    /// Absolute path of the repository root.
    pub repo_root: String,
    /// Shared or local repository settings file.
    #[doc(hidden)]
    pub(crate) scope: RepoSettingsEditScope,
    /// Required boolean, string or number for set, required string for add-item and remove-item, and forbidden for remove. The runtime rejects all other JSON values.
    pub value: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(RepoSettingsEditRequest { key: "key", operation: "operation", repo_root: "repoRoot", scope: "scope", value: "value" if is_none });

/// Which repository settings file to read.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct RepoSettingsReadRawRequest {
    /// Absolute path of the repository root.
    pub repo_root: String,
    /// Use repo for shared settings or local for settings.local.json. Other scopes are rejected.
    pub scope: String,
}
crate::record_serde::wire_record!(RepoSettingsReadRawRequest {
    repo_root: "repoRoot",
    scope: "scope"
});

/// Parsed repository settings without normalization or layering.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct RepoSettingsReadRawResult {
    /// Stored JSON values after the existing JSONC parser and prototype-safety checks. Empty when the file is missing, blank, or invalid.
    pub config: serde_json::Value,
    /// True when the file exists but did not parse, so `config` is empty for that reason rather than because nothing is set.
    pub invalid: bool,
}
crate::record_serde::wire_record!(RepoSettingsReadRawResult {
    config: "config",
    invalid: "invalid"
});

/// The complete set of repository-overridable settings to store.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct RepoSettingsWriteAllRequest {
    /// Absolute path of the repository root.
    pub repo_root: String,
    /// Use repo for shared settings or local for settings.local.json. Other scopes are rejected.
    pub scope: String,
    /// Object of allowed settings to store. Missing allowlisted keys are removed. Other input keys are ignored, while unknown stored keys are preserved.
    pub settings: serde_json::Value,
}
crate::record_serde::wire_record!(RepoSettingsWriteAllRequest {
    repo_root: "repoRoot",
    scope: "scope",
    settings: "settings"
});

/// One key to write or remove in a repository settings file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct RepoSettingsWriteKeyRequest {
    /// True to remove the key instead of writing it.
    pub delete: Option<bool>,
    /// Canonical top-level key permitted by the repository settings allowlist.
    pub key: String,
    /// Absolute path of the repository root.
    pub repo_root: String,
    /// Use repo for shared settings or local for settings.local.json. Other scopes are rejected.
    pub scope: String,
    /// Value to validate and write. Required unless delete is true; a null value is validated, not treated as deletion.
    pub value: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(RepoSettingsWriteKeyRequest { delete: "delete" if is_none, key: "key", repo_root: "repoRoot", scope: "scope", value: "value" if is_none });

/// Provider-native structured output format. JSON Schema is forwarded without rewriting or validating the schema or the generated output.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ResponseFormat {
    /// JSON Schema and provider options for the turn's output.
    pub json_schema: JsonSchemaResponseFormat,
    /// Output format discriminator. Currently only json_schema is supported.
    pub r#type: ResponseFormatType,
}
crate::record_serde::wire_record!(ResponseFormat {
    json_schema: "jsonSchema",
    r#type: "type"
});

/// An authorization URL for the host browser.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct RuntimeOpenBrowserRequest {
    /// The URL to open.
    pub url: String,
}
crate::record_serde::wire_record!(RuntimeOpenBrowserRequest { url: "url" });

/// Result of adding a model to the saved provider configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationAddModelResult {
    /// True when this operation created the provider row.
    pub provider_added: bool,
}
crate::record_serde::wire_record!(SavedProviderConfigurationAddModelResult {
    provider_added: "providerAdded"
});

/// Saved configuration. Reading does not activate, select, or load a model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationReadResult {
    /// Saved model definitions in file order.
    pub models: Vec<ProviderModelConfig>,
    /// Providers in saved order.
    pub providers: Vec<SavedProviderConfigurationProvider>,
}
crate::record_serde::wire_record!(SavedProviderConfigurationReadResult {
    models: "models",
    providers: "providers"
});

/// Request to remove one saved provider-local model definition.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationRemoveModelRequest {
    /// Provider-local model id to remove.
    pub model_id: String,
    /// Name of the provider that owns the model.
    pub provider_name: String,
}
crate::record_serde::wire_record!(SavedProviderConfigurationRemoveModelRequest {
    model_id: "modelId",
    provider_name: "providerName"
});

/// Result of removing one saved provider model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationRemoveModelResult {
    /// True when removing the final model also removed its provider and managed credential.
    pub provider_removed: bool,
}
crate::record_serde::wire_record!(SavedProviderConfigurationRemoveModelResult {
    provider_removed: "providerRemoved"
});

/// Request to replace one saved provider-local model definition.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationUpdateModelRequest {
    /// Replacement model definition. Its provider and id identify the saved row to replace.
    pub model: ProviderModelConfig,
}
crate::record_serde::wire_record!(SavedProviderConfigurationUpdateModelRequest { model: "model" });

/// Result of updating one saved provider model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationUpdateModelResult {
    /// Provider-local id of the updated model.
    pub model_id: String,
    /// Name of the provider that owns the updated model.
    pub provider_name: String,
}
crate::record_serde::wire_record!(SavedProviderConfigurationUpdateModelResult {
    model_id: "modelId",
    provider_name: "providerName"
});

/// Request to replace shared settings for one saved provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationUpdateProviderRequest {
    /// Optional replacement API key. Stored in the OS keychain and never returned.
    pub credential: Option<String>,
    /// Replacement provider settings. Existing credentials and custom headers are retained; omitted local-model settings are retained.
    pub provider: SavedProviderConfigurationProviderInput,
}
crate::record_serde::wire_record!(SavedProviderConfigurationUpdateProviderRequest { credential: "credential" if is_none, provider: "provider" });

/// Result of updating shared saved provider settings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationUpdateProviderResult {
    /// Name of the provider whose settings were updated.
    pub provider_name: String,
}
crate::record_serde::wire_record!(SavedProviderConfigurationUpdateProviderResult {
    provider_name: "providerName"
});

/// Register an absolute-time scheduled prompt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleAddAtRequest {
    /// Epoch milliseconds when the prompt should fire.
    pub at: i64,
    /// Optional display-only prompt label.
    pub display_prompt: Option<String>,
    /// Prompt text to enqueue when the schedule fires.
    pub prompt: String,
    /// Whether the schedule should re-arm after each tick. Defaults to false.
    pub recurring: Option<bool>,
}
crate::record_serde::wire_record!(ScheduleAddAtRequest { at: "at", display_prompt: "displayPrompt" if is_none, prompt: "prompt", recurring: "recurring" if is_none });

/// Register a cron scheduled prompt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleAddCronRequest {
    /// 5-field cron expression.
    pub cron: String,
    /// Optional display-only prompt label.
    pub display_prompt: Option<String>,
    /// Prompt text to enqueue when the schedule fires.
    pub prompt: String,
    /// Whether the schedule should re-arm after each tick. Defaults to true.
    pub recurring: Option<bool>,
    /// IANA timezone for evaluating the cron expression.
    pub tz: Option<String>,
}
crate::record_serde::wire_record!(ScheduleAddCronRequest { cron: "cron", display_prompt: "displayPrompt" if is_none, prompt: "prompt", recurring: "recurring" if is_none, tz: "tz" if is_none });

/// Register a relative-interval scheduled prompt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleAddRequest {
    /// Optional display-only prompt label.
    pub display_prompt: Option<String>,
    /// Human-readable interval such as `30s`, `5m`, or `2h`.
    pub interval: String,
    /// Prompt text to enqueue when the schedule fires.
    pub prompt: String,
    /// Whether the schedule should re-arm after each tick. Defaults to true.
    pub recurring: Option<bool>,
}
crate::record_serde::wire_record!(ScheduleAddRequest { display_prompt: "displayPrompt" if is_none, interval: "interval", prompt: "prompt", recurring: "recurring" if is_none });

/// Result of registering or re-arming a scheduled prompt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleAddResult {
    /// The registered or updated schedule entry.
    pub entry: Option<ScheduleEntry>,
    /// User-facing validation error, when registration failed.
    pub error: Option<String>,
}
crate::record_serde::wire_record!(ScheduleAddResult { entry: "entry" if is_none, error: "error" if is_none });

/// Register a self-paced scheduled prompt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleAddSelfPacedRequest {
    /// Optional display-only prompt label.
    pub display_prompt: Option<String>,
    /// Prompt text to enqueue when the schedule fires.
    pub prompt: String,
}
crate::record_serde::wire_record!(ScheduleAddSelfPacedRequest { display_prompt: "displayPrompt" if is_none, prompt: "prompt" });

/// Whether the session currently has an active self-paced schedule.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleHasSelfPacedResult {
    /// True when at least one active schedule is self-paced.
    pub has_self_paced: bool,
}
crate::record_serde::wire_record!(ScheduleHasSelfPacedResult {
    has_self_paced: "hasSelfPaced"
});

/// Snapshot of the currently active recurring prompts for this session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleList {
    /// Active scheduled prompts, ordered by id.
    pub entries: Vec<ScheduleEntry>,
}
crate::record_serde::wire_record!(ScheduleList { entries: "entries" });

/// Re-arm a self-paced scheduled prompt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleRearmSelfPacedRequest {
    /// Epoch milliseconds when the prompt should next fire.
    pub at: i64,
    /// Id of the self-paced scheduled prompt.
    pub id: i64,
}
crate::record_serde::wire_record!(ScheduleRearmSelfPacedRequest { at: "at", id: "id" });

/// Identifier of the scheduled prompt to remove.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleStopRequest {
    /// Id of the scheduled prompt to remove.
    pub id: i64,
}
crate::record_serde::wire_record!(ScheduleStopRequest { id: "id" });

/// Remove a scheduled prompt by id. The result entry is omitted if the id was unknown.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ScheduleStopResult {
    /// The removed entry, or omitted if no entry matched.
    pub entry: Option<ScheduleEntry>,
}
crate::record_serde::wire_record!(ScheduleStopResult { entry: "entry" if is_none });

/// Secret values to add to the redaction filter.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SecretsAddFilterValuesRequest {
    /// Raw secret values to register for redaction
    pub values: Vec<String>,
}
crate::record_serde::wire_record!(SecretsAddFilterValuesRequest { values: "values" });

/// Confirmation that the secret values were registered.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SecretsAddFilterValuesResult {
    /// Whether the values were successfully registered
    pub ok: bool,
}
crate::record_serde::wire_record!(SecretsAddFilterValuesResult { ok: "ok" });

/// Content to redact with the runtime's process filter.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SecretsFilterRequest {
    /// Content representation. Defaults to plain text.
    pub format: Option<SecretFilterFormat>,
    /// Content to redact. The runtime never returns its registered secret values.
    pub text: String,
}
crate::record_serde::wire_record!(SecretsFilterRequest { format: "format" if is_none, text: "text" });

/// Content after runtime-owned redaction.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SecretsFilterResult {
    /// Whether the returned content differs from the submitted text.
    pub changed: bool,
    /// Redacted content. JSON mode preserves the runtime's JSON serialization behavior.
    pub value: String,
}
crate::record_serde::wire_record!(SecretsFilterResult {
    changed: "changed",
    value: "value"
});

/// Secret values this client no longer needs redacted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SecretsRemoveFilterValuesRequest {
    /// Raw secret values previously added by this client.
    pub values: Vec<String>,
}
crate::record_serde::wire_record!(SecretsRemoveFilterValuesRequest { values: "values" });

/// Confirmation that this client's registrations were released.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SecretsRemoveFilterValuesResult {
    /// Whether removal completed successfully.
    pub ok: bool,
}
crate::record_serde::wire_record!(SecretsRemoveFilterValuesResult { ok: "ok" });

/// Internal request-correlated completion of one submitted message.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SendAndWaitResult {
    /// Whether the request was aborted or removed before completing normally.
    pub aborted: bool,
    /// Existing event at the completion boundary. SDK helpers drain its synchronous subscriber delivery before returning. Absent for quiet queue removal.
    pub completion_event_id: Option<String>,
    /// Identifier assigned to the submitted user message.
    pub message_id: String,
    /// The exact existing root assistant event owned by this request. Absent when the request completed without an assistant response.
    pub response: Option<SessionEvent>,
}
crate::record_serde::wire_record!(SendAndWaitResult { aborted: "aborted", completion_event_id: "completionEventId" if is_none, message_id: "messageId", response: "response" if is_none });

/// Parameters for session.extensions.sendAttachmentsToMessage.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SendAttachmentsToMessageParams {
    /// Attachments to push into the next user-message turn. extension_context entries take the slim shape; standard variants take their full AttachmentSchema shape.
    pub attachments: Vec<serde_json::Value>,
    /// Optional canvas instance binding the push for provenance. When supplied, the runtime resolves the canvas, verifies it is owned by the calling extension, and stamps canvasId/instanceId onto each extension_context entry. When omitted, no resolution runs and those fields stay unset on the attachment.
    pub instance_id: Option<String>,
}
crate::record_serde::wire_record!(SendAttachmentsToMessageParams { attachments: "attachments", instance_id: "instanceId" if is_none });

/// A single user message to append to the session as part of a `session.sendMessages` turn
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SendMessageItem {
    /// Optional attachments (files, directories, selections, blobs, GitHub references) to include with this message
    pub attachments: Option<Vec<serde_json::Value>>,
    /// If false, this message will not trigger a Premium Request Unit charge. User messages default to billable.
    #[doc(hidden)]
    pub(crate) billable: Option<bool>,
    /// If provided, this is shown in the timeline instead of `prompt`
    pub display_prompt: Option<String>,
    /// The user message text
    pub prompt: String,
    /// If set, the request will fail if the named tool is not available when this message is among the user messages at the start of the current exchange
    pub required_tool: Option<String>,
    /// Optional provenance tag copied to the resulting user.message event. Must be `user`, `system`, `command-<command-id>` for command-originated messages, `schedule-<numeric-id>` for scheduled prompts, `agent-<agent-id>` for prompts sent by another agent, or `automation-<automation-id>` for turns a host sends on behalf of an automation the user configured. An automation turn defines the session's task like a user message but carries no human authority.
    #[doc(hidden)]
    pub(crate) source: Option<String>,
}
crate::record_serde::wire_record!(SendMessageItem { attachments: "attachments" if is_none, billable: "billable" if is_none, display_prompt: "displayPrompt" if is_none, prompt: "prompt", required_tool: "requiredTool" if is_none, source: "source" if is_none });

#[derive(Debug, Clone, Default)]
pub struct SendMessagesRequestResponseFormat {
    /// JSON Schema and provider options for the turn's output.
    pub json_schema: JsonSchemaResponseFormat,
    /// Output format discriminator. Currently only json_schema is supported.
    pub r#type: SendMessagesRequestResponseFormatType,
}
crate::record_serde::wire_record!(SendMessagesRequestResponseFormat {
    json_schema: "jsonSchema",
    r#type: "type"
});

/// Parameters for sending zero or more user messages to the session in a single turn. Remote-backed (Mission Control) sessions do not support this method and will return an error.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SendMessagesRequest {
    /// The UI mode the agent was in when these messages were sent. Defaults to the session's current mode.
    pub agent_mode: Option<SendAgentMode>,
    /// Opaque caller-chosen ID for the turn this batch starts. The runtime copies it unchanged into `params._meta["com.github.copilot/turnId"]` on every MCP `tools/call` the agent makes while serving that turn, including calls from sub-agents it spawns and from Autopilot continuations of it. Visible user steering (`mode: "immediate"`) that a running turn consumes hands it to a successor turn: the steering's value replaces the running turn's from the next model call onward, and steering without one clears it. System and hidden steering leave it unchanged. A turn started without one, including one the runtime starts itself, carries no turn ID. The `user.message` event for each message the send delivers echoes it as `clientTurnId` when the message is consumed, and is persisted like other events; the value stamped on MCP calls is held in memory only and is not restored on resume.
    pub client_turn_id: Option<String>,
    /// The user messages to append to the conversation, in order, before running one agent loop. When the batch starts a run, its final message is the primary initiating message; earlier messages provide context, not separate runs or replies. May be empty, in which case a single turn runs over the existing history with no new user message or originatingMessageId.
    pub messages: Vec<SendMessageItem>,
    /// How to deliver the messages. `enqueue` (default) appends to the message queue. `immediate` interjects during an in-progress turn.
    pub mode: Option<SendMode>,
    /// If true, adds the messages to the front of the queue instead of the end
    pub prepend: Option<bool>,
    /// Custom HTTP headers to include in outbound model requests for this turn. Merged with session-level provider headers; per-turn headers augment and overwrite session-level headers with the same key.
    pub request_headers: Option<HashMap<String, String>>,
    /// Provider-native output format for the whole turn, including an empty message batch and all tool-call iterations. Not inherited by later turns or subagents. Ordinary steering inherits the active format; specifying responseFormat with mode: immediate is an error, even while idle. Returned assistant content remains text; the runtime does not parse or validate it. Unsupported models or schemas produce provider errors.
    pub response_format: Option<SendMessagesRequestResponseFormat>,
    /// W3C Trace Context traceparent header for distributed tracing of this agent turn
    pub traceparent: Option<String>,
    /// W3C Trace Context tracestate header for distributed tracing
    pub tracestate: Option<String>,
    /// If true, await completion of the agentic loop for this turn before returning. Defaults to false (fire-and-forget). When true, the result still contains the same `messageIds`; the caller can rely on the agent having processed the messages before the call resolves. Transport-dependent tail semantics: on a LOCAL (in-process) session the wait additionally blocks until the completed turn's event tail has been dispatched to this session's in-process subscribers, so a subsequent read of subscriber state already reflects the turn; on a REMOTE session the wait resolves once the loop completes and mirrored delivery follows over the wire. Callers that need the stronger local guarantee on remote sessions should await the event stream explicitly.
    pub wait: Option<bool>,
}
crate::record_serde::wire_record!(SendMessagesRequest { agent_mode: "agentMode" if is_none, client_turn_id: "clientTurnId" if is_none, messages: "messages", mode: "mode" if is_none, prepend: "prepend" if is_none, request_headers: "requestHeaders" if is_none, response_format: "responseFormat" if is_none, traceparent: "traceparent" if is_none, tracestate: "tracestate" if is_none, wait: "wait" if is_none });

/// Result of sending zero or more user messages
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SendMessagesResult {
    /// Unique identifiers assigned to the messages, one per provided message in order. For a batch that starts a run, assistant messages use the final ID as originatingMessageId throughout that run, including tool iterations and stop-hook corrections. Immediate steering does not replace the active run's origin. Empty when no messages were provided; that run has no originatingMessageId.
    pub message_ids: Vec<String>,
}
crate::record_serde::wire_record!(SendMessagesResult {
    message_ids: "messageIds"
});

#[derive(Debug, Clone, Default)]
pub struct SendRequestResponseFormat {
    /// JSON Schema and provider options for the turn's output.
    pub json_schema: JsonSchemaResponseFormat,
    /// Output format discriminator. Currently only json_schema is supported.
    pub r#type: SendRequestResponseFormatType,
}
crate::record_serde::wire_record!(SendRequestResponseFormat {
    json_schema: "jsonSchema",
    r#type: "type"
});

/// Parameters for sending a user message to the session
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SendRequest {
    /// The UI mode the agent was in when this message was sent. Defaults to the session's current mode.
    pub agent_mode: Option<SendAgentMode>,
    /// Optional attachments (files, directories, selections, blobs, GitHub references) to include with the message
    pub attachments: Option<Vec<serde_json::Value>>,
    /// If false, this message will not trigger a Premium Request Unit charge. User messages default to billable.
    pub billable: Option<bool>,
    /// Opaque caller-chosen ID for the turn this message starts. The runtime copies it unchanged into `params._meta["com.github.copilot/turnId"]` on every MCP `tools/call` the agent makes while serving that turn, including calls from sub-agents it spawns and from Autopilot continuations of it. Visible user steering (`mode: "immediate"`) that a running turn consumes hands it to a successor turn: the steering's value replaces the running turn's from the next model call onward, and steering without one clears it. System and hidden steering leave it unchanged. A turn started without one, including one the runtime starts itself, carries no turn ID. The `user.message` event for each message the send delivers echoes it as `clientTurnId` when the message is consumed, and is persisted like other events; the value stamped on MCP calls is held in memory only and is not restored on resume.
    pub client_turn_id: Option<String>,
    /// If provided, this is shown in the timeline instead of `prompt`
    pub display_prompt: Option<String>,
    /// How to deliver the message. `enqueue` (default) appends to the message queue. `immediate` interjects during an in-progress turn.
    pub mode: Option<SendMode>,
    /// If true, adds the message to the front of the queue instead of the end
    pub prepend: Option<bool>,
    /// The user message text
    pub prompt: String,
    /// Custom HTTP headers to include in outbound model requests for this turn. Merged with session-level provider headers; per-turn headers augment and overwrite session-level headers with the same key.
    pub request_headers: Option<HashMap<String, String>>,
    /// If set, the request will fail if the named tool is not available when this message is among the user messages at the start of the current exchange
    pub required_tool: Option<String>,
    /// Provider-native output format for this turn, including all tool-call iterations. Not inherited by later turns or subagents. Ordinary steering inherits the active format; specifying responseFormat with mode: immediate is an error, even while idle. Returned assistant content remains text; the runtime does not parse or validate it. Unsupported models or schemas produce provider errors.
    pub response_format: Option<SendRequestResponseFormat>,
    /// Optional provenance tag copied to the resulting user.message event. Must be `user`, `system`, `command-<command-id>` for command-originated messages, `schedule-<numeric-id>` for scheduled prompts, `agent-<agent-id>` for prompts sent by another agent, or `automation-<automation-id>` for turns a host sends on behalf of an automation the user configured. An automation turn defines the session's task like a user message but carries no human authority.
    #[doc(hidden)]
    pub(crate) source: Option<String>,
    /// W3C Trace Context traceparent header for distributed tracing of this agent turn
    pub traceparent: Option<String>,
    /// W3C Trace Context tracestate header for distributed tracing
    pub tracestate: Option<String>,
    /// If true, await completion of the agentic loop for this message before returning. Defaults to false (fire-and-forget). When true, the result still contains the same `messageId`; the caller can rely on the agent having processed the message before the call resolves. Transport-dependent tail semantics: on a LOCAL (in-process) session the wait additionally blocks until the completed turn's event tail has been dispatched to this session's in-process subscribers, so a subsequent read of subscriber state already reflects the turn; on a REMOTE session the wait resolves once the loop completes and mirrored delivery follows over the wire. Callers that need the stronger local guarantee on remote sessions should await the event stream explicitly.
    pub wait: Option<bool>,
}
crate::record_serde::wire_record!(SendRequest { agent_mode: "agentMode" if is_none, attachments: "attachments" if is_none, billable: "billable" if is_none, client_turn_id: "clientTurnId" if is_none, display_prompt: "displayPrompt" if is_none, mode: "mode" if is_none, prepend: "prepend" if is_none, prompt: "prompt", request_headers: "requestHeaders" if is_none, required_tool: "requiredTool" if is_none, response_format: "responseFormat" if is_none, source: "source" if is_none, traceparent: "traceparent" if is_none, tracestate: "tracestate" if is_none, wait: "wait" if is_none });

/// Result of sending a user message
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SendResult {
    /// Unique identifier assigned to the message
    pub message_id: String,
}
crate::record_serde::wire_record!(SendResult {
    message_id: "messageId"
});

/// Internal request for sending a system notification.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SendSystemNotificationRequest {
    /// Optional structured notification kind.
    pub kind: Option<serde_json::Value>,
    /// Notification text to deliver to the model.
    pub message: String,
    /// Internal delivery options, including passive policy.
    pub options: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(SendSystemNotificationRequest { kind: "kind" if is_none, message: "message", options: "options" if is_none });

/// Outcome of an atomic user or repository settings edit.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SettingsEditResult {
    /// Changed, unchanged or invalid existing file. Rejected input, validation failures and I/O errors remain RPC errors.
    #[doc(hidden)]
    pub(crate) status: SettingsEditStatus,
}
crate::record_serde::wire_record!(SettingsEditResult { status: "status" });

/// A settings value to redact before showing it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SettingsRedactValueForDisplayRequest {
    /// Where the value sits in the settings tree, which decides which parts of it are secret.
    pub base_path: Vec<String>,
    /// The value to redact.
    pub value: serde_json::Value,
}
crate::record_serde::wire_record!(SettingsRedactValueForDisplayRequest {
    base_path: "basePath",
    value: "value"
});

/// A settings value with its secret-bearing parts replaced.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SettingsRedactValueForDisplayResult {
    /// The value with every secret-bearing part replaced by a redacted placeholder.
    pub value: serde_json::Value,
}
crate::record_serde::wire_record!(SettingsRedactValueForDisplayResult { value: "value" });

/// One key to remove from a settings file, named exactly as it appears in the file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SettingsRemoveRawKeyRequest {
    /// Absolute path of the settings file to edit.
    pub file_path: String,
    /// The key to remove, spelled as it appears in the file. It need not be a recognized setting: this exists to remove keys the loader rejected.
    pub raw_key: String,
}
crate::record_serde::wire_record!(SettingsRemoveRawKeyRequest {
    file_path: "filePath",
    raw_key: "rawKey"
});

/// The outcome of the settings-file edit.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SettingsRemoveRawKeyResult {
    /// `ok` when the edit completed, including an absent key. `invalid` means the existing file could not be parsed and remained unchanged.
    pub status: String,
}
crate::record_serde::wire_record!(SettingsRemoveRawKeyResult { status: "status" });

/// Caller-owned environment to filter under the runtime's interactive-shell credential policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ShellBuildInteractiveEnvironmentRequest {
    /// Additional variable names to omit, in addition to the runtime's required filters.
    pub additional_blocked_names: Vec<String>,
    /// Whether the shell host treats environment names as case-insensitive. Defaults to the runtime platform when omitted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub case_insensitive: Option<bool>,
    /// Environment supplied by the host that will launch the shell.
    pub host_environment: HashMap<String, String>,
}

impl std::fmt::Debug for ShellBuildInteractiveEnvironmentRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ShellBuildInteractiveEnvironmentRequest")
            .field("additional_blocked_names", &self.additional_blocked_names)
            .field("case_insensitive", &self.case_insensitive)
            .field("host_environment", &"<redacted>")
            .finish()
    }
}

/// Filtered caller environment with the runtime's Git hardening applied; never includes the runtime process environment.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ShellBuildInteractiveEnvironmentResult {
    /// Environment for the host's interactive shell.
    pub environment: HashMap<String, String>,
}

impl std::fmt::Debug for ShellBuildInteractiveEnvironmentResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ShellBuildInteractiveEnvironmentResult")
            .field("environment", &"<redacted>")
            .finish()
    }
}

/// User-requested shell execution cancellation handle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ShellCancelUserRequestedRequest {
    /// Request ID previously passed to executeUserRequested
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(ShellCancelUserRequestedRequest {
    request_id: "requestId"
});

/// Shell command to run, with optional working directory and timeout in milliseconds. Spawn failures return an RPC error.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ShellExecRequest {
    /// Shell command to execute
    pub command: String,
    /// Working directory (defaults to session working directory)
    pub cwd: Option<String>,
    /// Timeout in milliseconds (default: 30000)
    pub timeout: Option<i64>,
}
crate::record_serde::wire_record!(ShellExecRequest { command: "command", cwd: "cwd" if is_none, timeout: "timeout" if is_none });

/// Identifier of the spawned shell process, usable with shell.kill while the process is running.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ShellExecResult {
    /// Identifier usable with shell.kill while the process is running
    pub process_id: String,
}
crate::record_serde::wire_record!(ShellExecResult {
    process_id: "processId"
});

/// User-requested shell command and cancellation handle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ShellExecuteUserRequestedRequest {
    /// Shell command to execute
    pub command: String,
    /// Caller-provided cancellation handle for this execution
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(ShellExecuteUserRequestedRequest {
    command: "command",
    request_id: "requestId"
});

/// Identifier of a process previously returned by "shell.exec" and the signal to send.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ShellKillRequest {
    /// Process identifier returned by shell.exec
    pub process_id: String,
    /// Signal to send (default: SIGTERM)
    pub signal: Option<ShellKillSignal>,
}
crate::record_serde::wire_record!(ShellKillRequest { process_id: "processId", signal: "signal" if is_none });

/// Indicates whether the signal was delivered; false if the process was unknown or already exited.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ShellKillResult {
    /// Whether the signal was sent successfully
    pub killed: bool,
}
crate::record_serde::wire_record!(ShellKillResult { killed: "killed" });

/// Parameters for shutting down the session
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ShutdownRequest {
    /// Dispatch deferred sessionEnd hooks in the background with their full per-hook timeoutSec instead of awaiting them under the short shared shutdown budget. Set this when the host process keeps running after the session closes (for example the CLI's /clear), so a slow hook neither blocks the close nor is aborted. Hooks still detached when the process later exits are terminated with it. Defaults to false.
    pub detach_session_end_hooks: Option<bool>,
    /// Optional human-readable reason. Typically the message of the error that triggered shutdown when type is 'error'.
    pub reason: Option<String>,
    /// Why the session is being shut down. Defaults to "routine" when omitted.
    pub r#type: Option<ShutdownType>,
}
crate::record_serde::wire_record!(ShutdownRequest { detach_session_end_hooks: "detachSessionEndHooks" if is_none, reason: "reason" if is_none, r#type: "type" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandTimelineEntry {
    /// What the user must do to recover, when the entry reports a failure the runtime knows an action for. The `text` never names a client affordance, so a client that offers one renders it from this value.
    pub remediation: Option<RemediationAction>,
    /// Text displayed for the timeline entry.
    pub text: String,
    /// Timeline entry presentation type.
    pub r#type: String,
    /// Optional URL associated with the timeline entry.
    pub url: Option<String>,
}
crate::record_serde::wire_record!(SlashCommandTimelineEntry { remediation: "remediation" if is_none, text: "text", r#type: "type", url: "url" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandAddTimelineEntryResult {
    /// Timeline entry the host should append.
    pub entry: SlashCommandTimelineEntry,
    /// Discriminator for an add-timeline-entry result.
    pub kind: SlashCommandAddTimelineEntryResultKind,
    /// Optional text the host should prefill into the input editor.
    pub prefill_input: Option<String>,
    /// Whether command execution changed persisted runtime settings.
    pub runtime_settings_changed: Option<bool>,
}
crate::record_serde::wire_record!(SlashCommandAddTimelineEntryResult { entry: "entry", kind: "kind", prefill_input: "prefillInput" if is_none, runtime_settings_changed: "runtimeSettingsChanged" if is_none });

/// Slash-command invocation result that submits an agent prompt, with display prompt, optional mode, optional user-facing notice, settings-change flag, and opaque reservation finalization token.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandAgentPromptResult {
    /// Prompt text to display to the user
    pub display_prompt: String,
    /// Agent prompt result discriminator
    pub kind: SlashCommandAgentPromptResultKind,
    /// Optional target session mode for the agent prompt
    pub mode: Option<SessionMode>,
    /// Optional user-facing notice to show before the prompt is submitted
    pub notice: Option<String>,
    /// Prompt to submit to the agent
    pub prompt: String,
    /// Opaque token used to finalize this invocation's objective prompt reservation
    pub reservation_token: Option<String>,
    /// True when the invocation mutated user runtime settings; consumers caching settings should refresh
    pub runtime_settings_changed: Option<bool>,
}
crate::record_serde::wire_record!(SlashCommandAgentPromptResult { display_prompt: "displayPrompt", kind: "kind", mode: "mode" if is_none, notice: "notice" if is_none, prompt: "prompt", reservation_token: "reservationToken" if is_none, runtime_settings_changed: "runtimeSettingsChanged" if is_none });

/// Slash-command invocation result indicating completion, with optional message and settings-change flag.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandCompletedResult {
    /// Completed result discriminator
    pub kind: SlashCommandCompletedResultKind,
    /// Optional user-facing message describing the completed command
    pub message: Option<String>,
    /// Optional target session mode applied without submitting an agent prompt
    pub mode: Option<SessionMode>,
    /// True when the invocation mutated user runtime settings; consumers caching settings should refresh
    pub runtime_settings_changed: Option<bool>,
}
crate::record_serde::wire_record!(SlashCommandCompletedResult { kind: "kind", message: "message" if is_none, mode: "mode" if is_none, runtime_settings_changed: "runtimeSettingsChanged" if is_none });

/// Slash-command invocation result containing text output plus Markdown/ANSI rendering flags.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandTextResult {
    /// Text result discriminator
    pub kind: SlashCommandTextResultKind,
    /// Whether text contains Markdown
    pub markdown: Option<bool>,
    /// Whether ANSI sequences should be preserved
    pub preserve_ansi: Option<bool>,
    /// True when the invocation mutated user runtime settings; consumers caching settings should refresh
    pub runtime_settings_changed: Option<bool>,
    /// Present when the invocation changed the sandbox for this session only. Nothing was persisted, so consumers must mirror the change onto the live session rather than reloading settings, and must not treat it as a settings change.
    pub sandbox_session_change: Option<SandboxSessionChange>,
    /// Text output for the client to render
    pub text: String,
}
crate::record_serde::wire_record!(SlashCommandTextResult { kind: "kind", markdown: "markdown" if is_none, preserve_ansi: "preserveAnsi" if is_none, runtime_settings_changed: "runtimeSettingsChanged" if is_none, sandbox_session_change: "sandboxSessionChange" if is_none, text: "text" });

/// Selectable slash-command subcommand option with name, description, and optional group label.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandSelectSubcommandOption {
    /// Human-readable description of the subcommand
    pub description: String,
    /// Optional group label for organizing options
    pub group: Option<String>,
    /// Subcommand name to invoke
    pub name: String,
}
crate::record_serde::wire_record!(SlashCommandSelectSubcommandOption { description: "description", group: "group" if is_none, name: "name" });

/// Slash-command invocation result asking the client to present subcommand options for a parent command.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandSelectSubcommandResult {
    /// Parent command name that requires subcommand selection
    pub command: String,
    /// Select subcommand result discriminator
    pub kind: SlashCommandSelectSubcommandResultKind,
    /// Available subcommand options for the client to present
    pub options: Vec<SlashCommandSelectSubcommandOption>,
    /// True when the invocation mutated user runtime settings; consumers caching settings should refresh
    pub runtime_settings_changed: Option<bool>,
    /// Human-readable title for the selection UI
    pub title: String,
}
crate::record_serde::wire_record!(SlashCommandSelectSubcommandResult { command: "command", kind: "kind", options: "options", runtime_settings_changed: "runtimeSettingsChanged" if is_none, title: "title" });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandModelPickerDialog {
    /// Discriminator for a model-picker dialog.
    pub kind: SlashCommandModelPickerDialogKind,
    /// Model that should be enabled before it can be selected.
    pub model_to_enable: Option<String>,
    /// Settings scope the picker should modify.
    pub scope: Option<String>,
    /// Model-selection target represented by the picker.
    pub target: Option<String>,
}
crate::record_serde::wire_record!(SlashCommandModelPickerDialog { kind: "kind", model_to_enable: "modelToEnable" if is_none, scope: "scope" if is_none, target: "target" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandShowDialogResult {
    /// Dialog the host should display.
    pub dialog: SlashCommandModelPickerDialog,
    /// Discriminator for a show-dialog result.
    pub kind: SlashCommandShowDialogResultKind,
    /// Whether command execution changed persisted runtime settings.
    pub runtime_settings_changed: Option<bool>,
}
crate::record_serde::wire_record!(SlashCommandShowDialogResult { dialog: "dialog", kind: "kind", runtime_settings_changed: "runtimeSettingsChanged" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandSetModelResult {
    /// Auto routing profile selected by the command, when the model is Auto.
    pub auto_tier: Option<AutoTier>,
    /// Discriminator for a set-model result.
    pub kind: SlashCommandSetModelResultKind,
    /// Model selected by the command.
    pub model: String,
    /// Reasoning effort selected for the model.
    pub reasoning_effort: Option<String>,
    /// Repository settings scope modified by the command.
    pub repo_scope: Option<String>,
    /// User-settings snapshot to restore if the host cancels the model switch.
    pub revert_on_cancel: Option<serde_json::Value>,
    /// Whether command execution changed persisted runtime settings.
    pub runtime_settings_changed: Option<bool>,
    /// Settings scope modified by the command.
    pub scope: Option<String>,
    /// User-facing warning produced while selecting the model.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(SlashCommandSetModelResult { auto_tier: "autoTier" if is_none, kind: "kind", model: "model", reasoning_effort: "reasoningEffort" if is_none, repo_scope: "repoScope" if is_none, revert_on_cancel: "revertOnCancel" if is_none, runtime_settings_changed: "runtimeSettingsChanged" if is_none, scope: "scope" if is_none, warning: "warning" if is_none });

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SlashCommandSetPlanModelResult {
    /// Discriminator for a set-plan-model result.
    pub kind: SlashCommandSetPlanModelResultKind,
    /// User-facing confirmation message for the plan-model selection.
    pub message: String,
    /// Dedicated model selected for plan mode.
    pub plan_model: Option<String>,
    /// Whether command execution changed persisted runtime settings.
    pub runtime_settings_changed: Option<bool>,
}
crate::record_serde::wire_record!(SlashCommandSetPlanModelResult { kind: "kind", message: "message", plan_model: "planModel" if is_none, runtime_settings_changed: "runtimeSettingsChanged" if is_none });

/// Feature override key/value pairs to attach to subsequent telemetry events from this session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct TelemetrySetFeatureOverridesRequest {
    /// Override key/value pairs to attach to subsequent telemetry events from this session. Replaces any previously-set overrides.
    pub features: HashMap<String, String>,
}
crate::record_serde::wire_record!(TelemetrySetFeatureOverridesRequest {
    features: "features"
});

/// Selectable option for a UI elicitation multi-select array item, with submitted value and display label.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationArrayAnyOfFieldItemsAnyOf {
    /// Value submitted when this option is selected.
    pub r#const: String,
    /// Display label for this option.
    pub title: String,
}
crate::record_serde::wire_record!(UIElicitationArrayAnyOfFieldItemsAnyOf {
    r#const: "const",
    title: "title"
});

/// Schema applied to each item in the array.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationArrayAnyOfFieldItems {
    /// Selectable options, each with a value and a display label.
    pub any_of: Vec<UIElicitationArrayAnyOfFieldItemsAnyOf>,
}
crate::record_serde::wire_record!(UIElicitationArrayAnyOfFieldItems { any_of: "anyOf" });

/// Multi-select string field where each option pairs a value with a display label.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationArrayAnyOfField {
    /// Default values selected when the form is first shown.
    pub default: Option<Vec<String>>,
    /// Help text describing the field.
    pub description: Option<String>,
    /// Schema applied to each item in the array.
    pub items: UIElicitationArrayAnyOfFieldItems,
    /// Maximum number of items the user may select.
    pub max_items: Option<i64>,
    /// Minimum number of items the user must select.
    pub min_items: Option<i64>,
    /// Human-readable label for the field.
    pub title: Option<String>,
    /// Type discriminator. Always "array".
    pub r#type: UIElicitationArrayAnyOfFieldType,
}
crate::record_serde::wire_record!(UIElicitationArrayAnyOfField { default: "default" if is_none, description: "description" if is_none, items: "items", max_items: "maxItems" if is_none, min_items: "minItems" if is_none, title: "title" if is_none, r#type: "type" });

/// Schema applied to each item in the array.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationArrayEnumFieldItems {
    /// Allowed string values for each selected item.
    pub r#enum: Vec<String>,
    /// Type discriminator. Always "string".
    pub r#type: UIElicitationArrayEnumFieldItemsType,
}
crate::record_serde::wire_record!(UIElicitationArrayEnumFieldItems {
    r#enum: "enum",
    r#type: "type"
});

/// Multi-select string field whose allowed values are defined inline.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationArrayEnumField {
    /// Default values selected when the form is first shown.
    pub default: Option<Vec<String>>,
    /// Help text describing the field.
    pub description: Option<String>,
    /// Schema applied to each item in the array.
    pub items: UIElicitationArrayEnumFieldItems,
    /// Maximum number of items the user may select.
    pub max_items: Option<i64>,
    /// Minimum number of items the user must select.
    pub min_items: Option<i64>,
    /// Human-readable label for the field.
    pub title: Option<String>,
    /// Type discriminator. Always "array".
    pub r#type: UIElicitationArrayEnumFieldType,
}
crate::record_serde::wire_record!(UIElicitationArrayEnumField { default: "default" if is_none, description: "description" if is_none, items: "items", max_items: "maxItems" if is_none, min_items: "minItems" if is_none, title: "title" if is_none, r#type: "type" });

/// JSON Schema describing the form fields to present to the user
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationSchema {
    /// Form field definitions, keyed by field name
    pub properties: HashMap<String, serde_json::Value>,
    /// List of required field names
    pub required: Option<Vec<String>>,
    /// Schema type indicator (always 'object')
    pub r#type: UIElicitationSchemaType,
}
crate::record_serde::wire_record!(UIElicitationSchema { properties: "properties", required: "required" if is_none, r#type: "type" });

/// Prompt message and JSON schema describing the form fields to elicit from the user.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationRequest {
    /// MCP request metadata.
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Message describing what information is needed from the user
    pub message: String,
    /// Elicitation mode. Omitted and form are equivalent for structured elicitation.
    pub mode: Option<McpElicitationFormMode>,
    /// JSON Schema describing the form fields to present to the user
    pub requested_schema: UIElicitationSchema,
    /// MCP task metadata.
    pub task: Option<McpTaskMetadata>,
}
crate::record_serde::wire_record!(UIElicitationRequest { meta: "_meta" if is_none, message: "message", mode: "mode" if is_none, requested_schema: "requestedSchema", task: "task" if is_none });

/// The elicitation response (accept with form values, decline, or cancel)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationResponse {
    /// MCP response metadata.
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// The user's response: accept (submitted), decline (rejected), or cancel (dismissed)
    pub action: UIElicitationResponseAction,
    /// The form values submitted by the user (present when action is 'accept')
    pub content: Option<HashMap<String, serde_json::Value>>,
}
crate::record_serde::wire_record!(UIElicitationResponse { meta: "_meta" if is_none, action: "action", content: "content" if is_none });

/// Indicates whether the elicitation response was accepted; false if it was already resolved by another client.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationResult {
    /// Whether the response was accepted. False if the request was already resolved by another client.
    pub success: bool,
}
crate::record_serde::wire_record!(UIElicitationResult { success: "success" });

/// Boolean field rendered as a yes/no toggle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationSchemaPropertyBoolean {
    /// Default value selected when the form is first shown.
    pub default: Option<bool>,
    /// Help text describing the field.
    pub description: Option<String>,
    /// Human-readable label for the field.
    pub title: Option<String>,
    /// Type discriminator. Always "boolean".
    pub r#type: UIElicitationSchemaPropertyBooleanType,
}
crate::record_serde::wire_record!(UIElicitationSchemaPropertyBoolean { default: "default" if is_none, description: "description" if is_none, title: "title" if is_none, r#type: "type" });

/// Numeric field accepting either a number or an integer.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationSchemaPropertyNumber {
    /// Default value populated in the input when the form is first shown.
    pub default: Option<f64>,
    /// Help text describing the field.
    pub description: Option<String>,
    /// Maximum allowed value (inclusive).
    pub maximum: Option<f64>,
    /// Minimum allowed value (inclusive).
    pub minimum: Option<f64>,
    /// Human-readable label for the field.
    pub title: Option<String>,
    /// Numeric type accepted by the field.
    pub r#type: UIElicitationSchemaPropertyNumberType,
}
crate::record_serde::wire_record!(UIElicitationSchemaPropertyNumber { default: "default" if is_none, description: "description" if is_none, maximum: "maximum" if is_none, minimum: "minimum" if is_none, title: "title" if is_none, r#type: "type" });

/// Free-text string field with optional length and format constraints.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationSchemaPropertyString {
    /// Default value populated in the input when the form is first shown.
    pub default: Option<String>,
    /// Help text describing the field.
    pub description: Option<String>,
    /// Optional format hint that constrains the accepted input.
    pub format: Option<UIElicitationSchemaPropertyStringFormat>,
    /// Maximum number of characters allowed.
    pub max_length: Option<i64>,
    /// Minimum number of characters required.
    pub min_length: Option<i64>,
    /// Human-readable label for the field.
    pub title: Option<String>,
    /// Type discriminator. Always "string".
    pub r#type: UIElicitationSchemaPropertyStringType,
}
crate::record_serde::wire_record!(UIElicitationSchemaPropertyString { default: "default" if is_none, description: "description" if is_none, format: "format" if is_none, max_length: "maxLength" if is_none, min_length: "minLength" if is_none, title: "title" if is_none, r#type: "type" });

/// Single-select string field whose allowed values are defined inline.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationStringEnumField {
    /// Default value selected when the form is first shown.
    pub default: Option<String>,
    /// Help text describing the field.
    pub description: Option<String>,
    /// Allowed string values.
    pub r#enum: Vec<String>,
    /// Optional display labels for each enum value, in the same order as `enum`.
    pub enum_names: Option<Vec<String>>,
    /// Human-readable label for the field.
    pub title: Option<String>,
    /// Type discriminator. Always "string".
    pub r#type: UIElicitationStringEnumFieldType,
}
crate::record_serde::wire_record!(UIElicitationStringEnumField { default: "default" if is_none, description: "description" if is_none, r#enum: "enum", enum_names: "enumNames" if is_none, title: "title" if is_none, r#type: "type" });

/// Selectable option for a UI elicitation single-select string field, with submitted value and display label.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationStringOneOfFieldOneOf {
    /// Value submitted when this option is selected.
    pub r#const: String,
    /// Display label for this option.
    pub title: String,
}
crate::record_serde::wire_record!(UIElicitationStringOneOfFieldOneOf {
    r#const: "const",
    title: "title"
});

/// Single-select string field where each option pairs a value with a display label.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIElicitationStringOneOfField {
    /// Default value selected when the form is first shown.
    pub default: Option<String>,
    /// Help text describing the field.
    pub description: Option<String>,
    /// Selectable options, each with a value and a display label.
    pub one_of: Vec<UIElicitationStringOneOfFieldOneOf>,
    /// Human-readable label for the field.
    pub title: Option<String>,
    /// Type discriminator. Always "string".
    pub r#type: UIElicitationStringOneOfFieldType,
}
crate::record_serde::wire_record!(UIElicitationStringOneOfField { default: "default" if is_none, description: "description" if is_none, one_of: "oneOf", title: "title" if is_none, r#type: "type" });

/// Transient question to answer without adding it to conversation history.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIEphemeralQueryRequest {
    /// In-process `AbortSignal` forwarded to the model client to cancel an in-flight request. Internal and excluded from the public SDK surface.
    #[doc(hidden)]
    pub(crate) abort_signal: Option<serde_json::Value>,
    /// In-process streaming callback `(text) => void` invoked with each token as the model emits it. Internal and excluded from the public SDK surface.
    #[doc(hidden)]
    pub(crate) on_chunk: Option<serde_json::Value>,
    /// Question to answer from the current conversation context.
    pub question: String,
}
crate::record_serde::wire_record!(UIEphemeralQueryRequest { abort_signal: "abortSignal" if is_none, on_chunk: "onChunk" if is_none, question: "question" });

/// Completed transient query. Ordered chunks and the terminal outcome are also delivered through `ui.ephemeral_query` session events while it runs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIEphemeralQueryResult {
    /// Answer returned by the model
    pub answer: String,
}
crate::record_serde::wire_record!(UIEphemeralQueryResult { answer: "answer" });

/// User response for a pending exit-plan-mode request, with approval state, selected action, auto-approve flag, and feedback.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIExitPlanModeResponse {
    /// Whether the plan was approved.
    pub approved: bool,
    /// Whether subsequent edits should be auto-approved without confirmation.
    pub auto_approve_edits: Option<bool>,
    /// When true, the agent is instructed to end its turn without starting implementation so the client can restore the session model and auto-submit a fresh implementation turn on it. Set only when a distinct plan configuration (a different model, reasoning effort, or context tier) actually ran the planning turn.
    pub defer_implementation: Option<bool>,
    /// Feedback from the user when they declined the plan or requested changes.
    pub feedback: Option<String>,
    /// The action the user selected. Defaults to 'autopilot' when autoApproveEdits is true, otherwise 'interactive'.
    pub selected_action: Option<UIExitPlanModeAction>,
}
crate::record_serde::wire_record!(UIExitPlanModeResponse { approved: "approved", auto_approve_edits: "autoApproveEdits" if is_none, defer_implementation: "deferImplementation" if is_none, feedback: "feedback" if is_none, selected_action: "selectedAction" if is_none });

/// Request ID of a pending `auto_mode_switch.requested` event and the user's response.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIHandlePendingAutoModeSwitchRequest {
    /// The unique request ID from the auto_mode_switch.requested event
    pub request_id: RequestId,
    /// User's choice for auto-mode switching: yes (allow this turn), yes_always (allow + persist as setting), or no (decline).
    pub response: UIAutoModeSwitchResponse,
}
crate::record_serde::wire_record!(UIHandlePendingAutoModeSwitchRequest {
    request_id: "requestId",
    response: "response"
});

/// Pending elicitation request ID and the user's response (accept/decline/cancel + form values).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIHandlePendingElicitationRequest {
    /// Who answered and how. When it says a person answered interactively (`source` `human_response` with `responseCapability` `interactive`), the runtime records the answer as the person's own authority for Assisted Permissions, but only from the session's authenticated owning connection or the in-process host; an answer from any other caller settles without it. The runtime decides which answers carry authority. Omit for automated answers.
    pub decision_context: Option<PermissionDecisionContext>,
    /// The unique request ID from the elicitation.requested event
    pub request_id: RequestId,
    /// The elicitation response (accept with form values, decline, or cancel)
    pub result: UIElicitationResponse,
}
crate::record_serde::wire_record!(UIHandlePendingElicitationRequest { decision_context: "decisionContext" if is_none, request_id: "requestId", result: "result" });

/// Request ID of a pending `exit_plan_mode.requested` event and the user's response.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIHandlePendingExitPlanModeRequest {
    /// Who answered and how. When it says a person answered interactively (`source` `human_response` with `responseCapability` `interactive`), the runtime records the answer as the person's own authority for Assisted Permissions, but only from the session's authenticated owning connection or the in-process host; an answer from any other caller settles without it. The runtime decides which answers carry authority. Omit for automated answers.
    pub decision_context: Option<PermissionDecisionContext>,
    /// The unique request ID from the exit_plan_mode.requested event
    pub request_id: RequestId,
    /// User response for a pending exit-plan-mode request, with approval state, selected action, auto-approve flag, and feedback.
    pub response: UIExitPlanModeResponse,
}
crate::record_serde::wire_record!(UIHandlePendingExitPlanModeRequest { decision_context: "decisionContext" if is_none, request_id: "requestId", response: "response" });

/// Indicates whether the pending UI request was resolved by this call.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIHandlePendingResult {
    /// True if the request was still pending and was resolved by this call. False if the request ID was unknown, already resolved by another client (e.g. GitHub), expired, or otherwise no longer pending.
    pub success: bool,
}
crate::record_serde::wire_record!(UIHandlePendingResult { success: "success" });

/// Optional sampling result payload. Omit to reject/cancel the sampling request without providing a result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UIHandlePendingSamplingResponse {}

/// Request ID of a pending `sampling.requested` event and an optional sampling result payload (omit to reject).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIHandlePendingSamplingRequest {
    /// The unique request ID from the sampling.requested event
    pub request_id: RequestId,
    /// Optional sampling result payload. Omit to reject/cancel the sampling request without providing a result.
    pub response: Option<UIHandlePendingSamplingResponse>,
}
crate::record_serde::wire_record!(UIHandlePendingSamplingRequest { request_id: "requestId", response: "response" if is_none });

/// The user's selected action for an exhausted session limit.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UISessionLimitsExhaustedResponse {
    /// Action selected by the user.
    pub action: UISessionLimitsExhaustedResponseAction,
    /// AI Credits to add to the current max when action is 'add'.
    pub additional_ai_credits: Option<f64>,
    /// New absolute max AI Credits when action is 'set'.
    pub max_ai_credits: Option<f64>,
}
crate::record_serde::wire_record!(UISessionLimitsExhaustedResponse { action: "action", additional_ai_credits: "additionalAiCredits" if is_none, max_ai_credits: "maxAiCredits" if is_none });

/// Request ID of a pending `session_limits_exhausted.requested` event and the user's selected limit action.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIHandlePendingSessionLimitsExhaustedRequest {
    /// The unique request ID from the session_limits_exhausted.requested event
    pub request_id: RequestId,
    /// The selected session-limit action.
    pub response: UISessionLimitsExhaustedResponse,
}
crate::record_serde::wire_record!(UIHandlePendingSessionLimitsExhaustedRequest {
    request_id: "requestId",
    response: "response"
});

/// User response for a pending user-input request, with answer text and whether it was typed freeform.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIUserInputResponse {
    /// The user's answer text
    pub answer: String,
    /// True if the user typed a freeform response, false if they selected a presented choice. Used by telemetry to differentiate between free text input and choice selection.
    pub was_freeform: bool,
}
crate::record_serde::wire_record!(UIUserInputResponse {
    answer: "answer",
    was_freeform: "wasFreeform"
});

/// Request ID of a pending `user_input.requested` event and the user's response.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIHandlePendingUserInputRequest {
    /// Who answered and how. When it says a person answered interactively (`source` `human_response` with `responseCapability` `interactive`), the runtime records the answer as the person's own authority for Assisted Permissions, but only from the session's authenticated owning connection or the in-process host; an answer from any other caller settles without it. The runtime decides which answers carry authority. Omit for automated answers.
    pub decision_context: Option<PermissionDecisionContext>,
    /// The unique request ID from the user_input.requested event
    pub request_id: RequestId,
    /// User response for a pending user-input request, with answer text and whether it was typed freeform.
    pub response: UIUserInputResponse,
}
crate::record_serde::wire_record!(UIHandlePendingUserInputRequest { decision_context: "decisionContext" if is_none, request_id: "requestId", response: "response" });

/// Optional recovery-aware enrollment for an in-process Auto-switch handler.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIRegisterDirectAutoModeSwitchHandlerRequest {
    /// Opt into active-request Auto recovery before starting a turn. Attach the handler separately, show the failure reason, and return the user's choice for the originating live session. Require a positive acknowledgment in the registration result. Dismiss offers on auto_mode_recovery_consent.completed and on session resume or disconnect. Omitted or false preserves legacy rate-limit switching.
    pub auto_mode_recovery: Option<bool>,
}
crate::record_serde::wire_record!(UIRegisterDirectAutoModeSwitchHandlerRequest { auto_mode_recovery: "autoModeRecovery" if is_none });

/// Registers an in-process Auto-switch responder and suppresses server-bridge dispatch to avoid competing responders for the same requestId. When requested, also enrolls the responder for active-request Auto recovery and acknowledges support. Attach the actual listener separately through standard event subscriptions before starting a turn, and release the handle when the responder is removed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIRegisterDirectAutoModeSwitchHandlerResult {
    /// Positive acknowledgment of recovery-aware enrollment. Absence on an older runtime means recovery is unsupported.
    pub auto_mode_recovery: Option<bool>,
    /// Opaque handle representing the registration. Pass this same handle to `unregisterDirectAutoModeSwitchHandler` when the in-process handler is no longer active. Multiple registrations are reference-counted; the server bridge will only dispatch auto-mode-switch requests when no handles are active.
    pub handle: String,
}
crate::record_serde::wire_record!(UIRegisterDirectAutoModeSwitchHandlerResult { auto_mode_recovery: "autoModeRecovery" if is_none, handle: "handle" });

/// Identifies one runtime-owned native action for an in-process SDK UI dispatcher.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UiThreadRunRequest {
    /// Opaque, one-shot action identifier scoped to the receiving native connection. Execute only through the synchronous native UI-action entry point.
    pub action_id: i64,
}
crate::record_serde::wire_record!(UiThreadRunRequest {
    action_id: "actionId"
});

/// Opaque handle previously returned by `registerDirectAutoModeSwitchHandler` to release.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIUnregisterDirectAutoModeSwitchHandlerRequest {
    /// Handle previously returned by `registerDirectAutoModeSwitchHandler`
    pub handle: String,
}
crate::record_serde::wire_record!(UIUnregisterDirectAutoModeSwitchHandlerRequest {
    handle: "handle"
});

/// Indicates whether the handle was active and the registration count was decremented.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UIUnregisterDirectAutoModeSwitchHandlerResult {
    /// True if the handle was active and decremented the counter; false if the handle was unknown.
    pub unregistered: bool,
}
crate::record_serde::wire_record!(UIUnregisterDirectAutoModeSwitchHandlerResult {
    unregistered: "unregistered"
});

/// Subagent settings to apply to the current session
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct UpdateSubagentSettingsRequest {
    /// Subagent settings to apply, or null to clear the live session override
    pub subagents: Option<SubagentSettingsConfig>,
}
crate::record_serde::wire_record!(UpdateSubagentSettingsRequest {
    subagents: "subagents"
});

/// Internal absolute code-change totals reported by the owning host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct UsageSetCodeChangesRequest {
    /// Absolute changed-file count; omission preserves the previous count.
    pub files_count: Option<f64>,
    /// Absolute added-line total, replacing the previous reading.
    pub lines_added: f64,
    /// Absolute removed-line total, replacing the previous reading.
    pub lines_removed: f64,
}
crate::record_serde::wire_record!(UsageSetCodeChangesRequest { files_count: "filesCount" if is_none, lines_added: "linesAdded", lines_removed: "linesRemoved" });

/// Current sharing status and shareable GitHub URL for a session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct VisibilityGetResult {
    /// Shareable GitHub URL for the session. Present when the session is synced and the URL can be resolved.
    pub share_url: Option<String>,
    /// Current sharing status. Absent when the session is not synced or the status could not be retrieved (e.g. the user is not authenticated).
    pub status: Option<SessionVisibilityStatus>,
    /// Whether the session has been synced to Mission Control (i.e. has a GitHub task). When false, the session cannot be shared and `status`/`shareUrl` are absent.
    pub synced: bool,
}
crate::record_serde::wire_record!(VisibilityGetResult { share_url: "shareUrl" if is_none, status: "status" if is_none, synced: "synced" });

/// Desired sharing status for the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct VisibilitySetRequest {
    /// Sharing status to apply. "repo" makes the session visible to repository readers; "unshared" restricts it to the creator and collaborators.
    pub status: SessionVisibilityStatus,
}
crate::record_serde::wire_record!(VisibilitySetRequest { status: "status" });

/// Effective sharing status and shareable GitHub URL after updating session visibility.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct VisibilitySetResult {
    /// Shareable GitHub URL for the session. Present when the session is synced and the URL can be resolved.
    pub share_url: Option<String>,
    /// Effective sharing status after the update. May differ from the requested status for task types that are already visible to repository readers by default. Absent when the update could not be applied (e.g. the session is not synced or the user is not authenticated).
    pub status: Option<SessionVisibilityStatus>,
    /// Whether the session has been synced to Mission Control (i.e. has a GitHub task). When false, the visibility change could not be applied and `status`/`shareUrl` are absent.
    pub synced: bool,
}
crate::record_serde::wire_record!(VisibilitySetResult { share_url: "shareUrl" if is_none, status: "status" if is_none, synced: "synced" });

/// Snapshot and transient notifications for one connection-owned manager.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsConfigureResult {
    /// Transient host notifications since the acknowledged revision.
    pub events: Vec<LocalModelsEvent>,
    /// Revision to acknowledge in the next watch call.
    pub revision: i64,
    /// Current catalog, machine state, active operations, progress, and notices.
    pub state: LocalModelsView,
}
crate::record_serde::wire_record!(LocalModelsConfigureResult {
    events: "events",
    revision: "revision",
    state: "state"
});

/// Snapshot and transient notifications for one connection-owned manager.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsInspectResult {
    /// Transient host notifications since the acknowledged revision.
    pub events: Vec<LocalModelsEvent>,
    /// Revision to acknowledge in the next watch call.
    pub revision: i64,
    /// Current catalog, machine state, active operations, progress, and notices.
    pub state: LocalModelsView,
}
crate::record_serde::wire_record!(LocalModelsInspectResult {
    events: "events",
    revision: "revision",
    state: "state"
});

/// Snapshot and transient notifications for one connection-owned manager.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsWatchResult {
    /// Transient host notifications since the acknowledged revision.
    pub events: Vec<LocalModelsEvent>,
    /// Revision to acknowledge in the next watch call.
    pub revision: i64,
    /// Current catalog, machine state, active operations, progress, and notices.
    pub state: LocalModelsView,
}
crate::record_serde::wire_record!(LocalModelsWatchResult {
    events: "events",
    revision: "revision",
    state: "state"
});

/// Runtime catalog entry, including installation and inference metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LocalModelsPrepareResult {
    /// Published download size, when known.
    pub approximate_bytes: Option<f64>,
    /// Recommended graphics memory, when published.
    pub approximate_vram_bytes: Option<f64>,
    /// Automatic registration metadata, when applicable.
    pub auto_local: Option<LocalModelAutoRegistration>,
    /// Exact registry asset identity.
    pub azure_ml: LocalModelAzureMlIdentity,
    /// Model display name.
    pub display_name: String,
    /// Default primary filename.
    pub file_name: String,
    /// Catalog identifier, distinct from the inference selection identifier.
    pub id: String,
    /// Published license name.
    pub license: String,
    /// Catalog lifecycle category.
    pub lifecycle: Option<String>,
    /// Published token limits.
    pub limits: LocalModelLimits,
    /// Registry metadata, preserved without mutation.
    pub metadata: serde_json::Value,
    /// Published peak installation size, when known.
    pub peak_disk_bytes: Option<f64>,
    /// Model publisher display name.
    pub provider: String,
    /// Stable BYOK provider name.
    pub provider_name: String,
    /// Inference runtime name.
    pub runtime: String,
    /// Whether acquisition installs the inference runtime.
    pub runtime_provisioned: Option<bool>,
    /// Provider-qualified selection identifier.
    pub selection_id: Option<String>,
    /// Published inference options, preserved without mutation.
    pub serve_options: serde_json::Value,
    /// Source and display order.
    pub source: LocalModelSource,
    /// Registry display label.
    pub source_label: String,
    /// Whether the model supports tool calls.
    pub supports_tool_calls: bool,
}
crate::record_serde::wire_record!(LocalModelsPrepareResult { approximate_bytes: "approximateBytes" if is_none, approximate_vram_bytes: "approximateVramBytes" if is_none, auto_local: "autoLocal" if is_none, azure_ml: "azureMl", display_name: "displayName", file_name: "fileName", id: "id", license: "license", lifecycle: "lifecycle" if is_none, limits: "limits", metadata: "metadata", peak_disk_bytes: "peakDiskBytes" if is_none, provider: "provider", provider_name: "providerName", runtime: "runtime", runtime_provisioned: "runtimeProvisioned" if is_none, selection_id: "selectionId" if is_none, serve_options: "serveOptions", source: "source", source_label: "sourceLabel", supports_tool_calls: "supportsToolCalls" });

/// Identifies a file logger owned by the originating SDK connection generation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LoggingFileOpenResult {
    /// Opaque identifier returned by logging.file.open.
    pub log_id: String,
}
crate::record_serde::wire_record!(LoggingFileOpenResult { log_id: "logId" });

/// A session created at the remote source and its refreshed connection snapshot.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionsCreateSessionResult {
    /// Connection state after the source refreshed its catalog.
    pub state_json: String,
    /// Remote AHP session URI.
    pub uri: String,
}
crate::record_serde::wire_record!(AhpConnectionsCreateSessionResult {
    state_json: "stateJson",
    uri: "uri"
});

/// The selected source catalog row.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionsSelectSessionResult {
    /// Serialized AHP session summary, or null when no row matches.
    pub summary_json: Option<String>,
}
crate::record_serde::wire_record!(AhpConnectionsSelectSessionResult {
    summary_json: "summaryJson"
});

/// The connection's current protocol snapshot and negotiated client identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionsCreateResult {
    /// Effective client identity, including any Mission Control assignment.
    pub client_id: String,
    /// Opaque connection identifier scoped to the originating SDK connection.
    pub connection_id: String,
    /// Serialized AHP relay state, including its monotonic revision.
    pub state_json: String,
}
crate::record_serde::wire_record!(AhpConnectionsCreateResult {
    client_id: "clientId",
    connection_id: "connectionId",
    state_json: "stateJson"
});

/// A registered runtime session and its independent attachment lease.
/// Read session.remote.readProjection before using host state.
/// The first read waits for subscription readiness and reports initialization errors.
/// Release the lease if initialization fails.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionsAttachSessionResult {
    /// Lease for independent facade teardown on this SDK connection.
    pub attachment_id: String,
    /// Host metadata used for the SDK attachment.
    pub metadata: AhpSessionMetadata,
    /// Runtime identity, distinct from the host's AHP URI.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(AhpConnectionsAttachSessionResult {
    attachment_id: "attachmentId",
    metadata: "metadata",
    session_id: "sessionId"
});

/// The connection's current protocol snapshot and negotiated client identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionsConnectResult {
    /// Effective client identity, including any Mission Control assignment.
    pub client_id: String,
    /// Opaque connection identifier scoped to the originating SDK connection.
    pub connection_id: String,
    /// Serialized AHP relay state, including its monotonic revision.
    pub state_json: String,
}
crate::record_serde::wire_record!(AhpConnectionsConnectResult {
    client_id: "clientId",
    connection_id: "connectionId",
    state_json: "stateJson"
});

/// The connection's current protocol snapshot and negotiated client identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionsReconnectResult {
    /// Effective client identity, including any Mission Control assignment.
    pub client_id: String,
    /// Opaque connection identifier scoped to the originating SDK connection.
    pub connection_id: String,
    /// Serialized AHP relay state, including its monotonic revision.
    pub state_json: String,
}
crate::record_serde::wire_record!(AhpConnectionsReconnectResult {
    client_id: "clientId",
    connection_id: "connectionId",
    state_json: "stateJson"
});

/// The connection's current protocol snapshot and negotiated client identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionsRefreshResult {
    /// Effective client identity, including any Mission Control assignment.
    pub client_id: String,
    /// Opaque connection identifier scoped to the originating SDK connection.
    pub connection_id: String,
    /// Serialized AHP relay state, including its monotonic revision.
    pub state_json: String,
}
crate::record_serde::wire_record!(AhpConnectionsRefreshResult {
    client_id: "clientId",
    connection_id: "connectionId",
    state_json: "stateJson"
});

/// Events admitted for this connection, in protocol order.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionsReadEventsResult {
    /// The next queued events. Empty only after disposal.
    pub events: Vec<AhpConnectionEvent>,
}
crate::record_serde::wire_record!(AhpConnectionsReadEventsResult { events: "events" });

/// Identifies a diagnostic log owned by the originating SDK connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AhpConnectionLogsOpenResult {
    /// Opaque identifier returned by ahp.connectionLogs.open.
    pub log_id: String,
}
crate::record_serde::wire_record!(AhpConnectionLogsOpenResult { log_id: "logId" });

/// Connection-owned upload state retained until host shutdown.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FeedbackUploadStartResult {
    /// Opaque identifier scoped to the launch-authorized owner.
    pub operation_id: String,
    /// Present exactly when the operation is terminal.
    #[doc(hidden)]
    pub(crate) outcome: Option<FeedbackUploadOutcome>,
    /// Current local upload progress.
    #[doc(hidden)]
    pub(crate) progress: FeedbackUploadProgress,
    /// Monotonically increasing revision of the local snapshot.
    pub revision: i64,
}
crate::record_serde::wire_record!(FeedbackUploadStartResult { operation_id: "operationId", outcome: "outcome" if is_none, progress: "progress", revision: "revision" });

/// Connection-owned upload state retained until host shutdown.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct FeedbackUploadReadResult {
    /// Opaque identifier scoped to the launch-authorized owner.
    pub operation_id: String,
    /// Present exactly when the operation is terminal.
    #[doc(hidden)]
    pub(crate) outcome: Option<FeedbackUploadOutcome>,
    /// Current local upload progress.
    #[doc(hidden)]
    pub(crate) progress: FeedbackUploadProgress,
    /// Monotonically increasing revision of the local snapshot.
    pub revision: i64,
}
crate::record_serde::wire_record!(FeedbackUploadReadResult { operation_id: "operationId", outcome: "outcome" if is_none, progress: "progress", revision: "revision" });

/// Outcome of an atomic user or repository settings edit.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct RepoSettingsEditResult {
    /// Changed, unchanged or invalid existing file. Rejected input, validation failures and I/O errors remain RPC errors.
    #[doc(hidden)]
    pub(crate) status: SettingsEditStatus,
}
crate::record_serde::wire_record!(RepoSettingsEditResult { status: "status" });

/// Projection of the authoritative runtime credential transaction.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostCredentialsResult {
    /// Pinned identity resolved through the original credential owner.
    pub auth_info: Option<serde_json::Value>,
    /// Whether the runtime transaction bound its registration.
    pub committed: Option<bool>,
    /// Current registration when one exists.
    pub registration_id: Option<String>,
    /// Whether rollback restored the displaced mapping.
    pub restored: Option<bool>,
    /// Whether resume retains the session's existing registration.
    pub reusing_session_registration: Option<bool>,
    /// Opaque runtime transaction capability.
    pub transaction_id: Option<String>,
}
crate::record_serde::wire_record!(HostCredentialsResult { auth_info: "authInfo" if is_none, committed: "committed" if is_none, registration_id: "registrationId" if is_none, restored: "restored" if is_none, reusing_session_registration: "reusingSessionRegistration" if is_none, transaction_id: "transactionId" if is_none });

/// The resident session the application has materialized on its own connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostCreateSessionResult {
    /// Runtime session UUID materialized on the application's SDK connection.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(HostCreateSessionResult {
    session_id: "sessionId"
});

/// Empty acknowledgement for a completed host lifecycle operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct HostReleaseSessionResult {}

/// Empty acknowledgement for a completed host lifecycle operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct HostDisposeResult {}

/// Empty acknowledgement for a completed host lifecycle operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct HostSetSessionCreationPolicyResult {}

/// Private credentials delivered only to a runtime-owned Mission Control hosting participant.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostGetEnvironmentCredentialsResult {
    /// GitHub API base URL for the authenticated service.
    pub github_api_url: String,
    /// Hostname of the authenticated GitHub service.
    pub github_host: String,
    /// Mission Control API origin for environment registration and management.
    pub mission_control_url: String,
    /// Current bearer token for the authenticated GitHub identity.
    pub token: String,
}
crate::record_serde::wire_record!(HostGetEnvironmentCredentialsResult {
    github_api_url: "githubApiUrl",
    github_host: "githubHost",
    mission_control_url: "missionControlUrl",
    token: "token"
});

/// Normalized listener settings delivered only to the supervised hosting participant.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostGetConfigurationResult {
    /// Initial remote creation policy, independent of application session factories. Defaults to denied when omitted.
    pub allow_creating_new_sessions: Option<bool>,
    /// Explicit host-wide admission policy. Always supplied by the runtime after validating host.start.
    pub authentication: Option<serde_json::Value>,
    /// Requested GitHub Mission Control registration.
    pub github_environment: Option<HostGitHubEnvironmentOptions>,
    /// Normalized local listener settings, absent for relay-only hosts.
    pub local_server: Option<HostLocalServerConfiguration>,
    /// Whether app-owned durable sessions are resumed by the owning application.
    pub resume_factory: bool,
    /// Whether session materialization is delegated to the owning application.
    pub session_factory: bool,
}
crate::record_serde::wire_record!(HostGetConfigurationResult { allow_creating_new_sessions: "allowCreatingNewSessions" if is_none, authentication: "authentication" if is_none, github_environment: "githubEnvironment" if is_none, local_server: "localServer" if is_none, resume_factory: "resumeFactory", session_factory: "sessionFactory" });

/// Private credentials delivered only to a runtime-owned Mission Control hosting participant.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct HostGetAuthenticationCredentialsResult {
    /// GitHub API base URL for the authenticated service.
    pub github_api_url: String,
    /// Hostname of the authenticated GitHub service.
    pub github_host: String,
    /// Mission Control API origin for environment registration and management.
    pub mission_control_url: String,
    /// Current bearer token for the authenticated GitHub identity.
    pub token: String,
}
crate::record_serde::wire_record!(HostGetAuthenticationCredentialsResult {
    github_api_url: "githubApiUrl",
    github_host: "githubHost",
    mission_control_url: "missionControlUrl",
    token: "token"
});

/// Empty acknowledgement for a completed host lifecycle operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct HostCancelAuthenticationResult {}

/// Empty acknowledgement for a completed host lifecycle operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct HostReadyResult {}

/// Eligible Azure ML model version and its provider metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlLatestResult {
    /// Provider metadata retained for registration.
    pub metadata: serde_json::Value,
    /// Stable model-family name.
    pub name: String,
    /// Validated Windows ML requirements.
    pub profile: AzureMlProfile,
    /// Registry that owns this model version.
    pub registry: AzureMlRegistry,
    /// Exact published model version.
    pub version: String,
}
crate::record_serde::wire_record!(AzureMlLatestResult {
    metadata: "metadata",
    name: "name",
    profile: "profile",
    registry: "registry",
    version: "version"
});

/// Opaque handle to a connection-owned family mutation interval.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AzureMlAcquireFamilyResult {
    /// Server-generated handle, valid only on the originating SDK connection.
    pub lease_id: String,
}
crate::record_serde::wire_record!(AzureMlAcquireFamilyResult {
    lease_id: "leaseId"
});

/// Empty acknowledgement for a completed host lifecycle operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AzureMlReleaseFamilyResult {}

/// Empty acknowledgement for a completed host lifecycle operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AzureMlDiscardResult {}

/// The prompt the init command runs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct InstructionsGetInitPromptResult {
    /// Prompt text authoring repository instructions.
    pub prompt: String,
}
crate::record_serde::wire_record!(InstructionsGetInitPromptResult { prompt: "prompt" });

/// Instruction sources discovered across user, repository, and plugin sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstructionsDiscoverResult {
    /// All discovered instruction sources
    pub sources: Vec<InstructionSource>,
}
crate::record_serde::wire_record!(InstructionsDiscoverResult { sources: "sources" });

/// Instruction sources discovered across user, repository, and plugin sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstructionsDiscoverRepoSourcesResult {
    /// All discovered instruction sources
    pub sources: Vec<InstructionSource>,
}
crate::record_serde::wire_record!(InstructionsDiscoverRepoSourcesResult { sources: "sources" });

/// Canonical files and directories where custom instructions can be created so the runtime will recognize them.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct InstructionsGetDiscoveryPathsResult {
    /// Canonical instruction create/discovery files and directories, in priority order
    pub paths: Vec<InstructionDiscoveryPath>,
}
crate::record_serde::wire_record!(InstructionsGetDiscoveryPathsResult { paths: "paths" });

/// Agents discovered across user, project, plugin, and remote sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentsDiscoverResult {
    /// All discovered agents across all sources
    pub agents: Vec<AgentInfo>,
}
crate::record_serde::wire_record!(AgentsDiscoverResult { agents: "agents" });

/// Canonical locations where custom agents can be created so the runtime will recognize them.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentsGetDiscoveryPathsResult {
    /// Canonical agent create/discovery directories, in priority order
    pub paths: Vec<AgentDiscoveryPath>,
}
crate::record_serde::wire_record!(AgentsGetDiscoveryPathsResult { paths: "paths" });

/// The host's machine-wide state. Every field is optional because a fresh install has recorded nothing yet, so a reader must treat an absent field as `not yet`, never as a negative answer. Stored credentials are deliberately absent from this shape.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct GlobalStateLoadForConfigDirResult {
    /// Whether the app tip has been shown.
    pub app_tip_shown: Option<bool>,
    /// Terminals the user has already been asked to set up, so the host does not ask twice.
    pub asked_setup_terminals: Option<Vec<String>>,
    /// When the Auto-feedback hint was last shown, as an ISO 8601 timestamp. It enforces the once-per-day cap for non-staff users across restarts.
    pub auto_feedback_last_prompted_at: Option<String>,
    /// When the host first ran on this machine.
    pub first_launch_at: Option<String>,
    /// Plugins installed on this machine.
    pub installed_plugins: Option<Vec<InstalledPlugin>>,
    /// Account used for the most recent sign-in.
    #[doc(hidden)]
    pub(crate) last_logged_in_user: Option<LoggedInUser>,
    /// Every account the host has signed in to on this machine.
    #[doc(hidden)]
    pub(crate) logged_in_users: Option<Vec<LoggedInUser>>,
    /// Whether the one-off cleanup of stored reasoning summaries has run.
    pub reasoning_summaries_cleanup_done: Option<bool>,
    /// Models the user selected recently, most recent first.
    pub recent_model_ids: Option<Vec<String>>,
    /// Whether the user declined to trust the sandbox credential proxy CA.
    pub sandbox_credential_proxy_ca_declined: Option<bool>,
    /// Whether the sandbox onboarding has been shown.
    pub sandbox_onboarding_shown: Option<bool>,
    /// Whether the user is a GitHub or Microsoft staff member, which unlocks internal-only behavior.
    pub staff: Option<bool>,
    /// Whether the user was recognized as GitHub staff.
    pub staff_github: Option<bool>,
    /// When the staff-only log level migration last ran.
    pub staff_log_level_migration_at: Option<String>,
    /// Whether the user was recognized as Microsoft staff.
    pub staff_microsoft: Option<bool>,
    /// When the staff-only model reset last ran.
    pub staff_model_reset_at: Option<String>,
    /// When the staff-only update channel migration last ran.
    pub staff_update_channel_migration_at: Option<String>,
    /// Folders where the user declined the init prompt, so it stays hidden there.
    pub suppress_init_folders: Option<Vec<String>>,
    /// Folders the user has marked as trusted.
    pub trusted_folders: Option<Vec<String>>,
}
crate::record_serde::wire_record!(GlobalStateLoadForConfigDirResult { app_tip_shown: "appTipShown" if is_none, asked_setup_terminals: "askedSetupTerminals" if is_none, auto_feedback_last_prompted_at: "autoFeedbackLastPromptedAt" if is_none, first_launch_at: "firstLaunchAt" if is_none, installed_plugins: "installedPlugins" if is_none, last_logged_in_user: "lastLoggedInUser" if is_none, logged_in_users: "loggedInUsers" if is_none, reasoning_summaries_cleanup_done: "reasoningSummariesCleanupDone" if is_none, recent_model_ids: "recentModelIds" if is_none, sandbox_credential_proxy_ca_declined: "sandboxCredentialProxyCaDeclined" if is_none, sandbox_onboarding_shown: "sandboxOnboardingShown" if is_none, staff: "staff" if is_none, staff_github: "staffGithub" if is_none, staff_log_level_migration_at: "staffLogLevelMigrationAt" if is_none, staff_microsoft: "staffMicrosoft" if is_none, staff_model_reset_at: "staffModelResetAt" if is_none, staff_update_channel_migration_at: "staffUpdateChannelMigrationAt" if is_none, suppress_init_folders: "suppressInitFolders" if is_none, trusted_folders: "trustedFolders" if is_none });

/// Slash commands available in the session, after applying any include/exclude filters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CommandsListResult {
    /// Commands available in this session
    pub commands: Vec<SlashCommandInfo>,
}
crate::record_serde::wire_record!(CommandsListResult {
    commands: "commands"
});

/// Feature availability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorsGetCapabilitiesResult {
    /// API version.
    pub api_version: i64,
    /// Availability.
    pub availability: ConnectorDiscoveryAvailability,
    /// Whether results are cached.
    pub conditional_cache: bool,
    /// Whether accounts are selected by opaque ID.
    pub opaque_account_selection: bool,
}
crate::record_serde::wire_record!(ConnectorsGetCapabilitiesResult {
    api_version: "apiVersion",
    availability: "availability",
    conditional_cache: "conditionalCache",
    opaque_account_selection: "opaqueAccountSelection"
});

/// Eligible accounts.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorsGetAccountsResult {
    /// Eligible accounts.
    pub accounts: Vec<ConnectorDiscoveryAccount>,
    /// Availability.
    pub availability: ConnectorDiscoveryAvailability,
}
crate::record_serde::wire_record!(ConnectorsGetAccountsResult {
    accounts: "accounts",
    availability: "availability"
});

/// Entries for the selected account.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorsListResult {
    /// Opaque account ID.
    pub account_id: String,
    /// Entries.
    pub connectors: Vec<ConnectorDiscoveryCatalogEntry>,
    /// Refresh time in Unix epoch milliseconds.
    pub refreshed_at_ms: i64,
    /// Revision.
    pub revision: i64,
}
crate::record_serde::wire_record!(ConnectorsListResult {
    account_id: "accountId",
    connectors: "connectors",
    refreshed_at_ms: "refreshedAtMs",
    revision: "revision"
});

/// Entries for the selected account.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ConnectorsRefreshResult {
    /// Opaque account ID.
    pub account_id: String,
    /// Entries.
    pub connectors: Vec<ConnectorDiscoveryCatalogEntry>,
    /// Refresh time in Unix epoch milliseconds.
    pub refreshed_at_ms: i64,
    /// Revision.
    pub revision: i64,
}
crate::record_serde::wire_record!(ConnectorsRefreshResult {
    account_id: "accountId",
    connectors: "connectors",
    refreshed_at_ms: "refreshedAtMs",
    revision: "revision"
});

/// Runtime-owned configuration inventory and configuration diagnostics.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct LspListResult {
    /// Effective servers in merge order.
    pub configs: Vec<LspServerSummary>,
    /// Effective plugin entries.
    pub plugin_servers: Vec<LspServerSummary>,
    /// Plugin configuration diagnostics.
    pub plugin_warnings: Vec<String>,
    /// Nearest project configuration file path.
    pub project_config_path: Option<String>,
    /// Failure to inspect project configuration.
    pub project_inspection_error: Option<String>,
    /// Project files rejected by the loader.
    pub project_problems: Vec<LspConfigurationProblem>,
    /// Project entries, including disabled entries absent from the effective set.
    pub project_servers: Vec<LspServerSummary>,
    /// User configuration read or validation failure.
    pub user_config_error: Option<String>,
    /// User configuration file path.
    pub user_config_path: String,
    /// Identifiers disabled through the user toggle list.
    pub user_disabled_servers: Vec<String>,
    /// User entries, including entries disabled by project configuration.
    pub user_servers: Vec<LspServerSummary>,
    /// Configuration loader diagnostics.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(LspListResult { configs: "configs", plugin_servers: "pluginServers", plugin_warnings: "pluginWarnings", project_config_path: "projectConfigPath" if is_none, project_inspection_error: "projectInspectionError" if is_none, project_problems: "projectProblems", project_servers: "projectServers", user_config_error: "userConfigError" if is_none, user_config_path: "userConfigPath", user_disabled_servers: "userDisabledServers", user_servers: "userServers", warnings: "warnings" });

/// The sharing status a cloud task carries after the call. Read `credentialResolved` first: when it is false no request was attempted at all, which is a different outcome from a request the service declined.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksGetSharingStatusResult {
    /// Whether a credential could be resolved for the supplied `authInfo` at all. When false no request was attempted and every other field is absent, which a caller must not report as a transport failure: the account simply has no usable credential.
    pub credential_resolved: bool,
    /// Whether the service returned the task successfully. Meaningful only when `credentialResolved` is true.
    pub ok: bool,
    /// The task's sharing status, as the service reported it. Absent when the call did not succeed, and also when the service returned a task that carries no sharing status at all. It is a free-form string rather than a closed set because the service owns these values; a caller that only understands some of them should treat an unrecognized one as unknown rather than as an error.
    pub sharing_status: Option<String>,
}
crate::record_serde::wire_record!(CloudTasksGetSharingStatusResult { credential_resolved: "credentialResolved", ok: "ok", sharing_status: "sharingStatus" if is_none });

/// The sharing status a cloud task carries after the call. Read `credentialResolved` first: when it is false no request was attempted at all, which is a different outcome from a request the service declined.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct CloudTasksSetSharingStatusResult {
    /// Whether a credential could be resolved for the supplied `authInfo` at all. When false no request was attempted and every other field is absent, which a caller must not report as a transport failure: the account simply has no usable credential.
    pub credential_resolved: bool,
    /// Whether the service returned the task successfully. Meaningful only when `credentialResolved` is true.
    pub ok: bool,
    /// The task's sharing status, as the service reported it. Absent when the call did not succeed, and also when the service returned a task that carries no sharing status at all. It is a free-form string rather than a closed set because the service owns these values; a caller that only understands some of them should treat an unrecognized one as unknown rather than as an error.
    pub sharing_status: Option<String>,
}
crate::record_serde::wire_record!(CloudTasksSetSharingStatusResult { credential_resolved: "credentialResolved", ok: "ok", sharing_status: "sharingStatus" if is_none });

/// Whether an active connection-owned transfer accepted cancellation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type AzureMlCancelResult = bool;

/// Active Azure ML download progress or null.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type AzureMlStatusResult = Option<AzureMlDownloadStatus>;

/// SHA-256 digest encoded as exactly 64 lowercase hexadecimal characters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type CardDigestValue = String;

/// Bounded extensible wire-feature identifier. Known values are described by `CatalogCapability`; newer callers may send future identifiers so an older runtime can return a typed negotiation refusal instead of failing schema validation. Capability negotiation establishes contract understanding, while each operation's result separately reports runtime availability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type CatalogCapabilityId = String;

/// Canonical catalogue resource identity. The runtime rewrites accepted urn:ai and urn:air identifiers to urn:air, lowercases the authority, and preserves the remaining resource components.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type CatalogResourceIdentity = String;

/// Bounded source-declared resource version, preserved exactly after validation and never inferred.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type CatalogResourceVersion = String;

/// Session account selection, or null.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type ConnectorSessionAccountResult = Option<ConnectorSessionAccount>;

/// Validated admission credential, or null when the application rejects it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type HostAuthenticateResult = Option<HostAuthenticationResult>;

/// HTTP headers as a map from lowercased header name to a list of values. Multi-valued headers (e.g. Set-Cookie) preserve all values.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type LlmInferenceHeaders = HashMap<String, Vec<String>>;

/// Pending warnings returned and cleared by session.quota.takeWarnings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type QuotaTakeWarningsResult = Vec<QuotaWarningProjection>;

/// Configured subagent settings, or null when absent
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SubagentSettings = Option<SubagentSettingsConfig>;

/// The form values submitted by the user (present when action is 'accept')
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type UIElicitationResponseContent = HashMap<String, serde_json::Value>;

/// Authentication outcome for each protected resource.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type AhpConnectionsAuthenticateResult = Vec<AhpConnectionResourceAuthResult>;

/// Indicates whether the command was accepted into the local execution queue.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum EnqueueCommandResult {
    AcceptedEnqueueCommandResult(AcceptedEnqueueCommandResult),
    UnsupportedEnqueueCommandResult(UnsupportedEnqueueCommandResult),
}

/// Which tier this directory belongs to
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentDiscoveryPathScope {
    /// The user's personal agent configuration directory.
    #[serde(rename = "user")]
    User,
    /// A project's repository agent directory.
    #[serde(rename = "project")]
    Project,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentDiscoveryPathScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentDiscoveryPathScope",
            &["user", "project", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::Project,
            _ => Self::Unknown,
        })
    }
}

/// Kind of attention required when status === "attention". Meaningful only when status === "attention".
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistryLiveTargetEntryAttentionKind {
    /// Session is blocked on an unrecoverable error
    #[serde(rename = "error")]
    Error,
    /// Session is waiting for a tool-permission decision
    #[serde(rename = "permission")]
    Permission,
    /// Session is waiting for the user to approve or reject a plan
    #[serde(rename = "exit_plan")]
    ExitPlan,
    /// Session is waiting on an elicitation prompt
    #[serde(rename = "elicitation")]
    Elicitation,
    /// Session is waiting for free-form user input
    #[serde(rename = "user_input")]
    UserInput,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentRegistryLiveTargetEntryAttentionKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistryLiveTargetEntryAttentionKind",
            &[
                "error",
                "permission",
                "exit_plan",
                "elicitation",
                "user_input",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Error,
            1 => Self::Permission,
            2 => Self::ExitPlan,
            3 => Self::Elicitation,
            4 => Self::UserInput,
            _ => Self::Unknown,
        })
    }
}

/// Process kind tag for the registry entry
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistryLiveTargetEntryKind {
    /// Interactive Copilot CLI exposing a UI server (legacy/normal CLI process)
    #[serde(rename = "ui-server")]
    UiServer,
    /// Headless `--server --managed-server` child spawned by a controller
    #[serde(rename = "managed-server")]
    ManagedServer,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentRegistryLiveTargetEntryKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistryLiveTargetEntryKind",
            &["ui-server", "managed-server", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::UiServer,
            1 => Self::ManagedServer,
            _ => Self::Unknown,
        })
    }
}

/// How the most recent turn ended (clean vs aborted). Lets the renderer distinguish done from done_cancelled.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistryLiveTargetEntryLastTerminalEvent {
    /// Last turn ended cleanly (model returned a final assistant message)
    #[serde(rename = "turn_end")]
    TurnEnd,
    /// Last turn was aborted (e.g. user interrupted)
    #[serde(rename = "abort")]
    Abort,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentRegistryLiveTargetEntryLastTerminalEvent {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistryLiveTargetEntryLastTerminalEvent",
            &["turn_end", "abort", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::TurnEnd,
            1 => Self::Abort,
            _ => Self::Unknown,
        })
    }
}

/// Coarse lifecycle status of the foreground session
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistryLiveTargetEntryStatus {
    /// Session is actively processing a turn
    #[serde(rename = "working")]
    Working,
    /// Session is idle, waiting for input
    #[serde(rename = "waiting")]
    Waiting,
    /// Last turn completed successfully
    #[serde(rename = "done")]
    Done,
    /// Session needs user attention (see attentionKind for the specific reason)
    #[serde(rename = "attention")]
    Attention,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentRegistryLiveTargetEntryStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistryLiveTargetEntryStatus",
            &["working", "waiting", "done", "attention", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Working,
            1 => Self::Waiting,
            2 => Self::Done,
            3 => Self::Attention,
            _ => Self::Unknown,
        })
    }
}

/// Categorized reason no canonical process log could be opened
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistryLogCaptureOpenErrorReason {
    /// Filesystem permission denied opening the log file
    #[serde(rename = "permission")]
    Permission,
    /// No space left on device
    #[serde(rename = "disk_full")]
    DiskFull,
    /// Other / uncategorized open failure
    #[serde(rename = "other")]
    Other,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentRegistryLogCaptureOpenErrorReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistryLogCaptureOpenErrorReason",
            &["permission", "disk_full", "other", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Permission,
            1 => Self::DiskFull,
            2 => Self::Other,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: child_process.spawn itself failed
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistrySpawnErrorKind {
    #[serde(rename = "spawn-error")]
    #[default]
    SpawnError,
}
impl<'de> Deserialize<'de> for AgentRegistrySpawnErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistrySpawnErrorKind",
            &["spawn-error"],
            None,
        )?;
        Ok(Self::SpawnError)
    }
}

/// Permission posture for the new session. 'yolo' requires the controller-local session to currently be in allow-all mode.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistrySpawnPermissionMode {
    /// Standard permission posture (prompts for each request)
    #[serde(rename = "default")]
    Default,
    /// Full allow-all (requires the controller-local session to currently be in allow-all mode)
    #[serde(rename = "yolo")]
    Yolo,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentRegistrySpawnPermissionMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistrySpawnPermissionMode",
            &["default", "yolo", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Default,
            1 => Self::Yolo,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: spawn succeeded but child never registered
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistrySpawnRegistryTimeoutKind {
    #[serde(rename = "registry-timeout")]
    #[default]
    RegistryTimeout,
}
impl<'de> Deserialize<'de> for AgentRegistrySpawnRegistryTimeoutKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistrySpawnRegistryTimeoutKind",
            &["registry-timeout"],
            None,
        )?;
        Ok(Self::RegistryTimeout)
    }
}

/// Discriminator: managed-server child spawned successfully
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistrySpawnSpawnedKind {
    #[serde(rename = "spawned")]
    #[default]
    Spawned,
}
impl<'de> Deserialize<'de> for AgentRegistrySpawnSpawnedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistrySpawnSpawnedKind",
            &["spawned"],
            None,
        )?;
        Ok(Self::Spawned)
    }
}

/// Which parameter field was invalid. Omitted when the rejection is not field-specific.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistrySpawnValidationErrorField {
    /// The cwd parameter
    #[serde(rename = "cwd")]
    Cwd,
    /// The session name parameter
    #[serde(rename = "name")]
    Name,
    /// The agentName parameter
    #[serde(rename = "agentName")]
    AgentName,
    /// The model parameter
    #[serde(rename = "model")]
    Model,
    /// The permissionMode parameter
    #[serde(rename = "permissionMode")]
    PermissionMode,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentRegistrySpawnValidationErrorField {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistrySpawnValidationErrorField",
            &[
                "cwd",
                "name",
                "agentName",
                "model",
                "permissionMode",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Cwd,
            1 => Self::Name,
            2 => Self::AgentName,
            3 => Self::Model,
            4 => Self::PermissionMode,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: synchronous pre-validation rejected the request
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistrySpawnValidationErrorKind {
    #[serde(rename = "validation-error")]
    #[default]
    ValidationError,
}
impl<'de> Deserialize<'de> for AgentRegistrySpawnValidationErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistrySpawnValidationErrorKind",
            &["validation-error"],
            None,
        )?;
        Ok(Self::ValidationError)
    }
}

/// Categorized reason for the rejection. Low-cardinality enum so telemetry can aggregate by reason without leaking raw paths or agent/model names.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AgentRegistrySpawnValidationErrorReason {
    /// Provided cwd does not exist on disk
    #[serde(rename = "cwd-not-found")]
    CwdNotFound,
    /// Provided cwd exists but is not a directory
    #[serde(rename = "cwd-not-directory")]
    CwdNotDirectory,
    /// Session name failed validateSessionName
    #[serde(rename = "invalid-name")]
    InvalidName,
    /// Requested agent name was not found in builtin or custom agents
    #[serde(rename = "unknown-agent")]
    UnknownAgent,
    /// Requested model is not available to this session
    #[serde(rename = "unknown-model")]
    UnknownModel,
    /// Caller asked for permissionMode='yolo' but the controller is not currently in allow-all mode
    #[serde(rename = "yolo-not-allowed")]
    YoloNotAllowed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AgentRegistrySpawnValidationErrorReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AgentRegistrySpawnValidationErrorReason",
            &[
                "cwd-not-found",
                "cwd-not-directory",
                "invalid-name",
                "unknown-agent",
                "unknown-model",
                "yolo-not-allowed",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::CwdNotFound,
            1 => Self::CwdNotDirectory,
            2 => Self::InvalidName,
            3 => Self::UnknownAgent,
            4 => Self::UnknownModel,
            5 => Self::YoloNotAllowed,
            _ => Self::Unknown,
        })
    }
}

/// Outcome of an agentRegistry.spawn call.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AgentRegistrySpawnResult {
    Spawned(AgentRegistrySpawnSpawned),
    SpawnError(AgentRegistrySpawnError),
    RegistryTimeout(AgentRegistrySpawnRegistryTimeout),
    ValidationError(AgentRegistrySpawnValidationError),
}

/// Outgoing AHP transport discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AhpConnectionTransportDirectKind {
    #[serde(rename = "direct")]
    #[default]
    Direct,
}
impl<'de> Deserialize<'de> for AhpConnectionTransportDirectKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AhpConnectionTransportDirectKind",
            &["direct"],
            None,
        )?;
        Ok(Self::Direct)
    }
}

/// Outgoing AHP transport discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AhpConnectionTransportMissionControlKind {
    #[serde(rename = "missionControl")]
    #[default]
    MissionControl,
}
impl<'de> Deserialize<'de> for AhpConnectionTransportMissionControlKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AhpConnectionTransportMissionControlKind",
            &["missionControl"],
            None,
        )?;
        Ok(Self::MissionControl)
    }
}

/// Transport selected for an outgoing connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AhpConnectionTransport {
    Direct(AhpConnectionTransportDirect),
    MissionControl(AhpConnectionTransportMissionControl),
}

/// Outgoing AHP event discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AhpConnectionEventStateKind {
    #[serde(rename = "state")]
    #[default]
    State,
}
impl<'de> Deserialize<'de> for AhpConnectionEventStateKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AhpConnectionEventStateKind",
            &["state"],
            None,
        )?;
        Ok(Self::State)
    }
}

/// Outgoing AHP event discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AhpConnectionEventProjectedKind {
    #[serde(rename = "projected")]
    #[default]
    Projected,
}
impl<'de> Deserialize<'de> for AhpConnectionEventProjectedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AhpConnectionEventProjectedKind",
            &["projected"],
            None,
        )?;
        Ok(Self::Projected)
    }
}

/// An event from the runtime-owned protocol engine.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum AhpConnectionEvent {
    State(AhpConnectionEventState),
    Projected(AhpConnectionEventProjected),
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentBlobType {
    #[serde(rename = "blob")]
    #[default]
    Blob,
}
impl<'de> Deserialize<'de> for AttachmentBlobType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentBlobType",
            &["blob"],
            None,
        )?;
        Ok(Self::Blob)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentDirectoryType {
    #[serde(rename = "directory")]
    #[default]
    Directory,
}
impl<'de> Deserialize<'de> for AttachmentDirectoryType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentDirectoryType",
            &["directory"],
            None,
        )?;
        Ok(Self::Directory)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentExtensionContextType {
    #[serde(rename = "extension_context")]
    #[default]
    ExtensionContext,
}
impl<'de> Deserialize<'de> for AttachmentExtensionContextType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentExtensionContextType",
            &["extension_context"],
            None,
        )?;
        Ok(Self::ExtensionContext)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentFileType {
    #[serde(rename = "file")]
    #[default]
    File,
}
impl<'de> Deserialize<'de> for AttachmentFileType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentFileType",
            &["file"],
            None,
        )?;
        Ok(Self::File)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentGitHubActionsJobType {
    #[serde(rename = "github_actions_job")]
    #[default]
    GitHubActionsJob,
}
impl<'de> Deserialize<'de> for AttachmentGitHubActionsJobType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentGitHubActionsJobType",
            &["github_actions_job"],
            None,
        )?;
        Ok(Self::GitHubActionsJob)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentGitHubCommitType {
    #[serde(rename = "github_commit")]
    #[default]
    GitHubCommit,
}
impl<'de> Deserialize<'de> for AttachmentGitHubCommitType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentGitHubCommitType",
            &["github_commit"],
            None,
        )?;
        Ok(Self::GitHubCommit)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentGitHubFileType {
    #[serde(rename = "github_file")]
    #[default]
    GitHubFile,
}
impl<'de> Deserialize<'de> for AttachmentGitHubFileType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentGitHubFileType",
            &["github_file"],
            None,
        )?;
        Ok(Self::GitHubFile)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentGitHubFileDiffType {
    #[serde(rename = "github_file_diff")]
    #[default]
    GitHubFileDiff,
}
impl<'de> Deserialize<'de> for AttachmentGitHubFileDiffType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentGitHubFileDiffType",
            &["github_file_diff"],
            None,
        )?;
        Ok(Self::GitHubFileDiff)
    }
}

/// Type of GitHub reference
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentGitHubReferenceType {
    /// GitHub issue reference.
    #[serde(rename = "issue")]
    Issue,
    /// GitHub pull request reference.
    #[serde(rename = "pr")]
    Pr,
    /// GitHub discussion reference.
    #[serde(rename = "discussion")]
    Discussion,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AttachmentGitHubReferenceType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentGitHubReferenceType",
            &["issue", "pr", "discussion", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Issue,
            1 => Self::Pr,
            2 => Self::Discussion,
            _ => Self::Unknown,
        })
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentGitHubReleaseType {
    #[serde(rename = "github_release")]
    #[default]
    GitHubRelease,
}
impl<'de> Deserialize<'de> for AttachmentGitHubReleaseType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentGitHubReleaseType",
            &["github_release"],
            None,
        )?;
        Ok(Self::GitHubRelease)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentGitHubRepositoryType {
    #[serde(rename = "github_repository")]
    #[default]
    GitHubRepository,
}
impl<'de> Deserialize<'de> for AttachmentGitHubRepositoryType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentGitHubRepositoryType",
            &["github_repository"],
            None,
        )?;
        Ok(Self::GitHubRepository)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentGitHubSnippetType {
    #[serde(rename = "github_snippet")]
    #[default]
    GitHubSnippet,
}
impl<'de> Deserialize<'de> for AttachmentGitHubSnippetType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentGitHubSnippetType",
            &["github_snippet"],
            None,
        )?;
        Ok(Self::GitHubSnippet)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentGitHubTreeComparisonType {
    #[serde(rename = "github_tree_comparison")]
    #[default]
    GitHubTreeComparison,
}
impl<'de> Deserialize<'de> for AttachmentGitHubTreeComparisonType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentGitHubTreeComparisonType",
            &["github_tree_comparison"],
            None,
        )?;
        Ok(Self::GitHubTreeComparison)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentGitHubUrlType {
    #[serde(rename = "github_url")]
    #[default]
    GitHubUrl,
}
impl<'de> Deserialize<'de> for AttachmentGitHubUrlType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentGitHubUrlType",
            &["github_url"],
            None,
        )?;
        Ok(Self::GitHubUrl)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentSelectionType {
    #[serde(rename = "selection")]
    #[default]
    Selection,
}
impl<'de> Deserialize<'de> for AttachmentSelectionType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentSelectionType",
            &["selection"],
            None,
        )?;
        Ok(Self::Selection)
    }
}

/// Attachment type discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AttachmentSessionReferenceType {
    #[serde(rename = "session_reference")]
    #[default]
    SessionReference,
}
impl<'de> Deserialize<'de> for AttachmentSessionReferenceType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "AttachmentSessionReferenceType",
            &["session_reference"],
            None,
        )?;
        Ok(Self::SessionReference)
    }
}

/// How catalog discovery handles a recent sign-in failure.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AzureMlCatalogRetryMode {
    /// Reuse a recent failure for automatic background discovery.
    #[serde(rename = "background")]
    Background,
    /// Retry sign-in after an explicit refresh.
    #[serde(rename = "explicit")]
    Explicit,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AzureMlCatalogRetryMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AzureMlCatalogRetryMode",
            &["background", "explicit", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Background,
            1 => Self::Explicit,
            _ => Self::Unknown,
        })
    }
}

/// Registry that owns an Azure ML model version.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AzureMlRegistry {
    /// Public Azure ML model registry.
    #[serde(rename = "azureml")]
    Azureml,
    /// Microsoft staff development registry.
    #[serde(rename = "azureml-dev")]
    AzuremlDev,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AzureMlRegistry {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AzureMlRegistry",
            &["azureml", "azureml-dev", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Azureml,
            1 => Self::AzuremlDev,
            _ => Self::Unknown,
        })
    }
}

/// Result category of a managed provider configuration change.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AzureMlRegistrationKind {
    /// The requested managed rows now exist or are removed.
    #[serde(rename = "written")]
    Written,
    /// The document is malformed and remains unchanged.
    #[serde(rename = "skipped")]
    Skipped,
    /// A read, transaction, or write failed.
    #[serde(rename = "failed")]
    Failed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AzureMlRegistrationKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AzureMlRegistrationKind",
            &["written", "skipped", "failed", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Written,
            1 => Self::Skipped,
            2 => Self::Failed,
            _ => Self::Unknown,
        })
    }
}

/// A managed registration change that leaves unrelated provider rows unchanged.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AzureMlRegistrationOperation {
    /// Replace the managed rows and return their prior contents.
    #[serde(rename = "upsert")]
    Upsert,
    /// Remove this model and remove its provider only when unreferenced.
    #[serde(rename = "remove")]
    Remove,
    /// Restore the prior managed rows without changing unrelated entries.
    #[serde(rename = "restore")]
    Restore,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AzureMlRegistrationOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AzureMlRegistrationOperation",
            &["upsert", "remove", "restore", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Upsert,
            1 => Self::Remove,
            2 => Self::Restore,
            _ => Self::Unknown,
        })
    }
}

/// Embedded package identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum CafeMetricsEmbeddedPackageIdentity {
    /// Development package.
    #[serde(rename = "dev")]
    Dev,
    /// Release package.
    #[serde(rename = "release")]
    Release,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CafeMetricsEmbeddedPackageIdentity {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsEmbeddedPackageIdentity",
            &["dev", "release", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Dev,
            1 => Self::Release,
            _ => Self::Unknown,
        })
    }
}

/// Initial requested CLI launch intent, not the eventual selection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum CafeMetricsLaunchScenario {
    /// Intentional new session.
    #[serde(rename = "fresh")]
    Fresh,
    /// Explicit resume or continue.
    #[serde(rename = "resume")]
    Resume,
    /// Undecided startup selection.
    #[serde(rename = "picker")]
    Picker,
    /// Explicit remote entry.
    #[serde(rename = "remote")]
    Remote,
    /// Restart manifest.
    #[serde(rename = "restart")]
    Restart,
    /// Intent unavailable.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CafeMetricsLaunchScenario {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsLaunchScenario",
            &[
                "fresh", "resume", "picker", "remote", "restart", "unknown", "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::Fresh,
            1 => Self::Resume,
            2 => Self::Picker,
            3 => Self::Remote,
            4 => Self::Restart,
            5 => Self::UnknownValue,
            _ => Self::Unknown,
        })
    }
}

/// Terminal resume outcome.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum CafeMetricsResumeOutcome {
    /// Resumed.
    #[serde(rename = "success")]
    Success,
    /// Recovered and resumed.
    #[serde(rename = "recovered")]
    Recovered,
    /// Resume failed.
    #[serde(rename = "failed")]
    Failed,
    /// No session found.
    #[serde(rename = "not_found")]
    NotFound,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CafeMetricsResumeOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsResumeOutcome",
            &["success", "recovered", "failed", "not_found", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Success,
            1 => Self::Recovered,
            2 => Self::Failed,
            3 => Self::NotFound,
            _ => Self::Unknown,
        })
    }
}

/// Bounded resume reason, without error text.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum CafeMetricsResumeReason {
    /// No detail.
    #[serde(rename = "none")]
    None,
    /// Recovered event log.
    #[serde(rename = "log_recovered")]
    LogRecovered,
    /// Corrupt event log.
    #[serde(rename = "log_corrupt")]
    LogCorrupt,
    /// Legacy format.
    #[serde(rename = "legacy_format")]
    LegacyFormat,
    /// Recovery I/O failed.
    #[serde(rename = "recovery_io")]
    RecoveryIo,
    /// Hydration failed.
    #[serde(rename = "hydration")]
    Hydration,
    /// Construction failed.
    #[serde(rename = "construction")]
    Construction,
    /// Queue restore failed.
    #[serde(rename = "queue")]
    Queue,
    /// Session not found.
    #[serde(rename = "not_found")]
    NotFound,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CafeMetricsResumeReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsResumeReason",
            &[
                "none",
                "log_recovered",
                "log_corrupt",
                "legacy_format",
                "recovery_io",
                "hydration",
                "construction",
                "queue",
                "not_found",
                "Unknown",
            ],
            Some(9),
        )?;
        Ok(match index {
            0 => Self::None,
            1 => Self::LogRecovered,
            2 => Self::LogCorrupt,
            3 => Self::LegacyFormat,
            4 => Self::RecoveryIo,
            5 => Self::Hydration,
            6 => Self::Construction,
            7 => Self::Queue,
            8 => Self::NotFound,
            _ => Self::Unknown,
        })
    }
}

/// Last resume phase.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum CafeMetricsResumeStage {
    /// Read the event log.
    #[serde(rename = "event_log")]
    EventLog,
    /// Construct the session.
    #[serde(rename = "construction")]
    Construction,
    /// Restore the queue.
    #[serde(rename = "queue_restore")]
    QueueRestore,
    /// Resume complete.
    #[serde(rename = "complete")]
    Complete,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CafeMetricsResumeStage {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsResumeStage",
            &[
                "event_log",
                "construction",
                "queue_restore",
                "complete",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::EventLog,
            1 => Self::Construction,
            2 => Self::QueueRestore,
            3 => Self::Complete,
            _ => Self::Unknown,
        })
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CafeMetricsResolveAdmissionRequestSuccessKind {
    #[serde(rename = "success")]
    #[default]
    Success,
}
impl<'de> Deserialize<'de> for CafeMetricsResolveAdmissionRequestSuccessKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsResolveAdmissionRequestSuccessKind",
            &["success"],
            None,
        )?;
        Ok(Self::Success)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CafeMetricsResolveAdmissionRequestFailedKind {
    #[serde(rename = "failed")]
    #[default]
    Failed,
}
impl<'de> Deserialize<'de> for CafeMetricsResolveAdmissionRequestFailedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsResolveAdmissionRequestFailedKind",
            &["failed"],
            None,
        )?;
        Ok(Self::Failed)
    }
}

/// Immutable release lookup result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum CafeMetricsResolveAdmissionRequest {
    Success(CafeMetricsResolveAdmissionRequestSuccess),
    Failed(CafeMetricsResolveAdmissionRequestFailed),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CafeMetricsResolveAdmissionResultAppliedKind {
    #[serde(rename = "applied")]
    #[default]
    Applied,
}
impl<'de> Deserialize<'de> for CafeMetricsResolveAdmissionResultAppliedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsResolveAdmissionResultAppliedKind",
            &["applied"],
            None,
        )?;
        Ok(Self::Applied)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CafeMetricsResolveAdmissionResultNotStartedKind {
    #[serde(rename = "not-started")]
    #[default]
    NotStarted,
}
impl<'de> Deserialize<'de> for CafeMetricsResolveAdmissionResultNotStartedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsResolveAdmissionResultNotStartedKind",
            &["not-started"],
            None,
        )?;
        Ok(Self::NotStarted)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CafeMetricsResolveAdmissionResultAlreadyResolvedKind {
    #[serde(rename = "already-resolved")]
    #[default]
    AlreadyResolved,
}
impl<'de> Deserialize<'de> for CafeMetricsResolveAdmissionResultAlreadyResolvedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsResolveAdmissionResultAlreadyResolvedKind",
            &["already-resolved"],
            None,
        )?;
        Ok(Self::AlreadyResolved)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CafeMetricsResolveAdmissionResultShutdownKind {
    #[serde(rename = "shutdown")]
    #[default]
    Shutdown,
}
impl<'de> Deserialize<'de> for CafeMetricsResolveAdmissionResultShutdownKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsResolveAdmissionResultShutdownKind",
            &["shutdown"],
            None,
        )?;
        Ok(Self::Shutdown)
    }
}

/// Admission result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum CafeMetricsResolveAdmissionResult {
    Applied(CafeMetricsResolveAdmissionResultApplied),
    NotStarted(CafeMetricsResolveAdmissionResultNotStarted),
    AlreadyResolved(CafeMetricsResolveAdmissionResultAlreadyResolved),
    Shutdown(CafeMetricsResolveAdmissionResultShutdown),
}

/// Reason startup was refused.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum CafeMetricsStartRefusal {
    /// The process is offline.
    #[serde(rename = "offline")]
    Offline,
    /// Unsupported platform.
    #[serde(rename = "unsupportedPlatform")]
    UnsupportedPlatform,
    /// The process already started CAFE.
    #[serde(rename = "alreadyStarted")]
    AlreadyStarted,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CafeMetricsStartRefusal {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CafeMetricsStartRefusal",
            &[
                "offline",
                "unsupportedPlatform",
                "alreadyStarted",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Offline,
            1 => Self::UnsupportedPlatform,
            2 => Self::AlreadyStarted,
            _ => Self::Unknown,
        })
    }
}

/// Explicit Agent Plugin compatibility declared by exact catalog tags. Clients must not infer these values from display text or other metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAgentPluginCompatibilityTag {
    /// The plugin contributes at least one GitHub Copilot Canvas.
    #[serde(rename = "canvas")]
    Canvas,
    /// The plugin depends on Canvas for its intended functionality.
    #[serde(rename = "canvas-only")]
    CanvasOnly,
    /// The plugin targets GitHub Copilot.
    #[serde(rename = "github-copilot")]
    GitHubCopilot,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAgentPluginCompatibilityTag {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAgentPluginCompatibilityTag",
            &["canvas", "canvas-only", "github-copilot", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Canvas,
            1 => Self::CanvasOnly,
            2 => Self::GitHubCopilot,
            _ => Self::Unknown,
        })
    }
}

/// Typed installability state for an Agent Plugin candidate
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAgentPluginInstallability {
    /// The candidate carries complete, verified install data and the selected session may install it. Its install plan is present.
    #[serde(rename = "installable")]
    Installable,
    /// The catalogue supplied no complete, verified install data, so no install target is offered.
    #[serde(rename = "install-data-unavailable")]
    InstallDataUnavailable,
    /// Plugin installation is understood but disabled for the selected session.
    #[serde(rename = "feature-disabled")]
    FeatureDisabled,
    /// Reserved for plugin formats this runtime cannot install. Every format surfaced today installs through a marketplace.
    #[serde(rename = "not-installable-kind")]
    NotInstallableKind,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAgentPluginInstallability {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAgentPluginInstallability",
            &[
                "installable",
                "install-data-unavailable",
                "feature-disabled",
                "not-installable-kind",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Installable,
            1 => Self::InstallDataUnavailable,
            2 => Self::FeatureDisabled,
            3 => Self::NotInstallableKind,
            _ => Self::Unknown,
        })
    }
}

/// A kind of component an Agent Plugin contributes
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAgentPluginComponent {
    /// Custom agents.
    #[serde(rename = "agent")]
    Agent,
    /// Slash commands.
    #[serde(rename = "command")]
    Command,
    /// Skills.
    #[serde(rename = "skill")]
    Skill,
    /// Hooks.
    #[serde(rename = "hook")]
    Hook,
    /// Bundled MCP servers.
    #[serde(rename = "mcp-server")]
    McpServer,
    /// Bundled LSP servers.
    #[serde(rename = "lsp-server")]
    LspServer,
    /// Extensions.
    #[serde(rename = "extension")]
    Extension,
    /// Canvases.
    #[serde(rename = "canvas")]
    Canvas,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAgentPluginComponent {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAgentPluginComponent",
            &[
                "agent",
                "command",
                "skill",
                "hook",
                "mcp-server",
                "lsp-server",
                "extension",
                "canvas",
                "Unknown",
            ],
            Some(8),
        )?;
        Ok(match index {
            0 => Self::Agent,
            1 => Self::Command,
            2 => Self::Skill,
            3 => Self::Hook,
            4 => Self::McpServer,
            5 => Self::LspServer,
            6 => Self::Extension,
            7 => Self::Canvas,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for a materialisation install source
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAgentPluginMaterializationSourceKind {
    /// Install from Agent Finder's verified materialisation.
    #[serde(rename = "materialization")]
    Materialization,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAgentPluginMaterializationSourceKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAgentPluginMaterializationSourceKind",
            &["materialization", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Materialization,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for a marketplace install source
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAgentPluginMarketplaceSourceKind {
    /// Install from a plugin marketplace.
    #[serde(rename = "marketplace")]
    Marketplace,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAgentPluginMarketplaceSourceKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAgentPluginMarketplaceSourceKind",
            &["marketplace", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Marketplace,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for a repository install source
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAgentPluginRepositorySourceKind {
    /// Install directly from a GitHub repository.
    #[serde(rename = "repository")]
    Repository,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAgentPluginRepositorySourceKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAgentPluginRepositorySourceKind",
            &["repository", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Repository,
            _ => Self::Unknown,
        })
    }
}

/// One place an Agent Plugin can install from: Agent Finder's materialisation, a marketplace, or a GitHub repository. Each variant carries only its own fields.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CatalogAgentPluginInstallSource {
    Materialization(CatalogAgentPluginMaterializationSource),
    Marketplace(CatalogAgentPluginMarketplaceSource),
    Repository(CatalogAgentPluginRepositorySource),
}

/// Discriminator for an Agent Plugin candidate
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAgentPluginCandidateKind {
    /// An Agent Plugin.
    #[serde(rename = "plugin")]
    Plugin,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAgentPluginCandidateKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAgentPluginCandidateKind",
            &["plugin", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Plugin,
            _ => Self::Unknown,
        })
    }
}

/// Media type naming the plugin format of an Agent Plugin candidate. Values other than `application/vnd.github.copilot-plugin` appear only when the caller requires the agent-plugin-formats capability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAgentPluginMediaType {
    /// A GitHub Copilot Agent Plugin descriptor.
    #[serde(rename = "application/vnd.github.copilot-plugin")]
    ApplicationVndGitHubCopilotPlugin,
    /// A Claude plugin descriptor (`.claude-plugin/plugin.json` layout).
    #[serde(rename = "application/vnd.anthropic.claude-plugin+json")]
    ApplicationVndAnthropicClaudePluginJson,
    /// A portable Agent Plugin descriptor.
    #[serde(rename = "application/agent-plugin+json")]
    ApplicationAgentPluginJson,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAgentPluginMediaType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAgentPluginMediaType",
            &[
                "application/vnd.github.copilot-plugin",
                "application/vnd.anthropic.claude-plugin+json",
                "application/agent-plugin+json",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::ApplicationVndGitHubCopilotPlugin,
            1 => Self::ApplicationVndAnthropicClaudePluginJson,
            2 => Self::ApplicationAgentPluginJson,
            _ => Self::Unknown,
        })
    }
}

/// Typed installability state for an AI skill candidate
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAiSkillInstallability {
    /// This AI skill candidate carries verified materialisation metadata and the selected session may plan installation.
    #[serde(rename = "installable")]
    Installable,
    /// Skill installation is understood but disabled for the selected session.
    #[serde(rename = "feature-disabled")]
    FeatureDisabled,
    /// The candidate lacks verified materialisation metadata required for installation.
    #[serde(rename = "materialisation-unavailable")]
    MaterialisationUnavailable,
    /// Policy refuses Skill installation for the selected session or authority.
    #[serde(rename = "policy-forbids")]
    PolicyForbids,
    /// Compatibility value for discovery-only callers that did not negotiate Skill installation.
    #[serde(rename = "not-installable-kind")]
    NotInstallableKind,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAiSkillInstallability {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAiSkillInstallability",
            &[
                "installable",
                "feature-disabled",
                "materialisation-unavailable",
                "policy-forbids",
                "not-installable-kind",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Installable,
            1 => Self::FeatureDisabled,
            2 => Self::MaterialisationUnavailable,
            3 => Self::PolicyForbids,
            4 => Self::NotInstallableKind,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for an AI skill candidate
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAiSkillCandidateKind {
    /// An AI skill.
    #[serde(rename = "ai-skill")]
    AiSkill,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAiSkillCandidateKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAiSkillCandidateKind",
            &["ai-skill", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::AiSkill,
            _ => Self::Unknown,
        })
    }
}

/// Canonical AI skill media type
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogAiSkillMediaType {
    /// An AI skill card.
    #[serde(rename = "application/ai-skill")]
    ApplicationAiSkill,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogAiSkillMediaType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogAiSkillMediaType",
            &["application/ai-skill", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::ApplicationAiSkill,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: the card is URL-backed, and carries no embedded data
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogCandidateSourceUrlKind {
    #[serde(rename = "url")]
    #[default]
    Url,
}
impl<'de> Deserialize<'de> for CatalogCandidateSourceUrlKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogCandidateSourceUrlKind",
            &["url"],
            None,
        )?;
        Ok(Self::Url)
    }
}

/// Discriminator: the card is embedded, and carries no URL
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogCandidateSourceEmbeddedKind {
    #[serde(rename = "embedded")]
    #[default]
    Embedded,
}
impl<'de> Deserialize<'de> for CatalogCandidateSourceEmbeddedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogCandidateSourceEmbeddedKind",
            &["embedded"],
            None,
        )?;
        Ok(Self::Embedded)
    }
}

/// Where a candidate's card came from. Exactly one of a URL or embedded data: the union has no variant carrying both, and no variant carrying neither, so the rule holds structurally rather than by validation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CatalogCandidateSource {
    Url(CatalogCandidateSourceUrl),
    Embedded(CatalogCandidateSourceEmbedded),
}

/// Whether an MCP server candidate can be planned for installation
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogMcpServerInstallability {
    /// An install plan can be computed for this MCP server candidate.
    #[serde(rename = "installable")]
    Installable,
    /// Policy forbids installing this MCP server candidate.
    #[serde(rename = "not-installable-policy")]
    NotInstallablePolicy,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogMcpServerInstallability {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogMcpServerInstallability",
            &["installable", "not-installable-policy", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Installable,
            1 => Self::NotInstallablePolicy,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for an MCP server candidate
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogMcpServerCandidateKind {
    /// An MCP server.
    #[serde(rename = "mcp-server")]
    McpServer,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogMcpServerCandidateKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogMcpServerCandidateKind",
            &["mcp-server", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::McpServer,
            _ => Self::Unknown,
        })
    }
}

/// One inert catalog result, represented as an MCP server, discovery-only AI skill, or opt-in Agent Plugin variant so kind, media type, provenance, and available operations cannot contradict each other.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CatalogCandidate {
    McpServer(CatalogMcpServerCandidate),
    AiSkill(CatalogAiSkillCandidate),
    Plugin(CatalogAgentPluginCandidate),
}

/// What kind of resource a catalog candidate describes
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogCandidateKind {
    /// An MCP server, which can be planned for installation.
    #[serde(rename = "mcp-server")]
    McpServer,
    /// An AI skill, which is discoverable but not installable through this surface.
    #[serde(rename = "ai-skill")]
    AiSkill,
    /// An inert Agent Plugin candidate, available only when explicitly requested.
    #[serde(rename = "plugin")]
    Plugin,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogCandidateKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogCandidateKind",
            &["mcp-server", "ai-skill", "plugin", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::McpServer,
            1 => Self::AiSkill,
            2 => Self::Plugin,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for an available feed entry
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogFeedAvailableStatus {
    /// The entry's resource resolves and can be presented.
    #[serde(rename = "available")]
    Available,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogFeedAvailableStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogFeedAvailableStatus",
            &["available", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Available,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for a feed entry filtered by trust tier
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogFeedFilteredStatus {
    /// The entry's resource is not in a requested trust tier.
    #[serde(rename = "filtered")]
    Filtered,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogFeedFilteredStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogFeedFilteredStatus",
            &["filtered", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Filtered,
            _ => Self::Unknown,
        })
    }
}

/// Why a feed entry cannot be presented
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogFeedUnavailableReason {
    /// The identifier no longer resolves to a resource.
    #[serde(rename = "not-found")]
    NotFound,
    /// The resource is of a kind this caller did not negotiate or this runtime does not support.
    #[serde(rename = "unsupported-kind")]
    UnsupportedKind,
    /// The resource failed the same validation catalog.search applies, or repeats an earlier entry.
    #[serde(rename = "invalid-resource")]
    InvalidResource,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogFeedUnavailableReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogFeedUnavailableReason",
            &[
                "not-found",
                "unsupported-kind",
                "invalid-resource",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::NotFound,
            1 => Self::UnsupportedKind,
            2 => Self::InvalidResource,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for an unavailable feed entry
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogFeedUnavailableStatus {
    /// The entry's resource cannot be presented.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogFeedUnavailableStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogFeedUnavailableStatus",
            &["unavailable", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Unavailable,
            _ => Self::Unknown,
        })
    }
}

/// Service-computed trust tier currently emitted by Agent Finder. It is independent of search score, popularity, and client-side ranking.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustTier {
    /// Tier one as assigned by the catalogue authority.
    T1,
    /// Tier two as assigned by the catalogue authority.
    T2,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustTier {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustTier",
            &["T1", "T2", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::T1,
            1 => Self::T2,
            _ => Self::Unknown,
        })
    }
}

/// One feed entry, in its feed position. Only available entries carry a candidate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CatalogFeedEntry {
    Available(CatalogFeedAvailableEntry),
    Unavailable(CatalogFeedUnavailableEntry),
    Filtered(CatalogFeedFilteredEntry),
}

/// Discriminator: the feed was retrieved
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogGetGeneratedFeedSucceededKind {
    #[serde(rename = "succeeded")]
    #[default]
    Succeeded,
}
impl<'de> Deserialize<'de> for CatalogGetGeneratedFeedSucceededKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogGetGeneratedFeedSucceededKind",
            &["succeeded"],
            None,
        )?;
        Ok(Self::Succeeded)
    }
}

/// Discriminator: the feed was retrieved
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogGetSavedFeedSucceededKind {
    #[serde(rename = "succeeded")]
    #[default]
    Succeeded,
}
impl<'de> Deserialize<'de> for CatalogGetSavedFeedSucceededKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogGetSavedFeedSucceededKind",
            &["succeeded"],
            None,
        )?;
        Ok(Self::Succeeded)
    }
}

/// Bounded refusal categories for planning from a materialisation reference.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogReferenceInstallationFailureReason {
    /// The selected session does not have the installation feature flag for the referenced kind enabled.
    #[serde(rename = "feature-disabled")]
    FeatureDisabled,
    /// The request is unsupported or malformed.
    #[serde(rename = "invalid-request")]
    InvalidRequest,
    /// The bounded operation limit was reached.
    #[serde(rename = "operation-limit")]
    OperationLimit,
    /// Installation storage or admission is busy.
    #[serde(rename = "busy")]
    Busy,
    /// The original host cannot receive human confirmation.
    #[serde(rename = "confirmation-unavailable")]
    ConfirmationUnavailable,
    /// Existing authenticated session and host authority is unavailable.
    #[serde(rename = "policy-context-unavailable")]
    PolicyContextUnavailable,
    /// Managed policy forbids installing this resource.
    #[serde(rename = "policy-blocked")]
    PolicyBlocked,
    /// The materialisation reference is malformed or uses an unsupported schema version.
    #[serde(rename = "invalid-reference")]
    InvalidReference,
    /// The materialisation descriptor could not be retrieved safely.
    #[serde(rename = "descriptor-unavailable")]
    DescriptorUnavailable,
    /// Agent Finder has no materialisation for the referenced resource revision (HTTP 404).
    #[serde(rename = "descriptor-not-found")]
    DescriptorNotFound,
    /// Agent Finder withdrew the referenced resource revision (HTTP 410).
    #[serde(rename = "descriptor-withdrawn")]
    DescriptorWithdrawn,
    /// The materialisation descriptor failed validation.
    #[serde(rename = "descriptor-invalid")]
    DescriptorInvalid,
    /// The retrieved descriptor does not match the referenced descriptor or bundle digest.
    #[serde(rename = "digest-mismatch")]
    DigestMismatch,
    /// The referenced resource is a kind this runtime cannot install, or one whose installation capability the caller did not require.
    #[serde(rename = "unsupported-resource")]
    UnsupportedResource,
    /// The resource's entrypoint could not be retrieved or verified.
    #[serde(rename = "entrypoint-unavailable")]
    EntrypointUnavailable,
    /// GitHub rate limited retrieval from the pinned public source. Retry after retryAfterSeconds when present.
    #[serde(rename = "source-rate-limited")]
    SourceRateLimited,
    /// No GitHub sign-in or credential is available, so the caller must sign in.
    #[serde(rename = "no-credential")]
    NoCredential,
    /// Agent Finder refused the GitHub credential (HTTP 401, or a 403 without rate-limit evidence); the caller must sign in again.
    #[serde(rename = "credential-rejected")]
    CredentialRejected,
    /// The Skill entrypoint is not a valid Skill.
    #[serde(rename = "invalid-skill")]
    InvalidSkill,
    /// The plugin manifest is not a valid plugin manifest, or the bundle has another manifest that would take precedence over the reviewed one.
    #[serde(rename = "invalid-plugin")]
    InvalidPlugin,
    /// The entrypoint exceeds the bounded complete review size.
    #[serde(rename = "review-too-large")]
    ReviewTooLarge,
    /// A resource with the same identity or install location already exists.
    #[serde(rename = "already-installed")]
    AlreadyInstalled,
    /// A storage operation failed; nothing was installed.
    #[serde(rename = "write-failed")]
    WriteFailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogReferenceInstallationFailureReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogReferenceInstallationFailureReason",
            &[
                "feature-disabled",
                "invalid-request",
                "operation-limit",
                "busy",
                "confirmation-unavailable",
                "policy-context-unavailable",
                "policy-blocked",
                "invalid-reference",
                "descriptor-unavailable",
                "descriptor-not-found",
                "descriptor-withdrawn",
                "descriptor-invalid",
                "digest-mismatch",
                "unsupported-resource",
                "entrypoint-unavailable",
                "source-rate-limited",
                "no-credential",
                "credential-rejected",
                "invalid-skill",
                "invalid-plugin",
                "review-too-large",
                "already-installed",
                "write-failed",
                "Unknown",
            ],
            Some(23),
        )?;
        Ok(match index {
            0 => Self::FeatureDisabled,
            1 => Self::InvalidRequest,
            2 => Self::OperationLimit,
            3 => Self::Busy,
            4 => Self::ConfirmationUnavailable,
            5 => Self::PolicyContextUnavailable,
            6 => Self::PolicyBlocked,
            7 => Self::InvalidReference,
            8 => Self::DescriptorUnavailable,
            9 => Self::DescriptorNotFound,
            10 => Self::DescriptorWithdrawn,
            11 => Self::DescriptorInvalid,
            12 => Self::DigestMismatch,
            13 => Self::UnsupportedResource,
            14 => Self::EntrypointUnavailable,
            15 => Self::SourceRateLimited,
            16 => Self::NoCredential,
            17 => Self::CredentialRejected,
            18 => Self::InvalidSkill,
            19 => Self::InvalidPlugin,
            20 => Self::ReviewTooLarge,
            21 => Self::AlreadyInstalled,
            22 => Self::WriteFailed,
            _ => Self::Unknown,
        })
    }
}

/// Reference planning outcome discriminator naming the planned kind.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogReferenceInstallationOutcomeSkillInstallPlannedKind {
    #[serde(rename = "skill-install-planned")]
    #[default]
    SkillInstallPlanned,
}
impl<'de> Deserialize<'de> for CatalogReferenceInstallationOutcomeSkillInstallPlannedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogReferenceInstallationOutcomeSkillInstallPlannedKind",
            &["skill-install-planned"],
            None,
        )?;
        Ok(Self::SkillInstallPlanned)
    }
}

/// Reference planning outcome discriminator naming the planned kind.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogReferenceInstallationOutcomePluginInstallPlannedKind {
    #[serde(rename = "plugin-install-planned")]
    #[default]
    PluginInstallPlanned,
}
impl<'de> Deserialize<'de> for CatalogReferenceInstallationOutcomePluginInstallPlannedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogReferenceInstallationOutcomePluginInstallPlannedKind",
            &["plugin-install-planned"],
            None,
        )?;
        Ok(Self::PluginInstallPlanned)
    }
}

/// Reference planning outcome discriminator naming the planned kind.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogReferenceInstallationOutcomeRefusedKind {
    #[serde(rename = "refused")]
    #[default]
    Refused,
}
impl<'de> Deserialize<'de> for CatalogReferenceInstallationOutcomeRefusedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogReferenceInstallationOutcomeRefusedKind",
            &["refused"],
            None,
        )?;
        Ok(Self::Refused)
    }
}

/// Planning outcome for a materialisation reference. A plan names its kind, so the caller applies it with that kind's apply method.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CatalogReferenceInstallationOutcome {
    SkillInstallPlanned(CatalogReferenceInstallationOutcomeSkillInstallPlanned),
    PluginInstallPlanned(CatalogReferenceInstallationOutcomePluginInstallPlanned),
    Refused(CatalogReferenceInstallationOutcomeRefused),
}

/// Discriminator: handled planning outcome.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogReferenceInstallationResultOutcomeKind {
    #[serde(rename = "outcome")]
    #[default]
    Outcome,
}
impl<'de> Deserialize<'de> for CatalogReferenceInstallationResultOutcomeKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogReferenceInstallationResultOutcomeKind",
            &["outcome"],
            None,
        )?;
        Ok(Self::Outcome)
    }
}

/// Planning result for a materialisation reference with the honoured contract, or a typed refusal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CatalogReferenceInstallationResult {
    Outcome(CatalogReferenceInstallationResultOutcome),
    NegotiationRefused(CatalogNegotiationRefusedError),
    InvalidRequest(CatalogInvalidRequestError),
    NetworkFailure(CatalogNetworkFailureError),
}

/// Outcome of catalog.getGeneratedFeed: the feed with its entries, or one typed refusal. An entry that cannot be presented never fails the feed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CatalogResourceGeneratedFeedResult {
    Succeeded(CatalogGetGeneratedFeedSucceeded),
    NegotiationRefused(CatalogNegotiationRefusedError),
    InvalidRequest(CatalogInvalidRequestError),
    AuthenticationRequired(CatalogAuthenticationRequiredError),
    PolicyRejected(CatalogPolicyRejectedError),
    NetworkFailure(CatalogNetworkFailureError),
    UnsafeRetrieval(CatalogUnsafeRetrievalError),
    MalformedCard(CatalogMalformedCardError),
    Unavailable(CatalogUnavailableError),
}

/// Outcome of catalog.getSavedFeed: the feed with its entries, or one typed refusal. An entry that cannot be presented never fails the feed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CatalogResourceSavedFeedResult {
    Succeeded(CatalogGetSavedFeedSucceeded),
    NegotiationRefused(CatalogNegotiationRefusedError),
    InvalidRequest(CatalogInvalidRequestError),
    AuthenticationRequired(CatalogAuthenticationRequiredError),
    PolicyRejected(CatalogPolicyRejectedError),
    NetworkFailure(CatalogNetworkFailureError),
    UnsafeRetrieval(CatalogUnsafeRetrievalError),
    MalformedCard(CatalogMalformedCardError),
    Unavailable(CatalogUnavailableError),
}

/// Discriminator: the search completed
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSearchSucceededKind {
    #[serde(rename = "succeeded")]
    #[default]
    Succeeded,
}
impl<'de> Deserialize<'de> for CatalogSearchSucceededKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSearchSucceededKind",
            &["succeeded"],
            None,
        )?;
        Ok(Self::Succeeded)
    }
}

/// Discriminator: an unsupported candidate kind was requested
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogUnsupportedKindErrorKind {
    #[serde(rename = "unsupported-kind")]
    #[default]
    UnsupportedKind,
}
impl<'de> Deserialize<'de> for CatalogUnsupportedKindErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogUnsupportedKindErrorKind",
            &["unsupported-kind"],
            None,
        )?;
        Ok(Self::UnsupportedKind)
    }
}

/// Outcome of catalog.search or catalog.recommend: either bounded inert candidates, or one typed refusal. Never a partial success.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CatalogSearchResult {
    Succeeded(CatalogSearchSucceeded),
    NegotiationRefused(CatalogNegotiationRefusedError),
    UnsupportedKind(CatalogUnsupportedKindError),
    InvalidRequest(CatalogInvalidRequestError),
    AuthenticationRequired(CatalogAuthenticationRequiredError),
    PolicyRejected(CatalogPolicyRejectedError),
    NetworkFailure(CatalogNetworkFailureError),
    UnsafeRetrieval(CatalogUnsafeRetrievalError),
    MalformedCard(CatalogMalformedCardError),
    ContractViolation(CatalogContractViolationError),
    Unavailable(CatalogUnavailableError),
}

/// Discriminator: selection was cancelled
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSelectionCancelledKind {
    #[serde(rename = "cancelled")]
    #[default]
    Cancelled,
}
impl<'de> Deserialize<'de> for CatalogSelectionCancelledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSelectionCancelledKind",
            &["cancelled"],
            None,
        )?;
        Ok(Self::Cancelled)
    }
}

/// Terminal outcome declared for a retained catalog selection group
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSelectionDecision {
    /// Choose the candidate named by selectionRef.
    #[serde(rename = "selected")]
    Selected,
    /// Explicitly decline every candidate in the search.
    #[serde(rename = "declined")]
    Declined,
    /// Cancel the selection interaction without choosing a candidate.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// Declare that the host's live interaction deadline elapsed while the reference remained valid.
    #[serde(rename = "timed-out")]
    TimedOut,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogSelectionDecision {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSelectionDecision",
            &["selected", "declined", "cancelled", "timed-out", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Selected,
            1 => Self::Declined,
            2 => Self::Cancelled,
            3 => Self::TimedOut,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: the candidates were declined
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSelectionDeclinedKind {
    #[serde(rename = "declined")]
    #[default]
    Declined,
}
impl<'de> Deserialize<'de> for CatalogSelectionDeclinedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSelectionDeclinedKind",
            &["declined"],
            None,
        )?;
        Ok(Self::Declined)
    }
}

/// Discriminator for this typed selection rejection
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSelectionForeignKind {
    #[serde(rename = "foreign")]
    #[default]
    Foreign,
}
impl<'de> Deserialize<'de> for CatalogSelectionForeignKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSelectionForeignKind",
            &["foreign"],
            None,
        )?;
        Ok(Self::Foreign)
    }
}

/// Discriminator for this typed selection rejection
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSelectionInvalidKind {
    #[serde(rename = "invalid")]
    #[default]
    Invalid,
}
impl<'de> Deserialize<'de> for CatalogSelectionInvalidKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSelectionInvalidKind",
            &["invalid"],
            None,
        )?;
        Ok(Self::Invalid)
    }
}

/// Discriminator for this typed selection rejection
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSelectionReplayedKind {
    #[serde(rename = "replayed")]
    #[default]
    Replayed,
}
impl<'de> Deserialize<'de> for CatalogSelectionReplayedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSelectionReplayedKind",
            &["replayed"],
            None,
        )?;
        Ok(Self::Replayed)
    }
}

/// Discriminator: one candidate was selected
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSelectionSelectedKind {
    #[serde(rename = "selected")]
    #[default]
    Selected,
}
impl<'de> Deserialize<'de> for CatalogSelectionSelectedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSelectionSelectedKind",
            &["selected"],
            None,
        )?;
        Ok(Self::Selected)
    }
}

/// Discriminator: the host's live interaction timed out
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSelectionTimedOutKind {
    #[serde(rename = "timed-out")]
    #[default]
    TimedOut,
}
impl<'de> Deserialize<'de> for CatalogSelectionTimedOutKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSelectionTimedOutKind",
            &["timed-out"],
            None,
        )?;
        Ok(Self::TimedOut)
    }
}

/// Discriminator for this typed selection rejection
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSelectionStaleKind {
    #[serde(rename = "stale")]
    #[default]
    Stale,
}
impl<'de> Deserialize<'de> for CatalogSelectionStaleKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSelectionStaleKind",
            &["stale"],
            None,
        )?;
        Ok(Self::Stale)
    }
}

/// Discriminator for this typed selection rejection
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogSelectionWrongKindKind {
    #[serde(rename = "wrong-kind")]
    #[default]
    WrongKind,
}
impl<'de> Deserialize<'de> for CatalogSelectionWrongKindKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogSelectionWrongKindKind",
            &["wrong-kind"],
            None,
        )?;
        Ok(Self::WrongKind)
    }
}

/// Typed outcome of catalog.select. Only the selected host result carries a fresh candidate handle; the model-facing projection removes both that handle and searchId.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CatalogSelectionResult {
    Selected(CatalogSelectionSelected),
    Declined(CatalogSelectionDeclined),
    Cancelled(CatalogSelectionCancelled),
    TimedOut(CatalogSelectionTimedOut),
    Invalid(CatalogSelectionInvalid),
    Stale(CatalogSelectionStale),
    Replayed(CatalogSelectionReplayed),
    Foreign(CatalogSelectionForeign),
    WrongKind(CatalogSelectionWrongKind),
    NegotiationRefused(CatalogNegotiationRefusedError),
    InvalidRequest(CatalogInvalidRequestError),
    Unavailable(CatalogUnavailableError),
}

/// Authority-computed exposure eligibility, kept separate from tier. The current tier-only Agent Finder response maps to `unknown`, never to a locally inferred eligibility.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustEligibility {
    /// Eligible for default catalogue exposure.
    #[serde(rename = "default")]
    Default,
    /// Eligible only when expanded or community results are requested.
    #[serde(rename = "expanded")]
    Expanded,
    /// Not eligible for normal catalogue exposure.
    #[serde(rename = "hidden")]
    Hidden,
    /// The authority did not supply an eligibility decision.
    #[serde(rename = "unknown")]
    UnknownValue,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustEligibility {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustEligibility",
            &["default", "expanded", "hidden", "unknown", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Default,
            1 => Self::Expanded,
            2 => Self::Hidden,
            3 => Self::UnknownValue,
            _ => Self::Unknown,
        })
    }
}

/// Bounded authority that supplied a catalogue trust observation
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustSource {
    /// GitHub Agent Finder supplied the trust field on its search result.
    #[serde(rename = "agent-finder")]
    AgentFinder,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustSource",
            &["agent-finder", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::AgentFinder,
            _ => Self::Unknown,
        })
    }
}

/// Schema version of the catalogue trust snapshot envelope
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustSnapshotSchemaVersion {
    /// Initial envelope carrying one bounded service tier or one explicit unavailable state.
    #[serde(rename = "v1")]
    V1,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustSnapshotSchemaVersion {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustSnapshotSchemaVersion",
            &["v1", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::V1,
            _ => Self::Unknown,
        })
    }
}

/// The authority omitted trust metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustSnapshotAbsentStatus {
    /// The authority omitted trust metadata.
    #[serde(rename = "absent")]
    Absent,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustSnapshotAbsentStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustSnapshotAbsentStatus",
            &["absent", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Absent,
            _ => Self::Unknown,
        })
    }
}

/// A recognised T1 or T2 service tier was observed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustSnapshotCurrentStatus {
    /// A recognised T1 or T2 service tier was observed.
    #[serde(rename = "current")]
    Current,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustSnapshotCurrentStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustSnapshotCurrentStatus",
            &["current", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Current,
            _ => Self::Unknown,
        })
    }
}

/// The authority explicitly reported a downgraded assessment.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustSnapshotDowngradedStatus {
    /// The authority explicitly reported a downgraded assessment.
    #[serde(rename = "downgraded")]
    Downgraded,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustSnapshotDowngradedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustSnapshotDowngradedStatus",
            &["downgraded", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Downgraded,
            _ => Self::Unknown,
        })
    }
}

/// The trust field was empty, unbounded, or had the wrong JSON type.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustSnapshotMalformedStatus {
    /// The trust field was empty, unbounded, or had the wrong JSON type.
    #[serde(rename = "malformed")]
    Malformed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustSnapshotMalformedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustSnapshotMalformedStatus",
            &["malformed", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Malformed,
            _ => Self::Unknown,
        })
    }
}

/// The authority explicitly revoked its assessment.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustSnapshotRevokedStatus {
    /// The authority explicitly revoked its assessment.
    #[serde(rename = "revoked")]
    Revoked,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustSnapshotRevokedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustSnapshotRevokedStatus",
            &["revoked", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Revoked,
            _ => Self::Unknown,
        })
    }
}

/// The authority explicitly marked its assessment stale.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustSnapshotStaleStatus {
    /// The authority explicitly marked its assessment stale.
    #[serde(rename = "stale")]
    Stale,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustSnapshotStaleStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustSnapshotStaleStatus",
            &["stale", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Stale,
            _ => Self::Unknown,
        })
    }
}

/// The authority supplied a bounded trust value this runtime does not understand.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogTrustSnapshotUnsupportedStatus {
    /// The authority supplied a bounded trust value this runtime does not understand.
    #[serde(rename = "unsupported")]
    Unsupported,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogTrustSnapshotUnsupportedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogTrustSnapshotUnsupportedStatus",
            &["unsupported", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Unsupported,
            _ => Self::Unknown,
        })
    }
}

/// Whether a pending slash-command invocation effect was applied or cancelled by the host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CommandsInvocationEffectOutcome {
    /// The host applied the pending invocation effect.
    #[serde(rename = "applied")]
    Applied,
    /// The host cancelled the pending invocation effect, so any provisional state must be reverted.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CommandsInvocationEffectOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CommandsInvocationEffectOutcome",
            &["applied", "cancelled", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Applied,
            1 => Self::Cancelled,
            _ => Self::Unknown,
        })
    }
}

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CommandsInvocationOrigin {
    #[serde(rename = "settings")]
    Settings,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CommandsInvocationOrigin {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CommandsInvocationOrigin",
            &["settings", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Settings,
            _ => Self::Unknown,
        })
    }
}

/// Runtime state a host waits for while it executes a queued command.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum QueuedCommandWaitRequirement {
    /// The host waits for managed policy to be published for the session's current account.
    #[serde(rename = "managed-policy-published")]
    ManagedPolicyPublished,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for QueuedCommandWaitRequirement {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "QueuedCommandWaitRequirement",
            &["managed-policy-published", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::ManagedPolicyPublished,
            _ => Self::Unknown,
        })
    }
}

/// The service is connected and the session MCP graph was reconciled.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ConnectorConnectResultConnectedKind {
    #[serde(rename = "connected")]
    #[default]
    Connected,
}
impl<'de> Deserialize<'de> for ConnectorConnectResultConnectedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ConnectorConnectResultConnectedKind",
            &["connected"],
            None,
        )?;
        Ok(Self::Connected)
    }
}

/// Host-owned consent is required before bounded continuation can complete.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ConnectorConnectResultConsentRequiredKind {
    #[serde(rename = "consent_required")]
    #[default]
    ConsentRequired,
}
impl<'de> Deserialize<'de> for ConnectorConnectResultConsentRequiredKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ConnectorConnectResultConsentRequiredKind",
            &["consent_required"],
            None,
        )?;
        Ok(Self::ConsentRequired)
    }
}

/// The service is still completing the connection without a consent URL.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ConnectorConnectResultPendingKind {
    #[serde(rename = "pending")]
    #[default]
    Pending,
}
impl<'de> Deserialize<'de> for ConnectorConnectResultPendingKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ConnectorConnectResultPendingKind",
            &["pending"],
            None,
        )?;
        Ok(Self::Pending)
    }
}

/// Typed result of initiating or continuing a Connector connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ConnectorConnectResult {
    Connected(ConnectorConnectResultConnected),
    ConsentRequired(ConnectorConnectResultConsentRequired),
    Pending(ConnectorConnectResultPending),
}

/// Availability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ConnectorDiscoveryAvailability {
    /// Enabled.
    #[serde(rename = "enabled")]
    Enabled,
    /// Disabled.
    #[serde(rename = "disabled")]
    Disabled,
    /// Unavailable.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ConnectorDiscoveryAvailability {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ConnectorDiscoveryAvailability",
            &["enabled", "disabled", "unavailable", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Enabled,
            1 => Self::Disabled,
            2 => Self::Unavailable,
            _ => Self::Unknown,
        })
    }
}

/// Closed set of public task kinds a connection can negotiate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum TaskKind {
    /// Runtime-owned background agent task.
    #[serde(rename = "agent")]
    Agent,
    /// Runtime-owned shell task.
    #[serde(rename = "shell")]
    Shell,
    /// Client-owned externally executed task.
    #[serde(rename = "client")]
    Client,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for TaskKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "TaskKind",
            &["agent", "shell", "client", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Agent,
            1 => Self::Shell,
            2 => Self::Client,
            _ => Self::Unknown,
        })
    }
}

/// Controls how MCP tool result content is filtered: none leaves content unchanged, markdown sanitizes HTML while preserving Markdown-friendly output, and hidden_characters removes characters that can hide directives.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ContentFilterMode {
    /// Leave MCP tool result content unchanged.
    #[serde(rename = "none")]
    None,
    /// Sanitize HTML while preserving Markdown-friendly output.
    #[serde(rename = "markdown")]
    Markdown,
    /// Remove characters that can hide directives.
    #[serde(rename = "hidden_characters")]
    HiddenCharacters,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ContentFilterMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ContentFilterMode",
            &["none", "markdown", "hidden_characters", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::None,
            1 => Self::Markdown,
            2 => Self::HiddenCharacters,
            _ => Self::Unknown,
        })
    }
}

/// Stable credential-context failure categories, independent of the credential provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CredentialContextErrorCode {
    /// The current owner has no usable credential.
    #[serde(rename = "credential_unavailable")]
    CredentialUnavailable,
    /// The context, owner, or lease is no longer current.
    #[serde(rename = "credential_revoked")]
    CredentialRevoked,
    /// The caller, destination, or requested credential use is not authorized.
    #[serde(rename = "credential_denied")]
    CredentialDenied,
    /// The original owner failed to resolve a credential. This is not credential absence.
    #[serde(rename = "credential_resolution_failed")]
    CredentialResolutionFailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CredentialContextErrorCode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CredentialContextErrorCode",
            &[
                "credential_unavailable",
                "credential_revoked",
                "credential_denied",
                "credential_resolution_failed",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::CredentialUnavailable,
            1 => Self::CredentialRevoked,
            2 => Self::CredentialDenied,
            3 => Self::CredentialResolutionFailed,
            _ => Self::Unknown,
        })
    }
}

/// Result of reloading a customization component.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(from = "String", into = "String")]
pub enum CustomizationReloadStatus {
    /// The component was refreshed successfully.
    Reloaded,
    /// The component was not configured, loaded, or eligible for refresh.
    Skipped,
    /// The component could not be refreshed; other components may still reload.
    Failed,
    /// Unknown variant for forward compatibility.
    Unknown(String),
}

impl Default for CustomizationReloadStatus {
    fn default() -> Self {
        Self::Unknown("unknown".to_owned())
    }
}

impl From<String> for CustomizationReloadStatus {
    fn from(value: String) -> Self {
        match value.as_str() {
            "reloaded" => Self::Reloaded,
            "skipped" => Self::Skipped,
            "failed" => Self::Failed,
            _ => Self::Unknown(value),
        }
    }
}

impl From<CustomizationReloadStatus> for String {
    fn from(value: CustomizationReloadStatus) -> Self {
        match value {
            CustomizationReloadStatus::Reloaded => "reloaded".to_owned(),
            CustomizationReloadStatus::Skipped => "skipped".to_owned(),
            CustomizationReloadStatus::Failed => "failed".to_owned(),
            CustomizationReloadStatus::Unknown(value) => value,
        }
    }
}

/// Component of session customization discovery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(from = "String", into = "String")]
pub enum CustomizationReloadSubsystem {
    /// Repository metadata and working-directory context.
    RepositoryContext,
    /// Session instructions and their cached dynamic context.
    Instructions,
    /// Discovered plugin configuration.
    Plugins,
    /// Configured session and plugin hooks.
    Hooks,
    /// Discovered skills.
    Skills,
    /// Discovered custom agents.
    Agents,
    /// Loaded MCP server configuration.
    Mcp,
    /// Configured session extensions.
    Extensions,
    /// Unknown variant for forward compatibility.
    Unknown(String),
}

impl Default for CustomizationReloadSubsystem {
    fn default() -> Self {
        Self::Unknown("unknown".to_owned())
    }
}

impl From<String> for CustomizationReloadSubsystem {
    fn from(value: String) -> Self {
        match value.as_str() {
            "repositoryContext" => Self::RepositoryContext,
            "instructions" => Self::Instructions,
            "plugins" => Self::Plugins,
            "hooks" => Self::Hooks,
            "skills" => Self::Skills,
            "agents" => Self::Agents,
            "mcp" => Self::Mcp,
            "extensions" => Self::Extensions,
            _ => Self::Unknown(value),
        }
    }
}

impl From<CustomizationReloadSubsystem> for String {
    fn from(value: CustomizationReloadSubsystem) -> Self {
        match value {
            CustomizationReloadSubsystem::RepositoryContext => "repositoryContext".to_owned(),
            CustomizationReloadSubsystem::Instructions => "instructions".to_owned(),
            CustomizationReloadSubsystem::Plugins => "plugins".to_owned(),
            CustomizationReloadSubsystem::Hooks => "hooks".to_owned(),
            CustomizationReloadSubsystem::Skills => "skills".to_owned(),
            CustomizationReloadSubsystem::Agents => "agents".to_owned(),
            CustomizationReloadSubsystem::Mcp => "mcp".to_owned(),
            CustomizationReloadSubsystem::Extensions => "extensions".to_owned(),
            CustomizationReloadSubsystem::Unknown(value) => value,
        }
    }
}

/// Destination variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DebugCollectLogsDestinationArchiveKind {
    #[serde(rename = "archive")]
    #[default]
    Archive,
}
impl<'de> Deserialize<'de> for DebugCollectLogsDestinationArchiveKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DebugCollectLogsDestinationArchiveKind",
            &["archive"],
            None,
        )?;
        Ok(Self::Archive)
    }
}

/// Destination variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DebugCollectLogsDestinationDirectoryKind {
    #[serde(rename = "directory")]
    #[default]
    Directory,
}
impl<'de> Deserialize<'de> for DebugCollectLogsDestinationDirectoryKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DebugCollectLogsDestinationDirectoryKind",
            &["directory"],
            None,
        )?;
        Ok(Self::Directory)
    }
}

/// Destination for the session debug bundle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DebugCollectLogsDestination {
    Archive(DebugCollectLogsDestinationArchive),
    Directory(DebugCollectLogsDestinationDirectory),
}

/// Kind of caller-provided debug log entry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DebugCollectLogsEntryKind {
    /// Include a single server-local file.
    #[serde(rename = "file")]
    File,
    /// Include files from a server-local directory recursively.
    #[serde(rename = "directory")]
    Directory,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DebugCollectLogsEntryKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DebugCollectLogsEntryKind",
            &["file", "directory", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::File,
            1 => Self::Directory,
            _ => Self::Unknown,
        })
    }
}

/// How a collected debug entry should be redacted before being staged.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DebugCollectLogsRedaction {
    /// Redact the file as plain UTF-8 log text.
    #[serde(rename = "plain-text")]
    PlainText,
    /// Redact each non-empty line as a session event JSON object, falling back to plain-text redaction for malformed lines.
    #[serde(rename = "events-jsonl")]
    EventsJsonl,
    /// No redaction is applied. The caller must ensure any necessary redaction is performed before this call.
    #[serde(rename = "none")]
    None,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DebugCollectLogsRedaction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DebugCollectLogsRedaction",
            &["plain-text", "events-jsonl", "none", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::PlainText,
            1 => Self::EventsJsonl,
            2 => Self::None,
            _ => Self::Unknown,
        })
    }
}

/// Answered question kind discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DecisionAnswerBooleanKind {
    #[serde(rename = "boolean")]
    #[default]
    Boolean,
}
impl<'de> Deserialize<'de> for DecisionAnswerBooleanKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DecisionAnswerBooleanKind",
            &["boolean"],
            None,
        )?;
        Ok(Self::Boolean)
    }
}

/// Answered question kind discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DecisionAnswerChoiceKind {
    #[serde(rename = "choice")]
    #[default]
    Choice,
}
impl<'de> Deserialize<'de> for DecisionAnswerChoiceKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DecisionAnswerChoiceKind",
            &["choice"],
            None,
        )?;
        Ok(Self::Choice)
    }
}

/// Answered question kind discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DecisionAnswerScoreKind {
    #[serde(rename = "score")]
    #[default]
    Score,
}
impl<'de> Deserialize<'de> for DecisionAnswerScoreKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DecisionAnswerScoreKind",
            &["score"],
            None,
        )?;
        Ok(Self::Score)
    }
}

/// Answered question kind discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DecisionAnswerRefusedKind {
    #[serde(rename = "refused")]
    #[default]
    Refused,
}
impl<'de> Deserialize<'de> for DecisionAnswerRefusedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DecisionAnswerRefusedKind",
            &["refused"],
            None,
        )?;
        Ok(Self::Refused)
    }
}

/// The answer to one question. Probabilities are normalized the same way for every backend.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DecisionAnswer {
    Boolean(DecisionAnswerBoolean),
    Choice(DecisionAnswerChoice),
    Score(DecisionAnswerScore),
    Refused(DecisionAnswerRefused),
}

/// Context format discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DecisionContextTextKind {
    #[serde(rename = "text")]
    #[default]
    Text,
}
impl<'de> Deserialize<'de> for DecisionContextTextKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DecisionContextTextKind",
            &["text"],
            None,
        )?;
        Ok(Self::Text)
    }
}

/// Context format discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DecisionContextJsonKind {
    #[serde(rename = "json")]
    #[default]
    Json,
}
impl<'de> Deserialize<'de> for DecisionContextJsonKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DecisionContextJsonKind",
            &["json"],
            None,
        )?;
        Ok(Self::Json)
    }
}

/// Context shared by every question in a request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DecisionContext {
    Text(DecisionContextText),
    Json(DecisionContextJson),
}

/// Why a decisions request failed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DecisionErrorKind {
    /// The decisions feature is disabled for this runtime.
    #[serde(rename = "notEnabled")]
    NotEnabled,
    /// No usable decision model: unknown id, not entitled, or rejected by the backend.
    #[serde(rename = "noModel")]
    NoModel,
    /// No account, or the backend rejected its credential.
    #[serde(rename = "unauthenticated")]
    Unauthenticated,
    /// The request is malformed or breaks a limit other than size.
    #[serde(rename = "invalidRequest")]
    InvalidRequest,
    /// The request is over a size limit.
    #[serde(rename = "tooLarge")]
    TooLarge,
    /// The backend answered with something that breaks the answer rules.
    #[serde(rename = "upstreamProtocol")]
    UpstreamProtocol,
    /// The backend is rate limiting this account.
    #[serde(rename = "rateLimited")]
    RateLimited,
    /// The deadline passed.
    #[serde(rename = "timeout")]
    Timeout,
    /// The backend could not be reached or failed.
    #[serde(rename = "unavailable")]
    Unavailable,
    /// Too many decisions requests are already running or waiting on this connection.
    #[serde(rename = "overloaded")]
    Overloaded,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DecisionErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DecisionErrorKind",
            &[
                "notEnabled",
                "noModel",
                "unauthenticated",
                "invalidRequest",
                "tooLarge",
                "upstreamProtocol",
                "rateLimited",
                "timeout",
                "unavailable",
                "overloaded",
                "Unknown",
            ],
            Some(10),
        )?;
        Ok(match index {
            0 => Self::NotEnabled,
            1 => Self::NoModel,
            2 => Self::Unauthenticated,
            3 => Self::InvalidRequest,
            4 => Self::TooLarge,
            5 => Self::UpstreamProtocol,
            6 => Self::RateLimited,
            7 => Self::Timeout,
            8 => Self::Unavailable,
            9 => Self::Overloaded,
            _ => Self::Unknown,
        })
    }
}

/// Question kind discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DecisionQuestionBooleanKind {
    #[serde(rename = "boolean")]
    #[default]
    Boolean,
}
impl<'de> Deserialize<'de> for DecisionQuestionBooleanKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DecisionQuestionBooleanKind",
            &["boolean"],
            None,
        )?;
        Ok(Self::Boolean)
    }
}

/// Question kind discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DecisionQuestionChoiceKind {
    #[serde(rename = "choice")]
    #[default]
    Choice,
}
impl<'de> Deserialize<'de> for DecisionQuestionChoiceKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DecisionQuestionChoiceKind",
            &["choice"],
            None,
        )?;
        Ok(Self::Choice)
    }
}

/// Question kind discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DecisionQuestionScoreKind {
    #[serde(rename = "score")]
    #[default]
    Score,
}
impl<'de> Deserialize<'de> for DecisionQuestionScoreKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "DecisionQuestionScoreKind",
            &["score"],
            None,
        )?;
        Ok(Self::Score)
    }
}

/// One question for the decision model to answer about the context.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DecisionQuestion {
    Boolean(DecisionQuestionBoolean),
    Choice(DecisionQuestionChoice),
    Score(DecisionQuestionScore),
}

/// Hook event name. Discovery emits the file-configurable subset; SDK callbacks additionally support callback-only events.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HookType {
    /// Runs before a tool is invoked.
    #[serde(rename = "preToolUse")]
    PreToolUse,
    /// Runs before an MCP tool is invoked.
    #[serde(rename = "preMcpToolCall")]
    PreMcpToolCall,
    /// Runs after a tool completes successfully.
    #[serde(rename = "postToolUse")]
    PostToolUse,
    /// Runs after a tool fails.
    #[serde(rename = "postToolUseFailure")]
    PostToolUseFailure,
    /// Runs after the user submits a prompt.
    #[serde(rename = "userPromptSubmitted")]
    UserPromptSubmitted,
    /// Runs after the runtime transforms the submitted prompt for the model, before it is added to session history.
    #[serde(rename = "userPromptTransformed")]
    UserPromptTransformed,
    /// Runs when a session starts.
    #[serde(rename = "sessionStart")]
    SessionStart,
    /// Runs when a session ends.
    #[serde(rename = "sessionEnd")]
    SessionEnd,
    /// Runs when the agent encounters an error.
    #[serde(rename = "errorOccurred")]
    ErrorOccurred,
    /// Runs when the agent stops.
    #[serde(rename = "agentStop")]
    AgentStop,
    /// Runs when a subagent starts.
    #[serde(rename = "subagentStart")]
    SubagentStart,
    /// Runs when a subagent stops.
    #[serde(rename = "subagentStop")]
    SubagentStop,
    /// Runs before conversation context is compacted.
    #[serde(rename = "preCompact")]
    PreCompact,
    /// Runs when the agent requests permission.
    #[serde(rename = "permissionRequest")]
    PermissionRequest,
    /// Runs when the agent emits a notification.
    #[serde(rename = "notification")]
    Notification,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HookType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HookType",
            &[
                "preToolUse",
                "preMcpToolCall",
                "postToolUse",
                "postToolUseFailure",
                "userPromptSubmitted",
                "userPromptTransformed",
                "sessionStart",
                "sessionEnd",
                "errorOccurred",
                "agentStop",
                "subagentStart",
                "subagentStop",
                "preCompact",
                "permissionRequest",
                "notification",
                "Unknown",
            ],
            Some(15),
        )?;
        Ok(match index {
            0 => Self::PreToolUse,
            1 => Self::PreMcpToolCall,
            2 => Self::PostToolUse,
            3 => Self::PostToolUseFailure,
            4 => Self::UserPromptSubmitted,
            5 => Self::UserPromptTransformed,
            6 => Self::SessionStart,
            7 => Self::SessionEnd,
            8 => Self::ErrorOccurred,
            9 => Self::AgentStop,
            10 => Self::SubagentStart,
            11 => Self::SubagentStop,
            12 => Self::PreCompact,
            13 => Self::PermissionRequest,
            14 => Self::Notification,
            _ => Self::Unknown,
        })
    }
}

/// Configuration tier that contributed a discovered hook action.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HookOrigin {
    /// Hook loaded from user settings or the user's hook directory.
    #[serde(rename = "user")]
    User,
    /// Hook loaded from repository settings or the repository hook directory.
    #[serde(rename = "repository")]
    Repository,
    /// Hook provided by an enabled installed or explicit plugin. Projectless rows omit projectPath and do not expand a project directory.
    #[serde(rename = "plugin")]
    Plugin,
    /// Hook enforced by centrally managed policy.
    #[serde(rename = "policy")]
    Policy,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HookOrigin {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HookOrigin",
            &["user", "repository", "plugin", "policy", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::Repository,
            2 => Self::Plugin,
            3 => Self::Policy,
            _ => Self::Unknown,
        })
    }
}

/// Interaction policy for host-owned Microsoft Entra acquisition.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EntraTokenAcquireReason {
    /// Establish an identity; interaction is permitted if necessary.
    #[serde(rename = "signIn")]
    SignIn,
    /// Renew the pinned identity silently; never select another account or prompt.
    #[serde(rename = "renewal")]
    Renewal,
    /// Explicitly show account selection rather than satisfying identity selection from cache.
    #[serde(rename = "forceInteractive")]
    ForceInteractive,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for EntraTokenAcquireReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "EntraTokenAcquireReason",
            &["signIn", "renewal", "forceInteractive", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::SignIn,
            1 => Self::Renewal,
            2 => Self::ForceInteractive,
            _ => Self::Unknown,
        })
    }
}

/// Entra identity or resource-token acquisition discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EntraTokenAcquireRequestIdentityKind {
    #[serde(rename = "identity")]
    #[default]
    Identity,
}
impl<'de> Deserialize<'de> for EntraTokenAcquireRequestIdentityKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "EntraTokenAcquireRequestIdentityKind",
            &["identity"],
            None,
        )?;
        Ok(Self::Identity)
    }
}

/// Entra identity or resource-token acquisition discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EntraTokenAcquireRequestTokenKind {
    #[serde(rename = "token")]
    #[default]
    Token,
}
impl<'de> Deserialize<'de> for EntraTokenAcquireRequestTokenKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "EntraTokenAcquireRequestTokenKind",
            &["token"],
            None,
        )?;
        Ok(Self::Token)
    }
}

/// Requests a host-selected Entra identity or a resource token for that exact identity. Host account ids are opaque and distinct from runtime account-selection ids.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum EntraTokenAcquireRequest {
    Identity(EntraTokenAcquireRequestIdentity),
    Token(EntraTokenAcquireRequestToken),
}

/// Entra acquisition response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EntraTokenAcquireResultIdentityKind {
    #[serde(rename = "identity")]
    #[default]
    Identity,
}
impl<'de> Deserialize<'de> for EntraTokenAcquireResultIdentityKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "EntraTokenAcquireResultIdentityKind",
            &["identity"],
            None,
        )?;
        Ok(Self::Identity)
    }
}

/// Entra acquisition response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EntraTokenAcquireResultTokenKind {
    #[serde(rename = "token")]
    #[default]
    Token,
}
impl<'de> Deserialize<'de> for EntraTokenAcquireResultTokenKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "EntraTokenAcquireResultTokenKind",
            &["token"],
            None,
        )?;
        Ok(Self::Token)
    }
}

/// Entra acquisition response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EntraTokenAcquireResultUnavailableKind {
    #[serde(rename = "unavailable")]
    #[default]
    Unavailable,
}
impl<'de> Deserialize<'de> for EntraTokenAcquireResultUnavailableKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "EntraTokenAcquireResultUnavailableKind",
            &["unavailable"],
            None,
        )?;
        Ok(Self::Unavailable)
    }
}

/// Entra acquisition response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EntraTokenAcquireResultCancelledKind {
    #[serde(rename = "cancelled")]
    #[default]
    Cancelled,
}
impl<'de> Deserialize<'de> for EntraTokenAcquireResultCancelledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "EntraTokenAcquireResultCancelledKind",
            &["cancelled"],
            None,
        )?;
        Ok(Self::Cancelled)
    }
}

/// Host response to Entra acquisition. Unavailable, cancellation and errors are terminal: the runtime must not substitute another account or sign-in transport.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum EntraTokenAcquireResult {
    Identity(EntraTokenAcquireResultIdentity),
    Token(EntraTokenAcquireResultToken),
    Unavailable(EntraTokenAcquireResultUnavailable),
    Cancelled(EntraTokenAcquireResultCancelled),
}

/// GitHub Mission Control compute kind.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EnvironmentKind {
    /// A user-managed environment on a local machine.
    #[serde(rename = "user-local")]
    UserLocal,
    /// A user-managed environment in a GitHub Codespace.
    #[serde(rename = "user-codespace")]
    UserCodespace,
    /// A GitHub-managed environment backed by GitHub Actions.
    #[serde(rename = "managed-actions")]
    ManagedActions,
    /// A GitHub-managed sandbox environment.
    #[serde(rename = "managed-sandbox")]
    ManagedSandbox,
    /// A GitHub-managed cloud coding agent environment.
    #[serde(rename = "managed-cca")]
    ManagedCca,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for EnvironmentKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "EnvironmentKind",
            &[
                "user-local",
                "user-codespace",
                "managed-actions",
                "managed-sandbox",
                "managed-cca",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::UserLocal,
            1 => Self::UserCodespace,
            2 => Self::ManagedActions,
            3 => Self::ManagedSandbox,
            4 => Self::ManagedCca,
            _ => Self::Unknown,
        })
    }
}

/// Agent-scope filter: 'primary' returns only main-agent events plus events whose type starts with 'subagent.' (matching the typed-subscription default behavior); 'all' returns events from all agents (matching wildcard-subscription behavior). Default is 'all' to preserve wildcard semantics for catch-up callers.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EventsAgentScope {
    /// Return main-agent events and typed subagent lifecycle events.
    #[serde(rename = "primary")]
    Primary,
    /// Return events from all agents.
    #[serde(rename = "all")]
    All,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for EventsAgentScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "EventsAgentScope",
            &["primary", "all", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Primary,
            1 => Self::All,
            _ => Self::Unknown,
        })
    }
}

/// Original linked history or the self-contained active replay head.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum EventsReadView {
    /// Original events across linked journals, excluding checkpoint copies.
    #[serde(rename = "history")]
    History,
    /// The active replay head, including checkpoint state, without predecessor journals.
    #[serde(rename = "replay")]
    Replay,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for EventsReadView {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "EventsReadView",
            &["history", "replay", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::History,
            1 => Self::Replay,
            _ => Self::Unknown,
        })
    }
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExternalToolTextResultForLlmContentAudioType {
    #[serde(rename = "audio")]
    #[default]
    Audio,
}
impl<'de> Deserialize<'de> for ExternalToolTextResultForLlmContentAudioType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ExternalToolTextResultForLlmContentAudioType",
            &["audio"],
            None,
        )?;
        Ok(Self::Audio)
    }
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExternalToolTextResultForLlmContentImageType {
    #[serde(rename = "image")]
    #[default]
    Image,
}
impl<'de> Deserialize<'de> for ExternalToolTextResultForLlmContentImageType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ExternalToolTextResultForLlmContentImageType",
            &["image"],
            None,
        )?;
        Ok(Self::Image)
    }
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExternalToolTextResultForLlmContentResourceType {
    #[serde(rename = "resource")]
    #[default]
    Resource,
}
impl<'de> Deserialize<'de> for ExternalToolTextResultForLlmContentResourceType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ExternalToolTextResultForLlmContentResourceType",
            &["resource"],
            None,
        )?;
        Ok(Self::Resource)
    }
}

/// Theme variant this icon is intended for
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExternalToolTextResultForLlmContentResourceLinkIconTheme {
    /// Icon intended for light themes.
    #[serde(rename = "light")]
    Light,
    /// Icon intended for dark themes.
    #[serde(rename = "dark")]
    Dark,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ExternalToolTextResultForLlmContentResourceLinkIconTheme {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ExternalToolTextResultForLlmContentResourceLinkIconTheme",
            &["light", "dark", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Light,
            1 => Self::Dark,
            _ => Self::Unknown,
        })
    }
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExternalToolTextResultForLlmContentResourceLinkType {
    #[serde(rename = "resource_link")]
    #[default]
    ResourceLink,
}
impl<'de> Deserialize<'de> for ExternalToolTextResultForLlmContentResourceLinkType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ExternalToolTextResultForLlmContentResourceLinkType",
            &["resource_link"],
            None,
        )?;
        Ok(Self::ResourceLink)
    }
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExternalToolTextResultForLlmContentShellExitType {
    #[serde(rename = "shell_exit")]
    #[default]
    ShellExit,
}
impl<'de> Deserialize<'de> for ExternalToolTextResultForLlmContentShellExitType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ExternalToolTextResultForLlmContentShellExitType",
            &["shell_exit"],
            None,
        )?;
        Ok(Self::ShellExit)
    }
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExternalToolTextResultForLlmContentTerminalType {
    #[serde(rename = "terminal")]
    #[default]
    Terminal,
}
impl<'de> Deserialize<'de> for ExternalToolTextResultForLlmContentTerminalType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ExternalToolTextResultForLlmContentTerminalType",
            &["terminal"],
            None,
        )?;
        Ok(Self::Terminal)
    }
}

/// Content block type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExternalToolTextResultForLlmContentTextType {
    #[serde(rename = "text")]
    #[default]
    Text,
}
impl<'de> Deserialize<'de> for ExternalToolTextResultForLlmContentTextType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ExternalToolTextResultForLlmContentTextType",
            &["text"],
            None,
        )?;
        Ok(Self::Text)
    }
}

/// Final acceptance-only upload outcome.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum FeedbackUploadStatus {
    /// Accepted for processing, not necessarily processed.
    #[serde(rename = "accepted")]
    Accepted,
    /// Failed, possibly after partial transmission.
    #[serde(rename = "failed")]
    Failed,
    /// Canceled locally, not proof of remote erasure.
    #[serde(rename = "canceled")]
    Canceled,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FeedbackUploadStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FeedbackUploadStatus",
            &["accepted", "failed", "canceled", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Accepted,
            1 => Self::Failed,
            2 => Self::Canceled,
            _ => Self::Unknown,
        })
    }
}

/// Current phase of the archive upload.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum FeedbackUploadPhase {
    /// Waiting to start.
    #[serde(rename = "created")]
    Created,
    /// Opening and hashing the saved archive.
    #[serde(rename = "packaging")]
    Packaging,
    /// Creating the remote job.
    #[serde(rename = "creating_job")]
    CreatingJob,
    /// Sending archive parts.
    #[serde(rename = "staging_blocks")]
    StagingBlocks,
    /// Requesting service acceptance.
    #[serde(rename = "completing_job")]
    CompletingJob,
    /// Stopping upload work.
    #[serde(rename = "canceling")]
    Canceling,
    /// Accepted for processing.
    #[serde(rename = "accepted")]
    Accepted,
    /// Upload failed.
    #[serde(rename = "failed")]
    Failed,
    /// Upload canceled locally.
    #[serde(rename = "canceled")]
    Canceled,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FeedbackUploadPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FeedbackUploadPhase",
            &[
                "created",
                "packaging",
                "creating_job",
                "staging_blocks",
                "completing_job",
                "canceling",
                "accepted",
                "failed",
                "canceled",
                "Unknown",
            ],
            Some(9),
        )?;
        Ok(match index {
            0 => Self::Created,
            1 => Self::Packaging,
            2 => Self::CreatingJob,
            3 => Self::StagingBlocks,
            4 => Self::CompletingJob,
            5 => Self::Canceling,
            6 => Self::Accepted,
            7 => Self::Failed,
            8 => Self::Canceled,
            _ => Self::Unknown,
        })
    }
}

/// Identifies a local filesystem failure, not a network failure or retry decision.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FetchFileErrorKind {
    #[serde(rename = "fileSystem")]
    FileSystem,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FetchFileErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FetchFileErrorKind",
            &["fileSystem", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::FileSystem,
            _ => Self::Unknown,
        })
    }
}

/// The filesystem operation that failed while producing or cleaning up a response file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FetchFileOperation {
    /// Create the destination file without overwriting an existing path.
    #[serde(rename = "create")]
    Create,
    /// Write decoded response bytes.
    #[serde(rename = "write")]
    Write,
    /// Flush, synchronize, and close the completed file.
    #[serde(rename = "flush")]
    Flush,
    /// Remove the incomplete response file.
    #[serde(rename = "remove")]
    Remove,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FetchFileOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FetchFileOperation",
            &["create", "write", "flush", "remove", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Create,
            1 => Self::Write,
            2 => Self::Flush,
            3 => Self::Remove,
            _ => Self::Unknown,
        })
    }
}

/// HTTP redirect policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FetchRedirect {
    /// Follow redirects, up to the transport's limit of 20.
    #[serde(rename = "follow")]
    Follow,
    /// Follow only redirects within the original origin.
    #[serde(rename = "sameOrigin")]
    SameOrigin,
    /// Return the redirect response without following it.
    #[serde(rename = "manual")]
    Manual,
    /// Reject redirect responses.
    #[serde(rename = "error")]
    Error,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FetchRedirect {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FetchRedirect",
            &["follow", "sameOrigin", "manual", "error", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Follow,
            1 => Self::SameOrigin,
            2 => Self::Manual,
            3 => Self::Error,
            _ => Self::Unknown,
        })
    }
}

/// How fetch.send delivers the HTTP response.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FetchResponseMode {
    /// Return headers immediately. Read body chunks with fetch.read or cancel without reading.
    #[serde(rename = "stream")]
    Stream,
    /// Return the complete UTF-8 response text.
    #[serde(rename = "text")]
    Text,
    /// Write the response directly to a new caller-owned file on the runtime host.
    #[serde(rename = "file")]
    File,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FetchResponseMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FetchResponseMode",
            &["stream", "text", "file", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Stream,
            1 => Self::Text,
            2 => Self::File,
            _ => Self::Unknown,
        })
    }
}

/// HTTP response metadata and the selected body representation. HTTP error statuses remain successful transport outcomes.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum FetchSendResult {
    Stream(FetchSendResultStream),
    Text(FetchSendResultText),
    File(FetchSendResultFile),
}

/// Shared HTTP engine failure categories. HTTP response statuses are not errors in this enumeration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum FetchTransportErrorKind {
    /// An HTTP client request or response-body failure.
    #[serde(rename = "request")]
    Request,
    /// The request deadline expired.
    #[serde(rename = "timeout")]
    Timeout,
    /// A proxy tunnel failed.
    #[serde(rename = "tunnel")]
    Tunnel,
    /// Proxy authentication failed.
    #[serde(rename = "proxy_authentication")]
    ProxyAuthentication,
    /// A request or transport configuration is invalid.
    #[serde(rename = "request_configuration")]
    RequestConfiguration,
    /// The response exceeded the caller's body limit.
    #[serde(rename = "response_body_limit")]
    ResponseBodyLimit,
    /// The redirect policy or transport rejected a redirect.
    #[serde(rename = "redirect")]
    Redirect,
    /// The request was canceled.
    #[serde(rename = "canceled")]
    Canceled,
    /// The response stream is closed.
    #[serde(rename = "stream_closed")]
    StreamClosed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for FetchTransportErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "FetchTransportErrorKind",
            &[
                "request",
                "timeout",
                "tunnel",
                "proxy_authentication",
                "request_configuration",
                "response_body_limit",
                "redirect",
                "canceled",
                "stream_closed",
                "Unknown",
            ],
            Some(9),
        )?;
        Ok(match index {
            0 => Self::Request,
            1 => Self::Timeout,
            2 => Self::Tunnel,
            3 => Self::ProxyAuthentication,
            4 => Self::RequestConfiguration,
            5 => Self::ResponseBodyLimit,
            6 => Self::Redirect,
            7 => Self::Canceled,
            8 => Self::StreamClosed,
            _ => Self::Unknown,
        })
    }
}

/// What initiated this compaction request, recorded as the `trigger` on the persisted `session.compaction_start` / `session.compaction_complete` events. When absent, the compaction is persisted without trigger attribution (initiator unknown).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HistoryCompactRequestTrigger {
    /// User-requested compaction, e.g. the /compact command or a direct history.compact call.
    #[serde(rename = "manual")]
    Manual,
    /// Compaction requested while switching to a model with a smaller context window.
    #[serde(rename = "model_switch")]
    ModelSwitch,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HistoryCompactRequestTrigger {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HistoryCompactRequestTrigger",
            &["manual", "model_switch", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Manual,
            1 => Self::ModelSwitch,
            _ => Self::Unknown,
        })
    }
}

/// Scope of a rewind operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HistoryRewindMode {
    /// Discard conversation events while leaving files unchanged.
    #[serde(rename = "conversation")]
    Conversation,
    /// Discard conversation events and restore captured files changed by those turns.
    #[serde(rename = "conversation-and-files")]
    ConversationAndFiles,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HistoryRewindMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HistoryRewindMode",
            &["conversation", "conversation-and-files", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Conversation,
            1 => Self::ConversationAndFiles,
            _ => Self::Unknown,
        })
    }
}

/// Private transition in an admitted host's credential lifecycle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HostCredentialLifecycleAction {
    /// Start a credential transaction for session creation or resume.
    #[serde(rename = "begin")]
    Begin,
    /// Bind the transaction's registration to its session.
    #[serde(rename = "bind")]
    Bind,
    /// Adopt the session's current registration.
    #[serde(rename = "adopt")]
    Adopt,
    /// Restore a displaced registration after a failed transaction.
    #[serde(rename = "restore")]
    Restore,
    /// Discard an unaccepted credential transaction.
    #[serde(rename = "discard")]
    Discard,
    /// Accept the transaction and retain its session registration.
    #[serde(rename = "accept")]
    Accept,
    /// Read the transaction's resolved auth information.
    #[serde(rename = "auth")]
    Auth,
    /// Read a session's current credential registration.
    #[serde(rename = "session")]
    Session,
    /// Release a session's credential registration.
    #[serde(rename = "release")]
    Release,
    /// Restore a session's previous credential registration.
    #[serde(rename = "restoreSession")]
    RestoreSession,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HostCredentialLifecycleAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HostCredentialLifecycleAction",
            &[
                "begin",
                "bind",
                "adopt",
                "restore",
                "discard",
                "accept",
                "auth",
                "session",
                "release",
                "restoreSession",
                "Unknown",
            ],
            Some(10),
        )?;
        Ok(match index {
            0 => Self::Begin,
            1 => Self::Bind,
            2 => Self::Adopt,
            3 => Self::Restore,
            4 => Self::Discard,
            5 => Self::Accept,
            6 => Self::Auth,
            7 => Self::Session,
            8 => Self::Release,
            9 => Self::RestoreSession,
            _ => Self::Unknown,
        })
    }
}

/// The cached host error-reporting policy. Pending means startup policy has not resolved. This does not describe network delivery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HostErrorReportingStatus {
    /// Startup policy resolution is incomplete.
    #[serde(rename = "pending")]
    Pending,
    /// The host policy permits error reports.
    #[serde(rename = "enabled")]
    Enabled,
    /// The host policy blocks error reports.
    #[serde(rename = "disabled")]
    Disabled,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HostErrorReportingStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HostErrorReportingStatus",
            &["pending", "enabled", "disabled", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Pending,
            1 => Self::Enabled,
            2 => Self::Disabled,
            _ => Self::Unknown,
        })
    }
}

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum HostExitReason {
    /// The owner requested disposal.
    #[serde(rename = "disposed")]
    Disposed,
    /// The hosting task or its SDK transport exited; this does not mean the runtime process exited.
    #[serde(rename = "exited")]
    Exited,
    /// The owning SDK connection disconnected.
    #[serde(rename = "ownerDisconnected")]
    OwnerDisconnected,
    /// The runtime is shutting down.
    #[serde(rename = "runtimeShutdown")]
    RuntimeShutdown,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for HostExitReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "HostExitReason",
            &[
                "disposed",
                "exited",
                "ownerDisconnected",
                "runtimeShutdown",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Disposed,
            1 => Self::Exited,
            2 => Self::OwnerDisconnected,
            3 => Self::RuntimeShutdown,
            _ => Self::Unknown,
        })
    }
}

/// Constant value. Always "github".
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstalledPluginSourceGitHubSource {
    #[serde(rename = "github")]
    #[default]
    GitHub,
}
impl<'de> Deserialize<'de> for InstalledPluginSourceGitHubSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "InstalledPluginSourceGitHubSource",
            &["github"],
            None,
        )?;
        Ok(Self::GitHub)
    }
}

/// Constant value. Always "local".
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstalledPluginSourceLocalSource {
    #[serde(rename = "local")]
    #[default]
    Local,
}
impl<'de> Deserialize<'de> for InstalledPluginSourceLocalSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "InstalledPluginSourceLocalSource",
            &["local"],
            None,
        )?;
        Ok(Self::Local)
    }
}

/// Constant value. Always "url".
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstalledPluginSourceUrlSource {
    #[serde(rename = "url")]
    #[default]
    Url,
}
impl<'de> Deserialize<'de> for InstalledPluginSourceUrlSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "InstalledPluginSourceUrlSource",
            &["url"],
            None,
        )?;
        Ok(Self::Url)
    }
}

/// Whether the target is a single file or a directory of instruction files
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstructionDiscoveryPathKind {
    /// The target is a single instruction file.
    #[serde(rename = "file")]
    File,
    /// The target is a directory that holds instruction files.
    #[serde(rename = "directory")]
    Directory,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for InstructionDiscoveryPathKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "InstructionDiscoveryPathKind",
            &["file", "directory", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::File,
            1 => Self::Directory,
            _ => Self::Unknown,
        })
    }
}

/// Which tier this target belongs to
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstructionDiscoveryPathLocation {
    /// Instructions live in user-level configuration.
    #[serde(rename = "user")]
    User,
    /// Instructions live in repository-level configuration.
    #[serde(rename = "repository")]
    Repository,
    /// Instructions live under the current working directory.
    #[serde(rename = "working-directory")]
    WorkingDirectory,
    /// Instructions live in plugin-provided configuration.
    #[serde(rename = "plugin")]
    Plugin,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for InstructionDiscoveryPathLocation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "InstructionDiscoveryPathLocation",
            &[
                "user",
                "repository",
                "working-directory",
                "plugin",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::User,
            1 => Self::Repository,
            2 => Self::WorkingDirectory,
            3 => Self::Plugin,
            _ => Self::Unknown,
        })
    }
}

/// Transport the runtime would otherwise use for this request. `http` (the default when absent) covers plain HTTP and SSE responses; `websocket` indicates a full-duplex message channel where each body chunk maps to one WebSocket message and the `binary` flag distinguishes text from binary frames. The SDK consumer uses this to decide whether to service the request with an HTTP client or a WebSocket client. It is the one piece of request metadata the consumer cannot reliably infer from the URL or headers alone.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LlmInferenceHttpRequestStartTransport {
    /// Plain HTTP or SSE response. Each body chunk is an opaque byte range; the response is a status line, headers, and a (possibly streamed) body.
    #[serde(rename = "http")]
    Http,
    /// Full-duplex WebSocket channel. Each body chunk maps to exactly one WebSocket message and the `binary` flag distinguishes text from binary frames; request and response chunks flow concurrently.
    #[serde(rename = "websocket")]
    Websocket,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for LlmInferenceHttpRequestStartTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "LlmInferenceHttpRequestStartTransport",
            &["http", "websocket", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Http,
            1 => Self::Websocket,
            _ => Self::Unknown,
        })
    }
}

/// Local disk requirement evaluation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelDiskRequirementUnknownKind {
    #[serde(rename = "unknown")]
    #[default]
    Unknown,
}
impl<'de> Deserialize<'de> for LocalModelDiskRequirementUnknownKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelDiskRequirementUnknownKind",
            &["unknown"],
            None,
        )?;
        Ok(Self::Unknown)
    }
}

/// Local disk requirement evaluation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelDiskRequirementOkKind {
    #[serde(rename = "ok")]
    #[default]
    Ok,
}
impl<'de> Deserialize<'de> for LocalModelDiskRequirementOkKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelDiskRequirementOkKind",
            &["ok"],
            None,
        )?;
        Ok(Self::Ok)
    }
}

/// Local disk requirement evaluation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelDiskRequirementInsufficientKind {
    #[serde(rename = "insufficient")]
    #[default]
    Insufficient,
}
impl<'de> Deserialize<'de> for LocalModelDiskRequirementInsufficientKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelDiskRequirementInsufficientKind",
            &["insufficient"],
            None,
        )?;
        Ok(Self::Insufficient)
    }
}

/// Local disk requirement evaluation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LocalModelDiskRequirement {
    Unknown(LocalModelDiskRequirementUnknown),
    Ok(LocalModelDiskRequirementOk),
    Insufficient(LocalModelDiskRequirementInsufficient),
}

/// Graphics memory requirement evaluation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelGpuRequirementUnknownKind {
    #[serde(rename = "unknown")]
    #[default]
    Unknown,
}
impl<'de> Deserialize<'de> for LocalModelGpuRequirementUnknownKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelGpuRequirementUnknownKind",
            &["unknown"],
            None,
        )?;
        Ok(Self::Unknown)
    }
}

/// Graphics memory requirement evaluation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelGpuRequirementUnspecifiedKind {
    #[serde(rename = "unspecified")]
    #[default]
    Unspecified,
}
impl<'de> Deserialize<'de> for LocalModelGpuRequirementUnspecifiedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelGpuRequirementUnspecifiedKind",
            &["unspecified"],
            None,
        )?;
        Ok(Self::Unspecified)
    }
}

/// Graphics memory requirement evaluation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelGpuRequirementOkKind {
    #[serde(rename = "ok")]
    #[default]
    Ok,
}
impl<'de> Deserialize<'de> for LocalModelGpuRequirementOkKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelGpuRequirementOkKind",
            &["ok"],
            None,
        )?;
        Ok(Self::Ok)
    }
}

/// Graphics memory requirement evaluation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelGpuRequirementInsufficientKind {
    #[serde(rename = "insufficient")]
    #[default]
    Insufficient,
}
impl<'de> Deserialize<'de> for LocalModelGpuRequirementInsufficientKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelGpuRequirementInsufficientKind",
            &["insufficient"],
            None,
        )?;
        Ok(Self::Insufficient)
    }
}

/// Graphics memory requirement evaluation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LocalModelGpuRequirement {
    Unknown(LocalModelGpuRequirementUnknown),
    Unspecified(LocalModelGpuRequirementUnspecified),
    Ok(LocalModelGpuRequirementOk),
    Insufficient(LocalModelGpuRequirementInsufficient),
}

/// Effective local installation or transfer state.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelStatusNotDownloadedKind {
    #[serde(rename = "not-downloaded")]
    #[default]
    NotDownloaded,
}
impl<'de> Deserialize<'de> for LocalModelStatusNotDownloadedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelStatusNotDownloadedKind",
            &["not-downloaded"],
            None,
        )?;
        Ok(Self::NotDownloaded)
    }
}

/// Effective local installation or transfer state.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelStatusDownloadedKind {
    #[serde(rename = "downloaded")]
    #[default]
    Downloaded,
}
impl<'de> Deserialize<'de> for LocalModelStatusDownloadedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelStatusDownloadedKind",
            &["downloaded"],
            None,
        )?;
        Ok(Self::Downloaded)
    }
}

/// Effective local installation or transfer state.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelStatusDownloadingKind {
    #[serde(rename = "downloading")]
    #[default]
    Downloading,
}
impl<'de> Deserialize<'de> for LocalModelStatusDownloadingKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelStatusDownloadingKind",
            &["downloading"],
            None,
        )?;
        Ok(Self::Downloading)
    }
}

/// Effective file or download state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LocalModelStatus {
    NotDownloaded(LocalModelStatusNotDownloaded),
    Downloaded(LocalModelStatusDownloaded),
    Downloading(LocalModelStatusDownloading),
}

/// Active acquisition or removal stage.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelOperation {
    /// Install the inference engine.
    #[serde(rename = "installing")]
    Installing,
    /// Resolve the model and inspect existing files.
    #[serde(rename = "probing")]
    Probing,
    /// Transfer the model files.
    #[serde(rename = "downloading")]
    Downloading,
    /// Save or activate the model registration.
    #[serde(rename = "registering")]
    Registering,
    /// Remove registration and files.
    #[serde(rename = "deleting")]
    Deleting,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for LocalModelOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelOperation",
            &[
                "installing",
                "probing",
                "downloading",
                "registering",
                "deleting",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Installing,
            1 => Self::Probing,
            2 => Self::Downloading,
            3 => Self::Registering,
            4 => Self::Deleting,
            _ => Self::Unknown,
        })
    }
}

/// Information, warning, or error.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelTimelineMessageType {
    /// Inform the user about progress or completion.
    #[serde(rename = "info")]
    Info,
    /// Report a condition that requires attention.
    #[serde(rename = "warning")]
    Warning,
    /// Report a failed operation.
    #[serde(rename = "error")]
    Error,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for LocalModelTimelineMessageType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelTimelineMessageType",
            &["info", "warning", "error", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Info,
            1 => Self::Warning,
            2 => Self::Error,
            _ => Self::Unknown,
        })
    }
}

/// Host notification or model-list refresh.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelsEventType {
    /// Display a model-management message.
    #[serde(rename = "notify")]
    Notify,
    /// Refresh the host's available model list.
    #[serde(rename = "refresh")]
    Refresh,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for LocalModelsEventType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelsEventType",
            &["notify", "refresh", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Notify,
            1 => Self::Refresh,
            _ => Self::Unknown,
        })
    }
}

/// Download, update, or repair intent.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum LocalModelsSetupRequestIntent {
    /// Acquire and register an available model.
    #[serde(rename = "download")]
    Download,
    /// Acquire the latest version of an installed model.
    #[serde(rename = "update")]
    Update,
    /// Restore registration for an existing installation without downloading a missing model.
    #[serde(rename = "repair")]
    Repair,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for LocalModelsSetupRequestIntent {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "LocalModelsSetupRequestIntent",
            &["download", "update", "repair", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Download,
            1 => Self::Update,
            2 => Self::Repair,
            _ => Self::Unknown,
        })
    }
}

/// Log severity level. Determines how the message is displayed in the timeline. Defaults to "info".
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SessionLogLevel {
    /// Informational message.
    #[serde(rename = "info")]
    Info,
    /// Warning message that may require attention.
    #[serde(rename = "warning")]
    Warning,
    /// Error message describing a failure.
    #[serde(rename = "error")]
    Error,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SessionLogLevel {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SessionLogLevel",
            &["info", "warning", "error", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Info,
            1 => Self::Warning,
            2 => Self::Error,
            _ => Self::Unknown,
        })
    }
}

/// Structured MCP elicitation mode.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpElicitationFormMode {
    #[serde(rename = "form")]
    Form,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpElicitationFormMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpElicitationFormMode",
            &["form", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Form,
            _ => Self::Unknown,
        })
    }
}

/// Selects which traffic a client's network callbacks serve.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum NetworkScope {
    /// Serve only requests made for this connection and its sessions.
    #[serde(rename = "connection")]
    Connection,
    /// Also serve the runtime's shared background traffic. Only one connection at a time may claim this scope.
    #[serde(rename = "runtime")]
    Runtime,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for NetworkScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "NetworkScope",
            &["connection", "runtime", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Connection,
            1 => Self::Runtime,
            _ => Self::Unknown,
        })
    }
}

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProtocolAppendMode {
    #[serde(rename = "append")]
    Append,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProtocolAppendMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProtocolAppendMode",
            &["append", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Append,
            _ => Self::Unknown,
        })
    }
}

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProtocolCustomizeMode {
    #[serde(rename = "customize")]
    Customize,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProtocolCustomizeMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProtocolCustomizeMode",
            &["customize", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Customize,
            _ => Self::Unknown,
        })
    }
}

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProtocolReplaceMode {
    #[serde(rename = "replace")]
    Replace,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProtocolReplaceMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProtocolReplaceMode",
            &["replace", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Replace,
            _ => Self::Unknown,
        })
    }
}

///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProtocolStaticSectionAction {
    /// Replace the section content.
    #[serde(rename = "replace")]
    Replace,
    /// Remove the section content.
    #[serde(rename = "remove")]
    Remove,
    /// Append content to the section.
    #[serde(rename = "append")]
    Append,
    /// Prepend content to the section.
    #[serde(rename = "prepend")]
    Prepend,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProtocolStaticSectionAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProtocolStaticSectionAction",
            &["replace", "remove", "append", "prepend", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Replace,
            1 => Self::Remove,
            2 => Self::Append,
            3 => Self::Prepend,
            _ => Self::Unknown,
        })
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentBlobType {
    #[serde(rename = "blob")]
    #[default]
    Blob,
}
impl<'de> Deserialize<'de> for PushAttachmentBlobType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentBlobType",
            &["blob"],
            None,
        )?;
        Ok(Self::Blob)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentDirectoryType {
    #[serde(rename = "directory")]
    #[default]
    Directory,
}
impl<'de> Deserialize<'de> for PushAttachmentDirectoryType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentDirectoryType",
            &["directory"],
            None,
        )?;
        Ok(Self::Directory)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentFileType {
    #[serde(rename = "file")]
    #[default]
    File,
}
impl<'de> Deserialize<'de> for PushAttachmentFileType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentFileType",
            &["file"],
            None,
        )?;
        Ok(Self::File)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentGitHubActionsJobType {
    #[serde(rename = "github_actions_job")]
    #[default]
    GitHubActionsJob,
}
impl<'de> Deserialize<'de> for PushAttachmentGitHubActionsJobType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentGitHubActionsJobType",
            &["github_actions_job"],
            None,
        )?;
        Ok(Self::GitHubActionsJob)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentGitHubCommitType {
    #[serde(rename = "github_commit")]
    #[default]
    GitHubCommit,
}
impl<'de> Deserialize<'de> for PushAttachmentGitHubCommitType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentGitHubCommitType",
            &["github_commit"],
            None,
        )?;
        Ok(Self::GitHubCommit)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentGitHubFileType {
    #[serde(rename = "github_file")]
    #[default]
    GitHubFile,
}
impl<'de> Deserialize<'de> for PushAttachmentGitHubFileType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentGitHubFileType",
            &["github_file"],
            None,
        )?;
        Ok(Self::GitHubFile)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentGitHubFileDiffType {
    #[serde(rename = "github_file_diff")]
    #[default]
    GitHubFileDiff,
}
impl<'de> Deserialize<'de> for PushAttachmentGitHubFileDiffType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentGitHubFileDiffType",
            &["github_file_diff"],
            None,
        )?;
        Ok(Self::GitHubFileDiff)
    }
}

/// Type of GitHub reference
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentGitHubReferenceType {
    /// GitHub issue reference.
    #[serde(rename = "issue")]
    Issue,
    /// GitHub pull request reference.
    #[serde(rename = "pr")]
    Pr,
    /// GitHub discussion reference.
    #[serde(rename = "discussion")]
    Discussion,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PushAttachmentGitHubReferenceType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentGitHubReferenceType",
            &["issue", "pr", "discussion", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Issue,
            1 => Self::Pr,
            2 => Self::Discussion,
            _ => Self::Unknown,
        })
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentGitHubReleaseType {
    #[serde(rename = "github_release")]
    #[default]
    GitHubRelease,
}
impl<'de> Deserialize<'de> for PushAttachmentGitHubReleaseType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentGitHubReleaseType",
            &["github_release"],
            None,
        )?;
        Ok(Self::GitHubRelease)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentGitHubRepositoryType {
    #[serde(rename = "github_repository")]
    #[default]
    GitHubRepository,
}
impl<'de> Deserialize<'de> for PushAttachmentGitHubRepositoryType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentGitHubRepositoryType",
            &["github_repository"],
            None,
        )?;
        Ok(Self::GitHubRepository)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentGitHubSnippetType {
    #[serde(rename = "github_snippet")]
    #[default]
    GitHubSnippet,
}
impl<'de> Deserialize<'de> for PushAttachmentGitHubSnippetType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentGitHubSnippetType",
            &["github_snippet"],
            None,
        )?;
        Ok(Self::GitHubSnippet)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentGitHubTreeComparisonType {
    #[serde(rename = "github_tree_comparison")]
    #[default]
    GitHubTreeComparison,
}
impl<'de> Deserialize<'de> for PushAttachmentGitHubTreeComparisonType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentGitHubTreeComparisonType",
            &["github_tree_comparison"],
            None,
        )?;
        Ok(Self::GitHubTreeComparison)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentGitHubUrlType {
    #[serde(rename = "github_url")]
    #[default]
    GitHubUrl,
}
impl<'de> Deserialize<'de> for PushAttachmentGitHubUrlType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentGitHubUrlType",
            &["github_url"],
            None,
        )?;
        Ok(Self::GitHubUrl)
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PushAttachmentSelectionType {
    #[serde(rename = "selection")]
    #[default]
    Selection,
}
impl<'de> Deserialize<'de> for PushAttachmentSelectionType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PushAttachmentSelectionType",
            &["selection"],
            None,
        )?;
        Ok(Self::Selection)
    }
}

/// How to deliver the message. `enqueue` (default) appends to the message queue. `immediate` interjects during an in-progress turn.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SendMode {
    /// Append the message to the normal session queue.
    #[serde(rename = "enqueue")]
    Enqueue,
    /// Interject the message during the in-progress turn.
    #[serde(rename = "immediate")]
    Immediate,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SendMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SendMode",
            &["enqueue", "immediate", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Enqueue,
            1 => Self::Immediate,
            _ => Self::Unknown,
        })
    }
}

/// Which repository settings file to edit.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum RepoSettingsEditScope {
    /// The shared repository settings file.
    #[serde(rename = "repo")]
    Repo,
    /// The local repository settings file.
    #[serde(rename = "local")]
    Local,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for RepoSettingsEditScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "RepoSettingsEditScope",
            &["repo", "local", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Repo,
            1 => Self::Local,
            _ => Self::Unknown,
        })
    }
}

/// Output format discriminator. Currently only json_schema is supported.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ResponseFormatType {
    #[serde(rename = "json_schema")]
    #[default]
    JsonSchema,
}
impl<'de> Deserialize<'de> for ResponseFormatType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ResponseFormatType",
            &["json_schema"],
            None,
        )?;
        Ok(Self::JsonSchema)
    }
}

/// A session-scoped sandbox transition applied while handling a slash command
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxSessionChange {
    /// The sandbox is off for the rest of this session; nothing was persisted and a new session starts from saved and managed policy.
    #[serde(rename = "disabled")]
    Disabled,
    /// The sandbox is enabled for this session; nothing was persisted.
    #[serde(rename = "restored")]
    Restored,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SandboxSessionChange {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxSessionChange",
            &["disabled", "restored", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Disabled,
            1 => Self::Restored,
            _ => Self::Unknown,
        })
    }
}

/// Representation of content submitted to the runtime redaction filter.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SecretFilterFormat {
    /// Redacts plain text.
    #[serde(rename = "text")]
    Text,
    /// Redacts decoded JSON values. Malformed JSON uses plain-text redaction.
    #[serde(rename = "json")]
    Json,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SecretFilterFormat {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SecretFilterFormat",
            &["text", "json", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Text,
            1 => Self::Json,
            _ => Self::Unknown,
        })
    }
}

/// Output format discriminator. Currently only json_schema is supported.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SendMessagesRequestResponseFormatType {
    #[serde(rename = "json_schema")]
    #[default]
    JsonSchema,
}
impl<'de> Deserialize<'de> for SendMessagesRequestResponseFormatType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SendMessagesRequestResponseFormatType",
            &["json_schema"],
            None,
        )?;
        Ok(Self::JsonSchema)
    }
}

/// Output format discriminator. Currently only json_schema is supported.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SendRequestResponseFormatType {
    #[serde(rename = "json_schema")]
    #[default]
    JsonSchema,
}
impl<'de> Deserialize<'de> for SendRequestResponseFormatType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SendRequestResponseFormatType",
            &["json_schema"],
            None,
        )?;
        Ok(Self::JsonSchema)
    }
}

/// Signal to send (default: SIGTERM)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ShellKillSignal {
    /// Request graceful process termination.
    SIGTERM,
    /// Forcefully terminate the process.
    SIGKILL,
    /// Send an interrupt signal to the process.
    SIGINT,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ShellKillSignal {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ShellKillSignal",
            &["SIGTERM", "SIGKILL", "SIGINT", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::SIGTERM,
            1 => Self::SIGKILL,
            2 => Self::SIGINT,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for an add-timeline-entry result.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandAddTimelineEntryResultKind {
    #[serde(rename = "add-timeline-entry")]
    #[default]
    AddTimelineEntry,
}
impl<'de> Deserialize<'de> for SlashCommandAddTimelineEntryResultKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandAddTimelineEntryResultKind",
            &["add-timeline-entry"],
            None,
        )?;
        Ok(Self::AddTimelineEntry)
    }
}

/// Agent prompt result discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandAgentPromptResultKind {
    #[serde(rename = "agent-prompt")]
    #[default]
    AgentPrompt,
}
impl<'de> Deserialize<'de> for SlashCommandAgentPromptResultKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandAgentPromptResultKind",
            &["agent-prompt"],
            None,
        )?;
        Ok(Self::AgentPrompt)
    }
}

/// Completed result discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandCompletedResultKind {
    #[serde(rename = "completed")]
    #[default]
    Completed,
}
impl<'de> Deserialize<'de> for SlashCommandCompletedResultKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandCompletedResultKind",
            &["completed"],
            None,
        )?;
        Ok(Self::Completed)
    }
}

/// Text result discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandTextResultKind {
    #[serde(rename = "text")]
    #[default]
    Text,
}
impl<'de> Deserialize<'de> for SlashCommandTextResultKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandTextResultKind",
            &["text"],
            None,
        )?;
        Ok(Self::Text)
    }
}

/// Select subcommand result discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandSelectSubcommandResultKind {
    #[serde(rename = "select-subcommand")]
    #[default]
    SelectSubcommand,
}
impl<'de> Deserialize<'de> for SlashCommandSelectSubcommandResultKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandSelectSubcommandResultKind",
            &["select-subcommand"],
            None,
        )?;
        Ok(Self::SelectSubcommand)
    }
}

/// Discriminator for a model-picker dialog.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandModelPickerDialogKind {
    #[serde(rename = "model-picker")]
    #[default]
    ModelPicker,
}
impl<'de> Deserialize<'de> for SlashCommandModelPickerDialogKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandModelPickerDialogKind",
            &["model-picker"],
            None,
        )?;
        Ok(Self::ModelPicker)
    }
}

/// Discriminator for a show-dialog result.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandShowDialogResultKind {
    #[serde(rename = "show-dialog")]
    #[default]
    ShowDialog,
}
impl<'de> Deserialize<'de> for SlashCommandShowDialogResultKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandShowDialogResultKind",
            &["show-dialog"],
            None,
        )?;
        Ok(Self::ShowDialog)
    }
}

/// Discriminator for a set-model result.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandSetModelResultKind {
    #[serde(rename = "set-model")]
    #[default]
    SetModel,
}
impl<'de> Deserialize<'de> for SlashCommandSetModelResultKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandSetModelResultKind",
            &["set-model"],
            None,
        )?;
        Ok(Self::SetModel)
    }
}

/// Discriminator for a set-plan-model result.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SlashCommandSetPlanModelResultKind {
    #[serde(rename = "set-plan-model")]
    #[default]
    SetPlanModel,
}
impl<'de> Deserialize<'de> for SlashCommandSetPlanModelResultKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SlashCommandSetPlanModelResultKind",
            &["set-plan-model"],
            None,
        )?;
        Ok(Self::SetPlanModel)
    }
}

/// Result of invoking the slash command (text output, prompt to send to the agent, completion, or subcommand selection).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SlashCommandInvocationResult {
    Text(SlashCommandTextResult),
    AgentPrompt(SlashCommandAgentPromptResult),
    Completed(SlashCommandCompletedResult),
    SelectSubcommand(SlashCommandSelectSubcommandResult),
    AddTimelineEntry(SlashCommandAddTimelineEntryResult),
    ShowDialog(SlashCommandShowDialogResult),
    SetModel(SlashCommandSetModelResult),
    SetPlanModel(SlashCommandSetPlanModelResult),
}

/// User's choice for auto-mode switching: yes (allow this turn), yes_always (allow + persist as setting), or no (decline).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIAutoModeSwitchResponse {
    /// Allow the automatic mode switch for this turn.
    #[serde(rename = "yes")]
    Yes,
    /// Allow this mode switch and persist the preference.
    #[serde(rename = "yes_always")]
    YesAlways,
    /// Decline the automatic mode switch.
    #[serde(rename = "no")]
    No,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for UIAutoModeSwitchResponse {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "UIAutoModeSwitchResponse",
            &["yes", "yes_always", "no", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Yes,
            1 => Self::YesAlways,
            2 => Self::No,
            _ => Self::Unknown,
        })
    }
}

/// Type discriminator. Always "array".
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationArrayAnyOfFieldType {
    #[serde(rename = "array")]
    #[default]
    Array,
}
impl<'de> Deserialize<'de> for UIElicitationArrayAnyOfFieldType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationArrayAnyOfFieldType",
            &["array"],
            None,
        )?;
        Ok(Self::Array)
    }
}

/// Type discriminator. Always "string".
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationArrayEnumFieldItemsType {
    #[serde(rename = "string")]
    #[default]
    String,
}
impl<'de> Deserialize<'de> for UIElicitationArrayEnumFieldItemsType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationArrayEnumFieldItemsType",
            &["string"],
            None,
        )?;
        Ok(Self::String)
    }
}

/// Type discriminator. Always "array".
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationArrayEnumFieldType {
    #[serde(rename = "array")]
    #[default]
    Array,
}
impl<'de> Deserialize<'de> for UIElicitationArrayEnumFieldType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationArrayEnumFieldType",
            &["array"],
            None,
        )?;
        Ok(Self::Array)
    }
}

/// Schema type indicator (always 'object')
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationSchemaType {
    #[serde(rename = "object")]
    #[default]
    Object,
}
impl<'de> Deserialize<'de> for UIElicitationSchemaType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationSchemaType",
            &["object"],
            None,
        )?;
        Ok(Self::Object)
    }
}

/// Type discriminator. Always "boolean".
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationSchemaPropertyBooleanType {
    #[serde(rename = "boolean")]
    #[default]
    Boolean,
}
impl<'de> Deserialize<'de> for UIElicitationSchemaPropertyBooleanType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationSchemaPropertyBooleanType",
            &["boolean"],
            None,
        )?;
        Ok(Self::Boolean)
    }
}

/// Numeric type accepted by the field.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationSchemaPropertyNumberType {
    /// Any JSON number.
    #[serde(rename = "number")]
    Number,
    /// Integer JSON number.
    #[serde(rename = "integer")]
    Integer,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for UIElicitationSchemaPropertyNumberType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationSchemaPropertyNumberType",
            &["number", "integer", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Number,
            1 => Self::Integer,
            _ => Self::Unknown,
        })
    }
}

/// Optional format hint that constrains the accepted input.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationSchemaPropertyStringFormat {
    /// Email address string format.
    #[serde(rename = "email")]
    Email,
    /// URI string format.
    #[serde(rename = "uri")]
    Uri,
    /// Calendar date string format.
    #[serde(rename = "date")]
    Date,
    /// Date-time string format.
    #[serde(rename = "date-time")]
    DateTime,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for UIElicitationSchemaPropertyStringFormat {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationSchemaPropertyStringFormat",
            &["email", "uri", "date", "date-time", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Email,
            1 => Self::Uri,
            2 => Self::Date,
            3 => Self::DateTime,
            _ => Self::Unknown,
        })
    }
}

/// Type discriminator. Always "string".
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationSchemaPropertyStringType {
    #[serde(rename = "string")]
    #[default]
    String,
}
impl<'de> Deserialize<'de> for UIElicitationSchemaPropertyStringType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationSchemaPropertyStringType",
            &["string"],
            None,
        )?;
        Ok(Self::String)
    }
}

/// Type discriminator. Always "string".
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationStringEnumFieldType {
    #[serde(rename = "string")]
    #[default]
    String,
}
impl<'de> Deserialize<'de> for UIElicitationStringEnumFieldType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationStringEnumFieldType",
            &["string"],
            None,
        )?;
        Ok(Self::String)
    }
}

/// Type discriminator. Always "string".
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIElicitationStringOneOfFieldType {
    #[serde(rename = "string")]
    #[default]
    String,
}
impl<'de> Deserialize<'de> for UIElicitationStringOneOfFieldType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "UIElicitationStringOneOfFieldType",
            &["string"],
            None,
        )?;
        Ok(Self::String)
    }
}

/// The action the user selected. Defaults to 'autopilot' when autoApproveEdits is true, otherwise 'interactive'.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UIExitPlanModeAction {
    /// Exit plan mode without starting implementation.
    #[serde(rename = "exit_only")]
    ExitOnly,
    /// Exit plan mode and continue interactively.
    #[serde(rename = "interactive")]
    Interactive,
    /// Exit plan mode and continue in autopilot mode.
    #[serde(rename = "autopilot")]
    Autopilot,
    /// Exit plan mode and continue in autopilot mode with parallel subagent execution.
    #[serde(rename = "autopilot_fleet")]
    AutopilotFleet,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for UIExitPlanModeAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "UIExitPlanModeAction",
            &[
                "exit_only",
                "interactive",
                "autopilot",
                "autopilot_fleet",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::ExitOnly,
            1 => Self::Interactive,
            2 => Self::Autopilot,
            3 => Self::AutopilotFleet,
            _ => Self::Unknown,
        })
    }
}

/// User action selected for an exhausted session limit.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum UISessionLimitsExhaustedResponseAction {
    /// Increase the current max by an exact AI Credits amount.
    #[serde(rename = "add")]
    Add,
    /// Set a new absolute max AI Credits value.
    #[serde(rename = "set")]
    Set,
    /// Remove the current session limit.
    #[serde(rename = "unset")]
    Unset,
    /// Leave the limit unchanged and cancel the blocked model request.
    #[serde(rename = "cancel")]
    Cancel,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for UISessionLimitsExhaustedResponseAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "UISessionLimitsExhaustedResponseAction",
            &["add", "set", "unset", "cancel", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Add,
            1 => Self::Set,
            2 => Self::Unset,
            3 => Self::Cancel,
            _ => Self::Unknown,
        })
    }
}
