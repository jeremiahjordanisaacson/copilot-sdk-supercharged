//! Auto-generated from api.schema.json — do not edit manually.

#![allow(clippy::large_enum_variant)]
#![allow(deprecated)]
#![allow(dead_code)]
#![allow(rustdoc::invalid_html_tags)]

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

pub use super::session_events::{
    McpOauthHttpResponse, McpOauthWWWAuthenticateParams, PermissionDecisionSource,
};
use crate::types::SessionId;

/// JSON-RPC method name constants.
pub mod rpc_methods {
    /// `ahp.connections.createSession`
    pub const AHP_CONNECTIONS_CREATESESSION: &str = "ahp.connections.createSession";
    /// `ahp.connections.selectSession`
    pub const AHP_CONNECTIONS_SELECTSESSION: &str = "ahp.connections.selectSession";
    /// `ahp.connections.disposeSession`
    pub const AHP_CONNECTIONS_DISPOSESESSION: &str = "ahp.connections.disposeSession";
    /// `ahp.connections.create`
    pub const AHP_CONNECTIONS_CREATE: &str = "ahp.connections.create";
    /// `ahp.connections.attachSession`
    pub const AHP_CONNECTIONS_ATTACHSESSION: &str = "ahp.connections.attachSession";
    /// `ahp.connections.detachSession`
    pub const AHP_CONNECTIONS_DETACHSESSION: &str = "ahp.connections.detachSession";
    /// `ahp.connections.connect`
    pub const AHP_CONNECTIONS_CONNECT: &str = "ahp.connections.connect";
    /// `ahp.connections.reconnect`
    pub const AHP_CONNECTIONS_RECONNECT: &str = "ahp.connections.reconnect";
    /// `ahp.connections.refresh`
    pub const AHP_CONNECTIONS_REFRESH: &str = "ahp.connections.refresh";
    /// `ahp.connections.authenticate`
    pub const AHP_CONNECTIONS_AUTHENTICATE: &str = "ahp.connections.authenticate";
    /// `ahp.connections.readEvents`
    pub const AHP_CONNECTIONS_READEVENTS: &str = "ahp.connections.readEvents";
    /// `ahp.connections.dispose`
    pub const AHP_CONNECTIONS_DISPOSE: &str = "ahp.connections.dispose";
    /// `ahp.connectionLogs.open`
    pub const AHP_CONNECTIONLOGS_OPEN: &str = "ahp.connectionLogs.open";
    /// `ahp.connectionLogs.write`
    pub const AHP_CONNECTIONLOGS_WRITE: &str = "ahp.connectionLogs.write";
    /// `ahp.connectionLogs.flush`
    pub const AHP_CONNECTIONLOGS_FLUSH: &str = "ahp.connectionLogs.flush";
    /// `ahp.connectionLogs.close`
    pub const AHP_CONNECTIONLOGS_CLOSE: &str = "ahp.connectionLogs.close";
    /// `providersSource.addModel`
    pub const PROVIDERSSOURCE_ADDMODEL: &str = "providersSource.addModel";
    /// `agents.getBuiltins`
    pub const AGENTS_GETBUILTINS: &str = "agents.getBuiltins";
    /// `agents.getAvailableBuiltins`
    pub const AGENTS_GETAVAILABLEBUILTINS: &str = "agents.getAvailableBuiltins";
    /// `agents.getBuiltinDefinition`
    pub const AGENTS_GETBUILTINDEFINITION: &str = "agents.getBuiltinDefinition";
    /// `agents.customAgentInitialModelDecision`
    pub const AGENTS_CUSTOMAGENTINITIALMODELDECISION: &str =
        "agents.customAgentInitialModelDecision";
    /// `cafeMetrics.start`
    pub const CAFEMETRICS_START: &str = "cafeMetrics.start";
    /// `cafeMetrics.resolveAdmission`
    pub const CAFEMETRICS_RESOLVEADMISSION: &str = "cafeMetrics.resolveAdmission";
    /// `cafeMetrics.recordCliReady`
    pub const CAFEMETRICS_RECORDCLIREADY: &str = "cafeMetrics.recordCliReady";
    /// `cafeMetrics.recordResumeResult`
    pub const CAFEMETRICS_RECORDRESUMERESULT: &str = "cafeMetrics.recordResumeResult";
    /// `cafeMetrics.shutdown`
    pub const CAFEMETRICS_SHUTDOWN: &str = "cafeMetrics.shutdown";
    /// `acp.serve`
    pub const ACP_SERVE: &str = "acp.serve";
    /// `acp.shutdown`
    pub const ACP_SHUTDOWN: &str = "acp.shutdown";
    /// `mcpSource.prepareGitHubAuth`
    pub const MCPSOURCE_PREPAREGITHUBAUTH: &str = "mcpSource.prepareGitHubAuth";
    /// `session.mcp.externalClient.exchange`
    pub const SESSION_MCP_EXTERNALCLIENT_EXCHANGE: &str = "session.mcp.externalClient.exchange";
    /// `session.mcp.configureGitHub`
    pub const SESSION_MCP_CONFIGUREGITHUB: &str = "session.mcp.configureGitHub";
    /// `session.mcp.oauth.prepareFlow`
    pub const SESSION_MCP_OAUTH_PREPAREFLOW: &str = "session.mcp.oauth.prepareFlow";
    /// `session.mcp.oauth.cancelFlow`
    pub const SESSION_MCP_OAUTH_CANCELFLOW: &str = "session.mcp.oauth.cancelFlow";
    /// `session.mcp.oauth.authenticate`
    pub const SESSION_MCP_OAUTH_AUTHENTICATE: &str = "session.mcp.oauth.authenticate";
    /// `session.mcp.oauth.hasValidTokens`
    pub const SESSION_MCP_OAUTH_HASVALIDTOKENS: &str = "session.mcp.oauth.hasValidTokens";
    /// `session.mcp.oauth.tryRefreshTokens`
    pub const SESSION_MCP_OAUTH_TRYREFRESHTOKENS: &str = "session.mcp.oauth.tryRefreshTokens";
    /// `session.mcp.oauth.saveStaticClientSecret`
    pub const SESSION_MCP_OAUTH_SAVESTATICCLIENTSECRET: &str =
        "session.mcp.oauth.saveStaticClientSecret";
    /// `session.mcp.oauth.disposeStore`
    pub const SESSION_MCP_OAUTH_DISPOSESTORE: &str = "session.mcp.oauth.disposeStore";
    /// `session.mcp.oauth.testConnection`
    pub const SESSION_MCP_OAUTH_TESTCONNECTION: &str = "session.mcp.oauth.testConnection";
    /// `session.mcp.oauth.storeKey`
    pub const SESSION_MCP_OAUTH_STOREKEY: &str = "session.mcp.oauth.storeKey";
    /// `session.eventLog.publish`
    pub const SESSION_EVENTLOG_PUBLISH: &str = "session.eventLog.publish";
    /// `session.remote.reattach`
    pub const SESSION_REMOTE_REATTACH: &str = "session.remote.reattach";
    /// `session.remote.readProjection`
    pub const SESSION_REMOTE_READPROJECTION: &str = "session.remote.readProjection";
    /// `session.remote.sendWithAdmission`
    pub const SESSION_REMOTE_SENDWITHADMISSION: &str = "session.remote.sendWithAdmission";
    /// `session.remote.publishState`
    pub const SESSION_REMOTE_PUBLISHSTATE: &str = "session.remote.publishState";
    /// `session.sandbox.invokeOwned`
    pub const SESSION_SANDBOX_INVOKEOWNED: &str = "session.sandbox.invokeOwned";
    /// `session.canvasSource.close`
    pub const SESSION_CANVASSOURCE_CLOSE: &str = "session.canvasSource.close";
    /// `session.cafeMetrics.attach`
    pub const SESSION_CAFEMETRICS_ATTACH: &str = "session.cafeMetrics.attach";
    /// `session.cafeMetrics.detach`
    pub const SESSION_CAFEMETRICS_DETACH: &str = "session.cafeMetrics.detach";
    /// `session.mcpSource.ideClient`
    pub const SESSION_MCPSOURCE_IDECLIENT: &str = "session.mcpSource.ideClient";
}

/// ACP transport configuration for an existing in-process SDK engine.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AcpServeRequest {
    /// The host application version.
    pub agent_version: String,
    /// The session options resolved by the embedding host.
    pub boot_options: Option<serde_json::Value>,
    /// The TCP interface, or absent for loopback.
    pub host: Option<String>,
    /// The authentication command arguments.
    pub login_args: Vec<String>,
    /// The executable an editor can use to authenticate.
    pub login_command: String,
    /// The TCP port, required unless stdio is selected.
    pub port: Option<i64>,
    /// The SDK engine that owns the calling connection.
    pub server_id: i64,
    /// Whether to serve the process's standard input and output.
    pub stdio: bool,
    /// Whether the SDK client supplies the ACP telemetry callback.
    pub telemetry: bool,
    /// An absolute attachment directory, or absent for the platform default.
    pub temp_dir: Option<String>,
}
crate::record_serde::wire_record!(AcpServeRequest { agent_version: "agentVersion", boot_options: "bootOptions" if is_none, host: "host" if is_none, login_args: "loginArgs", login_command: "loginCommand", port: "port" if is_none, server_id: "serverId", stdio: "stdio", telemetry: "telemetry", temp_dir: "tempDir" if is_none });

/// The models a custom agent asks for, and the models actually available.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentsCustomAgentInitialModelDecisionParams {
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
pub struct AgentsCustomAgentInitialModelDecisionResult {
    /// The reasoning effort attached to the selected model preference. Absent when that preference does not specify an effort.
    pub reasoning_effort: Option<String>,
    /// The first available model that matches the agent's preferences. Absent when none of the requested models is available.
    pub target_model: Option<String>,
    /// What to tell the user about an unmet preference. Absent when the preference was met. A warning with no `targetModel` means the agent's models are all unavailable.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(AgentsCustomAgentInitialModelDecisionResult { reasoning_effort: "reasoningEffort" if is_none, target_model: "targetModel" if is_none, warning: "warning" if is_none });

/// The feature flags to evaluate shipped agents against.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct AgentsGetAvailableBuiltinsRequest {
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
pub struct BuiltinAgentSummary {
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
pub struct AgentsGetAvailableBuiltinsResult {
    /// Available shipped agents, in the runtime's own order.
    pub agents: Vec<BuiltinAgentSummary>,
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
pub struct AgentsGetBuiltinDefinitionRequest {
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
pub struct AgentsGetBuiltinDefinitionResult {
    /// The agent's definition, serialized as JSON. It carries the authored keys plus the runtime's projected `__nativeCustomAgent` view of the same agent. It is a string rather than an object because the runtime parses it with the agent schema's tolerant shape, which accepts keys this contract does not name.
    pub definition_json: String,
}
crate::record_serde::wire_record!(AgentsGetBuiltinDefinitionResult {
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
pub struct AgentsGetBuiltinsResult {
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
    /// Mission Control service URL. Defaults to the original owner's COPILOT_MC_BASE_URL or GitHub API URL.
    pub base_url: Option<String>,
    /// Target environment identifier.
    pub environment_id: String,
    /// Outgoing AHP transport discriminator.
    pub kind: AhpConnectionTransportMissionControlKind,
}
crate::record_serde::wire_record!(AhpConnectionTransportMissionControl { base_url: "baseUrl" if is_none, environment_id: "environmentId", kind: "kind" });

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

/// Vision-specific limits
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesOverrideLimitsVision {
    /// Maximum image size in bytes
    pub max_prompt_image_size: Option<i64>,
    /// Maximum number of images per prompt
    pub max_prompt_images: Option<i64>,
    /// MIME types the model accepts
    pub supported_media_types: Option<Vec<String>>,
}
crate::record_serde::wire_record!(ModelCapabilitiesOverrideLimitsVision { max_prompt_image_size: "max_prompt_image_size" if is_none, max_prompt_images: "max_prompt_images" if is_none, supported_media_types: "supported_media_types" if is_none });

/// Token limits for prompts, outputs, and context window
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesOverrideLimits {
    /// Maximum total context window size in tokens
    pub max_context_window_tokens: Option<i64>,
    /// Maximum number of output/completion tokens
    pub max_output_tokens: Option<i64>,
    /// Maximum number of prompt/input tokens
    pub max_prompt_tokens: Option<i64>,
    /// Vision-specific limits
    pub vision: Option<ModelCapabilitiesOverrideLimitsVision>,
}
crate::record_serde::wire_record!(ModelCapabilitiesOverrideLimits { max_context_window_tokens: "max_context_window_tokens" if is_none, max_output_tokens: "max_output_tokens" if is_none, max_prompt_tokens: "max_prompt_tokens" if is_none, vision: "vision" if is_none });

/// Feature flags indicating what the model supports
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesOverrideSupports {
    /// Resolved Anthropic adaptive-thinking capability — unsupported / optional / required / adaptive_only. 'required' models reject thinking.type='enabled' with HTTP 400 but still accept 'disabled' (e.g. opus-4.7/4.8/5, sonnet-5); 'adaptive_only' models accept nothing but 'adaptive' (e.g. fable, mythos).
    pub adaptive_thinking: Option<AdaptiveThinkingSupport>,
    /// Whether this model supports reasoning effort configuration
    pub reasoning_effort: Option<bool>,
    /// Whether this model supports canonical tool calling
    pub tool_calls: Option<bool>,
    /// Whether this model supports vision/image input
    pub vision: Option<bool>,
}
crate::record_serde::wire_record!(ModelCapabilitiesOverrideSupports { adaptive_thinking: "adaptive_thinking" if is_none, reasoning_effort: "reasoningEffort" if is_none, tool_calls: "toolCalls" if is_none, vision: "vision" if is_none });

/// Optional capability overrides (vision, tool_calls, reasoning, etc.).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ModelCapabilitiesOverride {
    /// Token limits for prompts, outputs, and context window
    pub limits: Option<ModelCapabilitiesOverrideLimits>,
    /// Feature flags indicating what the model supports
    pub supports: Option<ModelCapabilitiesOverrideSupports>,
}
crate::record_serde::wire_record!(ModelCapabilitiesOverride { limits: "limits" if is_none, supports: "supports" if is_none });

/// A BYOK model definition referencing a named provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ProviderModelConfig {
    /// Forward-compatible persisted on-device Auto annotation. The live counterpart is metadata.autoLocal.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auto_local: Option<serde_json::Value>,
    /// Optional capability overrides (vision, tool_calls, reasoning, etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<ModelCapabilitiesOverride>,
    /// Provider-local model id, unique within its provider. The session-wide selection id (shown in the model list and passed to switchTo) is the provider-qualified `provider/id`.
    pub id: String,
    /// Maximum context window tokens for the model.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(
        default,
        deserialize_with = "ProviderModelConfig::deserialize_token_limit"
    )]
    pub max_context_window_tokens: Option<i64>,
    /// Maximum output tokens for the model.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(
        default,
        deserialize_with = "ProviderModelConfig::deserialize_token_limit"
    )]
    pub max_output_tokens: Option<i64>,
    /// Maximum prompt/input tokens for the model.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(
        default,
        deserialize_with = "ProviderModelConfig::deserialize_token_limit"
    )]
    pub max_prompt_tokens: Option<i64>,
    /// Provider-published model metadata, preserved verbatim as the public Model.metadata object.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
    /// Well-known base model id used for behavior/capability/config lookup. Defaults to `id`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model_id: Option<String>,
    /// Display name for model pickers. Defaults to the provider-qualified selection id (`provider/id`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Name of the configured provider that serves this model.
    pub provider: String,
    /// System-message configuration used when the runtime builds the standard prompt for this provider-qualified model, including general-purpose subagents. It uses the same object hierarchy as session-level systemMessage configuration, except transform actions are rejected because the current callback protocol is not model-scoped. When present, it overrides the session-wide configuration on those prompt paths. Selected custom-agent and specialized-subagent prompts remain authoritative.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system_message: Option<serde_json::Value>,
    /// The model name sent to the provider API for inference. Defaults to `id`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub wire_model: Option<String>,
}

impl ProviderModelConfig {
    fn deserialize_token_limit<'de, D>(deserializer: D) -> Result<Option<i64>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let Some(number) = Option::<serde_json::Number>::deserialize(deserializer)? else {
            return Ok(None);
        };
        if let Some(value) = number.as_i64() {
            return Ok(Some(value));
        }
        if let Some(value) = number.as_f64()
            && value.fract() == 0.0
            // i64::MAX rounds up to 2^63 as f64, so the upper bound is exclusive.
            && ((i64::MIN as f64)..-(i64::MIN as f64)).contains(&value)
        {
            return Ok(Some(value as i64));
        }
        Err(serde::de::Error::custom(
            "expected an integer token limit in the i64 range",
        ))
    }
}

/// Azure-specific provider options.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProviderConfigAzure {
    /// API version. When set, uses the versioned deployment route. When omitted, uses the GA versionless v1 route.
    pub api_version: Option<String>,
}
crate::record_serde::wire_record!(ProviderConfigAzure { api_version: "apiVersion" if is_none });

/// Editable provider settings, without credential values or custom headers.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SavedProviderConfigurationProviderInput {
    /// Safe Azure connection settings, including API version.
    pub azure: Option<ProviderConfigAzure>,
    /// Provider endpoint.
    pub base_url: String,
    /// Non-secret local model runtime options, such as the WinML device.
    pub local_model: Option<serde_json::Value>,
    /// Serving product used for attribution.
    pub model_provider: Option<ProviderConfigModelProvider>,
    /// Provider name used to qualify model selection ids.
    pub name: String,
    /// HTTP or WebSocket transport.
    pub transport: Option<ProviderConfigTransport>,
    /// Provider protocol family.
    pub r#type: Option<SavedProviderConfigurationType>,
    /// OpenAI-compatible wire API.
    pub wire_api: Option<ProviderConfigWireApi>,
}
crate::record_serde::wire_record!(SavedProviderConfigurationProviderInput { azure: "azure" if is_none, base_url: "baseUrl", local_model: "localModel" if is_none, model_provider: "modelProvider" if is_none, name: "name", transport: "transport" if is_none, r#type: "type" if is_none, wire_api: "wireApi" if is_none });

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

/// Existing initial interactive CLI readiness observation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CafeMetricsRecordCliReadyRequest {
    /// Existing startup_duration_ms, without rounding or a new clock. Absent when unavailable.
    pub duration_ms: Option<f64>,
    /// Intent captured before the first interactive mount.
    pub launch_scenario: CafeMetricsLaunchScenario,
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
pub struct CafeMetricsRecordResumeResultRequest {
    /// Terminal outcome.
    pub outcome: CafeMetricsResumeOutcome,
    /// Bounded result classification.
    pub reason: CafeMetricsResumeReason,
    /// Whether an event-log recovery projection was used.
    pub recovered: bool,
    /// Last phase reached.
    pub stage: CafeMetricsResumeStage,
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
pub struct CafeMetricsRelease {
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
    pub releases: Vec<CafeMetricsRelease>,
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

/// Versions and provider configuration for CAFE.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CafeMetricsStartRequest {
    /// Effective comparison version and appVersion tag.
    pub app_version: String,
    /// Raw package build version.
    pub build_version: String,
    /// Whether the host uses a custom model provider.
    pub custom_provider_configured: bool,
    /// Embedded package identity. Omission enables the unbundled development fallback.
    pub embedded_package_identity: Option<CafeMetricsEmbeddedPackageIdentity>,
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
pub struct CafeMetricsStartResult {
    /// Refusal reason, absent on success.
    pub reason: Option<CafeMetricsStartRefusal>,
    /// Whether this call started CAFE.
    pub started: bool,
}
crate::record_serde::wire_record!(CafeMetricsStartResult { reason: "reason" if is_none, started: "started" });

/// Result of configuring GitHub MCP.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigureGitHubResult {
    /// Whether GitHub MCP configuration changed.
    pub changed: bool,
}
crate::record_serde::wire_record!(McpConfigureGitHubResult { changed: "changed" });

/// Operation-specific reply or a complete live-client callback frame.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpExternalClientResult {
    /// Operation-specific reply or callback frame. A null next value closes the channel.
    /// Memory opens return bridgeId, completion returns registered, and next returns
    /// {"kind":"memory","frameJson":"..."} for each complete client-to-server packet.
    pub value: serde_json::Value,
}
crate::record_serde::wire_record!(McpExternalClientResult { value: "value" });

/// A caller-owned client's serializable MCP advertisement.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpExternalClientSnapshot {
    /// Original server configuration for policy and tool selection.
    pub config: Option<serde_json::Value>,
    /// Initialize instructions from the caller-owned MCP server.
    pub instructions: Option<String>,
    /// Backing server capabilities, not client capabilities.
    pub server_capabilities: Option<serde_json::Value>,
    /// MCP server name.
    pub server_name: String,
    /// MCP server version.
    pub server_version: String,
    /// Complete MCP tool descriptors, without executable JavaScript.
    pub tools: Vec<serde_json::Value>,
}
crate::record_serde::wire_record!(McpExternalClientSnapshot { config: "config" if is_none, instructions: "instructions" if is_none, server_capabilities: "serverCapabilities" if is_none, server_name: "serverName", server_version: "serverVersion", tools: "tools" });

/// Result of an original-host IDE operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpIdeClientResult {
    /// Whether this operation changed or addressed the current registration.
    pub applied: bool,
    /// Opaque token returned only after successful registration.
    pub token: Option<String>,
}
crate::record_serde::wire_record!(McpIdeClientResult { applied: "applied", token: "token" if is_none });

/// Serializable snapshot of a host-owned IDE client.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpIdeClientSnapshot {
    /// Actual backing-server capabilities as JSON.
    pub capabilities_json: String,
    /// Serializable configuration, without live client or transport objects.
    pub config_json: Option<String>,
    /// Logical server name.
    pub server_name: String,
    /// Backing server version.
    pub server_version: String,
    /// Complete MCP tool descriptors as JSON.
    pub tools_json: String,
}
crate::record_serde::wire_record!(McpIdeClientSnapshot { capabilities_json: "capabilitiesJson", config_json: "configJson" if is_none, server_name: "serverName", server_version: "serverVersion", tools_json: "toolsJson" });

/// Token projection for the existing host-delegated provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthAuthenticateResult {
    /// Access token from the runtime-owned store.
    pub access_token: String,
    /// Remaining lifetime in seconds when the issuer supplied an expiry.
    pub expires_in: Option<i64>,
    /// Token scheme. Defaults to Bearer.
    pub token_type: String,
}
crate::record_serde::wire_record!(McpOauthAuthenticateResult { access_token: "accessToken", expires_in: "expiresIn" if is_none, token_type: "tokenType" });

/// A pre-registered OAuth client for this attempt.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthFlowStaticClientConfig {
    /// Pre-registered client identifier.
    pub client_id: String,
    /// Ephemeral secret that the runtime never persists.
    pub client_secret: Option<String>,
    /// OAuth grant override.
    pub grant_type: Option<McpOauthLoginGrantType>,
    /// Whether this client has no secret. Defaults to true.
    pub public_client: Option<bool>,
    /// Scope used when the server omits its challenge scope.
    pub scope: Option<String>,
}
crate::record_serde::wire_record!(McpOauthFlowStaticClientConfig { client_id: "clientId", client_secret: "clientSecret" if is_none, grant_type: "grantType" if is_none, public_client: "publicClient" if is_none, scope: "scope" if is_none });

/// Use the current session authorizer's store.
#[derive(Debug, Clone, Default)]
pub struct McpOauthFlowStoreSession {
    pub kind: McpOauthFlowStoreSessionKind,
}
crate::record_serde::wire_record!(McpOauthFlowStoreSession { kind: "kind" });

/// Retain an isolated in-memory store until disposal or requester detachment.
#[derive(Debug, Clone, Default)]
pub struct McpOauthFlowStoreMemory {
    pub kind: McpOauthFlowStoreMemoryKind,
    /// Identity chosen by the host, scoped to this session requester.
    pub store_id: String,
}
crate::record_serde::wire_record!(McpOauthFlowStoreMemory {
    kind: "kind",
    store_id: "storeId"
});

/// Use the host's configured profile directory.
#[derive(Debug, Clone, Default)]
pub struct McpOauthFlowStorePersistent {
    /// Profile directory containing the OAuth credential store.
    pub config_dir: String,
    pub kind: McpOauthFlowStorePersistentKind,
    /// Identity chosen by the host, scoped to this session requester.
    pub store_id: String,
    /// Original store's effective token session. An empty value selects unscoped storage.
    pub token_session_id: String,
}
crate::record_serde::wire_record!(McpOauthFlowStorePersistent {
    config_dir: "configDir",
    kind: "kind",
    store_id: "storeId",
    token_session_id: "tokenSessionId"
});

/// Call the original requester's OAuth store host effects.
#[derive(Debug, Clone, Default)]
pub struct McpOauthFlowStoreCallback {
    pub kind: McpOauthFlowStoreCallbackKind,
    /// Host-owned callback store identity.
    pub store_id: String,
}
crate::record_serde::wire_record!(McpOauthFlowStoreCallback {
    kind: "kind",
    store_id: "storeId"
});

/// The server accepted an unauthenticated initialize request.
#[derive(Debug, Clone, Default)]
pub struct McpOauthTestConnectionResultConnected {
    pub status: McpOauthTestConnectionResultConnectedStatus,
}
crate::record_serde::wire_record!(McpOauthTestConnectionResultConnected { status: "status" });

/// The server refused the initialize request with an authentication challenge.
#[derive(Debug, Clone, Default)]
pub struct McpOauthTestConnectionResultNeedsAuth {
    /// The transport's authentication error message.
    pub error: String,
    /// Exact refusing HTTP response, when the transport captured it.
    pub http_response: Option<McpOauthHttpResponse>,
    pub status: McpOauthTestConnectionResultNeedsAuthStatus,
    /// Raw WWW-Authenticate header, when the refusing response supplies one.
    pub www_authenticate_header: Option<String>,
    /// Parsed challenge parameters from the refusing HTTP response.
    pub www_authenticate_params: Option<McpOauthWWWAuthenticateParams>,
}
crate::record_serde::wire_record!(McpOauthTestConnectionResultNeedsAuth { error: "error", http_response: "httpResponse" if is_none, status: "status", www_authenticate_header: "wwwAuthenticateHeader" if is_none, www_authenticate_params: "wwwAuthenticateParams" if is_none });

/// The handshake failed for a reason other than authentication.
#[derive(Debug, Clone, Default)]
pub struct McpOauthTestConnectionResultFailed {
    /// Transport or session policy failure detail.
    pub error: String,
    pub status: McpOauthTestConnectionResultFailedStatus,
}
crate::record_serde::wire_record!(McpOauthTestConnectionResultFailed {
    error: "error",
    status: "status"
});

/// Who answered a prompt and how. A label saying a person answered (`source` `human_response`) gives the answer human authority for Assisted Permissions: for permission prompts it becomes `permission.completed.decisionSource`, and with `responseCapability` `interactive` it also covers `ask_user`, user-input and plan-review answers. The runtime accepts that claim only from the session's authenticated owning, non-extension connection, the in-process host, or a Mission Control answer the session owner wrote; any other answer settles without it. Other fields attribute the decision in telemetry.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PermissionDecisionContext {
    /// Disposition of the permission request as observed by the responding client.
    pub outcome: PermissionDecisionOutcome,
    /// Whether the responding client could ask a user interactively, was running headlessly, or had no response path. Omit when the client cannot determine this authoritatively.
    pub response_capability: Option<PermissionResponseCapability>,
    /// Controlled reason or actor responsible for the response.
    pub source: PermissionDecisionSource,
    /// Client surface that submitted the response.
    pub surface: PermissionDecisionSurface,
    /// Optional user-authored explanation of this decision, independent of approval scope. Collected only in restricted telemetry for accepted human responses to Assisted Permissions prompts. Never persisted in session history, sent to the agent or judge, or used to create approval rules. Existing rejection feedback is separate.
    pub user_feedback: Option<String>,
}
crate::record_serde::wire_record!(PermissionDecisionContext { outcome: "outcome", response_capability: "responseCapability" if is_none, source: "source", surface: "surface", user_feedback: "userFeedback" if is_none });

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

/// Existing result of the sandbox operation on its original transport owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxInvokeOwnedResult {
    /// Operation-specific result, including null for operations without a value.
    pub value: serde_json::Value,
}
crate::record_serde::wire_record!(SandboxInvokeOwnedResult { value: "value" });

/// One local RPC attachment, not the current route for a session ID.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxTransportOwnerLocalRpc {
    /// Identity of the original authenticated attachment.
    pub attach_id: i64,
    /// Original sandbox transport owner discriminator.
    pub kind: SandboxTransportOwnerLocalRpcKind,
}
crate::record_serde::wire_record!(SandboxTransportOwnerLocalRpc {
    attach_id: "attachId",
    kind: "kind"
});

/// One AHP connection and its runtime-bound session channel.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SandboxTransportOwnerAhp {
    /// Original sandbox transport owner discriminator.
    pub kind: SandboxTransportOwnerAhpKind,
    /// Identity of the original relay connection.
    pub relay_id: i64,
    /// Owner-side session channel bound to the local presentation session.
    pub session_uri: String,
}
crate::record_serde::wire_record!(SandboxTransportOwnerAhp {
    kind: "kind",
    relay_id: "relayId",
    session_uri: "sessionUri"
});

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

/// Runtime acceptance of a controller event.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionEventPublishResult {
    /// The accepted event's identity.
    pub event_id: String,
}
crate::record_serde::wire_record!(SessionEventPublishResult {
    event_id: "eventId"
});

/// Explicit host approval for account-authenticated MCP endpoints.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpGitHubAuthPrepareRequest {
    /// Additional MCP configuration before approved servers leave ordinary configuration.
    pub additional_config_json: String,
    /// Explicit server-name and exact-origin approvals from the host command line.
    pub specifications: Vec<String>,
}
crate::record_serde::wire_record!(McpGitHubAuthPrepareRequest {
    additional_config_json: "additionalConfigJson",
    specifications: "specifications"
});

/// Configuration after the original engine retains approved account-authenticated servers.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpGitHubAuthPrepareResult {
    /// Remaining ordinary MCP configuration. Approval grants never leave the engine.
    pub additional_config_json: String,
}
crate::record_serde::wire_record!(McpGitHubAuthPrepareResult {
    additional_config_json: "additionalConfigJson"
});

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

/// Result of adding a model to the saved provider configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ProvidersSourceAddModelResult {
    /// True when this operation created the provider row.
    pub provider_added: bool,
}
crate::record_serde::wire_record!(ProvidersSourceAddModelResult {
    provider_added: "providerAdded"
});

/// Preliminary flush or final disposal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CafeMetricsShutdownRequest {
    /// Send at most one batch without stopping recording. Omission means disposal.
    pub flush_only: Option<bool>,
}
crate::record_serde::wire_record!(CafeMetricsShutdownRequest { flush_only: "flushOnly" if is_none });

/// Configuration after the original engine retains approved account-authenticated servers.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSourcePrepareGitHubAuthResult {
    /// Remaining ordinary MCP configuration. Approval grants never leave the engine.
    pub additional_config_json: String,
}
crate::record_serde::wire_record!(McpSourcePrepareGitHubAuthResult {
    additional_config_json: "additionalConfigJson"
});

/// Object envelope for one trusted live-client transport operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpExternalClientExchangeParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// The operation and its serializable arguments.
    pub request: serde_json::Value,
}
crate::record_serde::wire_record!(SessionMcpExternalClientExchangeParams {
    session_id: "sessionId",
    request: "request"
});

/// Operation-specific reply or a complete live-client callback frame.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpExternalClientExchangeResult {
    /// Operation-specific reply or callback frame. A null next value closes the channel.
    /// Memory opens return bridgeId, completion returns registered, and next returns
    /// {"kind":"memory","frameJson":"..."} for each complete client-to-server packet.
    pub value: serde_json::Value,
}
crate::record_serde::wire_record!(SessionMcpExternalClientExchangeResult { value: "value" });

/// Credential-free authentication identity used to configure GitHub MCP.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigureGitHubRequest {
    /// Target session identifier
    pub session_id: SessionId,
    /// Credential-free authentication identity for GitHub MCP configuration. The field retains its legacy opaque schema for compatibility.
    pub auth_info: serde_json::Value,
}
crate::record_serde::wire_record!(McpConfigureGitHubRequest {
    session_id: "sessionId",
    auth_info: "authInfo"
});

/// Result of configuring GitHub MCP.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpConfigureGitHubResult {
    /// Whether GitHub MCP configuration changed.
    pub changed: bool,
}
crate::record_serde::wire_record!(SessionMcpConfigureGitHubResult { changed: "changed" });

/// Correlation identity reserved before an effectful authentication request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpOauthPrepareFlowParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Host-chosen identity, unique within this session requester.
    pub flow_id: String,
}
crate::record_serde::wire_record!(SessionMcpOauthPrepareFlowParams {
    session_id: "sessionId",
    flow_id: "flowId"
});

/// Correlation identity reserved before an effectful authentication request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpOauthCancelFlowParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Host-chosen identity, unique within this session requester.
    pub flow_id: String,
}
crate::record_serde::wire_record!(SessionMcpOauthCancelFlowParams {
    session_id: "sessionId",
    flow_id: "flowId"
});

/// Full host-driven authentication for an arbitrary, possibly unsaved, MCP URL.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionMcpOauthAuthenticateParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Correlation identity reserved through prepareFlow.
    pub flow_id: String,
    /// MCP endpoint, without a requirement for saved configuration.
    pub server_url: String,
    /// Use environment session claims for a hostless compatibility call, while retaining this requester's egress authority.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sessionless: Option<bool>,
    /// Credential owner. Omission uses the authoritative session store.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub store: Option<McpOauthFlowStore>,
    /// Proxy fallback when the session has not published an egress entry.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sandbox_proxy: Option<String>,
    /// Static OAuth client settings for this attempt.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub static_client_config: Option<McpOauthFlowStaticClientConfig>,
    /// Bypass cached grants for explicit account selection.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub force_reauth: Option<bool>,
    /// Fail with browser_required when silent authentication cannot complete.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub browserless: Option<bool>,
    /// Start device authorization instead of a browser flow.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub force_device_code: Option<bool>,
    /// Let the host present the authorization URL without a runtime browser launch.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub skip_browser_open: Option<bool>,
    /// Authorization timeout in milliseconds. Defaults to five minutes.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub authorization_timeout_ms: Option<i64>,
    /// Override the device authorization poll interval in milliseconds.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub device_code_poll_interval_ms: Option<i64>,
    /// Delay before the runtime offers device authorization.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub device_code_offer_timeout_ms: Option<i64>,
    /// Display name for new dynamic client registrations.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_name: Option<String>,
    /// Client metadata document URL. Omission uses the session identity.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_id_metadata_url: Option<String>,
    /// Text displayed after a successful loopback callback.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub callback_success_message: Option<String>,
    /// Fixed callback port. A busy port fails without fallback.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub redirect_port: Option<i64>,
    /// Reject the cached access token but permit a refresh grant.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub skip_cached_access_token: Option<bool>,
    /// Preserve previously granted scopes during additional consent.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_existing_token_scopes: Option<bool>,
    /// Preserve replacement policy when another pending flow owns consent.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub preserve_pending_flow_replacement_flags: Option<bool>,
    /// Challenge captured by the host, including an empty challenge.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub www_authenticate_params: Option<McpOauthWWWAuthenticateParams>,
    /// Raw protected-resource metadata already fetched by the host.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resource_metadata: Option<String>,
}

/// Token projection for the existing host-delegated provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpOauthAuthenticateResult {
    /// Access token from the runtime-owned store.
    pub access_token: String,
    /// Remaining lifetime in seconds when the issuer supplied an expiry.
    pub expires_in: Option<i64>,
    /// Token scheme. Defaults to Bearer.
    pub token_type: String,
}
crate::record_serde::wire_record!(SessionMcpOauthAuthenticateResult { access_token: "accessToken", expires_in: "expiresIn" if is_none, token_type: "tokenType" });

/// A narrow token-validity query against the chosen credential owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpOauthHasValidTokensParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Runtime OAuth credential key.
    pub store_key: String,
    /// Preserve hostless credential and account selection.
    pub sessionless: Option<bool>,
    /// Credential owner. Omission uses the authoritative session store.
    pub store: Option<McpOauthFlowStore>,
    /// Static client identity required for refresh, when configured.
    pub static_client_config: Option<McpOauthFlowStaticClientConfig>,
}
crate::record_serde::wire_record!(SessionMcpOauthHasValidTokensParams { session_id: "sessionId", store_key: "storeKey", sessionless: "sessionless" if is_none, store: "store" if is_none, static_client_config: "staticClientConfig" if is_none });

/// A narrow token-validity query against the chosen credential owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpOauthTryRefreshTokensParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Runtime OAuth credential key.
    pub store_key: String,
    /// Preserve hostless credential and account selection.
    pub sessionless: Option<bool>,
    /// Credential owner. Omission uses the authoritative session store.
    pub store: Option<McpOauthFlowStore>,
    /// Static client identity required for refresh, when configured.
    pub static_client_config: Option<McpOauthFlowStaticClientConfig>,
}
crate::record_serde::wire_record!(SessionMcpOauthTryRefreshTokensParams { session_id: "sessionId", store_key: "storeKey", sessionless: "sessionless" if is_none, store: "store" if is_none, static_client_config: "staticClientConfig" if is_none });

/// Persist a confidential client's static secret without exposing token-store CRUD.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpOauthSaveStaticClientSecretParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Runtime OAuth credential key.
    pub store_key: String,
    /// Preserve hostless credential and account selection.
    pub sessionless: Option<bool>,
    /// Secret to persist in the selected credential owner.
    pub secret: String,
    /// Credential owner. Omission uses the authoritative session store.
    pub store: Option<McpOauthFlowStore>,
}
crate::record_serde::wire_record!(SessionMcpOauthSaveStaticClientSecretParams { session_id: "sessionId", store_key: "storeKey", sessionless: "sessionless" if is_none, secret: "secret", store: "store" if is_none });

/// Dispose an explicit host store without altering the session authorizer's owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpOauthDisposeStoreParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Host-selected identity of the explicit store.
    pub store_id: String,
}
crate::record_serde::wire_record!(SessionMcpOauthDisposeStoreParams {
    session_id: "sessionId",
    store_id: "storeId"
});

/// Tokenless initialize handshake for a possibly unsaved remote MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpOauthTestConnectionParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Remote endpoint to initialize without an OAuth token.
    pub url: String,
    /// Transport. Omission selects Streamable HTTP.
    pub r#type: Option<McpOauthTestConnectionTransport>,
    /// Headers sent during the handshake.
    pub headers: Option<HashMap<String, String>>,
    /// Proxy fallback when no session egress entry exists.
    pub sandbox_proxy: Option<String>,
}
crate::record_serde::wire_record!(SessionMcpOauthTestConnectionParams { session_id: "sessionId", url: "url", r#type: "type" if is_none, headers: "headers" if is_none, sandbox_proxy: "sandboxProxy" if is_none });

/// Derive a credential key through the selected account and store owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpOauthStoreKeyParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// MCP endpoint whose credentials the key identifies.
    pub server_url: String,
    /// Static OAuth client identity, when configured.
    pub client_id: Option<String>,
    /// Metadata document identity when no static client is configured.
    pub client_id_metadata_url: Option<String>,
    /// Credential owner. Omission uses the authoritative session store.
    pub store: Option<McpOauthFlowStore>,
    /// Preserve hostless credential and account selection.
    pub sessionless: Option<bool>,
}
crate::record_serde::wire_record!(SessionMcpOauthStoreKeyParams { session_id: "sessionId", server_url: "serverUrl", client_id: "clientId" if is_none, client_id_metadata_url: "clientIdMetadataUrl" if is_none, store: "store" if is_none, sessionless: "sessionless" if is_none });

/// Publishes a controller event through the runtime-owned session log.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionEventLogPublishParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Controller-assigned UUID returned to its synchronous UI caller.
    pub event_id: String,
    /// Existing session event type.
    pub event_type: String,
    /// Serialized event payload.
    pub data_json: String,
    /// Whether this event belongs only to the live event stream.
    pub ephemeral: bool,
    /// Optional agent attribution.
    pub agent_id: Option<String>,
}
crate::record_serde::wire_record!(SessionEventLogPublishParams { session_id: "sessionId", event_id: "eventId", event_type: "eventType", data_json: "dataJson", ephemeral: "ephemeral", agent_id: "agentId" if is_none });

/// Runtime acceptance of a controller event.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionEventLogPublishResult {
    /// The accepted event's identity.
    pub event_id: String,
}
crate::record_serde::wire_record!(SessionEventLogPublishResult {
    event_id: "eventId"
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
pub struct SessionRemoteReattachParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Release or restore operation for the caller's attachment lease.
    pub phase: serde_json::Value,
}
crate::record_serde::wire_record!(SessionRemoteReattachParams {
    session_id: "sessionId",
    phase: "phase"
});

/// Runtime generation that fences the two subscription replacement phases.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionRemoteReattachResult {
    /// Generation released or restored by this operation.
    pub generation: i64,
}
crate::record_serde::wire_record!(SessionRemoteReattachResult {
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
pub struct SessionRemoteReadProjectionParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Last observed revision. An unchanged projection waits for an update.
    pub after_revision: Option<i64>,
}
crate::record_serde::wire_record!(SessionRemoteReadProjectionParams { session_id: "sessionId", after_revision: "afterRevision" if is_none });

/// A read-only host projection for clients with synchronous render paths.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionRemoteReadProjectionResult {
    /// Whether this attachment has closed.
    pub closed: bool,
    /// Monotonic revision within this attachment.
    pub revision: i64,
    /// Serialized collaboration, approval, and pending-request snapshot.
    pub snapshot_json: String,
}
crate::record_serde::wire_record!(SessionRemoteReadProjectionResult {
    closed: "closed",
    revision: "revision",
    snapshot_json: "snapshotJson"
});

/// One trusted CLI host's message and its invocation-local admission identity.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionRemoteSendWithAdmissionParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Opaque identity registered by the originating client before this request.
    pub admission_id: String,
    /// Serialized canonical SendRequest, including clientTurnId and all supplied message options.
    /// The runtime validates these options and owns their preparation and routing.
    pub request_json: String,
}
crate::record_serde::wire_record!(SessionRemoteSendWithAdmissionParams {
    session_id: "sessionId",
    admission_id: "admissionId",
    request_json: "requestJson"
});

/// Result of sending a user message
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionRemoteSendWithAdmissionResult {
    /// Unique identifier assigned to the message
    pub message_id: String,
}
crate::record_serde::wire_record!(SessionRemoteSendWithAdmissionResult {
    message_id: "messageId"
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
pub struct SessionRemotePublishStateParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Latest composer text, when the composer changes.
    pub draft_text: Option<String>,
    /// Serialized peer view, when the visible surfaces change.
    pub view_json: Option<String>,
}
crate::record_serde::wire_record!(SessionRemotePublishStateParams { session_id: "sessionId", draft_text: "draftText" if is_none, view_json: "viewJson" if is_none });

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
pub struct SessionSandboxInvokeOwnedParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// Transport selector; runtime authority and bindings still govern access.
    pub owner: SandboxTransportOwner,
    /// Typed operation and its existing request parameters.
    pub operation: serde_json::Value,
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
pub struct SessionSandboxInvokeOwnedResult {
    /// Operation-specific result, including null for operations without a value.
    pub value: serde_json::Value,
}
crate::record_serde::wire_record!(SessionSandboxInvokeOwnedResult { value: "value" });

/// Closes a canvas through its original owned or attached session namespace.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasSourceCloseRequest {
    /// Target session identifier
    pub session_id: SessionId,
    /// Open canvas instance identifier
    pub instance_id: String,
    /// Exact authenticated local attachment identifier. Omit for the SDK connection's owned session.
    pub attach_id: Option<i64>,
}
crate::record_serde::wire_record!(CanvasSourceCloseRequest { session_id: "sessionId", instance_id: "instanceId", attach_id: "attachId" if is_none });

/// Identifies the target session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionCafeMetricsAttachParams {
    /// Target session identifier
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(SessionCafeMetricsAttachParams {
    session_id: "sessionId"
});

/// Identifies the target session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionCafeMetricsDetachParams {
    /// Target session identifier
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(SessionCafeMetricsDetachParams {
    session_id: "sessionId"
});

/// Installs a callback identity before the graph starts the server.
#[derive(Debug, Clone, Default)]
pub struct McpIdeClientOperationRegister {
    /// Operation on the original host's IDE client.
    pub action: McpIdeClientOperationRegisterAction,
    /// Unique callback identity retained by the original SDK host.
    pub callback_id: String,
    /// The current backing-client snapshot.
    pub snapshot: McpIdeClientSnapshot,
}
crate::record_serde::wire_record!(McpIdeClientOperationRegister {
    action: "action",
    callback_id: "callbackId",
    snapshot: "snapshot"
});

/// Replaces descriptors only while the registration token matches.
#[derive(Debug, Clone, Default)]
pub struct McpIdeClientOperationRefresh {
    /// Operation on the original host's IDE client.
    pub action: McpIdeClientOperationRefreshAction,
    /// Unique callback identity of the installed registration.
    pub callback_id: String,
    /// The current backing-client snapshot.
    pub snapshot: McpIdeClientSnapshot,
    /// Opaque graph ownership token.
    pub token: String,
}
crate::record_serde::wire_record!(McpIdeClientOperationRefresh {
    action: "action",
    callback_id: "callbackId",
    snapshot: "snapshot",
    token: "token"
});

/// Stops the graph before the host drops its callback identity.
#[derive(Debug, Clone, Default)]
pub struct McpIdeClientOperationRemove {
    /// Operation on the original host's IDE client.
    pub action: McpIdeClientOperationRemoveAction,
    /// Logical server name.
    pub server_name: String,
    /// Opaque graph ownership token.
    pub token: String,
}
crate::record_serde::wire_record!(McpIdeClientOperationRemove {
    action: "action",
    server_name: "serverName",
    token: "token"
});

/// Invalidates the matching registration's resource or prompt projection.
#[derive(Debug, Clone, Default)]
pub struct McpIdeClientOperationListChanged {
    /// Operation on the original host's IDE client.
    pub action: McpIdeClientOperationListChangedAction,
    /// Resources or prompts.
    pub kind: String,
    /// Logical server name.
    pub server_name: String,
    /// Opaque graph ownership token.
    pub token: String,
}
crate::record_serde::wire_record!(McpIdeClientOperationListChanged {
    action: "action",
    kind: "kind",
    server_name: "serverName",
    token: "token"
});

/// Routes a backing client's notification under its registration token.
#[derive(Debug, Clone, Default)]
pub struct McpIdeClientOperationNotification {
    /// Operation on the original host's IDE client.
    pub action: McpIdeClientOperationNotificationAction,
    /// Notification method.
    pub method: String,
    /// Notification parameters as JSON.
    pub params_json: String,
    /// Logical server name.
    pub server_name: String,
    /// Opaque graph ownership token.
    pub token: String,
}
crate::record_serde::wire_record!(McpIdeClientOperationNotification {
    action: "action",
    method: "method",
    params_json: "paramsJson",
    server_name: "serverName",
    token: "token"
});

/// Sends progress only for this callback identity's active call.
#[derive(Debug, Clone, Default)]
pub struct McpIdeClientOperationProgress {
    /// Operation on the original host's IDE client.
    pub action: McpIdeClientOperationProgressAction,
    /// Callback identity that owns the active call.
    pub callback_id: String,
    /// Complete MCP progress notification as JSON.
    pub notification_json: String,
    /// Active tool-call correlation token.
    pub token: i64,
}
crate::record_serde::wire_record!(McpIdeClientOperationProgress {
    action: "action",
    callback_id: "callbackId",
    notification_json: "notificationJson",
    token: "token"
});

/// Original-host request for a session-owned IDE bridge.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionMcpSourceIdeClientParams {
    /// Target session identifier
    pub session_id: SessionId,
    /// The operation to apply to the original host's IDE bridge.
    pub operation: McpIdeClientOperation,
}

/// Result of an original-host IDE operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SessionMcpSourceIdeClientResult {
    /// Whether this operation changed or addressed the current registration.
    pub applied: bool,
    /// Opaque token returned only after successful registration.
    pub token: Option<String>,
}
crate::record_serde::wire_record!(SessionMcpSourceIdeClientResult { applied: "applied", token: "token" if is_none });

/// Authentication outcome for each protected resource.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type AhpConnectionsAuthenticateResult = Vec<AhpConnectionResourceAuthResult>;

/// Whether the credential operation succeeded.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SessionMcpOauthHasValidTokensResult = bool;

/// Whether the credential operation succeeded.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SessionMcpOauthTryRefreshTokensResult = bool;

/// The selected owner's OAuth credential key.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type SessionMcpOauthStoreKeyResult = String;

/// Resolved Anthropic adaptive-thinking capability for a model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum AdaptiveThinkingSupport {
    /// The model does not accept thinking.type='adaptive'
    #[serde(rename = "unsupported")]
    Unsupported,
    /// The model accepts adaptive thinking but also accepts thinking.type='enabled'
    #[serde(rename = "optional")]
    Optional,
    /// The model defaults to adaptive thinking and rejects thinking.type='enabled' with HTTP 400, but still accepts thinking.type='disabled' (e.g. opus-4.7/4.8/5, sonnet-5)
    #[serde(rename = "required")]
    Required,
    /// The model accepts only thinking.type='adaptive'; 'enabled', 'disabled', and an omitted thinking block all fail with HTTP 400 (e.g. fable, mythos)
    #[serde(rename = "adaptive_only")]
    AdaptiveOnly,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for AdaptiveThinkingSupport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "AdaptiveThinkingSupport",
            &[
                "unsupported",
                "optional",
                "required",
                "adaptive_only",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Unsupported,
            1 => Self::Optional,
            2 => Self::Required,
            3 => Self::AdaptiveOnly,
            _ => Self::Unknown,
        })
    }
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

/// The product serving the model, reported in telemetry as `model_provider`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderConfigModelProvider {
    /// OpenAI API.
    #[serde(rename = "openai")]
    Openai,
    /// Anthropic API.
    #[serde(rename = "anthropic")]
    Anthropic,
    /// Azure OpenAI Service.
    #[serde(rename = "azure_openai")]
    AzureOpenai,
    /// Ollama.
    #[serde(rename = "ollama")]
    Ollama,
    /// LM Studio.
    #[serde(rename = "lm_studio")]
    LmStudio,
    /// Foundry Local.
    #[serde(rename = "foundry_local")]
    FoundryLocal,
    /// llama.cpp server.
    #[serde(rename = "llama_cpp")]
    LlamaCpp,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderConfigModelProvider {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderConfigModelProvider",
            &[
                "openai",
                "anthropic",
                "azure_openai",
                "ollama",
                "lm_studio",
                "foundry_local",
                "llama_cpp",
                "Unknown",
            ],
            Some(7),
        )?;
        Ok(match index {
            0 => Self::Openai,
            1 => Self::Anthropic,
            2 => Self::AzureOpenai,
            3 => Self::Ollama,
            4 => Self::LmStudio,
            5 => Self::FoundryLocal,
            6 => Self::LlamaCpp,
            _ => Self::Unknown,
        })
    }
}

/// Provider transport. Defaults to "http".
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderConfigTransport {
    /// HTTP request/streaming transport.
    #[serde(rename = "http")]
    Http,
    /// WebSocket transport.
    #[serde(rename = "websockets")]
    Websockets,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderConfigTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderConfigTransport",
            &["http", "websockets", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Http,
            1 => Self::Websockets,
            _ => Self::Unknown,
        })
    }
}

/// Provider families supported by saved configuration, including managed local models.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SavedProviderConfigurationType {
    /// Generic OpenAI-compatible API.
    #[serde(rename = "openai")]
    Openai,
    /// Azure OpenAI Service endpoint.
    #[serde(rename = "azure")]
    Azure,
    /// Anthropic API endpoint.
    #[serde(rename = "anthropic")]
    Anthropic,
    /// Experimental: Google Gemini API endpoint, served over the native Gemini Interactions API.
    #[serde(rename = "google")]
    Google,
    /// Runtime-managed local model server.
    #[serde(rename = "local_model")]
    LocalModel,
    /// Windows ML local model provider.
    #[serde(rename = "winml")]
    Winml,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SavedProviderConfigurationType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SavedProviderConfigurationType",
            &[
                "openai",
                "azure",
                "anthropic",
                "google",
                "local_model",
                "winml",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::Openai,
            1 => Self::Azure,
            2 => Self::Anthropic,
            3 => Self::Google,
            4 => Self::LocalModel,
            5 => Self::Winml,
            _ => Self::Unknown,
        })
    }
}

/// Wire API format (openai/azure only). Defaults to "completions".
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ProviderConfigWireApi {
    /// OpenAI Chat Completions wire format.
    #[serde(rename = "completions")]
    Completions,
    /// OpenAI Responses API wire format.
    #[serde(rename = "responses")]
    Responses,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for ProviderConfigWireApi {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "ProviderConfigWireApi",
            &["completions", "responses", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Completions,
            1 => Self::Responses,
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
pub enum CafeMetricsEmbeddedPackageIdentity {
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
pub enum CafeMetricsLaunchScenario {
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
pub enum CafeMetricsResumeOutcome {
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
pub enum CafeMetricsResumeReason {
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
pub enum CafeMetricsResumeStage {
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
pub enum CafeMetricsResolveAdmissionRequest {
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
pub enum CafeMetricsResolveAdmissionResult {
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
pub enum CafeMetricsStartRefusal {
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

/// OAuth grant type override for this login.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthLoginGrantType {
    /// Interactive browser-based OAuth flow using an authorization code, typically with PKCE.
    #[serde(rename = "authorization_code")]
    AuthorizationCode,
    /// Headless OAuth flow where a confidential client authenticates directly with a client secret.
    #[serde(rename = "client_credentials")]
    ClientCredentials,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpOauthLoginGrantType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthLoginGrantType",
            &["authorization_code", "client_credentials", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::AuthorizationCode,
            1 => Self::ClientCredentials,
            _ => Self::Unknown,
        })
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthFlowStoreSessionKind {
    #[serde(rename = "session")]
    #[default]
    Session,
}
impl<'de> Deserialize<'de> for McpOauthFlowStoreSessionKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthFlowStoreSessionKind",
            &["session"],
            None,
        )?;
        Ok(Self::Session)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthFlowStoreMemoryKind {
    #[serde(rename = "memory")]
    #[default]
    Memory,
}
impl<'de> Deserialize<'de> for McpOauthFlowStoreMemoryKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthFlowStoreMemoryKind",
            &["memory"],
            None,
        )?;
        Ok(Self::Memory)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthFlowStorePersistentKind {
    #[serde(rename = "persistent")]
    #[default]
    Persistent,
}
impl<'de> Deserialize<'de> for McpOauthFlowStorePersistentKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthFlowStorePersistentKind",
            &["persistent"],
            None,
        )?;
        Ok(Self::Persistent)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthFlowStoreCallbackKind {
    #[serde(rename = "callback")]
    #[default]
    Callback,
}
impl<'de> Deserialize<'de> for McpOauthFlowStoreCallbackKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthFlowStoreCallbackKind",
            &["callback"],
            None,
        )?;
        Ok(Self::Callback)
    }
}

/// Credential owner for a host-driven OAuth flow.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpOauthFlowStore {
    Session(McpOauthFlowStoreSession),
    Memory(McpOauthFlowStoreMemory),
    Persistent(McpOauthFlowStorePersistent),
    Callback(McpOauthFlowStoreCallback),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthTestConnectionResultConnectedStatus {
    #[serde(rename = "connected")]
    #[default]
    Connected,
}
impl<'de> Deserialize<'de> for McpOauthTestConnectionResultConnectedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthTestConnectionResultConnectedStatus",
            &["connected"],
            None,
        )?;
        Ok(Self::Connected)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthTestConnectionResultNeedsAuthStatus {
    #[serde(rename = "needs-auth")]
    #[default]
    NeedsAuth,
}
impl<'de> Deserialize<'de> for McpOauthTestConnectionResultNeedsAuthStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthTestConnectionResultNeedsAuthStatus",
            &["needs-auth"],
            None,
        )?;
        Ok(Self::NeedsAuth)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthTestConnectionResultFailedStatus {
    #[serde(rename = "failed")]
    #[default]
    Failed,
}
impl<'de> Deserialize<'de> for McpOauthTestConnectionResultFailedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthTestConnectionResultFailedStatus",
            &["failed"],
            None,
        )?;
        Ok(Self::Failed)
    }
}

/// Outcome of the tokenless handshake, with the original challenge when available.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpOauthTestConnectionResult {
    Connected(McpOauthTestConnectionResultConnected),
    NeedsAuth(McpOauthTestConnectionResultNeedsAuth),
    Failed(McpOauthTestConnectionResultFailed),
}

/// Remote protocol selected for the tokenless handshake.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthTestConnectionTransport {
    /// Streamable HTTP.
    #[serde(rename = "http")]
    Http,
    /// Legacy HTTP with server-sent events.
    #[serde(rename = "sse")]
    Sse,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpOauthTestConnectionTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthTestConnectionTransport",
            &["http", "sse", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Http,
            1 => Self::Sse,
            _ => Self::Unknown,
        })
    }
}

/// Disposition of a permission request as observed by the responding client.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionDecisionOutcome {
    /// The request was approved automatically without a new human decision.
    #[serde(rename = "auto_approved")]
    AutoApproved,
    /// The request was denied without an interactive user decision; source records why.
    #[serde(rename = "autopilot_denied")]
    AutopilotDenied,
    /// The response came from an interactive user prompt.
    #[serde(rename = "prompted_user")]
    PromptedUser,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionDecisionOutcome {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionDecisionOutcome",
            &[
                "auto_approved",
                "autopilot_denied",
                "prompted_user",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::AutoApproved,
            1 => Self::AutopilotDenied,
            2 => Self::PromptedUser,
            _ => Self::Unknown,
        })
    }
}

/// Response capability available to the client when it settled a permission request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionResponseCapability {
    /// The client could ask a user for this decision.
    #[serde(rename = "interactive")]
    Interactive,
    /// The client could return an automated response but could not ask a user.
    #[serde(rename = "headless")]
    Headless,
    /// The client had no response path available.
    #[serde(rename = "none")]
    None,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionResponseCapability {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionResponseCapability",
            &["interactive", "headless", "none", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Interactive,
            1 => Self::Headless,
            2 => Self::None,
            _ => Self::Unknown,
        })
    }
}

/// Client surface that submitted a permission response.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PermissionDecisionSurface {
    /// The interactive Copilot CLI terminal UI.
    #[serde(rename = "tui")]
    Tui,
    /// The non-interactive Copilot CLI prompt mode.
    #[serde(rename = "prompt_mode")]
    PromptMode,
    /// The Copilot App client.
    #[serde(rename = "copilot_app")]
    CopilotApp,
    /// An Agent Client Protocol host.
    #[serde(rename = "acp")]
    Acp,
    /// A generic Copilot SDK client.
    #[serde(rename = "sdk")]
    Sdk,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PermissionDecisionSurface {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PermissionDecisionSurface",
            &["tui", "prompt_mode", "copilot_app", "acp", "sdk", "Unknown"],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::Tui,
            1 => Self::PromptMode,
            2 => Self::CopilotApp,
            3 => Self::Acp,
            4 => Self::Sdk,
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

/// Original sandbox transport owner discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxTransportOwnerLocalRpcKind {
    #[serde(rename = "localRpc")]
    #[default]
    LocalRpc,
}
impl<'de> Deserialize<'de> for SandboxTransportOwnerLocalRpcKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxTransportOwnerLocalRpcKind",
            &["localRpc"],
            None,
        )?;
        Ok(Self::LocalRpc)
    }
}

/// Original sandbox transport owner discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SandboxTransportOwnerAhpKind {
    #[serde(rename = "ahp")]
    #[default]
    Ahp,
}
impl<'de> Deserialize<'de> for SandboxTransportOwnerAhpKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SandboxTransportOwnerAhpKind",
            &["ahp"],
            None,
        )?;
        Ok(Self::Ahp)
    }
}

/// Original authenticated transport for an owner-side sandbox operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SandboxTransportOwner {
    LocalRpc(SandboxTransportOwnerLocalRpc),
    Ahp(SandboxTransportOwnerAhp),
}

/// Operation on the original host's IDE client.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpIdeClientOperationRegisterAction {
    #[serde(rename = "register")]
    #[default]
    Register,
}
impl<'de> Deserialize<'de> for McpIdeClientOperationRegisterAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpIdeClientOperationRegisterAction",
            &["register"],
            None,
        )?;
        Ok(Self::Register)
    }
}

/// Operation on the original host's IDE client.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpIdeClientOperationRefreshAction {
    #[serde(rename = "refresh")]
    #[default]
    Refresh,
}
impl<'de> Deserialize<'de> for McpIdeClientOperationRefreshAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpIdeClientOperationRefreshAction",
            &["refresh"],
            None,
        )?;
        Ok(Self::Refresh)
    }
}

/// Operation on the original host's IDE client.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpIdeClientOperationRemoveAction {
    #[serde(rename = "remove")]
    #[default]
    Remove,
}
impl<'de> Deserialize<'de> for McpIdeClientOperationRemoveAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpIdeClientOperationRemoveAction",
            &["remove"],
            None,
        )?;
        Ok(Self::Remove)
    }
}

/// Operation on the original host's IDE client.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpIdeClientOperationListChangedAction {
    #[serde(rename = "listChanged")]
    #[default]
    ListChanged,
}
impl<'de> Deserialize<'de> for McpIdeClientOperationListChangedAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpIdeClientOperationListChangedAction",
            &["listChanged"],
            None,
        )?;
        Ok(Self::ListChanged)
    }
}

/// Operation on the original host's IDE client.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpIdeClientOperationNotificationAction {
    #[serde(rename = "notification")]
    #[default]
    Notification,
}
impl<'de> Deserialize<'de> for McpIdeClientOperationNotificationAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpIdeClientOperationNotificationAction",
            &["notification"],
            None,
        )?;
        Ok(Self::Notification)
    }
}

/// Operation on the original host's IDE client.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpIdeClientOperationProgressAction {
    #[serde(rename = "progress")]
    #[default]
    Progress,
}
impl<'de> Deserialize<'de> for McpIdeClientOperationProgressAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpIdeClientOperationProgressAction",
            &["progress"],
            None,
        )?;
        Ok(Self::Progress)
    }
}

/// Original-host operations for a session-owned IDE bridge.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpIdeClientOperation {
    Register(McpIdeClientOperationRegister),
    Refresh(McpIdeClientOperationRefresh),
    Remove(McpIdeClientOperationRemove),
    ListChanged(McpIdeClientOperationListChanged),
    Notification(McpIdeClientOperationNotification),
    Progress(McpIdeClientOperationProgress),
}
