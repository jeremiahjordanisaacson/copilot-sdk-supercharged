//! Wire-format structs for the `session.create` and `session.resume`
//! JSON-RPC payloads.
//!
//! Built explicitly from [`CreateSessionOptions`](crate::types::CreateSessionOptions) and
//! [`ResumeSessionOptions`](crate::types::ResumeSessionOptions) at
//! `Client::create_session` / `Client::resume_session` time via
//! [`CreateSessionOptions::into_wire`](crate::types::CreateSessionOptions::into_wire) and
//! [`ResumeSessionOptions::into_wire`](crate::types::ResumeSessionOptions::into_wire),
//! respectively.
//!
//! Keeping the wire shape separate from the user-facing config avoids
//! having callback fields on a serializable struct: the user-facing
//! configs hold trait-object handlers, the wire structs hold only the
//! plain data the runtime needs.

use std::collections::HashMap;
use std::path::PathBuf;

use indexmap::IndexMap;
use serde::Serialize;

use crate::canvas::CanvasDeclaration;
use crate::generated::api_types::{
    AutopilotOptions, ModelCapabilitiesOverride, OpenCanvasInstance, RemoteSessionMode,
    SandboxConfig,
};
use crate::generated::session_events::{PermissionMode, ReasoningSummary};
use crate::types::{
    AskUserVariant, CanvasProviderIdentity, CapiSessionOptions, CloudSessionOptions,
    CustomAgentConfig, DefaultAgentConfig, ExtensionInfo, GitHubMcpToolConfig,
    ImageGenerationConfig, InfiniteSessionConfig, LargeToolOutputConfig, McpServerConfig,
    MemoryConfiguration, NamedProviderConfig, ProviderModelConfig, SessionId, SessionLimitsConfig,
    SystemMessageConfig, Tool, ToolSearchConfig, WorkingDirectoryTrust,
};

/// Wire representation of a slash command (name + description only). The
/// runtime executes the command; the SDK's `CommandHandler` callback is
/// invoked from a separate dispatch path and never crosses the wire.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CommandWireDefinition {
    pub name: String,
    /// Always sent. The runtime requires a string, so a command declared
    /// without a description is serialized as `""` rather than omitted.
    pub description: String,
}

/// The exact JSON shape sent on the `session.create` JSON-RPC request.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionCreateWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_id: Option<SessionId>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system_message: Option<SystemMessageConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diagnostics: Option<crate::generated::api_types::DiagnosticsConfiguration>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lifecycle_diagnostics:
        Option<crate::generated::api_types::LifecycleDiagnosticsConfiguration>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub working_directory: Option<PathBuf>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub remote_session: Option<RemoteSessionMode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cloud: Option<CloudSessionOptions>,
    pub inference: SessionInferenceWire,
    pub policy: SessionPolicyWire,
    pub events: SessionEventWire,
    pub interaction: SessionInteractionWire,
    pub tools: SessionToolWire,
    pub canvases: SessionCreateCanvasWire,
    pub extensions: SessionExtensionWire,
    pub agents: SessionAgentWire,
    pub mcp: SessionMcpWire,
    pub state: SessionStateWire,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config_directory: Option<PathBuf>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_config_discovery: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extension_sources: Option<crate::types::ExtensionSources>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extension_paths: Option<Vec<PathBuf>>,
    pub filesystem: SessionCreateFilesystemWire,
    pub instructions: SessionCreateInstructionWire,
    pub hooks: SessionHookWire,
    pub git: SessionGitWire,
    pub skills: SessionSkillWire,
    pub permissions: SessionPermissionWire,
    pub github: SessionGitHubWire,
    pub plugins: SessionPluginWire,
    pub execution: SessionExecutionWire,
    pub experiments: SessionExperimentWire,
}

/// The exact JSON shape sent on the `session.resume` JSON-RPC request.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionResumeWire {
    pub session_id: SessionId,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system_message: Option<SystemMessageConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diagnostics: Option<crate::generated::api_types::DiagnosticsConfiguration>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lifecycle_diagnostics:
        Option<crate::generated::api_types::LifecycleDiagnosticsConfiguration>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub working_directory: Option<PathBuf>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub remote_session: Option<RemoteSessionMode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suppress_resume_event: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub continue_pending_work: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_transcript_recovery: Option<bool>,
    pub inference: SessionInferenceWire,
    pub policy: SessionPolicyWire,
    pub events: SessionEventWire,
    pub interaction: SessionInteractionWire,
    pub tools: SessionToolWire,
    pub canvases: SessionResumeCanvasWire,
    pub extensions: SessionExtensionWire,
    pub agents: SessionAgentWire,
    pub mcp: SessionMcpWire,
    pub state: SessionStateWire,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config_directory: Option<PathBuf>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_config_discovery: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extension_sources: Option<crate::types::ExtensionSources>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extension_paths: Option<Vec<PathBuf>>,
    pub instructions: SessionResumeInstructionWire,
    pub hooks: SessionHookWire,
    pub git: SessionGitWire,
    pub skills: SessionSkillWire,
    pub permissions: SessionPermissionWire,
    pub github: SessionGitHubWire,
    pub plugins: SessionPluginWire,
    pub filesystem: SessionResumeFilesystemWire,
    pub execution: SessionExecutionWire,
    pub experiments: SessionExperimentWire,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionInferenceWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_multiple_tool_calls: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning_effort: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning_summary: Option<ReasoningSummary>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context_tier: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_provider: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub providers: Option<Vec<NamedProviderConfig>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub models: Option<Vec<ProviderModelConfig>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_citations: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<ModelCapabilitiesOverride>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionPolicyWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allowed_models: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_managed_settings: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enforce_managed_model_defaults: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub managed_settings: Option<crate::types::ManagedSettings>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionEventWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub streaming: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_sub_agent_streaming_events: Option<bool>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionInteractionWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ask_user_variant: Option<AskUserVariant>,
    pub request_user_input: bool,
    pub request_exit_plan_mode: bool,
    pub request_auto_mode_switch: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_auto_mode_recovery: Option<bool>,
    pub request_elicitation: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_shell_authentication: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub commands: Option<Vec<CommandWireDefinition>>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionToolWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub definitions: Option<Vec<Tool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub available_tools: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_tools: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub large_output: Option<LargeToolOutputConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub search: Option<ToolSearchConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub image_generation: Option<ImageGenerationConfig>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionCreateCanvasWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub definitions: Option<Vec<CanvasDeclaration>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_renderer: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<CanvasProviderIdentity>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionExtensionWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sdk_path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub identity: Option<ExtensionInfo>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionAgentWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_builtin_agents: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub included_builtin_agents: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directories: Option<Vec<PathBuf>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub definitions: Option<Vec<CustomAgentConfig>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub local_discovery_only: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default: Option<DefaultAgentConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub selected: Option<String>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionMcpWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub servers: Option<IndexMap<String, McpServerConfig>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auth_client_id_metadata_url: Option<String>,
    pub env_value_mode: &'static str,
    pub enable_apps: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disabled_servers: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub oauth_token_storage: Option<String>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionStateWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub embedding_cache_storage: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub skip_embedding_retrieval: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_cross_session_store: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub has_memory_store: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub infinite_sessions: Option<InfiniteSessionConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memory: Option<MemoryConfiguration>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionCreateFilesystemWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trust_working_directory: Option<WorkingDirectoryTrust>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub track_file_changes: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_directories: Option<Vec<PathBuf>>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionCreateInstructionWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub organization_text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refresh_custom_instructions: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub on_demand_discovery: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directories: Option<Vec<PathBuf>>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionHookWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_repository_hooks: Option<bool>,
    pub has_handlers: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directories: Option<Vec<PathBuf>>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionGitWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_host_git_operations: Option<bool>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionSkillWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub has_provider: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directories: Option<Vec<PathBuf>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disabled_skills: Option<Vec<String>>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionPermissionWire {
    pub request: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sandbox: Option<SandboxConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<PermissionMode>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionGitHubWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mcp: Option<GitHubMcpToolConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capi: Option<CapiSessionOptions>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub telemetry_enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub token: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub token_provider_registration_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fetch_managed_settings: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub interactive_user: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub telemetry_forwarding: Option<bool>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionPluginWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directories: Option<Vec<PathBuf>>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionExecutionWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub limits: Option<SessionLimitsConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub autopilot: Option<AutopilotOptions>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionExperimentWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub flags: Option<HashMap<String, bool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub assignments: Option<crate::types::CopilotExpAssignmentResponse>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_experimental_mode: Option<bool>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionResumeCanvasWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub definitions: Option<Vec<CanvasDeclaration>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub open: Option<Vec<OpenCanvasInstance>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_renderer: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<CanvasProviderIdentity>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionResumeInstructionWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub organization_text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub on_demand_discovery: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directories: Option<Vec<PathBuf>>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SessionResumeFilesystemWire {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub track_file_changes: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_directories: Option<Vec<PathBuf>>,
}
