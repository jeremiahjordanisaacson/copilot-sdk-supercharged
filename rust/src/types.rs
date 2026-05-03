// Copyright (c) Microsoft Corporation. All rights reserved.

//! Type definitions for the Copilot SDK.
//!
//! This module contains all the data structures used to communicate with the
//! Copilot CLI server via JSON-RPC 2.0.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

// ============================================================================
// Connection State
// ============================================================================

/// Represents the connection state of the client.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionState {
    Disconnected,
    Connecting,
    Connected,
    Error,
}

// ============================================================================
// Tool Types
// ============================================================================

/// Result type for tool execution.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum ToolResultType {
    Success,
    Failure,
    Rejected,
    Denied,
}

/// Binary result from a tool execution.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolBinaryResult {
    pub data: String,
    pub mime_type: String,
    #[serde(rename = "type")]
    pub result_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

/// Structured tool result with metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolResultObject {
    pub text_result_for_llm: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub binary_results_for_llm: Option<Vec<ToolBinaryResult>>,
    pub result_type: ToolResultType,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_log: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_telemetry: Option<HashMap<String, serde_json::Value>>,
}

/// A tool result can be either a simple string or a structured object.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ToolResult {
    Text(String),
    Object(ToolResultObject),
}

/// Information about a tool invocation.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolInvocation {
    pub session_id: String,
    pub tool_call_id: String,
    pub tool_name: String,
    pub arguments: serde_json::Value,
}

/// Definition of a tool that can be exposed to the Copilot CLI.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolDefinition {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<serde_json::Value>,
}

/// Payload sent by the server when requesting a tool call.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolCallRequestPayload {
    pub session_id: String,
    pub tool_call_id: String,
    pub tool_name: String,
    pub arguments: serde_json::Value,
}

/// Response payload for a tool call.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolCallResponsePayload {
    pub result: ToolResultObject,
}

// ============================================================================
// System Message Configuration
// ============================================================================

/// Known system prompt section identifiers for the "customize" mode.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum SystemPromptSection {
    #[serde(rename = "identity")]
    Identity,
    #[serde(rename = "tone")]
    Tone,
    #[serde(rename = "tool_efficiency")]
    ToolEfficiency,
    #[serde(rename = "environment_context")]
    EnvironmentContext,
    #[serde(rename = "code_change_rules")]
    CodeChangeRules,
    #[serde(rename = "guidelines")]
    Guidelines,
    #[serde(rename = "safety")]
    Safety,
    #[serde(rename = "tool_instructions")]
    ToolInstructions,
    #[serde(rename = "custom_instructions")]
    CustomInstructions,
    #[serde(rename = "last_instructions")]
    LastInstructions,
}

/// Override action for a system prompt section.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum SectionOverrideAction {
    Replace,
    Remove,
    Append,
    Prepend,
}

/// Override operation for a single system prompt section.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SectionOverride {
    pub action: SectionOverrideAction,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,
}

/// System message configuration for session creation.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "mode", rename_all = "camelCase")]
pub enum SystemMessageConfig {
    /// Append mode: SDK foundation + optional custom content.
    #[serde(rename = "append")]
    Append {
        #[serde(skip_serializing_if = "Option::is_none")]
        content: Option<String>,
    },
    /// Replace mode: Full control, caller provides entire system message.
    #[serde(rename = "replace")]
    Replace { content: String },
    /// Customize mode: Override individual sections of the system prompt.
    #[serde(rename = "customize")]
    Customize {
        #[serde(skip_serializing_if = "Option::is_none")]
        sections: Option<HashMap<SystemPromptSection, SectionOverride>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        content: Option<String>,
    },
}

// ============================================================================
// Permission Types
// ============================================================================

/// Kind of permission being requested.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum PermissionKind {
    Shell,
    Write,
    Mcp,
    Read,
    Url,
}

/// Permission request from the server.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PermissionRequest {
    pub kind: PermissionKind,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_call_id: Option<String>,
    /// Additional fields from the server.
    #[serde(flatten)]
    pub extra: HashMap<String, serde_json::Value>,
}

/// Result of a permission request.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum PermissionResultKind {
    Approved,
    DeniedByRules,
    DeniedNoApprovalRuleAndCouldNotRequestFromUser,
    DeniedInteractivelyByUser,
}

/// Permission result returned by the handler.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PermissionRequestResult {
    pub kind: PermissionResultKind,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rules: Option<Vec<serde_json::Value>>,
}

// ============================================================================
// User Input Types
// ============================================================================

/// Request for user input from the agent (enables ask_user tool).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UserInputRequest {
    pub question: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub choices: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_freeform: Option<bool>,
}

/// Response to a user input request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UserInputResponse {
    pub answer: String,
    pub was_freeform: bool,
}

// ============================================================================
// Hook Types
// ============================================================================

/// Base fields common to all hook inputs.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BaseHookInput {
    pub timestamp: f64,
    pub cwd: String,
}

/// Input for pre-tool-use hook.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PreToolUseHookInput {
    pub timestamp: f64,
    pub cwd: String,
    pub tool_name: String,
    pub tool_args: serde_json::Value,
}

/// Output for pre-tool-use hook.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct PreToolUseHookOutput {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub permission_decision: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub permission_decision_reason: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub modified_args: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_context: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suppress_output: Option<bool>,
}

/// Input for post-tool-use hook.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PostToolUseHookInput {
    pub timestamp: f64,
    pub cwd: String,
    pub tool_name: String,
    pub tool_args: serde_json::Value,
    pub tool_result: ToolResultObject,
}

/// Output for post-tool-use hook.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct PostToolUseHookOutput {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub modified_result: Option<ToolResultObject>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_context: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suppress_output: Option<bool>,
}

/// Input for user-prompt-submitted hook.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserPromptSubmittedHookInput {
    pub timestamp: f64,
    pub cwd: String,
    pub prompt: String,
}

/// Output for user-prompt-submitted hook.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct UserPromptSubmittedHookOutput {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub modified_prompt: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_context: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suppress_output: Option<bool>,
}

/// Input for session-start hook.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionStartHookInput {
    pub timestamp: f64,
    pub cwd: String,
    pub source: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub initial_prompt: Option<String>,
}

/// Output for session-start hook.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct SessionStartHookOutput {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_context: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub modified_config: Option<HashMap<String, serde_json::Value>>,
}

/// Input for session-end hook.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionEndHookInput {
    pub timestamp: f64,
    pub cwd: String,
    pub reason: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub final_message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

/// Output for session-end hook.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct SessionEndHookOutput {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suppress_output: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cleanup_actions: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_summary: Option<String>,
}

/// Input for error-occurred hook.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ErrorOccurredHookInput {
    pub timestamp: f64,
    pub cwd: String,
    pub error: String,
    pub error_context: String,
    pub recoverable: bool,
}

/// Output for error-occurred hook.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ErrorOccurredHookOutput {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suppress_output: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error_handling: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub retry_count: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user_notification: Option<String>,
}

// ============================================================================
// MCP Server Configuration Types
// ============================================================================

/// Configuration for a local/stdio MCP server.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpLocalServerConfig {
    pub tools: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "type")]
    pub server_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeout: Option<u64>,
    pub command: String,
    pub args: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub env: Option<HashMap<String, String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cwd: Option<String>,
}

/// Configuration for a remote MCP server (HTTP or SSE).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpRemoteServerConfig {
    pub tools: Vec<String>,
    #[serde(rename = "type")]
    pub server_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeout: Option<u64>,
    pub url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub headers: Option<HashMap<String, String>>,
}

/// Union type for MCP server configurations.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpServerConfig {
    Local(McpLocalServerConfig),
    Remote(McpRemoteServerConfig),
}

// ============================================================================
// Custom Agent Configuration
// ============================================================================

/// Configuration for a custom agent.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CustomAgentConfig {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub display_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tools: Option<Vec<String>>,
    pub prompt: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mcp_servers: Option<HashMap<String, McpServerConfig>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub infer: Option<bool>,
    /// List of skill names to preload into this agent's context.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub skills: Option<Vec<String>>,
}

// ============================================================================
// Infinite Session Configuration
// ============================================================================

/// Configuration for infinite sessions with automatic context compaction.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InfiniteSessionConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub background_compaction_threshold: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub buffer_exhaustion_threshold: Option<f64>,
}

// ============================================================================
// Provider Configuration
// ============================================================================

/// Azure-specific provider options.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AzureProviderOptions {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_version: Option<String>,
}

/// Configuration for a custom API provider (BYOK).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ProviderConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "type")]
    pub provider_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub wire_api: Option<String>,
    pub base_url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_key: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bearer_token: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub azure: Option<AzureProviderOptions>,
}

// ============================================================================
// Reasoning Effort
// ============================================================================

/// Valid reasoning effort levels for models that support it.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum ReasoningEffort {
    Low,
    Medium,
    High,
    Xhigh,
}

// ============================================================================
// Session Configuration
// ============================================================================

/// Configuration for creating a session.
///
/// This struct holds the parameters sent to the server via `session.create`.
/// Tool handlers, permission handlers, user input handlers, and hooks
/// are registered separately on the Rust side and are not serialized.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning_effort: Option<ReasoningEffort>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config_dir: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tools: Option<Vec<ToolDefinition>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system_message: Option<SystemMessageConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub available_tools: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_tools: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<ProviderConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub working_directory: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub streaming: Option<bool>,
    /// Include sub-agent streaming events in the event stream. Defaults to true.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_sub_agent_streaming_events: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mcp_servers: Option<HashMap<String, McpServerConfig>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom_agents: Option<Vec<CustomAgentConfig>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub skill_directories: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disabled_skills: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub infinite_sessions: Option<InfiniteSessionConfig>,
    /// Per-property overrides for model capabilities, deep-merged over runtime defaults.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model_capabilities: Option<HashMap<String, serde_json::Value>>,
    /// When true, automatically discovers MCP server configurations from the working directory. Defaults to false.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_config_discovery: Option<bool>,
    /// GitHub token for authentication. When set on session config, overrides the client-level token for this session only.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub github_token: Option<String>,
    /// Set by the SDK based on whether handlers are registered (not user-set).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_permission: Option<bool>,
    /// Set by the SDK based on whether handlers are registered (not user-set).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_user_input: Option<bool>,
    /// Set by the SDK based on whether hooks are registered (not user-set).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hooks: Option<bool>,
    /// Slash commands registered for this session (not serialized).
    #[serde(skip)]
    pub commands: Option<Vec<CommandDefinition>>,
    /// Handler for elicitation requests from the server (not serialized).
    #[serde(skip)]
    pub on_elicitation_request: Option<ElicitationHandlerFn>,
}

// ============================================================================
// Commands
// ============================================================================

/// Context for a slash-command invocation.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CommandContext {
    pub session_id: String,
    pub command: String,
    pub command_name: String,
    pub args: String,
}

/// Clonable, debuggable wrapper for command handler functions.
#[derive(Clone)]
pub struct CommandHandlerFn(
    pub Arc<dyn Fn(CommandContext) -> Result<(), Box<dyn std::error::Error + Send + Sync>> + Send + Sync>,
);

impl std::fmt::Debug for CommandHandlerFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<command_handler>")
    }
}

/// Definition of a slash command registered with the session.
#[derive(Debug, Clone)]
pub struct CommandDefinition {
    /// Command name (without leading /).
    pub name: String,
    /// Human-readable description shown in command completion UI.
    pub description: Option<String>,
    /// Handler invoked when the command is executed.
    pub handler: CommandHandlerFn,
}

// ============================================================================
// UI Elicitation
// ============================================================================

/// Context for an elicitation request from the server.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ElicitationContext {
    pub session_id: String,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub requested_schema: Option<HashMap<String, serde_json::Value>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elicitation_source: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
}

/// Result returned from an elicitation handler.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ElicitationResult {
    /// User action: "accept", "decline", or "cancel".
    pub action: String,
    /// Form values submitted by the user (present when action is "accept").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<HashMap<String, serde_json::Value>>,
}

/// Clonable, debuggable wrapper for elicitation handler functions.
#[derive(Clone)]
pub struct ElicitationHandlerFn(
    pub Arc<dyn Fn(ElicitationContext) -> Result<ElicitationResult, Box<dyn std::error::Error + Send + Sync>> + Send + Sync>,
);

impl std::fmt::Debug for ElicitationHandlerFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<elicitation_handler>")
    }
}

/// Configuration for resuming a session.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ResumeSessionConfig {
    pub session_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reasoning_effort: Option<ReasoningEffort>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config_dir: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tools: Option<Vec<ToolDefinition>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system_message: Option<SystemMessageConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub available_tools: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_tools: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<ProviderConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub working_directory: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub streaming: Option<bool>,
    /// Include sub-agent streaming events in the event stream. Defaults to true.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_sub_agent_streaming_events: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mcp_servers: Option<HashMap<String, McpServerConfig>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom_agents: Option<Vec<CustomAgentConfig>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub skill_directories: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disabled_skills: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub infinite_sessions: Option<InfiniteSessionConfig>,
    /// Per-property overrides for model capabilities, deep-merged over runtime defaults.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model_capabilities: Option<HashMap<String, serde_json::Value>>,
    /// When true, automatically discovers MCP server configurations from the working directory. Defaults to false.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_config_discovery: Option<bool>,
    /// GitHub token for authentication. When set on session config, overrides the client-level token for this session only.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub github_token: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disable_resume: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_permission: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_user_input: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hooks: Option<bool>,
}

// ============================================================================
// Message Types
// ============================================================================

/// Attachment for a file.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FileAttachment {
    pub path: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub display_name: Option<String>,
}

/// Attachment for a directory.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DirectoryAttachment {
    pub path: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub display_name: Option<String>,
}

/// Selection range within a file.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SelectionRange {
    pub start: Position,
    pub end: Position,
}

/// A position in a file (line and character).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

/// Attachment for a code selection.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SelectionAttachment {
    pub file_path: String,
    pub display_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub selection: Option<SelectionRange>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,
}

/// Union type for message attachments.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum Attachment {
    File(FileAttachment),
    Directory(DirectoryAttachment),
    Selection(SelectionAttachment),
}

/// Response format for message responses.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ResponseFormat {
    #[serde(rename = "text")]
    Text,
    #[serde(rename = "image")]
    Image,
    #[serde(rename = "json_object")]
    JsonObject,
}

/// Options for image generation.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ImageOptions {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub size: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub quality: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub style: Option<String>,
}

/// Image data from an assistant image response.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AssistantImageData {
    pub format: String,
    pub base64: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub revised_prompt: Option<String>,
    pub width: u32,
    pub height: u32,
}

/// A content block in a mixed text+image response.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum ContentBlock {
    #[serde(rename = "text")]
    Text { text: String },
    #[serde(rename = "image")]
    Image { image: AssistantImageData },
}

/// Options for sending a message to a session.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MessageOptions {
    pub prompt: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attachments: Option<Vec<Attachment>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub response_format: Option<ResponseFormat>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub image_options: Option<ImageOptions>,
    /// Custom HTTP headers to include in outbound model requests for this turn.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_headers: Option<HashMap<String, String>>,
}

// ============================================================================
// Session Event Types
// ============================================================================

/// A session event received from the server.
///
/// Events are delivered via `session.event` JSON-RPC notifications.
/// The `event_type` field determines which variant's `data` is populated.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionEvent {
    pub id: String,
    pub timestamp: String,
    #[serde(rename = "parentId")]
    pub parent_id: Option<String>,
    #[serde(rename = "agentId")]
    pub agent_id: Option<String>,
    #[serde(default)]
    pub ephemeral: bool,
    #[serde(rename = "type")]
    pub event_type: String,
    /// The event data, varying by event_type. Use helper methods or
    /// deserialize into specific types based on event_type.
    pub data: serde_json::Value,
}

impl SessionEvent {
    /// Returns true if this is an `assistant.message` event.
    pub fn is_assistant_message(&self) -> bool {
        self.event_type == "assistant.message"
    }

    /// Returns true if this is a `session.idle` event.
    pub fn is_session_idle(&self) -> bool {
        self.event_type == "session.idle"
    }

    /// Returns true if this is a `session.error` event.
    pub fn is_session_error(&self) -> bool {
        self.event_type == "session.error"
    }

    /// Attempts to extract the assistant message content.
    /// Returns `None` if this is not an `assistant.message` event.
    pub fn assistant_message_content(&self) -> Option<&str> {
        if self.is_assistant_message() {
            self.data.get("content").and_then(|v| v.as_str())
        } else {
            None
        }
    }

    /// Attempts to extract the error message from a session.error event.
    pub fn error_message(&self) -> Option<&str> {
        if self.is_session_error() {
            self.data.get("message").and_then(|v| v.as_str())
        } else {
            None
        }
    }

    /// Attempts to extract the error stack from a session.error event.
    pub fn error_stack(&self) -> Option<&str> {
        if self.is_session_error() {
            self.data.get("stack").and_then(|v| v.as_str())
        } else {
            None
        }
    }
}

// ============================================================================
// Ping Response
// ============================================================================

/// Response from a ping request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PingResponse {
    pub message: String,
    pub timestamp: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub protocol_version: Option<u32>,
}

// ============================================================================
// Status Responses
// ============================================================================

/// Response from status.get.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GetStatusResponse {
    pub version: String,
    pub protocol_version: u32,
}

/// Response from auth.getStatus.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GetAuthStatusResponse {
    pub is_authenticated: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auth_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub host: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub login: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status_message: Option<String>,
}

// ============================================================================
// Model Types
// ============================================================================

/// Vision capabilities for a model.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VisionLimits {
    pub supported_media_types: Vec<String>,
    pub max_prompt_images: u32,
    pub max_prompt_image_size: u64,
}

/// Model capability limits.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ModelLimits {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_prompt_tokens: Option<u64>,
    pub max_context_window_tokens: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub vision: Option<VisionLimits>,
}

/// What a model supports.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ModelSupports {
    pub vision: bool,
    pub reasoning_effort: bool,
}

/// Model capabilities and limits.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelCapabilities {
    pub supports: ModelSupports,
    pub limits: ModelLimits,
}

/// Model policy state.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelPolicy {
    pub state: String,
    pub terms: String,
}

/// Model billing information.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelBilling {
    pub multiplier: f64,
}

/// Information about an available model.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ModelInfo {
    pub id: String,
    pub name: String,
    pub capabilities: ModelCapabilities,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub policy: Option<ModelPolicy>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub billing: Option<ModelBilling>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub supported_reasoning_efforts: Option<Vec<ReasoningEffort>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_reasoning_effort: Option<ReasoningEffort>,
}

// ============================================================================
// Session Metadata
// ============================================================================

/// Metadata about a session.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionMetadata {
    pub session_id: String,
    pub start_time: String,
    pub modified_time: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    pub is_remote: bool,
}

// ============================================================================
// Session Lifecycle Events
// ============================================================================

/// Types of session lifecycle events.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum SessionLifecycleEventType {
    #[serde(rename = "session.created")]
    SessionCreated,
    #[serde(rename = "session.deleted")]
    SessionDeleted,
    #[serde(rename = "session.updated")]
    SessionUpdated,
    #[serde(rename = "session.foreground")]
    SessionForeground,
    #[serde(rename = "session.background")]
    SessionBackground,
}

/// Metadata included in lifecycle events.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LifecycleMetadata {
    pub start_time: String,
    pub modified_time: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
}

/// Session lifecycle event notification.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionLifecycleEvent {
    #[serde(rename = "type")]
    pub event_type: SessionLifecycleEventType,
    pub session_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<LifecycleMetadata>,
}

// ============================================================================
// Foreground Session Info
// ============================================================================

/// Information about the foreground session in TUI+server mode.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ForegroundSessionInfo {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace_path: Option<String>,
}

// ============================================================================
// Session Filesystem Types
// ============================================================================

/// Configuration for a custom session filesystem provider.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionFsConfig {
    pub initial_cwd: String,
    pub session_state_path: String,
    pub conventions: String,
}

/// File metadata returned by session filesystem operations.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionFsFileInfo {
    pub name: String,
    pub size: i64,
    pub is_directory: bool,
    pub is_file: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub created_at: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub modified_at: Option<String>,
}

/// Interface for session filesystem providers.
///
/// Implementors provide file operations scoped to a session. Methods use
/// idiomatic Rust error handling: return an error for failures.
pub trait SessionFsProvider: Send + Sync {
    fn read_file(&self, session_id: &str, path: &str) -> Result<String, Box<dyn std::error::Error + Send + Sync>>;
    fn write_file(&self, session_id: &str, path: &str, content: &str) -> Result<(), Box<dyn std::error::Error + Send + Sync>>;
    fn append_file(&self, session_id: &str, path: &str, content: &str) -> Result<(), Box<dyn std::error::Error + Send + Sync>>;
    fn exists(&self, session_id: &str, path: &str) -> Result<bool, Box<dyn std::error::Error + Send + Sync>>;
    fn stat(&self, session_id: &str, path: &str) -> Result<SessionFsFileInfo, Box<dyn std::error::Error + Send + Sync>>;
    fn mkdir(&self, session_id: &str, path: &str, recursive: bool) -> Result<(), Box<dyn std::error::Error + Send + Sync>>;
    fn readdir(&self, session_id: &str, path: &str) -> Result<Vec<String>, Box<dyn std::error::Error + Send + Sync>>;
    fn readdir_with_types(&self, session_id: &str, path: &str) -> Result<Vec<SessionFsFileInfo>, Box<dyn std::error::Error + Send + Sync>>;
    fn rm(&self, session_id: &str, path: &str, recursive: bool) -> Result<(), Box<dyn std::error::Error + Send + Sync>>;
    fn rename(&self, session_id: &str, old_path: &str, new_path: &str) -> Result<(), Box<dyn std::error::Error + Send + Sync>>;
}

// ============================================================================
// Client Options
// ============================================================================

/// Options for creating a CopilotClient.
#[derive(Debug, Clone)]
pub struct CopilotClientOptions {
    /// Path to the CLI executable.
    pub cli_path: Option<String>,
    /// Extra arguments to pass to the CLI executable.
    pub cli_args: Vec<String>,
    /// Working directory for the CLI process.
    pub cwd: Option<String>,
    /// Port for the CLI server (TCP mode only).
    pub port: u16,
    /// Use stdio transport instead of TCP (default: true).
    pub use_stdio: bool,
    /// URL of an existing Copilot CLI server to connect to over TCP.
    pub cli_url: Option<String>,
    /// Log level for the CLI server.
    pub log_level: String,
    /// Auto-start the CLI server on first use (default: true).
    pub auto_start: bool,
    /// Auto-restart the CLI server if it crashes (default: true).
    pub auto_restart: bool,
    /// Environment variables to pass to the CLI process.
    pub env: Option<HashMap<String, String>>,
    /// GitHub token for authentication.
    pub github_token: Option<String>,
    /// Whether to use the logged-in user for authentication.
    pub use_logged_in_user: Option<bool>,
    /// Server-wide idle timeout for sessions in seconds. Sessions without activity for this duration are automatically cleaned up.
    pub session_idle_timeout_seconds: Option<u64>,
    /// Custom session filesystem configuration.
    pub session_fs: Option<SessionFsConfig>,
}

impl Default for CopilotClientOptions {
    fn default() -> Self {
        Self {
            cli_path: None,
            cli_args: Vec::new(),
            cwd: None,
            port: 0,
            use_stdio: true,
            cli_url: None,
            log_level: "info".to_string(),
            auto_start: true,
            auto_restart: true,
            env: None,
            github_token: None,
            use_logged_in_user: None,
            session_idle_timeout_seconds: None,
            session_fs: None,
        }
    }
}
