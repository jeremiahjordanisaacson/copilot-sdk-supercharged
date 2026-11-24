// Copyright (c) Microsoft Corporation. All rights reserved.

//! Type definitions for the Copilot SDK.
//!
//! This module contains all the data structures used to communicate with the
//! Copilot CLI server via JSON-RPC 2.0.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolDefinition {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<serde_json::Value>,
    /// Marks the tool as safe to retry: repeated calls with the same arguments
    /// have no additional side effects (`idempotent`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub idempotent: Option<bool>,
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
    /// Opaque context forwarded on permission replies.
    #[serde(rename = "decisionContext", skip_serializing_if = "Option::is_none")]
    pub decision_context: Option<serde_json::Value>,
}

// ============================================================================
// User Input Types
// ============================================================================

/// Request for user input from the agent (enables ask_user tool).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UserInputRequest {
    pub question: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub choices: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_freeform: Option<bool>,
    /// ID of the pending request (`requestId`), when the runtime sends it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_id: Option<String>,
    /// Image limits (`imageAttachmentConstraints`); `None` when the runtime does not
    /// accept image attachments in the answer.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub image_attachment_constraints: Option<UserInputImageAttachmentConstraints>,
    /// Opaque request metadata (`_meta`), forwarded unchanged.
    #[serde(rename = "_meta", skip_serializing_if = "Option::is_none")]
    pub meta: Option<serde_json::Value>,
}

/// Response to a user input request.
///
/// For an image-only answer, use an empty `answer` with `was_freeform: true`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UserInputResponse {
    pub answer: String,
    pub was_freeform: bool,
    /// Images sent with the answer (`attachments`), omitted from the response when `None`.
    /// Only valid when the request advertised `image_attachment_constraints`; the runtime
    /// enforces the limits.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub attachments: Option<Vec<UserInputImageAttachment>>,
}

/// Image limits a user input request advertises when the runtime accepts image
/// attachments in the answer (`imageAttachmentConstraints`).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct UserInputImageAttachmentConstraints {
    /// Accepted MIME types, for example `image/png`.
    pub allowed_mime_types: Vec<String>,
    /// Largest accepted image, in bytes.
    pub max_bytes_per_image: u64,
    /// Most images accepted in one answer.
    pub max_count: u64,
}

/// An inline image attached to a user input answer ([`UserInputResponse::attachments`]).
///
/// Serialized with `type: "image"`; `data` holds the base64-encoded image bytes.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename = "image", rename_all = "camelCase")]
pub struct UserInputImageAttachment {
    /// Base64-encoded image bytes.
    pub data: String,
    /// MIME type of the image, for example `image/png`.
    pub mime_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub display_name: Option<String>,
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

/// Input for user-prompt-transformed hook.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserPromptTransformedHookInput {
    pub timestamp: f64,
    pub cwd: String,
    pub prompt: String,
}

/// Output for user-prompt-transformed hook.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct UserPromptTransformedHookOutput {
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

/// Input for the sub-agent start hook (`subagentStart`), fired before a sub-agent's first turn.
///
/// The session metadata belongs to the parent session, not the child.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SubagentStartHookInput {
    pub timestamp: f64,
    pub cwd: String,
    pub transcript_path: String,
    pub agent_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_display_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_description: Option<String>,
}

/// Output for the sub-agent start hook. `additional_context` is prepended to the child's prompt.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct SubagentStartHookOutput {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_context: Option<String>,
}

/// Input for the sub-agent stop hook (`subagentStop`), fired after a sub-agent completes a turn.
///
/// `response` is the child's last assistant message before any hook rewrite.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SubagentStopHookInput {
    pub timestamp: f64,
    pub cwd: String,
    pub transcript_path: String,
    pub agent_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_display_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_id: Option<String>,
    pub agent_type: String,
    /// Why the sub-agent stopped (currently always `"end_turn"`).
    pub stop_reason: String,
    pub response: String,
}

/// Output for the sub-agent stop hook.
///
/// `decision: "block"` with a non-empty `reason` continues the child; otherwise
/// `modified_response` replaces the response reported to the parent.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct SubagentStopHookOutput {
    /// `"block"` or `"allow"`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub decision: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub modified_response: Option<String>,
}

/// Hook type identifiers delivered to the hooks handler by `hooks.invoke`.
pub mod hook_types {
    pub const PRE_TOOL_USE: &str = "preToolUse";
    pub const POST_TOOL_USE: &str = "postToolUse";
    pub const PRE_MCP_TOOL_CALL: &str = "preMcpToolCall";
    pub const USER_PROMPT_SUBMITTED: &str = "userPromptSubmitted";
    pub const USER_PROMPT_TRANSFORMED: &str = "userPromptTransformed";
    pub const SESSION_START: &str = "sessionStart";
    pub const SESSION_END: &str = "sessionEnd";
    pub const ERROR_OCCURRED: &str = "errorOccurred";
    pub const SUBAGENT_START: &str = "subagentStart";
    pub const SUBAGENT_STOP: &str = "subagentStop";
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
    /// JSON schema declaring a factory's arguments (`argsSchema`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub args_schema: Option<serde_json::Value>,
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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
    /// Model provider identity (`modelProvider`): `"openai"`, `"anthropic"`, `"azure_openai"`,
    /// `"ollama"`, `"lm_studio"`, `"foundry_local"`, or `"llama_cpp"`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model_provider: Option<String>,
    /// Command whose stdout is used as the API key (`apiKeyCommand`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_key_command: Option<String>,
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
// v2.6.0 Session Option Types
// ============================================================================

/// Routing preference used when the session model is `auto` (`capi.autoTier`).
///
/// Experimental: part of the Auto routing surface and may change in a future release.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum AutoTier {
    Efficiency,
    Balance,
    Intelligence,
    Fast,
}

/// Which ask-user surface the runtime uses (`askUserVariant`).
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum AskUserVariant {
    Legacy,
    Elicitation,
}

/// How long the working directory is trusted (`trustWorkingDirectory`).
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum TrustWorkingDirectory {
    Session,
    User,
}

/// Sandbox configuration for the session (`sandboxConfig`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SandboxConfig {
    /// Whether sandboxing is enabled for the session.
    pub enabled: bool,
    /// User-managed sandbox policy fragment merged into the auto-discovered base policy.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user_policy: Option<serde_json::Value>,
    /// Auto-add the current working directory to the read/write paths (runtime default: true).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub add_current_working_directory: Option<bool>,
    /// Confine MCP servers launched by the session (runtime default: true).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sandbox_mcp_servers: Option<bool>,
    /// Confine language servers launched by the session (runtime default: true).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sandbox_lsp_servers: Option<bool>,
    /// Confine extension processes launched by the session (runtime default: true).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sandbox_extensions: Option<bool>,
    /// Opt in to letting the agent request that an individual command run outside the
    /// sandbox; the request goes through the normal permission flow (default: false).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_bypass: Option<bool>,
}

/// Image generation opt-in (`imageGeneration`). Not persisted across runtime restarts.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ImageGenerationConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    /// Request a visible "Made with AI" label on generated images.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub watermark: Option<bool>,
    /// Save generated images under the session-state `images` folder (runtime default: true).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub save_to_disk: Option<bool>,
}

/// Autopilot continuation options (`autopilot`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AutopilotOptions {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub continuation_enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_continues: Option<u32>,
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
    /// Directories containing instruction files for the session.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub instruction_directories: Option<Vec<String>>,
    /// Set by the SDK based on whether handlers are registered (not user-set).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_permission: Option<bool>,
    /// Set by the SDK based on whether handlers are registered (not user-set).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_user_input: Option<bool>,
    /// Set by the SDK based on whether hooks are registered (not user-set).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hooks: Option<bool>,

    // --- Upstream-sync session options (parity with @github/copilot-sdk) ---
    /// Enable inline source citations in assistant responses.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_citations: Option<bool>,
    /// Built-in agents to exclude from this session.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_builtin_agents: Option<Vec<String>>,
    /// Per-session spending limits (e.g. AI-credit budget).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_limits: Option<SessionLimitsConfig>,
    /// Opt-in persistent session memory configuration.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memory: Option<MemoryConfiguration>,
    /// OTLP telemetry export protocol (e.g. "grpc" or "http/protobuf").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub otlp_protocol: Option<String>,
    /// Enable the WebSocket transport for streamed responses.
    ///
    /// Sent inside the `capi` wire object (`capi.enableWebSocketResponses`).
    #[serde(skip_serializing)]
    pub enable_web_socket_responses: Option<bool>,
    /// Experiment (feature-flag) assignment overrides.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exp_assignments: Option<HashMap<String, serde_json::Value>>,
    /// Set by the SDK to signal a registered MCP OAuth host-token handler (not user-set).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mcp_auth_handler: Option<bool>,

    // --- Upstream-sync session options, 2026-08 batch (parity with @github/copilot-sdk) ---
    /// Enable session rewind to restore earlier session points (`rewindEnabled`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rewind_enabled: Option<bool>,
    /// Extra working directories available to the session (`additionalDirectories`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_directories: Option<Vec<String>>,
    /// Names of configured MCP servers to disable for this session (`disabledMcpServers`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disabled_mcp_servers: Option<Vec<String>>,
    /// Configuration for the built-in GitHub MCP tools (`githubMcpToolConfig`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub github_mcp_tool_config: Option<serde_json::Value>,
    /// Identity of the canvas provider to register (`canvasProvider`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub canvas_provider: Option<serde_json::Value>,
    /// Only load workspace-local custom agents (`customAgentsLocalOnly`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom_agents_local_only: Option<bool>,
    /// Tool search configuration for large tool sets (`toolSearch`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_search: Option<serde_json::Value>,
    /// Enable experimental runtime features (`experimentalMode`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub experimental_mode: Option<bool>,
    /// Respect repository content-exclusion rules (`contentExclusion`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content_exclusion: Option<bool>,

    // --- v2.6.0 session options (parity with @github/copilot-sdk) ---
    /// Auto-model routing preference, sent as `capi.autoTier` (experimental).
    #[serde(skip_serializing)]
    pub auto_tier: Option<AutoTier>,
    /// Ask-user surface: legacy tool or elicitation (`askUserVariant`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ask_user_variant: Option<AskUserVariant>,
    /// Sandbox configuration (`sandboxConfig`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sandbox_config: Option<SandboxConfig>,
    /// Runtime feature-flag overrides (`featureFlags`); sent whenever set, even if empty.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub feature_flags: Option<HashMap<String, bool>>,
    /// Restrict the models this session may use (`allowedModels`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allowed_models: Option<Vec<String>>,
    /// OAuth client-ID metadata document URL for MCP auth (`authClientIdMetadataUrl`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auth_client_id_metadata_url: Option<String>,
    /// Human-readable session name (`name`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Directories to load custom agents from (`agentDirectories`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_directories: Option<Vec<String>>,
    /// Directories to load file-based hooks from (`hookDirectories`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hook_directories: Option<Vec<String>>,
    /// Whether a human is interactively driving the session (`interactiveUser`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub interactive_user: Option<bool>,
    /// Managed settings to ignore for this session (`excludedManagedSettings`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_managed_settings: Option<Vec<String>>,
    /// Enforce managed model defaults (`enforceManagedModelDefaults`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enforce_managed_model_defaults: Option<bool>,
    /// Trust the working directory for this session or for the user (`trustWorkingDirectory`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trust_working_directory: Option<TrustWorkingDirectory>,
    /// Reload custom instructions when the session starts (`refreshCustomInstructions`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refresh_custom_instructions: Option<bool>,
    /// Image generation opt-in (`imageGeneration`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub image_generation: Option<ImageGenerationConfig>,
    /// Autopilot continuation options (`autopilot`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub autopilot: Option<AutopilotOptions>,

    // --- v3.0.0 session options (SDK protocol 4) ---
    /// Named BYOK provider connections (`inference.providers`). Select a registered
    /// model with `"<provider>/<id>"`. Replaces the legacy single `provider`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub providers: Option<Vec<NamedProviderConfig>>,
    /// Model definitions for the named providers (`inference.models`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub models: Option<Vec<ProviderModelConfig>>,
    /// Provider connection used for unqualified model names (`inference.defaultProvider`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_provider: Option<String>,
    /// Built-in Copilot API (CAPI) policy, sent as `github.capi` (for example
    /// `enabled: Some(false)` for a BYOK-only catalog).
    #[serde(skip_serializing)]
    pub capi: Option<CapiConfig>,
    /// Handler for shell authentication prompts (`sudo`, `ssh`, `git`) raised by commands the
    /// agent runs (not serialized). When set, the SDK sends
    /// `interaction.requestShellAuthentication` and answers `shellAuthentication.request`.
    #[serde(skip)]
    pub on_shell_authentication_request: Option<ShellAuthenticationHandlerFn>,
    /// Stores memories in the application instead of the built-in memory backends
    /// (not serialized). When set, the SDK sends `state.hasMemoryStore` and answers
    /// `memoryStore.store`.
    #[serde(skip)]
    pub store_memory: Option<MemoryStoreHandlerFn>,
    /// Per-session GitHub token provider (not serialized; mutually exclusive with `github_token`).
    ///
    /// The SDK registers it and sends `gitHubTokenProviderRegistrationId`; the runtime then
    /// calls back through `gitHubToken.getToken`.
    #[serde(skip)]
    pub github_token_provider: Option<GitHubTokenProviderFn>,

    /// Slash commands registered for this session (not serialized).
    #[serde(skip)]
    pub commands: Option<Vec<CommandDefinition>>,
    /// Handler for elicitation requests from the server (not serialized).
    #[serde(skip)]
    pub on_elicitation_request: Option<ElicitationHandlerFn>,
    /// Handler invoked when an MCP server requests an OAuth host token (not serialized).
    ///
    /// When present, the SDK sets the `mcpAuthHandler` wire flag on `session.create`.
    #[serde(skip)]
    pub on_mcp_auth_request: Option<McpAuthHandlerFn>,
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
    /// Directories containing instruction files for the session.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub instruction_directories: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disable_resume: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_permission: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_user_input: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hooks: Option<bool>,

    // --- v2.6.0 session options (parity with @github/copilot-sdk) ---
    /// Enable the WebSocket transport for streamed responses (`capi.enableWebSocketResponses`).
    #[serde(skip_serializing)]
    pub enable_web_socket_responses: Option<bool>,
    /// Auto-model routing preference, sent as `capi.autoTier` (experimental).
    #[serde(skip_serializing)]
    pub auto_tier: Option<AutoTier>,
    /// Ask-user surface: legacy tool or elicitation (`askUserVariant`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ask_user_variant: Option<AskUserVariant>,
    /// Sandbox configuration (`sandboxConfig`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sandbox_config: Option<SandboxConfig>,
    /// Runtime feature-flag overrides (`featureFlags`); sent whenever set, even if empty.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub feature_flags: Option<HashMap<String, bool>>,
    /// Restrict the models this session may use (`allowedModels`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allowed_models: Option<Vec<String>>,
    /// OAuth client-ID metadata document URL for MCP auth (`authClientIdMetadataUrl`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auth_client_id_metadata_url: Option<String>,
    /// Directories to load custom agents from (`agentDirectories`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_directories: Option<Vec<String>>,
    /// Directories to load file-based hooks from (`hookDirectories`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hook_directories: Option<Vec<String>>,
    /// Whether a human is interactively driving the session (`interactiveUser`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub interactive_user: Option<bool>,
    /// Managed settings to ignore for this session (`excludedManagedSettings`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_managed_settings: Option<Vec<String>>,
    /// Enforce managed model defaults (`enforceManagedModelDefaults`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enforce_managed_model_defaults: Option<bool>,
    /// Allow recovering the conversation from the transcript on resume (`allowTranscriptRecovery`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allow_transcript_recovery: Option<bool>,
    /// Image generation opt-in (`imageGeneration`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub image_generation: Option<ImageGenerationConfig>,
    /// Autopilot continuation options (`autopilot`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub autopilot: Option<AutopilotOptions>,

    // --- v3.0.0 session options (SDK protocol 4) ---
    /// Named BYOK provider connections (`inference.providers`). Re-supply them after a
    /// runtime restart: credentials are not persisted in session history.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub providers: Option<Vec<NamedProviderConfig>>,
    /// Model definitions for the named providers (`inference.models`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub models: Option<Vec<ProviderModelConfig>>,
    /// Default provider connection (`inference.defaultProvider`). `None` preserves the
    /// existing default, `Some(None)` sends `null` to clear it, and `Some(Some(name))` sets it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub default_provider: Option<Option<String>>,
    /// Built-in Copilot API (CAPI) policy, sent as `github.capi`. Omit to preserve the
    /// session's routing choice.
    #[serde(skip_serializing)]
    pub capi: Option<CapiConfig>,
    /// Handler for shell authentication prompts (not serialized). When set, the SDK sends
    /// `interaction.requestShellAuthentication` and answers `shellAuthentication.request`.
    #[serde(skip)]
    pub on_shell_authentication_request: Option<ShellAuthenticationHandlerFn>,
    /// Stores memories in the application (not serialized). When set, the SDK sends
    /// `state.hasMemoryStore` and answers `memoryStore.store`.
    #[serde(skip)]
    pub store_memory: Option<MemoryStoreHandlerFn>,
    /// Per-session GitHub token provider (not serialized; mutually exclusive with `github_token`).
    #[serde(skip)]
    pub github_token_provider: Option<GitHubTokenProviderFn>,
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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MessageOptions {
    pub prompt: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attachments: Option<Vec<Attachment>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<String>,
    /// Agent mode to run this turn under (e.g. a custom agent name).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_mode: Option<String>,
    /// Alternate prompt text to show in the transcript in place of `prompt`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub display_prompt: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub response_format: Option<ResponseFormat>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub image_options: Option<ImageOptions>,
    /// Custom HTTP headers to include in outbound model requests for this turn.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_headers: Option<HashMap<String, String>>,
    /// Origin of this message (`source`): user, system, or another agent.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<MessageSource>,
    /// JSON Schema for structured output. When set, `session.send` carries
    /// `responseFormat: { type: "json_schema", jsonSchema: { name, strict, schema } }`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub response_schema: Option<serde_json::Value>,
    /// Opaque caller-chosen identifier for this turn (`clientTurnId`), echoed on the
    /// resulting turn events so the caller can correlate them.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_turn_id: Option<String>,
}

impl From<&str> for MessageOptions {
    fn from(prompt: &str) -> Self {
        MessageOptions {
            prompt: prompt.to_string(),
            ..Default::default()
        }
    }
}

impl From<String> for MessageOptions {
    fn from(prompt: String) -> Self {
        MessageOptions {
            prompt,
            ..Default::default()
        }
    }
}

/// Origin of a message sent with `session.send` (wire field `source`).
///
/// Serialized as `"user"`, `"system"`, or `"agent-<id>"`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MessageSource {
    /// A message from the user (`"user"`).
    User,
    /// A system-generated message (`"system"`).
    System,
    /// A message produced by another agent (`"agent-<id>"`).
    Agent(String),
}

impl MessageSource {
    /// Builds an agent source (`"agent-<id>"`).
    pub fn agent(agent_id: impl Into<String>) -> Self {
        MessageSource::Agent(agent_id.into())
    }
}

impl std::fmt::Display for MessageSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MessageSource::User => f.write_str("user"),
            MessageSource::System => f.write_str("system"),
            MessageSource::Agent(id) => write!(f, "agent-{}", id),
        }
    }
}

impl Serialize for MessageSource {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for MessageSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = String::deserialize(deserializer)?;
        match value.as_str() {
            "user" => Ok(MessageSource::User),
            "system" => Ok(MessageSource::System),
            other => other
                .strip_prefix("agent-")
                .map(|id| MessageSource::Agent(id.to_string()))
                .ok_or_else(|| {
                    serde::de::Error::custom(format!("invalid message source: {}", other))
                }),
        }
    }
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

    /// Returns the live output chunk of a `tool.shell_output` event.
    /// Returns `None` for other event types or malformed data.
    pub fn shell_output(&self) -> Option<ToolShellOutputData> {
        if self.event_type == event_types::TOOL_SHELL_OUTPUT {
            serde_json::from_value(self.data.clone()).ok()
        } else {
            None
        }
    }
}

/// Session event type identifiers (`SessionEvent::event_type`).
///
/// Unknown event types are always delivered unchanged, so handlers keep working
/// when the runtime adds new events.
pub mod event_types {
    pub const ASSISTANT_MESSAGE: &str = "assistant.message";
    pub const ASSISTANT_MESSAGE_DELTA: &str = "assistant.message_delta";
    pub const SESSION_IDLE: &str = "session.idle";
    pub const SESSION_ERROR: &str = "session.error";
    pub const TOOL_EXECUTION_START: &str = "tool.execution_start";
    pub const TOOL_EXECUTION_COMPLETE: &str = "tool.execution_complete";
    /// Removed in SDK protocol 4; still delivered if an older runtime sends it.
    pub const TOOL_EXECUTION_PARTIAL_RESULT: &str = "tool.execution_partial_result";
    /// Live, append-only shell output (ephemeral; not replayed to late subscribers).
    pub const TOOL_SHELL_OUTPUT: &str = "tool.shell_output";
}

/// Data of a `tool.shell_output` event: one chunk of live shell output.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolShellOutputData {
    /// Tool call that owns this shell output.
    pub tool_call_id: String,
    /// New output to append (chunks need not contain complete lines).
    pub text: String,
    /// Zero-based publication sequence across all output streams of the tool call.
    pub sequence: u64,
    /// Output source: `"stdout"` (also when omitted), `"stderr"`, or `"terminal"`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub stream: Option<String>,
}

// ============================================================================
// Ping Response
// ============================================================================

/// Response from a ping request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PingResponse {
    pub message: String,
    /// Server time in milliseconds since the Unix epoch. Protocol 4 servers report an
    /// RFC 3339 string, which is converted (an unparseable value becomes `0.0`).
    #[serde(default, deserialize_with = "deserialize_ping_timestamp")]
    pub timestamp: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub protocol_version: Option<u32>,
}

/// Accepts the ping `timestamp` as epoch milliseconds (older servers) or as an RFC 3339
/// string (protocol 4 servers).
fn deserialize_ping_timestamp<'de, D>(deserializer: D) -> Result<f64, D::Error>
where
    D: serde::Deserializer<'de>,
{
    Ok(match serde_json::Value::deserialize(deserializer)? {
        serde_json::Value::Number(n) => n.as_f64().unwrap_or(0.0),
        serde_json::Value::String(s) => rfc3339_to_epoch_millis(&s).unwrap_or(0.0),
        _ => 0.0,
    })
}

/// Parses `YYYY-MM-DDTHH:MM:SS[.fraction](Z|+HH:MM|-HH:MM)` into milliseconds since the
/// Unix epoch.
fn rfc3339_to_epoch_millis(s: &str) -> Option<f64> {
    let bytes = s.as_bytes();
    if bytes.len() < 19
        || bytes[4] != b'-'
        || bytes[7] != b'-'
        || !matches!(bytes[10], b'T' | b't' | b' ')
        || bytes[13] != b':'
        || bytes[16] != b':'
    {
        return None;
    }
    let field = |range: std::ops::Range<usize>| -> Option<i64> { s.get(range)?.parse().ok() };
    let (year, month, day) = (field(0..4)?, field(5..7)?, field(8..10)?);
    let (hour, minute, second) = (field(11..13)?, field(14..16)?, field(17..19)?);
    if !(1..=12).contains(&month) || !(1..=31).contains(&day) {
        return None;
    }

    let mut rest = s.get(19..)?;
    let mut fraction = 0.0;
    if let Some(digits) = rest.strip_prefix('.') {
        let len = digits
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(digits.len());
        if len == 0 {
            return None;
        }
        fraction = format!("0.{}", &digits[..len]).parse::<f64>().ok()?;
        rest = &digits[len..];
    }
    let offset_minutes = match rest {
        "Z" | "z" => 0,
        _ => {
            let sign = match rest.as_bytes().first()? {
                b'+' => 1,
                b'-' => -1,
                _ => return None,
            };
            let hours: i64 = rest.get(1..3)?.parse().ok()?;
            let minutes: i64 = rest.get(4..6)?.parse().ok()?;
            sign * (hours * 60 + minutes)
        }
    };

    // Days since 1970-01-01 for the proleptic Gregorian calendar (Hinnant's days_from_civil).
    let y = if month <= 2 { year - 1 } else { year };
    let era = (if y >= 0 { y } else { y - 399 }) / 400;
    let year_of_era = y - era * 400;
    let day_of_year = (153 * ((month + 9) % 12) + 2) / 5 + day - 1;
    let day_of_era = year_of_era * 365 + year_of_era / 4 - year_of_era / 100 + day_of_year;
    let days = era * 146_097 + day_of_era - 719_468;

    let seconds = days * 86_400 + hour * 3_600 + minute * 60 + second - offset_minutes * 60;
    Some((seconds as f64 + fraction) * 1000.0)
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
    /// Maximum output tokens (wire key `max_output_tokens`, as in model capability overrides).
    #[serde(rename = "max_output_tokens", skip_serializing_if = "Option::is_none")]
    pub max_output_tokens: Option<u64>,
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
// Upstream-sync feature types (parity with @github/copilot-sdk)
// ============================================================================

/// Per-session AI-credit budget; set `max_ai_credits` to cap spend.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionLimitsConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_ai_credits: Option<f64>,
}

/// Opt-in persistent session memory configuration.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MemoryConfiguration {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
}

/// Arguments passed to a BYOK bearer-token provider / MCP auth handler (per-session scoping).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ProviderTokenArgs {
    pub session_id: String,
}

/// Tool "defer" loading policy: eager pre-load (`Never`) or lazy via search (`Auto`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ToolDefer {
    Auto,
    Never,
}

/// System-message section identifiers used with system-message section overrides.
///
/// The `Preamble` section targets only the identity preamble; the `Preserve` action
/// protects an individually-addressable section from a group-level remove.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SystemMessageSection {
    Preamble,
    Identity,
    ToolInstructions,
    Preserve,
}

/// GitHub-anchored attachment type identifiers.
pub mod github_attachment {
    pub const GITHUB_COMMIT: &str = "GitHubCommit";
    pub const GITHUB_RELEASE: &str = "GitHubRelease";
    pub const GITHUB_ACTIONS_JOB: &str = "GitHubActionsJob";
    pub const GITHUB_REPOSITORY: &str = "GitHubRepository";
    pub const GITHUB_FILE_DIFF: &str = "GitHubFileDiff";
    pub const GITHUB_TREE_COMPARISON: &str = "GitHubTreeComparison";
    pub const GITHUB_URL: &str = "GitHubUrl";
    pub const GITHUB_FILE: &str = "GitHubFile";
    pub const GITHUB_SNIPPET: &str = "GitHubSnippet";
}

/// Input for the pre-MCP-tool-call hook (`on_pre_mcp_tool_call`), invoked before an
/// MCP-server tool is executed.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PreMcpToolCallHookInput {
    pub timestamp: f64,
    pub cwd: String,
    pub server_name: String,
    pub tool_name: String,
    pub tool_args: serde_json::Value,
}

/// Clonable, debuggable interceptor for outbound LLM inference HTTP/WebSocket requests.
///
/// Assign an instance to `CopilotClientOptions::request_handler` to mutate, replace,
/// or forward the outbound request payload.
#[derive(Clone)]
pub struct CopilotRequestHandler(
    pub Arc<dyn Fn(serde_json::Value) -> Result<serde_json::Value, Box<dyn std::error::Error + Send + Sync>> + Send + Sync>,
);

impl std::fmt::Debug for CopilotRequestHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<copilot_request_handler>")
    }
}

/// Clonable, debuggable BYOK bearer-token provider. Receives `ProviderTokenArgs` and
/// returns a fresh bearer token for outbound model requests (per-session rotation).
#[derive(Clone)]
pub struct BearerTokenProviderFn(
    pub Arc<dyn Fn(ProviderTokenArgs) -> Result<String, Box<dyn std::error::Error + Send + Sync>> + Send + Sync>,
);

impl std::fmt::Debug for BearerTokenProviderFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<bearer_token_provider>")
    }
}

/// Clonable, debuggable handler invoked when an MCP server requests an OAuth host token.
#[derive(Clone)]
pub struct McpAuthHandlerFn(
    pub Arc<dyn Fn(ProviderTokenArgs) -> Result<String, Box<dyn std::error::Error + Send + Sync>> + Send + Sync>,
);

impl std::fmt::Debug for McpAuthHandlerFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<mcp_auth_handler>")
    }
}

/// Identity of the integrating application, declared once on the `connect` handshake
/// (`clientInfo`) so runtime telemetry is attributed to a consistent surface.
///
/// Empty fields are dropped; when every field is empty, `connect` is not sent.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CopilotClientInfo {
    /// Name of the application using the SDK (wire: `editorName`).
    pub application_name: Option<String>,
    /// Version of the application using the SDK (wire: `editorVersion`).
    pub application_version: Option<String>,
    /// Name of an integration within the application, e.g. a plugin (wire: `extensionName`).
    pub integration_name: Option<String>,
    /// Version of the integration (wire: `extensionVersion`).
    pub integration_version: Option<String>,
}

impl CopilotClientInfo {
    /// Returns the `connect` wire shape, or `None` when no field carries a non-empty value.
    pub fn to_wire(&self) -> Option<serde_json::Value> {
        let fields = [
            ("editorName", &self.application_name),
            ("editorVersion", &self.application_version),
            ("extensionName", &self.integration_name),
            ("extensionVersion", &self.integration_version),
        ];
        let mut wire = serde_json::Map::new();
        for (key, value) in fields {
            if let Some(value) = value.as_deref().filter(|v| !v.is_empty()) {
                wire.insert(key.to_string(), serde_json::Value::String(value.to_string()));
            }
        }
        if wire.is_empty() {
            None
        } else {
            Some(serde_json::Value::Object(wire))
        }
    }
}

/// Arguments passed to a session's GitHub token provider (`gitHubToken.getToken`).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GitHubTokenProviderArgs {
    /// Effective GitHub host the token is for (e.g. `github.com`).
    pub host: String,
    /// Session receiving the token; `None` only before the session ID is known.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_id: Option<String>,
    /// Why the runtime wants a token: `"initial"` or `"refresh"`.
    pub reason: String,
}

/// Result returned by a GitHub token provider.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "kind")]
pub enum GitHubTokenProviderResult {
    /// A token plus its remaining lifetime in seconds (`{ kind: "token", ... }`).
    #[serde(rename = "token", rename_all = "camelCase")]
    Token {
        access_token: String,
        /// OAuth token type; the runtime assumes bearer when omitted.
        #[serde(skip_serializing_if = "Option::is_none")]
        token_type: Option<String>,
        expires_in: u64,
    },
    /// The host declined to supply a token (`{ kind: "cancelled" }`).
    #[serde(rename = "cancelled")]
    Cancelled,
}

/// Future returned by a [`GitHubTokenProviderFn`].
pub type GitHubTokenProviderFuture = Pin<
    Box<
        dyn Future<
                Output = Result<GitHubTokenProviderResult, Box<dyn std::error::Error + Send + Sync>>,
            > + Send,
    >,
>;

/// Clonable, debuggable async GitHub token provider for one session (`gitHubTokenProvider`).
#[derive(Clone)]
pub struct GitHubTokenProviderFn(
    pub Arc<dyn Fn(GitHubTokenProviderArgs) -> GitHubTokenProviderFuture + Send + Sync>,
);

impl GitHubTokenProviderFn {
    /// Wraps an async closure as a GitHub token provider.
    pub fn new<F, Fut>(provider: F) -> Self
    where
        F: Fn(GitHubTokenProviderArgs) -> Fut + Send + Sync + 'static,
        Fut: Future<
                Output = Result<GitHubTokenProviderResult, Box<dyn std::error::Error + Send + Sync>>,
            > + Send
            + 'static,
    {
        Self(Arc::new(
            move |args: GitHubTokenProviderArgs| -> GitHubTokenProviderFuture {
                Box::pin(provider(args))
            },
        ))
    }
}

impl std::fmt::Debug for GitHubTokenProviderFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<github_token_provider>")
    }
}

// ============================================================================
// v3.0.0 Types (SDK protocol 4)
// ============================================================================

/// Boxed error returned by application callbacks.
pub type CallbackError = Box<dyn std::error::Error + Send + Sync>;

/// How the client configures sessions (`mode`).
///
/// `CodingAgent` (the default) keeps the CLI-like coding tools and defaults. `Empty`
/// disables ambient tools and optional CLI behavior: every session must list its
/// `available_tools`, and safe defaults are applied under the session's own values.
/// The legacy name `"copilot-cli"` is accepted as an alias of `"coding-agent"`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ClientMode {
    /// CLI-like coding tools and defaults (`"coding-agent"`).
    #[default]
    #[serde(rename = "coding-agent", alias = "copilot-cli")]
    CodingAgent,
    /// Only explicitly enabled capabilities (`"empty"`), for shared or multi-user hosts.
    #[serde(rename = "empty")]
    Empty,
}

impl ClientMode {
    /// Returns the wire name (`"coding-agent"` or `"empty"`).
    pub fn as_str(&self) -> &'static str {
        match self {
            ClientMode::CodingAgent => "coding-agent",
            ClientMode::Empty => "empty",
        }
    }
}

impl std::fmt::Display for ClientMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl std::str::FromStr for ClientMode {
    type Err = String;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "coding-agent" | "copilot-cli" => Ok(ClientMode::CodingAgent),
            "empty" => Ok(ClientMode::Empty),
            other => Err(format!(
                "invalid client mode \"{}\": expected \"coding-agent\" or \"empty\"",
                other
            )),
        }
    }
}

/// A named BYOK provider connection (`inference.providers[]`).
///
/// A connection owns its endpoint, wire API, transport, and credentials. Model
/// definitions reference it by `name`, which cannot contain `/`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NamedProviderConfig {
    pub name: String,
    /// Provider type: `"openai"` (runtime default), `"azure"`, or `"anthropic"`.
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    pub provider_type: Option<String>,
    pub base_url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_key: Option<String>,
    /// Command whose stdout is used as the API key.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_key_command: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bearer_token: Option<String>,
    /// `"completions"` (runtime default) or `"responses"`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub wire_api: Option<String>,
    /// `"http"` (runtime default) or `"websockets"` (requires the Responses API).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub transport: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub azure: Option<AzureProviderOptions>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub headers: Option<HashMap<String, String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model_provider: Option<String>,
}

/// A model definition served by a named provider (`inference.models[]`).
///
/// Select it with the canonical `"<provider>/<id>"` model name.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ProviderModelConfig {
    /// Name of the `NamedProviderConfig` connection serving this model.
    pub provider: String,
    /// Provider-local model identifier.
    pub id: String,
    /// Known base model for behavior and capabilities (defaults to `id`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model_id: Option<String>,
    /// Model or deployment name sent to the provider API (defaults to `id`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub wire_model: Option<String>,
    /// Display label (defaults to the canonical selection ID).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_prompt_tokens: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_context_window_tokens: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_output_tokens: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<serde_json::Value>,
}

/// Built-in Copilot API (CAPI) policy (`github.capi`).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CapiConfig {
    /// Enables the built-in Copilot model catalog, authentication and inference
    /// (runtime default: true on create; omission on resume preserves the policy).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
}

/// A password or other answer that a shell command run by the agent asks for
/// (`shellAuthentication.request`). The answer goes only to the program that asked.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct ShellAuthenticationRequest {
    /// Identifies the request; a `shellAuthentication.cancel` carries the same ID.
    pub request_id: String,
    /// The program that asked: `sudo`, `ssh`, or `git`.
    pub program: String,
    /// The prompt text the program supplied.
    pub prompt: String,
    /// `"secret"` (mask the input), `"text"`, or `"confirm"` (answer `"yes"` or cancel).
    pub input_kind: String,
    /// The shell command that triggered the prompt.
    pub command: String,
    /// The command's working directory.
    pub cwd: String,
}

/// Answer to a [`ShellAuthenticationRequest`].
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "action", rename_all = "lowercase")]
pub enum ShellAuthenticationResponse {
    /// Send `value` to the program (`{ action: "accept", value }`).
    Accept { value: String },
    /// Decline the prompt (`{ action: "cancel" }`).
    Cancel,
}

impl std::fmt::Debug for ShellAuthenticationResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ShellAuthenticationResponse::Accept { .. } => f.write_str("Accept { value: <redacted> }"),
            ShellAuthenticationResponse::Cancel => f.write_str("Cancel"),
        }
    }
}

/// Future returned by a [`ShellAuthenticationHandlerFn`].
pub type ShellAuthenticationFuture =
    Pin<Box<dyn Future<Output = Result<ShellAuthenticationResponse, CallbackError>> + Send>>;

/// Clonable, debuggable async handler for shell authentication prompts.
///
/// Receives the request and the session ID. When the runtime cancels the prompt
/// (`shellAuthentication.cancel`), the SDK drops the pending future and answers `cancel`.
#[derive(Clone)]
pub struct ShellAuthenticationHandlerFn(
    pub Arc<dyn Fn(ShellAuthenticationRequest, String) -> ShellAuthenticationFuture + Send + Sync>,
);

impl ShellAuthenticationHandlerFn {
    /// Wraps an async closure as a shell authentication handler.
    pub fn new<F, Fut>(handler: F) -> Self
    where
        F: Fn(ShellAuthenticationRequest, String) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<ShellAuthenticationResponse, CallbackError>> + Send + 'static,
    {
        Self(Arc::new(
            move |request: ShellAuthenticationRequest, session_id: String| -> ShellAuthenticationFuture {
                Box::pin(handler(request, session_id))
            },
        ))
    }
}

impl std::fmt::Debug for ShellAuthenticationHandlerFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<shell_authentication_handler>")
    }
}

/// A memory the agent asked to store (`memoryStore.store`), after the runtime's
/// validation, secret filtering and approval.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct StoredMemory {
    /// Session that stored the memory.
    pub session_id: String,
    /// Short topic the memory is about.
    pub subject: String,
    /// The fact to remember.
    pub fact: String,
    /// Sources supporting the fact, such as file paths with line numbers.
    pub citations: String,
    /// Why the agent considers the fact worth remembering.
    pub reason: String,
}

/// Future returned by a [`MemoryStoreHandlerFn`].
pub type MemoryStoreFuture = Pin<Box<dyn Future<Output = Result<(), CallbackError>> + Send>>;

/// Clonable, debuggable async callback that stores memories in the application.
///
/// `store_memory` succeeds for the agent only after the future resolves; an error
/// fails the tool call and nothing is stored elsewhere.
#[derive(Clone)]
pub struct MemoryStoreHandlerFn(pub Arc<dyn Fn(StoredMemory) -> MemoryStoreFuture + Send + Sync>);

impl MemoryStoreHandlerFn {
    /// Wraps an async closure as a memory store callback.
    pub fn new<F, Fut>(handler: F) -> Self
    where
        F: Fn(StoredMemory) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<(), CallbackError>> + Send + 'static,
    {
        Self(Arc::new(move |memory: StoredMemory| -> MemoryStoreFuture {
            Box::pin(handler(memory))
        }))
    }
}

impl std::fmt::Debug for MemoryStoreHandlerFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<memory_store_handler>")
    }
}

/// Traffic the network callbacks serve (`network.setProvider` `scope`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum NetworkScope {
    /// Requests of this SDK connection (runtime default).
    Connection,
    /// All requests of the runtime process.
    Runtime,
}

/// Request for the proxies to use for a target (`network.resolveProxy`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct NetworkResolveProxyRequest {
    /// Request target as a PAC script sees it.
    pub url: String,
}

/// Request for an initial SPNEGO token (`network.authenticateNegotiate`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct NetworkAuthenticateNegotiateRequest {
    /// Kerberos service principal (`service/host`, optionally followed by `@REALM`).
    pub spn: String,
}

/// Request for proxy Basic credentials (`network.authenticateBasic`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct NetworkAuthenticateBasicRequest {
    /// Proxy that challenged, as `http://host:port` or `https://host:port`.
    pub proxy: String,
    /// Realm named by the proxy (untrusted text).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub realm: Option<String>,
    /// True when the proxy rejected the credentials sent last.
    pub previous_rejected: bool,
}

/// Proxy Basic credentials returned by an [`AuthenticateBasicFn`]. Never logged.
#[derive(Clone, Default, PartialEq, Eq)]
pub struct BasicCredentials {
    pub username: String,
    pub password: Option<String>,
}

impl std::fmt::Debug for BasicCredentials {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("BasicCredentials { <redacted> }")
    }
}

/// Future returned by the network callbacks.
pub type NetworkCallbackFuture<T> = Pin<Box<dyn Future<Output = Result<T, CallbackError>> + Send>>;

/// Resolves the PAC-style proxy list for a request target (`None` leaves the
/// request to the runtime's own proxy resolution).
#[derive(Clone)]
pub struct ResolveProxyFn(
    pub Arc<dyn Fn(NetworkResolveProxyRequest) -> NetworkCallbackFuture<Option<String>> + Send + Sync>,
);

impl ResolveProxyFn {
    /// Wraps an async closure as a proxy resolver.
    pub fn new<F, Fut>(callback: F) -> Self
    where
        F: Fn(NetworkResolveProxyRequest) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Option<String>, CallbackError>> + Send + 'static,
    {
        Self(Arc::new(move |request: NetworkResolveProxyRequest| -> NetworkCallbackFuture<Option<String>> {
            Box::pin(callback(request))
        }))
    }
}

impl std::fmt::Debug for ResolveProxyFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<resolve_proxy>")
    }
}

/// Supplies a base64 initial SPNEGO token (without the `Negotiate` prefix), or `None`.
#[derive(Clone)]
pub struct AuthenticateNegotiateFn(
    pub Arc<
        dyn Fn(NetworkAuthenticateNegotiateRequest) -> NetworkCallbackFuture<Option<String>>
            + Send
            + Sync,
    >,
);

impl AuthenticateNegotiateFn {
    /// Wraps an async closure as a Negotiate token provider.
    pub fn new<F, Fut>(callback: F) -> Self
    where
        F: Fn(NetworkAuthenticateNegotiateRequest) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Option<String>, CallbackError>> + Send + 'static,
    {
        Self(Arc::new(
            move |request: NetworkAuthenticateNegotiateRequest| -> NetworkCallbackFuture<Option<String>> {
                Box::pin(callback(request))
            },
        ))
    }
}

impl std::fmt::Debug for AuthenticateNegotiateFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<authenticate_negotiate>")
    }
}

/// Supplies proxy Basic credentials, or `None` when none are available.
#[derive(Clone)]
pub struct AuthenticateBasicFn(
    pub Arc<
        dyn Fn(NetworkAuthenticateBasicRequest) -> NetworkCallbackFuture<Option<BasicCredentials>>
            + Send
            + Sync,
    >,
);

impl AuthenticateBasicFn {
    /// Wraps an async closure as a Basic credentials provider.
    pub fn new<F, Fut>(callback: F) -> Self
    where
        F: Fn(NetworkAuthenticateBasicRequest) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Option<BasicCredentials>, CallbackError>> + Send + 'static,
    {
        Self(Arc::new(
            move |request: NetworkAuthenticateBasicRequest| -> NetworkCallbackFuture<Option<BasicCredentials>> {
                Box::pin(callback(request))
            },
        ))
    }
}

impl std::fmt::Debug for AuthenticateBasicFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<authenticate_basic>")
    }
}

/// Client network callbacks (`network.setProvider`): proxy resolution and proxy
/// authentication answered by the application instead of ambient settings.
#[derive(Debug, Clone, Default)]
pub struct NetworkConfig {
    /// Traffic the callbacks serve (runtime default: `Connection`).
    pub scope: Option<NetworkScope>,
    pub resolve_proxy: Option<ResolveProxyFn>,
    pub authenticate_negotiate: Option<AuthenticateNegotiateFn>,
    pub authenticate_basic: Option<AuthenticateBasicFn>,
}

impl NetworkConfig {
    /// Returns true when at least one callback is registered.
    pub fn has_callbacks(&self) -> bool {
        self.resolve_proxy.is_some()
            || self.authenticate_negotiate.is_some()
            || self.authenticate_basic.is_some()
    }

    /// Builds the `network.setProvider` params: an empty object marks each registered callback.
    pub fn set_provider_params(&self) -> serde_json::Value {
        let mut params = serde_json::Map::new();
        if let Some(scope) = self.scope {
            if let Ok(value) = serde_json::to_value(scope) {
                params.insert("scope".to_string(), value);
            }
        }
        let marker = || serde_json::Value::Object(serde_json::Map::new());
        if self.resolve_proxy.is_some() {
            params.insert("resolveProxy".to_string(), marker());
        }
        if self.authenticate_negotiate.is_some() {
            params.insert("authenticateNegotiate".to_string(), marker());
        }
        if self.authenticate_basic.is_some() {
            params.insert("authenticateBasic".to_string(), marker());
        }
        serde_json::Value::Object(params)
    }
}

/// One field-specific finding reported by a policy helper.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct PolicyHelperFailureDetail {
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
}

/// Validated help link supplied by a policy helper (sanitize again before navigating).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct PolicyHelperHelpLink {
    pub url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
}

/// The latest unresolved managed-settings policy helper failure (`policyHelperFailure`).
///
/// Reported in the JSON-RPC error data of a failed `session.create` / `session.resume`
/// (see [`crate::CopilotError::policy_helper_failure`]) and on managed settings snapshots.
/// Helper-authored text is untrusted: render it as plain text.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct PolicyHelperFailure {
    /// Runtime classification such as `timeout`, `spawn`, or `schema` (open set).
    pub category: String,
    /// `ignore` or `failClosed`.
    pub failure_mode: String,
    /// Plain-text primary explanation.
    pub message: String,
    /// True when an earlier helper-derived policy layer still applies.
    pub previous_policy_retained: bool,
    /// Invocation reason such as `sessionStart` or `pluginActivation` (open set).
    pub reason: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub authority: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub code: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<Vec<PolicyHelperFailureDetail>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diagnostic: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub help_link: Option<PolicyHelperHelpLink>,
}

impl PolicyHelperFailure {
    /// Extracts `policyHelperFailure` from a managed settings snapshot or RPC error data.
    pub fn from_data(data: &serde_json::Value) -> Option<Self> {
        data.get("policyHelperFailure")
            .filter(|value| value.is_object())
            .and_then(|value| serde_json::from_value(value.clone()).ok())
    }
}

/// Known refusal reasons of `session.permissions.setMode`.
pub mod permission_mode_refusal {
    /// Managed (enterprise) policy does not allow the requested mode.
    pub const MANAGED_POLICY: &str = "managed_policy";
}

/// Result of [`crate::CopilotSession::set_permission_mode`].
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct PermissionsSetModeResult {
    /// Whether the requested mode was applied.
    pub success: bool,
    /// Authoritative permission mode after the call (`manual`, `assisted`, `allow-all`).
    pub mode: String,
    /// Why the mode was refused, when known (for example `"managed_policy"`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
}

impl PermissionsSetModeResult {
    /// Returns true when managed policy refused the requested mode.
    pub fn refused_by_managed_policy(&self) -> bool {
        self.reason.as_deref() == Some(permission_mode_refusal::MANAGED_POLICY)
    }
}

// ============================================================================
// v3.1.0 Types (MCP secret store, plugin extension approvals)
// ============================================================================

/// Result of [`crate::CopilotClient::mcp_secrets_save`] (`mcp.secrets.save`).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct McpSecretsSaveResult {
    /// True when the OS keychain stored the credential, false for the file backend.
    pub used_keychain: bool,
}

/// A user-scoped approval for one installed plugin extension
/// (`permissions.extensionApprovals.*`).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct PermissionsExtensionApproval {
    /// Stable identity, for example `plugin:plugin-name:extension-name`.
    pub extension_name: String,
    pub plugin_name: String,
    /// Exact privileged capabilities the user approved.
    pub capabilities: Vec<String>,
    /// When the approval was granted (RFC 3339 timestamp).
    pub granted_at: String,
    /// Opaque installation identity; pass it back unchanged to revoke the approval.
    pub installation_key: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub installation_label: Option<String>,
}

/// Result of [`crate::CopilotClient::extension_approvals_grant`]
/// (`permissions.extensionApprovals.grant`).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct PermissionsExtensionApprovalsGrantResult {
    /// Whether the pending request was eligible and persisted.
    pub granted: bool,
    /// The persisted approval, when granted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub approval: Option<PermissionsExtensionApproval>,
}

/// Result of [`crate::CopilotClient::extension_approvals_list`]
/// (`permissions.extensionApprovals.list`).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct PermissionsExtensionApprovalsListResult {
    pub approvals: Vec<PermissionsExtensionApproval>,
}

/// Result of [`crate::CopilotClient::extension_approvals_revoke`]
/// (`permissions.extensionApprovals.revoke`).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct PermissionsExtensionApprovalsRevokeResult {
    /// Whether a matching approval was revoked.
    pub revoked: bool,
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
    /// Override the Copilot home directory.
    pub copilot_home: Option<String>,
    /// Token for TCP connection authentication.
    pub tcp_connection_token: Option<String>,
    /// Interceptor for outbound LLM inference HTTP/WebSocket requests.
    pub request_handler: Option<CopilotRequestHandler>,
    /// BYOK bearer-token provider used to mint fresh tokens for outbound model requests.
    pub bearer_token_provider: Option<BearerTokenProviderFn>,
    /// Directories to load built-in plugins from (`builtinPluginDirectories`).
    pub builtin_plugin_directories: Option<Vec<String>>,
    /// Use the in-process (FFI) transport instead of a subprocess (`inProcess`).
    pub in_process: Option<bool>,
    /// Application identity declared on the `connect` handshake (`clientInfo`).
    pub client_info: Option<CopilotClientInfo>,
    /// Session defaults policy: `CodingAgent` (default) or `Empty`.
    pub mode: ClientMode,
    /// Network callbacks registered with `network.setProvider` after connecting.
    pub network: Option<NetworkConfig>,
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
            copilot_home: None,
            tcp_connection_token: None,
            request_handler: None,
            bearer_token_provider: None,
            builtin_plugin_directories: None,
            in_process: None,
            client_info: None,
            mode: ClientMode::CodingAgent,
            network: None,
        }
    }
}
