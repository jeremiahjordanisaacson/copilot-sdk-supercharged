//! Protocol types shared between the SDK and the GitHub Copilot CLI.
//!
//! These types map directly to the JSON-RPC request/response payloads
//! defined by the GitHub Copilot CLI protocol. They are used for session
//! configuration, event handling, tool invocations, and model queries.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::canvas::{CanvasDeclaration, CanvasHandler};
/// Host's Entra registration and optional connection-owned acquisition.
///
/// **Experimental.** May change or be removed in future releases.
#[derive(Clone)]
pub struct EntraAuthOptions {
    /// **Experimental.** Application registration used for identity establishment.
    pub client_id: String,
    /// **Experimental.** OneAuth MATS application name. Omission disables MATS telemetry.
    pub mats_app_name: Option<String>,
    /// **Experimental.** Native OneAuth descriptor override. Omission uses `client_id`;
    /// this does not isolate accounts.
    pub one_auth_application_id: Option<String>,
    /// **Experimental.** Host callback. Omission retains built-in runtime authentication.
    pub token_provider: Option<Arc<dyn crate::entra::EntraTokenProvider>>,
}

impl EntraAuthOptions {
    /// Configure an Entra application with built-in runtime acquisition.
    ///
    /// **Experimental.** May change or be removed in future releases.
    pub fn new(client_id: impl Into<String>) -> Self {
        Self {
            client_id: client_id.into(),
            mats_app_name: None,
            one_auth_application_id: None,
            token_provider: None,
        }
    }

    /// Delegate identity and resource acquisition to the host.
    ///
    /// **Experimental.** May change or be removed in future releases.
    pub fn with_token_provider(mut self, provider: impl crate::entra::EntraTokenProvider) -> Self {
        self.token_provider = Some(Arc::new(provider));
        self
    }

    pub(crate) fn to_wire(&self) -> crate::rpc::EntraAuthConfig {
        crate::rpc::EntraAuthConfig {
            client_id: self.client_id.clone(),
            mats_app_name: self.mats_app_name.clone(),
            one_auth_application_id: self.one_auth_application_id.clone(),
        }
    }
}

impl From<crate::rpc::EntraAuthConfig> for EntraAuthOptions {
    fn from(config: crate::rpc::EntraAuthConfig) -> Self {
        Self {
            mats_app_name: config.mats_app_name,
            one_auth_application_id: config.one_auth_application_id,
            ..Self::new(config.client_id)
        }
    }
}

impl std::fmt::Debug for EntraAuthOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EntraAuthOptions")
            .field("client_id", &self.client_id)
            .field("mats_app_name", &self.mats_app_name)
            .field("one_auth_application_id", &self.one_auth_application_id)
            .field(
                "token_provider",
                &self.token_provider.as_ref().map(|_| "<provider>"),
            )
            .finish()
    }
}
pub use crate::copilot_request_handler::{
    CopilotHttpRequest, CopilotHttpResponse, CopilotHttpResponseBody, CopilotRequestContext,
    CopilotRequestError, CopilotRequestHandler, CopilotRequestTransport, CopilotWebSocketHandler,
    CopilotWebSocketMessage, CopilotWebSocketResponse, WebSocketContextTransform,
    WebSocketTransform,
};
#[cfg(feature = "network-forwarding")]
pub use crate::copilot_request_handler::{
    CopilotWebSocketForwarder, CopilotWebSocketForwarderBuilder, forward_http,
};
/// Copilot Fusion tier, matching the CLI's `--fusion-tier`. The runtime
/// resolves each tier to the Auto routing preference it stands for.
pub use crate::generated::api_types::FusionTier;
/// Outcome of an initial permission mode requested at create or resume.
pub use crate::generated::api_types::PermissionsSetModeResult;
pub use crate::generated::api_types::{
    AutopilotOptions, DiagnosticLogLevel, DiagnosticSourcesConfiguration, DiagnosticsConfiguration,
    McpDiagnosticSourceConfiguration, SessionManagementScope,
};
use crate::generated::api_types::{
    CurrentToolMetadata, LifecycleDiagnosticsConfiguration, OpenCanvasInstance, SandboxConfig,
};
/// Acknowledgement and Auto preference snapshot returned by an Auto tier switch.
pub use crate::generated::api_types::{ModelSwitchAutoTierResult, ModelSwitchAutoTierStatus};
pub use crate::generated::session_events::AgentModelPolicy;
/// Routing tier for the `auto` model with Auto mode V2.
///
/// [`AutoTier::Fast`] is an integrator-only latency preset, not a first-party
/// GitHub Copilot product preference.
pub use crate::generated::session_events::AutoTier;
/// Permission mode requested for a session.
pub use crate::generated::session_events::PermissionMode;
use crate::generated::session_events::ReasoningSummary;
/// Context window tier for models that support tiered context windows.
pub use crate::generated::session_events::{ContextTier, SessionLimitsConfig};
use crate::github_token::GitHubTokenProvider;
use crate::handler::{
    AutoModeSwitchHandler, ElicitationHandler, ExitPlanModeHandler, McpAuthHandler,
    PermissionHandler, ShellAuthenticationHandler, UserInputHandler,
};
use crate::hooks::SessionHooks;
use crate::memory_store::MemoryStore;
use crate::provider_token::BearerTokenProvider;
pub use crate::session_fs::{
    DirEntry, DirEntryKind, FileByteRange, FileInfo, FsError, SessionFsBinaryProvider,
    SessionFsCapabilities, SessionFsConfig, SessionFsConventions, SessionFsProvider,
    SessionFsReadFileRangeProvider, SessionFsSqliteProvider, SessionFsSqliteQueryResult,
    SessionFsSqliteQueryType, SessionFsSqliteTransactionError,
    SessionFsSqliteTransactionErrorClass, SessionFsSqliteTransactionStatement,
};
use crate::skill_provider::SkillProvider;
pub use crate::trace_context::{TraceContext, TraceContextProvider};
use crate::transforms::SystemMessageTransform;

/// Lifecycle state of a [`Client`](crate::Client) connection. Internal —
/// not part of the public API.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
#[non_exhaustive]
pub(crate) enum ConnectionState {
    /// No CLI process is attached or the process has exited cleanly.
    Disconnected,
    /// The client is starting up (spawning the CLI, negotiating protocol).
    Connecting,
    /// The client is connected and ready to handle RPC traffic.
    Connected,
    /// Startup failed or the connection encountered an unrecoverable error.
    Error,
}

/// Type of [`SessionLifecycleEvent`] received via [`Client::subscribe_lifecycle`](crate::Client::subscribe_lifecycle).
///
/// Values serialize as the dotted JSON strings the CLI sends (e.g.
/// `"session.created"`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[non_exhaustive]
pub enum SessionLifecycleEventType {
    /// A new session was created.
    #[serde(rename = "session.created")]
    Created,
    /// A session was deleted.
    #[serde(rename = "session.deleted")]
    Deleted,
    /// A session's metadata was updated (e.g. summary regenerated).
    #[serde(rename = "session.updated")]
    Updated,
    /// A session moved into the foreground.
    #[serde(rename = "session.foreground")]
    Foreground,
    /// A session moved into the background.
    #[serde(rename = "session.background")]
    Background,
}

/// Optional metadata attached to a [`SessionLifecycleEvent`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SessionLifecycleEventMetadata {
    /// ISO-8601 timestamp the session was created.
    #[serde(rename = "startTime")]
    pub start_time: String,
    /// ISO-8601 timestamp the session was last modified.
    #[serde(rename = "modifiedTime")]
    pub modified_time: String,
    /// Optional generated summary of the session conversation so far.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
}

/// A `session.lifecycle` notification dispatched to subscribers obtained via
/// [`Client::subscribe_lifecycle`](crate::Client::subscribe_lifecycle).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SessionLifecycleEvent {
    /// The kind of lifecycle change this event represents.
    #[serde(rename = "type")]
    pub event_type: SessionLifecycleEventType,
    /// Identifier of the session this event refers to.
    #[serde(rename = "sessionId")]
    pub session_id: SessionId,
    /// Optional metadata describing the session at the time of the event.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<SessionLifecycleEventMetadata>,
}

/// Opaque session identifier assigned by the CLI.
///
/// A newtype wrapper around `String` that provides type safety — prevents
/// accidentally passing a workspace ID or request ID where a session ID
/// is expected. Derefs to `str` for zero-friction borrowing.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct SessionId(String);

impl SessionId {
    /// Create a new session ID from any string-like value.
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    /// Borrow the inner string.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Consume the wrapper, returning the inner string.
    pub fn into_inner(self) -> String {
        self.0
    }
}

impl std::ops::Deref for SessionId {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for SessionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl From<String> for SessionId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for SessionId {
    fn from(s: &str) -> Self {
        Self(s.to_owned())
    }
}

impl AsRef<str> for SessionId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl std::borrow::Borrow<str> for SessionId {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl From<SessionId> for String {
    fn from(id: SessionId) -> String {
        id.0
    }
}

impl PartialEq<str> for SessionId {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<String> for SessionId {
    fn eq(&self, other: &String) -> bool {
        &self.0 == other
    }
}

impl PartialEq<SessionId> for String {
    fn eq(&self, other: &SessionId) -> bool {
        self == &other.0
    }
}

impl PartialEq<&str> for SessionId {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

impl PartialEq<&SessionId> for SessionId {
    fn eq(&self, other: &&SessionId) -> bool {
        self.0 == other.0
    }
}

impl PartialEq<SessionId> for &SessionId {
    fn eq(&self, other: &SessionId) -> bool {
        self.0 == other.0
    }
}

/// Opaque request identifier for pending CLI requests (permission, user-input, etc.).
///
/// A newtype wrapper around `String` that provides type safety — prevents
/// accidentally passing a session ID or workspace ID where a request ID
/// is expected. Derefs to `str` for zero-friction borrowing.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct RequestId(String);

impl RequestId {
    /// Create a new request ID from any string-like value.
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    /// Consume the wrapper, returning the inner string.
    pub fn into_inner(self) -> String {
        self.0
    }
}

impl std::ops::Deref for RequestId {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for RequestId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl From<String> for RequestId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for RequestId {
    fn from(s: &str) -> Self {
        Self(s.to_owned())
    }
}

impl AsRef<str> for RequestId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl std::borrow::Borrow<str> for RequestId {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl From<RequestId> for String {
    fn from(id: RequestId) -> String {
        id.0
    }
}

impl PartialEq<str> for RequestId {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<String> for RequestId {
    fn eq(&self, other: &String) -> bool {
        &self.0 == other
    }
}

impl PartialEq<RequestId> for String {
    fn eq(&self, other: &RequestId) -> bool {
        self == &other.0
    }
}

impl PartialEq<&str> for RequestId {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

/// A tool that the client exposes to the Copilot agent.
///
/// Sent to the CLI as part of [`SessionToolOptions::definitions`]
/// at session creation/resume time. The Rust SDK hand-authors this struct
/// (rather than using the schema-generated form) so it can carry runtime
/// hints — `overrides_built_in_tool`, `skip_permission` — that don't appear
/// in the wire schema but are honored by the CLI.
///
/// A `Tool` may optionally carry a [`handler`](Self::handler): an
/// `Arc<dyn ToolHandler>` that implements the tool's runtime behavior.
/// When present, the SDK dispatches matching `external_tool.requested`
/// broadcasts to it automatically. When absent (`None`), the tool is
/// declaration-only — another connected client must service incoming
/// invocations.
#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct Tool {
    /// Tool identifier (e.g., `"bash"`, `"grep"`, `"str_replace_editor"`).
    pub name: String,
    /// Optional namespaced name for declarative filtering (e.g., `"playwright/navigate"`
    /// for MCP tools).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub namespaced_name: Option<String>,
    /// Description of what the tool does.
    #[serde(default)]
    pub description: String,
    /// Optional instructions for how to use this tool effectively.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub instructions: Option<String>,
    /// JSON Schema for the tool's input parameters.
    #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
    pub parameters: IndexMap<String, Value>,
    /// When `true`, this tool replaces a built-in tool of the same name
    /// (e.g. supplying a custom `grep` that the agent uses in place of the
    /// CLI's built-in implementation).
    #[serde(default, skip_serializing_if = "is_false")]
    pub overrides_built_in_tool: bool,
    /// When `true`, the CLI does not request permission before invoking
    /// this tool. Use with caution — the tool is responsible for any
    /// access control.
    #[serde(default, skip_serializing_if = "is_false")]
    pub skip_permission: bool,
    /// When `true`, a successful call to this tool ends the agent turn: the
    /// runtime's tool phase halts instead of feeding the result back to the
    /// model for another round. A failed call leaves the loop running so the
    /// model can read the error and retry.
    #[serde(default, skip_serializing_if = "is_false")]
    pub is_terminal: bool,
    /// When `true`, declares that repeating a call with the same arguments
    /// has no additional effect beyond the first call, as is true of any tool
    /// without side effects. The runtime may use this to decide that a call
    /// whose outcome was lost can be safely repeated. It does not affect
    /// permission checks.
    #[serde(default, skip_serializing_if = "is_false")]
    pub idempotent: bool,
    /// Controls whether the tool may be deferred (loaded lazily via tool
    /// search) rather than always pre-loaded. When [`DeferMode::Auto`], the
    /// tool can be deferred and surfaced through tool search. When
    /// [`DeferMode::Never`], the tool is always pre-loaded. `None` lets the
    /// runtime decide.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub defer: Option<DeferMode>,
    /// Opaque, host-defined metadata associated with the tool definition.
    /// Keys are namespaced and not part of the stable public API; values are
    /// not interpreted and may be recognized to inform host-specific behavior.
    /// Unknown keys are preserved and round-tripped untouched.
    #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
    pub metadata: IndexMap<String, Value>,
    /// **Experimental.** Optional namespace grouping this tool with related
    /// tools for model tool discovery. Where the model provider supports
    /// native tool namespaces, deferred tools sharing a namespace are
    /// presented to the model as one group; elsewhere the tool stays flat.
    /// [`name`](Self::name) remains the tool's unique invocation name either
    /// way.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub namespace: Option<ToolNamespace>,
    /// Optional runtime implementation. When `Some`, the SDK dispatches
    /// matching `external_tool.requested` broadcasts to this handler.
    /// When `None`, the tool is declaration-only.
    ///
    /// Serialized only as a boolean `hasHandler` ownership marker.
    ///
    /// Crate-private to enforce builder semantics: external callers must
    /// install a handler through [`Tool::with_handler`] and inspect via
    /// [`Tool::handler`], so an already-attached handler cannot be
    /// silently overwritten by direct field assignment.
    #[serde(
        rename = "hasHandler",
        serialize_with = "serialize_tool_handler",
        skip_deserializing
    )]
    pub(crate) handler: Option<Arc<dyn crate::tool::ToolHandler>>,
}

fn serialize_tool_handler<S: serde::Serializer>(
    handler: &Option<Arc<dyn crate::tool::ToolHandler>>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    serializer.serialize_bool(handler.is_some())
}

#[inline]
fn is_false(b: &bool) -> bool {
    !*b
}

/// **Experimental.** Namespace grouping related [`Tool`]s for model tool
/// discovery. See [`Tool::namespace`].
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct ToolNamespace {
    /// Namespace name: 1-64 ASCII letters, digits, underscores, or hyphens.
    pub name: String,
    /// Optional model-visible explanation of the tools in this namespace.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

impl ToolNamespace {
    /// Construct a namespace with the given name and no description.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: None,
        }
    }

    /// Set the model-visible explanation of the tools in this namespace.
    #[must_use]
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }
}

/// Controls whether a [`Tool`] may be deferred (loaded lazily via tool search)
/// rather than always pre-loaded.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DeferMode {
    /// The tool can be deferred and surfaced through tool search.
    Auto,
    /// The tool is always pre-loaded.
    Never,
}

impl Tool {
    /// Construct a new [`Tool`] with the given name and otherwise default
    /// values. The struct is `#[non_exhaustive]`, so external callers
    /// cannot use struct-literal syntax — use this builder or
    /// [`Default::default`] plus mut-let.
    ///
    /// # Example
    ///
    /// ```
    /// # use copilot_sdk::types::Tool;
    /// # use serde_json::json;
    /// let tool = Tool::new("greet")
    ///     .with_description("Say hello to a user")
    ///     .with_parameters(json!({
    ///         "type": "object",
    ///         "properties": { "name": { "type": "string" } },
    ///         "required": ["name"]
    ///     }));
    /// # let _ = tool;
    /// ```
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    /// Set the namespaced name for declarative filtering (e.g.
    /// `"playwright/navigate"` for MCP tools).
    pub fn with_namespaced_name(mut self, namespaced_name: impl Into<String>) -> Self {
        self.namespaced_name = Some(namespaced_name.into());
        self
    }

    /// Set the human-readable description of what the tool does.
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = description.into();
        self
    }

    /// Set optional instructions for how to use this tool effectively.
    pub fn with_instructions(mut self, instructions: impl Into<String>) -> Self {
        self.instructions = Some(instructions.into());
        self
    }

    /// Set the JSON Schema for the tool's input parameters.
    ///
    /// Accepts a JSON Schema as a `serde_json::Value`, typically built with
    /// `serde_json::json!({...})` or returned by `schema_for` (available
    /// with the `derive` feature). Tool parameter schemas are always
    /// top-level JSON objects (`{"type": "object", ...}`).
    ///
    /// # Panics
    ///
    /// Panics if `parameters` is not a JSON object. Use
    /// [`crate::tool::try_tool_parameters`] and assign to
    /// [`Tool::parameters`] directly when the schema comes from dynamic
    /// input and should produce a recoverable error instead.
    pub fn with_parameters(mut self, parameters: Value) -> Self {
        self.parameters = crate::tool::tool_parameters(parameters);
        self
    }

    /// Mark this tool as overriding a built-in tool of the same name.
    /// E.g. supplying a custom `grep` that the agent uses in place of the
    /// CLI's built-in implementation.
    pub fn with_overrides_built_in_tool(mut self, overrides: bool) -> Self {
        self.overrides_built_in_tool = overrides;
        self
    }

    /// When `true`, the CLI will not request permission before invoking
    /// this tool. Use with caution — the tool is responsible for any
    /// access control.
    pub fn with_skip_permission(mut self, skip: bool) -> Self {
        self.skip_permission = skip;
        self
    }

    /// Sets whether a successful call to this tool ends the agent turn.
    ///
    /// When `true`, the runtime's tool phase halts after a successful call
    /// instead of feeding the result back to the model for another round. A
    /// failed call leaves the loop running so the model can read the error and
    /// retry.
    #[must_use]
    pub fn with_is_terminal(mut self, is_terminal: bool) -> Self {
        self.is_terminal = is_terminal;
        self
    }

    /// Declares whether repeating a call with the same arguments has no
    /// additional effect beyond the first call, as is true of any tool
    /// without side effects.
    ///
    /// The runtime may use this to decide that a call whose outcome was lost
    /// can be safely repeated. It does not affect permission checks.
    #[must_use]
    pub fn with_idempotent(mut self, idempotent: bool) -> Self {
        self.idempotent = idempotent;
        self
    }

    /// Set the deferral mode controlling whether the tool may be loaded
    /// lazily via tool search ([`DeferMode::Auto`]) or always pre-loaded
    /// ([`DeferMode::Never`]).
    pub fn with_defer(mut self, defer: DeferMode) -> Self {
        self.defer = Some(defer);
        self
    }

    /// Set opaque, host-defined metadata for the tool. Keys are namespaced and
    /// not part of the stable public API. Replaces any previously-set metadata.
    pub fn with_metadata(mut self, metadata: IndexMap<String, Value>) -> Self {
        self.metadata = metadata;
        self
    }

    /// **Experimental.** Group this tool with related tools under a namespace
    /// for model tool discovery. See [`Tool::namespace`].
    #[must_use]
    pub fn with_namespace(mut self, namespace: ToolNamespace) -> Self {
        self.namespace = Some(namespace);
        self
    }

    /// Attach a runtime implementation. The SDK will dispatch matching
    /// `external_tool.requested` broadcasts to `handler` for this tool's
    /// name. Without a handler the tool is declaration-only.
    pub fn with_handler(mut self, handler: Arc<dyn crate::tool::ToolHandler>) -> Self {
        self.handler = Some(handler);
        self
    }

    /// Returns the attached runtime handler, if any.
    ///
    /// Read-only inspection — to install or replace a handler, use
    /// [`Tool::with_handler`].
    pub fn handler(&self) -> Option<&Arc<dyn crate::tool::ToolHandler>> {
        self.handler.as_ref()
    }
}

/// Handlers for a session's client-supplied tools, keyed by tool name.
pub(crate) type ToolHandlerMap = HashMap<String, Arc<dyn crate::tool::ToolHandler>>;

/// Collect each tool's handler into a [`ToolHandlerMap`], preserving handler
/// presence for the wire ownership marker.
///
/// Two handlers for one name are rejected because dispatch, which is keyed by
/// name, could only ever reach one of them.
pub(crate) fn collect_tool_handlers(tools: &[Tool]) -> Result<ToolHandlerMap, crate::Error> {
    let mut handlers = ToolHandlerMap::new();
    for tool in tools {
        if let Some(handler) = &tool.handler
            && handlers
                .insert(tool.name.clone(), handler.clone())
                .is_some()
        {
            return Err(crate::Error::with_message(
                crate::ErrorKind::InvalidConfig,
                format!("duplicate tool handler registered for name {:?}", tool.name),
            ));
        }
    }
    Ok(handlers)
}

impl std::fmt::Debug for Tool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tool")
            .field("name", &self.name)
            .field("namespaced_name", &self.namespaced_name)
            .field("description", &self.description)
            .field("instructions", &self.instructions)
            .field("parameters", &self.parameters)
            .field("overrides_built_in_tool", &self.overrides_built_in_tool)
            .field("skip_permission", &self.skip_permission)
            .field("is_terminal", &self.is_terminal)
            .field("idempotent", &self.idempotent)
            .field("defer", &self.defer)
            .field("metadata", &self.metadata)
            .field("namespace", &self.namespace)
            .field(
                "handler",
                &self.handler.as_ref().map(|_| "<set>").unwrap_or("None"),
            )
            .finish()
    }
}

/// Context passed to a [`CommandHandler`] when a registered slash command
/// is executed by the user.
#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct CommandContext {
    /// Session ID where the command was invoked.
    pub session_id: SessionId,
    /// The full command text (e.g. `"/deploy production"`).
    pub command: String,
    /// Command name without the leading `/` (e.g. `"deploy"`).
    pub command_name: String,
    /// Raw argument string after the command name (e.g. `"production"`).
    pub args: String,
}

/// Handler invoked when a registered slash command is executed.
///
/// Returning `Err(_)` causes the SDK to forward the error message back to
/// the CLI via `session.commands.handlePendingCommand` so the TUI can
/// surface it. Returning `Ok(())` reports success. When panic unwinding is enabled,
/// a panic is reported as the error `command handler panicked`. With `panic = "abort"`,
/// a panic terminates the process instead.
#[async_trait::async_trait]
pub trait CommandHandler: Send + Sync {
    /// Called when the user invokes the command this handler is registered for.
    async fn on_command(&self, ctx: CommandContext) -> Result<(), crate::Error>;
}

/// Definition of a slash command registered with the session.
///
/// When the CLI is running with a TUI, registered commands appear as
/// `/name` for the user to invoke. Only `name` and `description` are sent
/// over the wire — the handler is local to this SDK process.
#[non_exhaustive]
#[derive(Clone)]
pub struct CommandDefinition {
    /// Command name (without leading `/`).
    pub name: String,
    /// Human-readable description shown in command-completion UI.
    pub description: Option<String>,
    /// Handler invoked when the command is executed.
    pub handler: Arc<dyn CommandHandler>,
}

impl CommandDefinition {
    /// Construct a new command definition. Use [`with_description`](Self::with_description)
    /// to add a description.
    pub fn new(name: impl Into<String>, handler: Arc<dyn CommandHandler>) -> Self {
        Self {
            name: name.into(),
            description: None,
            handler,
        }
    }

    /// Set the human-readable description shown in the CLI's command-completion UI.
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }
}

impl std::fmt::Debug for CommandDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CommandDefinition")
            .field("name", &self.name)
            .field("description", &self.description)
            .field("handler", &"<set>")
            .finish()
    }
}

impl Serialize for CommandDefinition {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("CommandDefinition", 2)?;
        state.serialize_field("name", &self.name)?;
        state.serialize_field("description", self.description.as_deref().unwrap_or(""))?;
        state.end()
    }
}

/// Configures a custom agent (sub-agent) for the session.
///
/// Custom agents have their own prompt, tool allowlist, and optionally
/// their own MCP servers and skill set. The agent named in
/// [`SessionAgentOptions::selected`] (or the runtime default) is the active one
/// when the session starts.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct CustomAgentConfig {
    /// Declarative hook action objects keyed by event name, active only for this agent.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub hooks: Option<IndexMap<String, Vec<IndexMap<String, Value>>>>,
    /// Unique name of the custom agent.
    pub name: String,
    /// Display name for UI purposes.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub display_name: Option<String>,
    /// Description of what the agent does.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// List of tool names the agent can use. `None` means all tools.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tools: Option<Vec<String>>,
    /// Prompt content for the agent.
    pub prompt: String,
    /// MCP servers specific to this agent.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mcp_servers: Option<IndexMap<String, McpServerConfig>>,
    /// Whether the agent is available for model inference.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub infer: Option<bool>,
    /// Skill names to preload into this agent's context at startup.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub skills: Option<Vec<String>>,
    /// Model identifier for this agent (e.g. `"claude-haiku-4.5"`).
    ///
    /// When set, the runtime will attempt to use this model for the agent,
    /// falling back to the parent session model if unavailable unless
    /// [`model_policy`](Self::model_policy) is [`AgentModelPolicy::Required`].
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    /// Whether the authored model is preferred (default) or required without fallback.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub model_policy: Option<AgentModelPolicy>,
    /// Reasoning effort level for this agent's model.
    ///
    /// When unset, the runtime resolves model configuration, then inherits the
    /// parent effort only for the same model.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub reasoning_effort: Option<String>,
}

impl CustomAgentConfig {
    /// Set declarative hook action objects scoped to this agent.
    pub fn with_hooks(mut self, hooks: IndexMap<String, Vec<IndexMap<String, Value>>>) -> Self {
        self.hooks = Some(hooks);
        self
    }

    /// Construct a custom agent configuration with the required `name`
    /// and `prompt` fields populated.
    ///
    /// All other fields default to unset; use the `with_*` chain to
    /// customize them. Fields are also `pub` if direct assignment is
    /// preferred for `Option<T>` pass-through.
    pub fn new(name: impl Into<String>, prompt: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            prompt: prompt.into(),
            ..Self::default()
        }
    }

    /// Set the display name shown in the CLI's agent-selection UI.
    pub fn with_display_name(mut self, display_name: impl Into<String>) -> Self {
        self.display_name = Some(display_name.into());
        self
    }

    /// Set the description of what the agent does.
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Restrict the agent to a specific tool allowlist. When unset, the
    /// agent inherits the parent session's tool set.
    pub fn with_tools<I, S>(mut self, tools: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.tools = Some(tools.into_iter().map(Into::into).collect());
        self
    }

    /// Configure agent-specific MCP servers.
    pub fn with_mcp_servers(mut self, mcp_servers: IndexMap<String, McpServerConfig>) -> Self {
        self.mcp_servers = Some(mcp_servers);
        self
    }

    /// Whether the agent participates in model inference.
    pub fn with_infer(mut self, infer: bool) -> Self {
        self.infer = Some(infer);
        self
    }

    /// Set the skills preloaded into the agent's context at startup.
    pub fn with_skills<I, S>(mut self, skills: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.skills = Some(skills.into_iter().map(Into::into).collect());
        self
    }

    /// Set the model identifier for this agent.
    pub fn with_model(mut self, model: impl Into<String>) -> Self {
        self.model = Some(model.into());
        self
    }

    /// Set whether the authored model is preferred or required.
    pub fn with_model_policy(mut self, model_policy: AgentModelPolicy) -> Self {
        self.model_policy = Some(model_policy);
        self
    }

    /// Set the reasoning effort level for this agent's model.
    pub fn with_reasoning_effort(mut self, reasoning_effort: impl Into<String>) -> Self {
        self.reasoning_effort = Some(reasoning_effort.into());
        self
    }
}

/// Configures the default (built-in) agent that handles turns when no
/// custom agent is selected.
///
/// Use [`Self::excluded_tools`] to hide tools from the default agent
/// while keeping them available to custom sub-agents that list them in
/// their [`CustomAgentConfig::tools`].
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DefaultAgentConfig {
    /// Tool names to exclude from the default agent.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub excluded_tools: Option<Vec<String>>,
}

/// Configuration for large tool output handling.
///
/// When a tool produces output exceeding [`max_size_bytes`](Self::max_size_bytes),
/// the runtime writes the full output to a file in [`output_directory`](Self::output_directory)
/// and returns the file path and a truncated preview to the model.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct LargeToolOutputConfig {
    /// Whether large tool output handling is enabled. Defaults to `true` on the CLI.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    /// Maximum tool output size in bytes before it is redirected to a file.
    /// Uses the runtime's configured threshold when unset
    /// (20480 bytes by default on the CLI).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_size_bytes: Option<u64>,
    /// Directory where large tool output files are written. Defaults to
    /// the OS temp directory on the CLI.
    #[serde(default, rename = "outputDir", skip_serializing_if = "Option::is_none")]
    pub output_directory: Option<PathBuf>,
}

impl LargeToolOutputConfig {
    /// Construct an empty [`LargeToolOutputConfig`]; all fields default to
    /// unset (the CLI applies its own defaults).
    pub fn new() -> Self {
        Self::default()
    }

    /// Toggle large tool output handling on or off.
    pub fn with_enabled(mut self, enabled: bool) -> Self {
        self.enabled = Some(enabled);
        self
    }

    /// Set the maximum tool output size in bytes before it is redirected to a file.
    pub fn with_max_size_bytes(mut self, max_size_bytes: u64) -> Self {
        self.max_size_bytes = Some(max_size_bytes);
        self
    }

    /// Set the directory where large tool output files are written.
    pub fn with_output_directory<P: Into<PathBuf>>(mut self, output_directory: P) -> Self {
        self.output_directory = Some(output_directory.into());
        self
    }
}

/// Opt in to image generation through an authorized Copilot image model.
///
/// **Experimental.** This API may change or be removed in a future release.
///
/// Omission disables on create/cold resume and preserves resident resume state.
/// Not persisted: re-supply after runtime restart. Policy, permissions, offline
/// mode, and tool filters still apply. BYOK image generation is not supported.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct ImageGenerationConfig {
    /// True opts in; false disables image generation.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    /// Request a visible "Made with AI" label on generated and edited images.
    /// Overrides the user's imageGenerationWatermark preference (default false).
    /// Re-supply after cold resume. Does not enable image generation or enforce
    /// enterprise policy.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub watermark: Option<bool>,
    /// Experimental. Save generated and edited images under the session-state
    /// directory's `images` folder and give the model each path as a
    /// `saved_to:` line in the tool result. Defaults to true; set false to opt
    /// out. Subagents follow
    /// their parent's value unless they set their own. A failed save still
    /// returns the image. Sessions without a local session-state directory
    /// never save. Re-supply after cold resume. Does not enable image
    /// generation.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub save_to_disk: Option<bool>,
}

impl ImageGenerationConfig {
    /// Construct an unset image generation configuration.
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable or disable image generation.
    pub fn with_enabled(mut self, enabled: bool) -> Self {
        self.enabled = Some(enabled);
        self
    }

    /// Request or opt out of the visible image label for this session.
    pub fn with_watermark(mut self, watermark: bool) -> Self {
        self.watermark = Some(watermark);
        self
    }

    /// Save generated and edited images to the session folder, or opt out.
    pub fn with_save_to_disk(mut self, save_to_disk: bool) -> Self {
        self.save_to_disk = Some(save_to_disk);
        self
    }
}

/// Overrides the runtime's built-in tool-search behavior.
///
/// Tool search defers tools to keep the model's active tool set small.
/// To override the tool-search tool's implementation, register a [`Tool`]
/// named `"tool_search_tool"` with [`Tool::overrides_built_in_tool`] set to `true`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct ToolSearchConfig {
    /// Toggle to enable/disable tool search.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    /// The tool count above which MCP and external tools are deferred behind
    /// tool search. When unset, the runtime default (30) applies.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub defer_threshold: Option<u32>,
}

impl ToolSearchConfig {
    /// Construct an empty [`ToolSearchConfig`]; all fields default to unset
    /// (the runtime applies its own defaults).
    pub fn new() -> Self {
        Self::default()
    }

    /// Toggle that enables or disables tool search.
    pub fn with_enabled(mut self, enabled: bool) -> Self {
        self.enabled = Some(enabled);
        self
    }

    /// Set the tool count above which MCP and external tools are deferred
    /// behind tool search.
    pub fn with_defer_threshold(mut self, defer_threshold: u32) -> Self {
        self.defer_threshold = Some(defer_threshold);
        self
    }
}

/// Configuration for the built-in GitHub MCP server.
///
/// `disable_form_deferral` only applies to the built-in GitHub MCP server and
/// only has an effect when MCP Apps and form-backed GitHub tools are enabled.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct GitHubMcpToolConfig {
    /// Whether all GitHub MCP tools are enabled.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enable_all_tools: Option<bool>,
    /// Additional GitHub MCP toolsets to enable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub additional_toolsets: Option<Vec<String>>,
    /// Additional GitHub MCP tools to enable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub additional_tools: Option<Vec<String>>,
    /// Whether GitHub MCP insiders mode is enabled.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enable_insiders_mode: Option<bool>,
    /// Disables form deferral for GitHub MCP tools. This only applies to the
    /// built-in GitHub MCP server and only has an effect when MCP Apps and
    /// form-backed GitHub tools are enabled.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub disable_form_deferral: Option<bool>,
}

impl GitHubMcpToolConfig {
    /// Construct an empty GitHub MCP tool configuration.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set whether all GitHub MCP tools are enabled.
    pub fn with_enable_all_tools(mut self, value: bool) -> Self {
        self.enable_all_tools = Some(value);
        self
    }

    /// Set the additional GitHub MCP toolsets to enable.
    pub fn with_additional_toolsets<I, S>(mut self, values: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.additional_toolsets = Some(values.into_iter().map(Into::into).collect());
        self
    }

    /// Set the additional GitHub MCP tools to enable.
    pub fn with_additional_tools<I, S>(mut self, values: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.additional_tools = Some(values.into_iter().map(Into::into).collect());
        self
    }

    /// Set whether GitHub MCP insiders mode is enabled.
    pub fn with_enable_insiders_mode(mut self, value: bool) -> Self {
        self.enable_insiders_mode = Some(value);
        self
    }

    /// Disable form deferral for GitHub MCP tools. This only applies to the
    /// built-in GitHub MCP server and only has an effect when MCP Apps and
    /// form-backed GitHub tools are enabled.
    pub fn with_disable_form_deferral(mut self, value: bool) -> Self {
        self.disable_form_deferral = Some(value);
        self
    }
}

/// Configures infinite sessions: persistent workspaces with automatic
/// context-window compaction.
///
/// When enabled (default), sessions automatically manage context limits
/// through background compaction and persist state to a workspace
/// directory.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct InfiniteSessionConfig {
    /// Whether infinite sessions are enabled. Defaults to `true` on the CLI.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    /// Context utilization (0.0–1.0) at which background compaction starts.
    /// Default: 0.80.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub background_compaction_threshold: Option<f64>,
    /// Context utilization (0.0–1.0) at which the session blocks until
    /// compaction completes. Default: 0.95.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub buffer_exhaustion_threshold: Option<f64>,
}

impl InfiniteSessionConfig {
    /// Construct an empty [`InfiniteSessionConfig`]; all fields default to
    /// unset (the CLI applies its own defaults).
    pub fn new() -> Self {
        Self::default()
    }

    /// Toggle infinite sessions on or off. Defaults to `true` on the CLI
    /// when unset.
    pub fn with_enabled(mut self, enabled: bool) -> Self {
        self.enabled = Some(enabled);
        self
    }

    /// Set the context utilization (0.0–1.0) at which background
    /// compaction starts.
    pub fn with_background_compaction_threshold(mut self, threshold: f64) -> Self {
        self.background_compaction_threshold = Some(threshold);
        self
    }

    /// Set the context utilization (0.0–1.0) at which the session blocks
    /// until compaction completes.
    pub fn with_buffer_exhaustion_threshold(mut self, threshold: f64) -> Self {
        self.buffer_exhaustion_threshold = Some(threshold);
        self
    }
}

/// Where retrieved memories enter the model conversation.
///
/// **Experimental.** This type may change or be removed in a future release.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[non_exhaustive]
pub enum MemoryMode {
    /// Include memories in the system prompt (default).
    #[default]
    SystemPrompt,
    /// Include memories in user messages.
    UserPrompt,
}

/// Per-session configuration for the runtime memory feature.
///
/// Supplied via [`SessionStateOptions::with_memory`]. When a session is created or resumed
/// without a memory configuration, the runtime applies its own default for the
/// memory feature.
///
/// Omitted [`mode`](Self::mode) preserves the runtime's system-prompt injection.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct MemoryConfiguration {
    /// Whether the memory feature is enabled for this session.
    pub enabled: bool,
    /// "system-prompt" (default) places memories in the system prompt.
    /// "user-prompt" places memories in generated user-role messages.
    ///
    /// **Experimental.** This option may change or be removed in a future release.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mode: Option<MemoryMode>,
}

impl MemoryConfiguration {
    /// A configuration with the memory feature enabled.
    pub fn enabled() -> Self {
        Self {
            enabled: true,
            mode: None,
        }
    }

    /// A configuration with the memory feature disabled.
    pub fn disabled() -> Self {
        Self {
            enabled: false,
            mode: None,
        }
    }

    /// Set whether the memory feature is enabled.
    pub fn with_enabled(mut self, enabled: bool) -> Self {
        self.enabled = enabled;
        self
    }

    /// Choose where retrieved memories enter the model conversation.
    ///
    /// **Experimental.** This method may change or be removed in a future release.
    pub fn with_mode(mut self, mode: MemoryMode) -> Self {
        self.mode = Some(mode);
        self
    }
}

/// GitHub repository metadata to associate with a cloud session.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct CloudSessionRepository {
    /// Repository owner.
    pub owner: String,
    /// Repository name.
    pub name: String,
    /// Optional branch name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub branch: Option<String>,
}

impl CloudSessionRepository {
    /// Create repository metadata for a cloud session.
    pub fn new(owner: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            owner: owner.into(),
            name: name.into(),
            branch: None,
        }
    }

    /// Set the branch associated with the repository.
    pub fn with_branch(mut self, branch: impl Into<String>) -> Self {
        self.branch = Some(branch.into());
        self
    }
}

/// Options for creating a remote session in the cloud.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct CloudSessionOptions {
    /// Optional GitHub repository metadata to associate with the cloud session.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repository: Option<CloudSessionRepository>,
}

impl CloudSessionOptions {
    /// Create cloud session options with repository metadata.
    pub fn with_repository(repository: CloudSessionRepository) -> Self {
        Self {
            repository: Some(repository),
        }
    }
}

/// Stable extension identity for session participants that provide canvases.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ExtensionInfo {
    /// Extension namespace/source, e.g. `"github-app"`.
    pub source: String,
    /// Stable provider name within the source namespace.
    pub name: String,
}

impl ExtensionInfo {
    /// Create stable extension identity metadata.
    pub fn new(source: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            name: name.into(),
        }
    }
}

/// Stable identity for a host/SDK connection that supplies built-in canvases.
///
/// When set on session create or resume, the runtime uses [`id`] verbatim as
/// the agent-facing canvas extension id, so canvases declared on a control
/// connection survive stdio reconnect and CLI process restart instead of being
/// re-keyed to a per-connection id. The id is opaque to the runtime; a
/// per-window-stable value such as `app:builtin:<windowId>` is recommended. An
/// id beginning with `connection:` is reserved and ignored by the runtime.
///
/// [`id`]: CanvasProviderIdentity::id
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct CanvasProviderIdentity {
    /// Opaque, stable provider id used verbatim as the canvas extension id.
    pub id: String,
    /// Optional display name surfaced as the canvas extension name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}

impl CanvasProviderIdentity {
    /// Create a canvas provider identity from a stable opaque id.
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: None,
        }
    }

    /// Set the optional display name surfaced as the canvas extension name.
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }
}

/// Configuration for a single MCP server.
///
/// MCP (Model Context Protocol) servers expose external tools to the
/// agent. Local servers run as a subprocess over stdio; remote servers
/// speak HTTP or Server-Sent Events.
///
/// Serialized as a JSON object with a `type` discriminator (`"stdio"` |
/// `"http"` | `"sse"`).
///
/// # Example
///
/// ```
/// # use copilot_sdk::types::{McpServerConfig, McpStdioServerConfig, McpHttpServerConfig};
/// # use copilot_sdk::IndexMap;
/// let mut servers = IndexMap::new();
/// servers.insert(
///     "playwright".to_string(),
///     McpServerConfig::Stdio(McpStdioServerConfig {
///         tools: Some(vec!["*".to_string()]),
///         command: "npx".to_string(),
///         args: vec!["-y".to_string(), "@playwright/mcp".to_string()],
///         ..Default::default()
///     }),
/// );
/// servers.insert(
///     "weather".to_string(),
///     McpServerConfig::Http(McpHttpServerConfig {
///         tools: Some(vec!["forecast".to_string()]),
///         url: "https://example.com/mcp".to_string(),
///         ..Default::default()
///     }),
/// );
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
#[non_exhaustive]
pub enum McpServerConfig {
    /// Local MCP server launched as a subprocess and addressed over stdio.
    /// On the wire this serializes as `{"type": "stdio", ...}`. The CLI
    /// also accepts `"local"` as an alias on input.
    #[serde(alias = "local")]
    Stdio(McpStdioServerConfig),
    /// Remote MCP server addressed over HTTP.
    Http(McpHttpServerConfig),
    /// Remote MCP server addressed over Server-Sent Events.
    Sse(McpHttpServerConfig),
}

/// Configuration for a local/stdio MCP server.
///
/// See [`McpServerConfig::Stdio`].
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpStdioServerConfig {
    /// Tools to expose from this server.
    ///
    /// - `None` (field omitted on the wire) — expose **all** tools.
    /// - `Some(vec![])` — expose **no** tools.
    /// - `Some(vec!["a", ...])` — expose only the listed tools.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tools: Option<Vec<String>>,
    /// Optional timeout in milliseconds for tool calls to this server.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub timeout: Option<i64>,
    /// Subprocess executable.
    pub command: String,
    /// Arguments to pass to the subprocess.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub args: Vec<String>,
    /// Environment variables to set on the subprocess. Values are passed
    /// through literally to the child process.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub env: HashMap<String, String>,
    /// Working directory for the subprocess.
    #[serde(default, skip_serializing_if = "Option::is_none", rename = "cwd")]
    pub working_directory: Option<String>,
}

/// Configuration for a remote MCP server (HTTP or SSE).
///
/// See [`McpServerConfig::Http`] and [`McpServerConfig::Sse`].
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpHttpServerConfig {
    /// Tools to expose from this server.
    ///
    /// - `None` (field omitted on the wire) — expose **all** tools.
    /// - `Some(vec![])` — expose **no** tools.
    /// - `Some(vec!["a", ...])` — expose only the listed tools.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tools: Option<Vec<String>>,
    /// Optional timeout in milliseconds for tool calls to this server.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub timeout: Option<i64>,
    /// Server URL.
    pub url: String,
    /// Optional HTTP headers to include on every request.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub headers: HashMap<String, String>,
}

/// Configures a custom inference provider (BYOK — Bring Your Own Key).
///
/// Routes session requests through an alternative model provider
/// (OpenAI-compatible, Azure, Anthropic, or local) instead of GitHub
/// Copilot's default routing.
#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct ProviderConfig {
    /// Provider type: `"openai"`, `"azure"`, `"anthropic"`, or `"google"`. Defaults to
    /// `"openai"` on the CLI.
    #[serde(default, skip_serializing_if = "Option::is_none", rename = "type")]
    pub provider_type: Option<String>,
    /// API format (openai/azure only): `"completions"` or `"responses"`.
    /// Defaults to `"completions"`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub wire_api: Option<String>,
    /// Transport for OpenAI Responses requests: `"http"` or `"websockets"`.
    /// Defaults to `"http"`. Set `"websockets"` to deliver Responses API
    /// requests over a persistent WebSocket connection instead of HTTP.
    /// Applies to OpenAI-compatible providers using `wire_api` `"responses"`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub transport: Option<String>,
    /// Product serving the model, such as `"ollama"` or `"lm_studio"`,
    /// reported in telemetry as `model_provider`. Allowed values are
    /// `"openai"`, `"anthropic"`, `"azure_openai"`, `"ollama"`,
    /// `"lm_studio"`, `"foundry_local"`, and `"llama_cpp"`; only affects telemetry.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub model_provider: Option<String>,
    /// API endpoint URL.
    pub base_url: String,
    /// API key. Optional for local providers like Ollama.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub api_key: Option<String>,
    /// Command run by the runtime before each provider request to obtain a fresh
    /// API key. Command failures fail the turn before sending a model request.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub api_key_command: Option<String>,
    /// Bearer token for authentication. Sets the `Authorization` header
    /// directly. Use for services requiring bearer-token auth instead of
    /// API key. Takes precedence over `api_key` when both are set.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bearer_token: Option<String>,
    /// **Experimental.** Callback used to acquire a bearer token before each
    /// outbound request to this provider.
    #[serde(skip)]
    pub bearer_token_provider: Option<Arc<dyn BearerTokenProvider>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) has_bearer_token_provider: Option<bool>,
    /// Azure-specific options.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub azure: Option<AzureProviderOptions>,
    /// Custom HTTP headers included in outbound provider requests.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub headers: Option<HashMap<String, String>>,
    /// Well-known model ID used to look up agent config and default token
    /// limits. Also used as the wire model when [`wire_model`](Self::wire_model)
    /// is unset. Falls back to [`CreateSessionOptions::model`](crate::CreateSessionOptions::model).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub model_id: Option<String>,
    /// Model name sent to the provider API for inference. Use this when
    /// the provider's model name (e.g. an Azure deployment name or a
    /// custom fine-tune name) differs from
    /// [`model_id`](Self::model_id). Falls back to
    /// [`model_id`](Self::model_id), then to
    /// [`CreateSessionOptions::model`](crate::CreateSessionOptions::model).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub wire_model: Option<String>,
    /// Overrides the resolved model's default max prompt tokens. The
    /// runtime triggers conversation compaction before sending a request
    /// when the prompt (system message, history, tool definitions, user
    /// message) would exceed this limit.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_prompt_tokens: Option<i64>,
    /// Overrides the resolved model's default max output tokens. When
    /// hit, the model stops generating and returns a truncated response.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_output_tokens: Option<i64>,
}

impl std::fmt::Debug for ProviderConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ProviderConfig")
            .field("provider_type", &self.provider_type)
            .field("wire_api", &self.wire_api)
            .field("transport", &self.transport)
            .field("model_provider", &self.model_provider)
            .field("base_url", &self.base_url)
            .field("api_key", &self.api_key)
            .field(
                "api_key_command",
                &self.api_key_command.as_ref().map(|_| "<set>"),
            )
            .field("bearer_token", &self.bearer_token)
            .field(
                "bearer_token_provider",
                &self.bearer_token_provider.as_ref().map(|_| "<set>"),
            )
            .field("has_bearer_token_provider", &self.has_bearer_token_provider)
            .field("azure", &self.azure)
            .field("headers", &self.headers)
            .field("model_id", &self.model_id)
            .field("wire_model", &self.wire_model)
            .field("max_prompt_tokens", &self.max_prompt_tokens)
            .field("max_output_tokens", &self.max_output_tokens)
            .finish()
    }
}

impl ProviderConfig {
    /// Construct a [`ProviderConfig`] with the required `base_url` set;
    /// all other fields default to unset.
    pub fn new(base_url: impl Into<String>) -> Self {
        Self {
            base_url: base_url.into(),
            ..Self::default()
        }
    }

    /// Set the provider type (`"openai"`, `"azure"`, `"anthropic"`, or `"google"`).
    pub fn with_provider_type(mut self, provider_type: impl Into<String>) -> Self {
        self.provider_type = Some(provider_type.into());
        self
    }

    /// Set the API format (`"completions"` or `"responses"`; openai/azure only).
    pub fn with_wire_api(mut self, wire_api: impl Into<String>) -> Self {
        self.wire_api = Some(wire_api.into());
        self
    }

    /// Set the transport (`"http"` or `"websockets"`) for OpenAI Responses
    /// requests. Defaults to `"http"`.
    pub fn with_transport(mut self, transport: impl Into<String>) -> Self {
        self.transport = Some(transport.into());
        self
    }

    /// Set the product serving the model. Allowed values are `"openai"`,
    /// `"anthropic"`, `"azure_openai"`, `"ollama"`, `"lm_studio"`,
    /// `"foundry_local"`, and `"llama_cpp"`. Only affects telemetry.
    pub fn with_model_provider(mut self, model_provider: impl Into<String>) -> Self {
        self.model_provider = Some(model_provider.into());
        self
    }

    /// Set the API key. Optional for local providers like Ollama.
    pub fn with_api_key(mut self, api_key: impl Into<String>) -> Self {
        self.api_key = Some(api_key.into());
        self
    }

    /// Set the command used to obtain a fresh API key before each provider
    /// request. Bearer authentication takes precedence over this command.
    pub fn with_api_key_command(mut self, command: impl Into<String>) -> Self {
        self.api_key_command = Some(command.into());
        self
    }

    /// Set the bearer token used to populate the `Authorization` header.
    /// Takes precedence over `api_key` when both are set.
    pub fn with_bearer_token(mut self, bearer_token: impl Into<String>) -> Self {
        self.bearer_token = Some(bearer_token.into());
        self
    }

    /// Set the callback used to acquire a bearer token before each outbound
    /// request to this provider.
    ///
    /// **Experimental.** This method is part of an experimental wire-protocol
    /// surface and may change or be removed in a future release.
    pub fn with_bearer_token_provider(mut self, provider: Arc<dyn BearerTokenProvider>) -> Self {
        self.bearer_token_provider = Some(provider);
        self
    }

    /// Set Azure-specific options.
    pub fn with_azure(mut self, azure: AzureProviderOptions) -> Self {
        self.azure = Some(azure);
        self
    }

    /// Set the custom HTTP headers attached to outbound provider requests.
    pub fn with_headers(mut self, headers: HashMap<String, String>) -> Self {
        self.headers = Some(headers);
        self
    }

    /// Set the well-known model ID used to look up agent config and default
    /// token limits. Falls back to the session's configured model when unset.
    pub fn with_model_id(mut self, model_id: impl Into<String>) -> Self {
        self.model_id = Some(model_id.into());
        self
    }

    /// Set the model name sent to the provider API for inference. Use this
    /// when the provider's model name (e.g. an Azure deployment name or a
    /// custom fine-tune name) differs from
    /// [`model_id`](Self::model_id).
    pub fn with_wire_model(mut self, wire_model: impl Into<String>) -> Self {
        self.wire_model = Some(wire_model.into());
        self
    }

    /// Override the resolved model's default max prompt tokens. The
    /// runtime triggers conversation compaction when the prompt would
    /// exceed this limit.
    pub fn with_max_prompt_tokens(mut self, max: i64) -> Self {
        self.max_prompt_tokens = Some(max);
        self
    }

    /// Override the resolved model's default max output tokens. When
    /// hit, the model stops generating and returns a truncated response.
    pub fn with_max_output_tokens(mut self, max: i64) -> Self {
        self.max_output_tokens = Some(max);
        self
    }
}

/// Provider-scoped Copilot API (CAPI) session options.
///
/// WebSocket transport is the default for the CAPI Responses API whenever
/// the model advertises the `ws:/responses` endpoint. Set
/// [`enable_web_socket_responses`](Self::enable_web_socket_responses) to
/// `false` to force the HTTP Responses transport instead, which is useful
/// for users behind proxies where WebSockets fail.
///
/// Setting it to `false` is equivalent to setting the
/// `COPILOT_CLI_DISABLE_WEBSOCKET_RESPONSES` environment variable. The option
/// is scoped under the `capi` namespace because a single session can host
/// multiple providers, so transport choice is provider-level.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct CapiSessionOptions {
    /// Enables Copilot API models independently of GitHub identity.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    /// Routing tier, meaningful only with model `auto` (Auto mode V2).
    /// Requires a runtime version that supports `capi.autoTier`.
    ///
    /// When omitted, the runtime chooses its default on create and restores
    /// the last committed tier on cold resume. On resident resume, a different
    /// tier requests a safe switch that takes effect after resume succeeds and
    /// never disturbs a turn that is already running.
    ///
    /// To change the preference on a live session, use
    /// [`Session::set_auto_tier`](crate::session::Session::set_auto_tier).
    ///
    /// For Copilot Fusion, prefer [`fusion_tier`](Self::fusion_tier). The
    /// `balance`, `hydrafusion` and `hydrafusion-max` identifiers it replaces
    /// remain accepted but are deprecated.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub auto_tier: Option<AutoTier>,

    /// Copilot Fusion tier for model `fusion` or `auto`, matching the CLI's
    /// `--fusion-tier`.
    ///
    /// The runtime resolves the tier to the Auto routing preference it stands
    /// for and then applies it exactly like [`auto_tier`](Self::auto_tier), so
    /// `session.model.getCurrent` and `session.model_change` events report that
    /// preference as `autoTier`. Cannot be combined with `auto_tier`.
    ///
    /// To change the tier on a live session, use
    /// [`Session::set_fusion_tier`](crate::session::Session::set_fusion_tier).
    ///
    /// **Experimental.** Part of an experimental Auto routing surface and may
    /// change or be removed in a future release.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fusion_tier: Option<FusionTier>,

    /// Whether to use WebSocket transport for CAPI Responses API calls.
    ///
    /// When `Some(false)`, the runtime uses HTTP Responses transport even if
    /// the selected model advertises `ws:/responses`. When unset, the runtime
    /// default applies (WebSocket transport when advertised).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub enable_web_socket_responses: Option<bool>,
}

impl CapiSessionOptions {
    /// Construct CAPI session options with all fields unset.
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable or disable Copilot API models without changing GitHub identity.
    pub fn with_enabled(mut self, enabled: bool) -> Self {
        self.enabled = Some(enabled);
        self
    }

    /// Set the routing tier for the `auto` model (Auto mode V2).
    pub fn with_auto_tier(mut self, auto_tier: AutoTier) -> Self {
        self.auto_tier = Some(auto_tier);
        self
    }

    /// Set the Copilot Fusion tier for the `fusion` or `auto` model.
    pub fn with_fusion_tier(mut self, fusion_tier: FusionTier) -> Self {
        self.fusion_tier = Some(fusion_tier);
        self
    }

    /// Set whether to use WebSocket transport for CAPI Responses API calls.
    pub fn with_enable_web_socket_responses(mut self, enable: bool) -> Self {
        self.enable_web_socket_responses = Some(enable);
        self
    }
}

/// Azure-specific provider options.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AzureProviderOptions {
    /// Azure API version. When omitted, the runtime uses the GA versionless v1 route.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub api_version: Option<String>,
}

/// A named BYOK provider connection in the multi-provider registry.
///
/// **Experimental.** Multi-provider BYOK configuration is part of an
/// experimental surface and may change or be removed in a future release.
///
/// Connections coexist with GitHub identity; [`CapiSessionOptions::enabled`]
/// controls Copilot model availability independently.
/// Models are attached via [`ProviderModelConfig`], which references a
/// provider by [`name`](Self::name).
#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct NamedProviderConfig {
    /// Forward-compatible runtime options for a local model.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub local_model: Option<serde_json::Value>,
    /// Unique name used by [`ProviderModelConfig::provider`] to reference
    /// this connection.
    pub name: String,
    /// Provider type: `"openai"`, `"azure"`, `"anthropic"`, or `"google"`. Defaults to
    /// `"openai"` on the CLI.
    #[serde(default, skip_serializing_if = "Option::is_none", rename = "type")]
    pub provider_type: Option<String>,
    /// API format (openai/azure only): `"completions"` or `"responses"`.
    /// Defaults to `"completions"`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub wire_api: Option<String>,
    /// Transport: `"http"` (default), or `"websockets"` for OpenAI Responses.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub transport: Option<String>,
    /// Product serving this provider's models, such as `"ollama"` or
    /// `"lm_studio"`, reported in telemetry as `model_provider`. Allowed values
    /// are `"openai"`, `"anthropic"`, `"azure_openai"`, `"ollama"`,
    /// `"lm_studio"`, `"foundry_local"`, and `"llama_cpp"`; only affects telemetry.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub model_provider: Option<String>,
    /// API endpoint URL.
    pub base_url: String,
    /// API key. Optional for local providers like Ollama.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub api_key: Option<String>,
    /// Command run before each request. Failure fails the request without
    /// static-key fallback. Bearer authentication takes precedence.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub api_key_command: Option<String>,
    /// Bearer token for authentication. Sets the `Authorization` header
    /// directly. Takes precedence over `api_key` when both are set.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub bearer_token: Option<String>,
    /// **Experimental.** Callback used to acquire a bearer token before each
    /// outbound request to this provider.
    #[serde(skip)]
    pub bearer_token_provider: Option<Arc<dyn BearerTokenProvider>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) has_bearer_token_provider: Option<bool>,
    /// Azure-specific options.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub azure: Option<AzureProviderOptions>,
    /// Custom HTTP headers included in outbound provider requests.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub headers: Option<HashMap<String, String>>,
}

impl std::fmt::Debug for NamedProviderConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NamedProviderConfig")
            .field("name", &self.name)
            .field("provider_type", &self.provider_type)
            .field("wire_api", &self.wire_api)
            .field("transport", &self.transport)
            .field("model_provider", &self.model_provider)
            .field("base_url", &self.base_url)
            .field("api_key", &self.api_key)
            .field("bearer_token", &self.bearer_token)
            .field(
                "bearer_token_provider",
                &self.bearer_token_provider.as_ref().map(|_| "<set>"),
            )
            .field("has_bearer_token_provider", &self.has_bearer_token_provider)
            .field("azure", &self.azure)
            .field("headers", &self.headers)
            .finish()
    }
}

impl NamedProviderConfig {
    /// Set runtime options for a local model without changing their JSON shape.
    pub fn with_local_model(mut self, options: serde_json::Value) -> Self {
        self.local_model = Some(options);
        self
    }

    /// Construct a [`NamedProviderConfig`] with the required `name` and
    /// `base_url` set; all other fields default to unset.
    pub fn new(name: impl Into<String>, base_url: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            base_url: base_url.into(),
            ..Self::default()
        }
    }

    /// Set the provider type (`"openai"`, `"azure"`, `"anthropic"`, or `"google"`).
    pub fn with_provider_type(mut self, provider_type: impl Into<String>) -> Self {
        self.provider_type = Some(provider_type.into());
        self
    }

    /// Set the API format (`"completions"` or `"responses"`; openai/azure only).
    pub fn with_wire_api(mut self, wire_api: impl Into<String>) -> Self {
        self.wire_api = Some(wire_api.into());
        self
    }

    /// Set the transport; WebSockets require OpenAI Responses.
    pub fn with_transport(mut self, transport: impl Into<String>) -> Self {
        self.transport = Some(transport.into());
        self
    }

    /// Set a command that obtains a fresh API key before every request.
    pub fn with_api_key_command(mut self, command: impl Into<String>) -> Self {
        self.api_key_command = Some(command.into());
        self
    }

    /// Set the product serving this provider's models. Allowed values are
    /// `"openai"`, `"anthropic"`, `"azure_openai"`, `"ollama"`,
    /// `"lm_studio"`, `"foundry_local"`, and `"llama_cpp"`. Only affects telemetry.
    pub fn with_model_provider(mut self, model_provider: impl Into<String>) -> Self {
        self.model_provider = Some(model_provider.into());
        self
    }

    /// Set the API key. Optional for local providers like Ollama.
    pub fn with_api_key(mut self, api_key: impl Into<String>) -> Self {
        self.api_key = Some(api_key.into());
        self
    }

    /// Set the bearer token used to populate the `Authorization` header.
    /// Takes precedence over `api_key` when both are set.
    pub fn with_bearer_token(mut self, bearer_token: impl Into<String>) -> Self {
        self.bearer_token = Some(bearer_token.into());
        self
    }

    /// Set the callback used to acquire a bearer token before each outbound
    /// request to this provider.
    ///
    /// **Experimental.** This method is part of an experimental wire-protocol
    /// surface and may change or be removed in a future release.
    pub fn with_bearer_token_provider(mut self, provider: Arc<dyn BearerTokenProvider>) -> Self {
        self.bearer_token_provider = Some(provider);
        self
    }

    /// Set Azure-specific options.
    pub fn with_azure(mut self, azure: AzureProviderOptions) -> Self {
        self.azure = Some(azure);
        self
    }

    /// Set the custom HTTP headers attached to outbound provider requests.
    pub fn with_headers(mut self, headers: HashMap<String, String>) -> Self {
        self.headers = Some(headers);
        self
    }
}

fn prepare_bearer_token_providers(
    providers: &mut Option<Vec<NamedProviderConfig>>,
) -> HashMap<String, Arc<dyn BearerTokenProvider>> {
    let mut bearer_token_providers = HashMap::new();

    if let Some(providers) = providers.as_mut() {
        for provider in providers {
            if let Some(token_provider) = provider.bearer_token_provider.take() {
                provider.has_bearer_token_provider = Some(true);
                bearer_token_providers.insert(provider.name.clone(), token_provider);
            }
        }
    }

    bearer_token_providers
}

/// A BYOK model definition in the multi-provider registry.
///
/// **Experimental.** Multi-provider BYOK configuration is part of an
/// experimental surface and may change or be removed in a future release.
///
/// References a [`NamedProviderConfig`] by [`provider`](Self::provider) and
/// becomes selectable under the provider-qualified id `provider/id`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct ProviderModelConfig {
    /// Persisted on-device Auto annotation.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub auto_local: Option<serde_json::Value>,
    /// Provider-published model metadata, including live Auto routing annotations.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
    /// Model identifier, unique within its provider. Combined with
    /// [`provider`](Self::provider) to form the selection id `provider/id`.
    pub id: String,
    /// Name of the [`NamedProviderConfig`] this model is served by.
    pub provider: String,
    /// Model name sent to the provider API for inference. Use when the
    /// provider's model name differs from [`id`](Self::id).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub wire_model: Option<String>,
    /// Well-known model ID used to look up agent config and default token
    /// limits.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub model_id: Option<String>,
    /// Human-readable display name.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Overrides the resolved model's default max prompt tokens.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_prompt_tokens: Option<i64>,
    /// Overrides the resolved model's default max context window tokens.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_context_window_tokens: Option<i64>,
    /// Overrides the resolved model's default max output tokens.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max_output_tokens: Option<i64>,
    /// Per-property overrides for model capabilities, deep-merged over
    /// runtime defaults.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<crate::generated::api_types::ModelCapabilitiesOverride>,
}

impl ProviderModelConfig {
    /// Set the persisted on-device Auto annotation.
    pub fn with_auto_local(mut self, annotation: serde_json::Value) -> Self {
        self.auto_local = Some(annotation);
        self
    }

    /// Set provider-published model metadata.
    pub fn with_metadata(mut self, metadata: HashMap<String, serde_json::Value>) -> Self {
        self.metadata = Some(metadata);
        self
    }

    /// Construct a [`ProviderModelConfig`] with the required `id` and
    /// `provider` set; all other fields default to unset.
    pub fn new(id: impl Into<String>, provider: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            provider: provider.into(),
            ..Self::default()
        }
    }

    /// Set the model name sent to the provider API for inference.
    pub fn with_wire_model(mut self, wire_model: impl Into<String>) -> Self {
        self.wire_model = Some(wire_model.into());
        self
    }

    /// Set the well-known model ID used to look up agent config and default
    /// token limits.
    pub fn with_model_id(mut self, model_id: impl Into<String>) -> Self {
        self.model_id = Some(model_id.into());
        self
    }

    /// Set the human-readable display name.
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Override the resolved model's default max prompt tokens.
    pub fn with_max_prompt_tokens(mut self, max: i64) -> Self {
        self.max_prompt_tokens = Some(max);
        self
    }

    /// Override the resolved model's default max context window tokens.
    pub fn with_max_context_window_tokens(mut self, max: i64) -> Self {
        self.max_context_window_tokens = Some(max);
        self
    }

    /// Override the resolved model's default max output tokens.
    pub fn with_max_output_tokens(mut self, max: i64) -> Self {
        self.max_output_tokens = Some(max);
        self
    }

    /// Set per-property model capability overrides.
    pub fn with_capabilities(
        mut self,
        capabilities: crate::generated::api_types::ModelCapabilitiesOverride,
    ) -> Self {
        self.capabilities = Some(capabilities);
        self
    }
}

/// A single ExP (Experiment Platform) flag value.
///
/// ExP assignments resolve to a string, number, boolean, or null.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ExpFlagValue {
    /// A boolean flag value.
    Bool(bool),
    /// An integer flag value.
    Integer(i64),
    /// A floating-point flag value.
    Float(f64),
    /// A string flag value.
    String(String),
    /// A null flag value.
    Null,
}

/// A single configuration entry in a [`CopilotExpAssignmentResponse`].
///
/// Each entry carries an identifier and a bag of typed parameter values.
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct ExpConfigEntry {
    /// Identifier of the configuration entry.
    pub id: String,
    /// Parameter values keyed by parameter name.
    pub parameters: HashMap<String, ExpFlagValue>,
}

/// ExP ("flight") assignment data, in the same JSON shape the Copilot CLI
/// fetches from the experimentation service.
///
/// Field names serialize as PascalCase (`Features`, `Flights`, ...) to match
/// the on-the-wire contract consumed by the runtime.
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct CopilotExpAssignmentResponse {
    /// Enabled feature names.
    #[serde(default)]
    pub features: Vec<String>,
    /// Assigned flights keyed by flight name.
    #[serde(default)]
    pub flights: HashMap<String, String>,
    /// Configuration entries carrying typed parameter values.
    #[serde(default)]
    pub configs: Vec<ExpConfigEntry>,
    /// Opaque parameter-group payload passed through untouched. Optional.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub parameter_groups: Option<Value>,
    /// Version of the flighting configuration. Optional.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub flighting_version: Option<i64>,
    /// Impression identifier for the assignment. Optional.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub impression_id: Option<String>,
    /// Assignment context string forwarded to CAPI and telemetry.
    #[serde(default)]
    pub assignment_context: String,
}

/// Well-known managed bypass-permissions policies.
pub struct DisableBypassPermissionsModes;

impl DisableBypassPermissionsModes {
    /// Permit automatic bypass but block full allow-all.
    pub const ALLOW_AUTO_ONLY: &'static str = "allow-auto-only";
    /// Turn off bypass-permissions mode entirely.
    pub const DISABLE: &'static str = "disable";
}

/// Permission rules injected as a managed-settings layer at session bootstrap.
///
/// All fields are optional; an omitted field imposes no constraint from this
/// layer. This layer composes restrictively with any server- or device-level
/// managed settings: [`deny`](Self::deny) and [`ask`](Self::ask) rules are
/// unioned across layers, every present [`allow`](Self::allow) list must admit a
/// tool for it to be allowed, and bypass-mode restrictions compose to the most
/// restrictive setting.
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct ManagedSettingsPermissions {
    /// Restricts bypass-permissions mode for the session. See
    /// [`DisableBypassPermissionsModes`] for well-known values. Unknown values
    /// are forwarded so newer runtime policies fail closed.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub disable_bypass_permissions_mode: Option<String>,
    /// When true, prevents Assisted Permissions from being activated.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub disable_assisted_permissions_mode: Option<bool>,
    /// Tool-permission patterns that are always denied.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub deny: Option<Vec<String>>,
    /// Tool-permission patterns that require an explicit ask.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub ask: Option<Vec<String>>,
    /// Tool-permission patterns that are allowed without prompting.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub allow: Option<Vec<String>>,
    /// Closed-world host boundary expressed as `Domain(hostname)`,
    /// `Domain(IP)`, or `Domain(*.example.com)` rules. Schemes, ports, paths,
    /// queries, and fragments are rejected. Multiple managed layers intersect
    /// their lists. A present empty list denies all hosts.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub limit_to: Option<Vec<String>>,
}

impl ManagedSettingsPermissions {
    /// Sets the bypass-permissions policy for this managed layer.
    pub fn with_disable_bypass_permissions_mode(mut self, value: impl Into<String>) -> Self {
        self.disable_bypass_permissions_mode = Some(value.into());
        self
    }

    /// Sets whether this managed layer disables Assisted Permissions.
    pub fn with_disable_assisted_permissions_mode(mut self, value: bool) -> Self {
        self.disable_assisted_permissions_mode = Some(value);
        self
    }

    /// Sets the rules that are always denied.
    pub fn with_deny(mut self, rules: Vec<String>) -> Self {
        self.deny = Some(rules);
        self
    }

    /// Sets the rules that require explicit approval.
    pub fn with_ask(mut self, rules: Vec<String>) -> Self {
        self.ask = Some(rules);
        self
    }

    /// Sets the rules that are allowed without prompting.
    pub fn with_allow(mut self, rules: Vec<String>) -> Self {
        self.allow = Some(rules);
        self
    }

    /// Sets the closed-world domain boundary for this managed layer.
    pub fn with_limit_to(mut self, rules: Vec<String>) -> Self {
        self.limit_to = Some(rules);
        self
    }
}

/// Managed-settings layer injected at session startup. Currently carries only a
/// [`permissions`](Self::permissions) object.
///
/// This layer is startup-only and is not persisted with the session. It must be
/// re-supplied on resume to remain in effect; omitting it on resume clears the
/// previously injected layer. It can be combined with
/// [`SessionGitHubOptions::fetch_managed_settings`]. Older runtimes may ignore this
/// additive field, so hosts must not rely on injected policy until they ship a
/// compatible runtime.
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct ManagedSettings {
    /// Permission rules for this managed-settings layer.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub permissions: Option<ManagedSettingsPermissions>,
}

impl ManagedSettings {
    /// Sets the permissions-only managed policy.
    pub fn with_permissions(mut self, permissions: ManagedSettingsPermissions) -> Self {
        self.permissions = Some(permissions);
        self
    }
}

/// Selects the model-facing shape of the built-in `ask_user` tool.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
#[non_exhaustive]
pub enum AskUserVariant {
    /// Use the legacy user-input request flow.
    #[default]
    Legacy,
    /// Use the elicitation request flow.
    Elicitation,
}

/// Scope of a working-directory trust grant made during session creation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum WorkingDirectoryTrust {
    /// Trust the directory for this session only.
    Session,
    /// Persist trust in the user's profile.
    User,
}

/// Model tuning and provider catalog. The selected model stays on the session options.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionInferenceOptions {
    /// Whether the model may request multiple tool calls per response (default true).
    /// False is a provider hint, not an execution guarantee: all returned calls
    /// are handled and handlers may run concurrently. Re-supply on cold resume.
    pub allow_multiple_tool_calls: Option<bool>,
    /// Desired reasoning effort to apply after resuming the session.
    pub reasoning_effort: Option<String>,
    /// Reasoning summary mode to apply after resuming the session. Use
    /// [`ReasoningSummary::None`] to suppress summary output regardless of
    /// whether reasoning is enabled.
    pub reasoning_summary: Option<ReasoningSummary>,
    /// Context window tier to apply after resuming the session. Use
    /// `"long_context"` to pin the session to the long-context tier.
    pub context_tier: Option<String>,
    /// Named connection for unqualified BYOK selections. Requires
    /// `github.capi.enabled=false`. `None` creates no default and preserves it on
    /// resume; `Some(None)` clears it. Does not select the initial model.
    /// Pass-through uses inferred metadata and session processing, without
    /// provider-wide model defaults.
    pub default_provider: Option<Option<String>>,
    /// **Experimental.** This field is part of an experimental multi-provider
    /// BYOK surface and may change or be removed in a future release.
    ///
    /// Re-supply named BYOK provider connections on resume. Additive to
    /// the default Copilot routing. Referenced by [`models`](Self::models).
    pub providers: Option<Vec<NamedProviderConfig>>,
    /// **Experimental.** This field is part of an experimental multi-provider
    /// BYOK surface and may change or be removed in a future release.
    ///
    /// Re-supply BYOK model definitions on resume, each referencing a
    /// [`providers`](Self::providers) entry by name.
    pub models: Option<Vec<ProviderModelConfig>>,
    /// **Experimental.** Enables native model citations for supported providers.
    pub enable_citations: Option<bool>,
    /// Per-property model capability overrides on resume.
    pub capabilities: Option<crate::generated::api_types::ModelCapabilitiesOverride>,
}
impl std::fmt::Debug for SessionInferenceOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionInferenceOptions")
            .finish_non_exhaustive()
    }
}
impl SessionInferenceOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set [`allow_multiple_tool_calls`](Self::allow_multiple_tool_calls).
    pub fn with_allow_multiple_tool_calls(mut self, allowed: bool) -> Self {
        self.allow_multiple_tool_calls = Some(allowed);
        self
    }

    /// Set the reasoning effort to apply on resume.
    pub fn with_reasoning_effort(mut self, effort: impl Into<String>) -> Self {
        self.reasoning_effort = Some(effort.into());
        self
    }

    /// Set [`reasoning_summary`](Self::reasoning_summary).
    pub fn with_reasoning_summary(mut self, summary: ReasoningSummary) -> Self {
        self.reasoning_summary = Some(summary);
        self
    }

    /// Set the context window tier to apply on resume (e.g. `"default"`,
    /// `"long_context"`).
    pub fn with_context_tier(mut self, tier: impl Into<String>) -> Self {
        self.context_tier = Some(tier.into());
        self
    }

    /// Set the named connection for unqualified BYOK selections.
    pub fn with_default_provider(mut self, name: impl Into<String>) -> Self {
        self.default_provider = Some(Some(name.into()));
        self
    }

    /// Explicitly clear the default connection, including on resume.
    pub fn clear_default_provider(mut self) -> Self {
        self.default_provider = Some(None);
        self
    }

    /// **Experimental.** This method is part of an experimental multi-provider
    /// BYOK surface and may change or be removed in a future release.
    ///
    /// Re-supply the named BYOK provider connections on resume. Attach
    /// models referencing these with [`Self::with_models`].
    pub fn with_providers(mut self, providers: Vec<NamedProviderConfig>) -> Self {
        self.providers = Some(providers);
        self
    }

    /// **Experimental.** This method is part of an experimental multi-provider
    /// BYOK surface and may change or be removed in a future release.
    ///
    /// Re-supply the BYOK model definitions on resume, each referencing a
    /// named provider supplied via [`Self::with_providers`].
    pub fn with_models(mut self, models: Vec<ProviderModelConfig>) -> Self {
        self.models = Some(models);
        self
    }

    /// **Experimental.** Enable native model citations for supported providers on resume.
    pub fn with_enable_citations(mut self, enable: bool) -> Self {
        self.enable_citations = Some(enable);
        self
    }

    /// Set per-property model capability overrides on resume.
    pub fn with_capabilities(
        mut self,
        capabilities: crate::generated::api_types::ModelCapabilitiesOverride,
    ) -> Self {
        self.capabilities = Some(capabilities);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionPolicyOptions {
    /// Exact model IDs the resumed session may use. When unset, the host
    /// imposes no model restriction. The runtime validates configured IDs,
    /// rejects an explicit empty list, and intersects the list with applicable
    /// model policies.
    pub allowed_models: Option<Vec<String>>,
    /// Exact dotted setting paths to exclude, as on [`CreateSessionOptions`].
    /// Omitting restores full enforcement on warm and cold resume.
    pub excluded_managed_settings: Option<Vec<String>>,
    /// Re-applies managed model lock enforcement on resume. See
    /// [`SessionPolicyOptions::enforce_managed_model_defaults`].
    pub enforce_managed_model_defaults: Option<bool>,
    /// Optional managed-settings layer injected on resume. See
    /// [`SessionPolicyOptions::managed_settings`]. This layer is not persisted, so it
    /// must be re-supplied on resume to remain in effect; omitting it clears the
    /// previously injected layer. Serialized on the wire as `managedSettings`.
    /// Set via [`with_managed_settings`](Self::with_managed_settings).
    pub managed_settings: Option<ManagedSettings>,
}
impl std::fmt::Debug for SessionPolicyOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionPolicyOptions")
            .finish_non_exhaustive()
    }
}
impl SessionPolicyOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Restrict the resumed session to the provided exact model IDs.
    ///
    /// Passing an empty iterator sends an explicit empty list, which the
    /// runtime rejects.
    pub fn with_allowed_models<I, S>(mut self, models: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.allowed_models = Some(models.into_iter().map(Into::into).collect());
        self
    }

    /// Exclude exact managed-setting paths on resume, as on [`CreateSessionOptions`].
    pub fn with_excluded_managed_settings<I, S>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.excluded_managed_settings = Some(paths.into_iter().map(Into::into).collect());
        self
    }

    /// Enforce locked managed model controls after resume.
    pub fn with_enforce_managed_model_defaults(mut self, enforce: bool) -> Self {
        self.enforce_managed_model_defaults = Some(enforce);
        self
    }

    /// Inject a managed-settings layer (currently permission rules) on resume.
    /// See [`SessionPolicyOptions::with_managed_settings`]. Must be re-supplied on
    /// resume; omitting it clears the previously injected layer.
    pub fn with_managed_settings(mut self, managed_settings: ManagedSettings) -> Self {
        self.managed_settings = Some(managed_settings);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionEventOptions {
    /// Enable streaming token deltas.
    pub streaming: Option<bool>,
    /// Forward sub-agent streaming events to this connection on resume.
    pub include_sub_agent_streaming_events: Option<bool>,
    /// Capacity of each subscriber's event buffer; omission uses the SDK default.
    pub buffer_capacity: Option<usize>,
}
impl std::fmt::Debug for SessionEventOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionEventOptions")
            .finish_non_exhaustive()
    }
}
impl SessionEventOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable streaming token deltas via `assistant.message_delta` events.
    pub fn with_streaming(mut self, streaming: bool) -> Self {
        self.streaming = Some(streaming);
        self
    }

    /// Forward sub-agent streaming events to this connection on resume.
    pub fn with_include_sub_agent_streaming_events(mut self, include: bool) -> Self {
        self.include_sub_agent_streaming_events = Some(include);
        self
    }

    /// Set [`Self::buffer_capacity`].
    ///
    /// A capacity of `0` is rejected with
    /// [`ErrorKind::InvalidConfig`](crate::ErrorKind::InvalidConfig) by
    /// [`Client::prepare_resume_session`](crate::Client::prepare_resume_session)
    /// and [`Client::resume_session`](crate::Client::resume_session); the
    /// value is never clamped.
    pub fn with_buffer_capacity(mut self, capacity: usize) -> Self {
        self.buffer_capacity = Some(capacity);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionInteractionOptions {
    /// Selects the model-facing shape of the built-in `ask_user` tool on a cold resume.
    ///
    /// When omitted, the runtime uses [`AskUserVariant::Legacy`]. To use
    /// [`AskUserVariant::Elicitation`], also install an [`ElicitationHandler`].
    pub ask_user_variant: Option<AskUserVariant>,
    /// Slash commands registered for this session on resume. See
    /// [`SessionInteractionOptions::commands`] — commands are not persisted server-side,
    /// so the resume payload re-supplies the registration.
    pub commands: Option<Vec<CommandDefinition>>,
    /// Optional elicitation handler. See
    /// [`SessionInteractionOptions::on_elicitation_request`].
    pub on_elicitation_request: Option<Arc<dyn ElicitationHandler>>,
    /// Optional user-input handler. See
    /// [`SessionInteractionOptions::on_user_input_request`].
    pub on_user_input_request: Option<Arc<dyn UserInputHandler>>,
    /// Optional exit-plan-mode handler. See
    /// [`SessionInteractionOptions::on_exit_plan_mode_request`].
    pub on_exit_plan_mode_request: Option<Arc<dyn ExitPlanModeHandler>>,
    /// Optional auto-mode-switch handler. See
    /// [`SessionInteractionOptions::on_auto_mode_switch_request`].
    pub on_auto_mode_switch_request: Option<Arc<dyn AutoModeSwitchHandler>>,
    /// Optional shell-authentication handler. See
    /// [`SessionInteractionOptions::on_shell_authentication_request`].
    pub on_shell_authentication_request: Option<Arc<dyn ShellAuthenticationHandler>>,
    /// Re-enroll in retained-request recovery. Requires an auto-mode-switch handler.
    pub auto_mode_recovery: bool,
}
impl std::fmt::Debug for SessionInteractionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionInteractionOptions")
            .finish_non_exhaustive()
    }
}
impl SessionInteractionOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Install an [`ElicitationHandler`] for the resumed session.
    pub fn with_on_elicitation_request(mut self, handler: Arc<dyn ElicitationHandler>) -> Self {
        self.on_elicitation_request = Some(handler);
        self
    }

    /// Install a [`UserInputHandler`] for the resumed session.
    pub fn with_on_user_input_request(mut self, handler: Arc<dyn UserInputHandler>) -> Self {
        self.on_user_input_request = Some(handler);
        self
    }

    /// Select the model-facing shape of the built-in `ask_user` tool on resume.
    pub fn with_ask_user_variant(mut self, variant: AskUserVariant) -> Self {
        self.ask_user_variant = Some(variant);
        self
    }

    /// Install an [`ExitPlanModeHandler`] for the resumed session.
    pub fn with_on_exit_plan_mode_request(mut self, handler: Arc<dyn ExitPlanModeHandler>) -> Self {
        self.on_exit_plan_mode_request = Some(handler);
        self
    }

    /// Install an [`AutoModeSwitchHandler`] for the resumed session.
    pub fn with_on_auto_mode_switch_request(
        mut self,
        handler: Arc<dyn AutoModeSwitchHandler>,
    ) -> Self {
        self.on_auto_mode_switch_request = Some(handler);
        self
    }

    /// Install a [`ShellAuthenticationHandler`] for the resumed session.
    pub fn with_on_shell_authentication_request(
        mut self,
        handler: Arc<dyn ShellAuthenticationHandler>,
    ) -> Self {
        self.on_shell_authentication_request = Some(handler);
        self
    }

    /// Re-enroll the resumed session in runtime-owned Auto recovery.
    /// Requires [`Self::on_auto_mode_switch_request`] and runtime acknowledgment.
    pub fn with_auto_mode_recovery(mut self, enabled: bool) -> Self {
        self.auto_mode_recovery = enabled;
        self
    }

    /// Register slash commands for the resumed session. See
    /// [`SessionInteractionOptions::with_commands`] — commands are not persisted
    /// server-side, so the resume payload re-supplies the registration.
    pub fn with_commands(mut self, commands: Vec<CommandDefinition>) -> Self {
        self.commands = Some(commands);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionToolOptions {
    /// Client-defined tool declarations to re-supply on resume.
    pub definitions: Option<Vec<Tool>>,
    /// Allowlist of tool names the agent may use.
    pub available_tools: Option<Vec<String>>,
    /// Blocklist of built-in tool names.
    pub excluded_tools: Option<Vec<String>>,
    /// Configuration for large tool output handling, forwarded to the CLI on resume.
    pub large_output: Option<LargeToolOutputConfig>,
    /// Overrides the runtime's built-in tool-search behavior on resume. When
    /// unset, the runtime default applies.
    pub search: Option<ToolSearchConfig>,
    /// **Experimental.** Non-persisted image generation opt-in. Re-supply after runtime restart.
    pub image_generation: Option<ImageGenerationConfig>,
    /// Enable or disable the scheduling tool.
    pub manage_schedule_enabled: Option<bool>,
}
impl std::fmt::Debug for SessionToolOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionToolOptions").finish_non_exhaustive()
    }
}
impl SessionToolOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Re-supply client-defined tools on resume.
    pub fn with_definitions<I: IntoIterator<Item = Tool>>(mut self, tools: I) -> Self {
        self.definitions = Some(tools.into_iter().collect());
        self
    }

    /// Set the allowlist of tool names the agent may use.
    pub fn with_available_tools<I, S>(mut self, tools: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.available_tools = Some(tools.into_iter().map(Into::into).collect());
        self
    }

    /// Set the blocklist of built-in tool names the agent must not use.
    pub fn with_excluded_tools<I, S>(mut self, tools: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.excluded_tools = Some(tools.into_iter().map(Into::into).collect());
        self
    }

    /// Set the [`LargeToolOutputConfig`] forwarded to the CLI on resume.
    pub fn with_large_output(mut self, config: LargeToolOutputConfig) -> Self {
        self.large_output = Some(config);
        self
    }

    /// Set the [`ToolSearchConfig`] overriding the runtime's built-in
    /// tool-search behavior on resume.
    pub fn with_search(mut self, config: ToolSearchConfig) -> Self {
        self.search = Some(config);
        self
    }

    /// **Experimental.** Set the image generation opt-in on resume, including after runtime restart.
    pub fn with_image_generation(mut self, config: ImageGenerationConfig) -> Self {
        self.image_generation = Some(config);
        self
    }

    /// Set [`Self::manage_schedule_enabled`].
    pub fn with_manage_schedule_enabled(mut self, value: bool) -> Self {
        self.manage_schedule_enabled = Some(value);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionCanvasOptions {
    /// Canvas declarations this connection provides to the runtime.
    pub definitions: Option<Vec<CanvasDeclaration>>,
    /// Provider-side canvas lifecycle handler. The SDK routes inbound
    /// `canvas.open` / `canvas.close` / `canvas.action.invoke` requests to
    /// this handler. Use [`with_canvas_handler`](Self::with_handler)
    /// to install one.
    pub handler: Option<Arc<dyn CanvasHandler>>,
    /// Request canvas renderer tools for this connection.
    pub request_renderer: Option<bool>,
    /// Stable identity for a host/SDK connection that supplies built-in
    /// canvases, so they survive reconnect and CLI restart.
    pub provider: Option<CanvasProviderIdentity>,
}
impl std::fmt::Debug for SessionCanvasOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionCanvasOptions")
            .finish_non_exhaustive()
    }
}
impl SessionCanvasOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set canvas declarations for this connection. The runtime advertises
    /// these to the agent; install a [`CanvasHandler`] via
    /// [`with_canvas_handler`](Self::with_handler) to receive the
    /// resulting provider callbacks.
    pub fn with_definitions<I: IntoIterator<Item = CanvasDeclaration>>(
        mut self,
        canvases: I,
    ) -> Self {
        self.definitions = Some(canvases.into_iter().collect());
        self
    }

    /// Install the provider-side [`CanvasHandler`] for this session.
    pub fn with_handler(mut self, handler: Arc<dyn CanvasHandler>) -> Self {
        self.handler = Some(handler);
        self
    }

    /// Request host canvas renderer tools for this connection.
    pub fn with_request_renderer(mut self, request: bool) -> Self {
        self.request_renderer = Some(request);
        self
    }

    /// Set the canvas provider identity for this connection so host-supplied
    /// canvases survive reconnect and CLI restart.
    pub fn with_provider(mut self, canvas_provider: CanvasProviderIdentity) -> Self {
        self.provider = Some(canvas_provider);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionExtensionOptions {
    /// Request extension tools and dispatch for this connection.
    pub enabled: Option<bool>,
    /// Optional override path to a `copilot-sdk/` folder to inject into
    /// extension subprocesses for this session on resume. See
    /// `SessionExtensionOptions::sdk_path`.
    pub sdk_path: Option<String>,
    /// Stable extension identity for canvas/tool providers on this connection.
    pub identity: Option<ExtensionInfo>,
}
impl std::fmt::Debug for SessionExtensionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionExtensionOptions")
            .finish_non_exhaustive()
    }
}
impl SessionExtensionOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Request extension tools and dispatch for this connection on resume.
    pub fn with_enabled(mut self, request: bool) -> Self {
        self.enabled = Some(request);
        self
    }

    /// Override the bundled `@microsoft/copilot-sdk` drop injected into extension
    /// subprocesses for this resumed session. Invalid paths fall back to the
    /// bundled SDK silently.
    pub fn with_sdk_path(mut self, path: impl Into<String>) -> Self {
        self.sdk_path = Some(path.into());
        self
    }

    /// Set stable extension identity metadata for this connection on resume.
    pub fn with_identity(mut self, extension_info: ExtensionInfo) -> Self {
        self.identity = Some(extension_info);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionAgentOptions {
    /// Names of built-in agents to exclude from the resumed session.
    ///
    /// Excluded built-in agents are hidden from discovery and cannot be
    /// selected or invoked unless a custom agent with the same name is
    /// configured.
    pub excluded_builtin_agents: Option<Vec<String>>,
    /// Built-in subagent names to allow, subject to availability and exclusions.
    /// `Some(vec![])` disables all built-in subagents; custom agents remain
    /// available, including same-name agents. `None` uses defaults on cold
    /// resume and preserves the current allowlist on warm resume.
    /// Not persisted; re-supply on resume after restart.
    pub included_builtin_agents: Option<Vec<String>>,
    /// Explicit discovery-only agent roots, separate from `custom_agents` objects.
    /// No general filesystem grant. Available without `enable_config_discovery`,
    /// subject to source/managed disables. Relative paths resolve once when supplied
    /// against the request's effective `working_directory`; retained roots do not move.
    /// `None` and omission are equivalent; an empty list clears only the explicit SDK contribution.
    pub directories: Option<Vec<PathBuf>>,
    /// Custom agents to re-supply on resume.
    pub definitions: Option<Vec<CustomAgentConfig>>,
    /// Configures the built-in default agent on resume.
    pub default: Option<DefaultAgentConfig>,
    /// Name of the custom agent to activate.
    pub selected: Option<String>,
    /// Limit custom-agent discovery to local sources.
    pub local_discovery_only: Option<bool>,
}
impl std::fmt::Debug for SessionAgentOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionAgentOptions")
            .finish_non_exhaustive()
    }
}
impl SessionAgentOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the built-in agent names to exclude from the resumed session.
    pub fn with_excluded_builtin_agents<I, S>(mut self, agents: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.excluded_builtin_agents = Some(agents.into_iter().map(Into::into).collect());
        self
    }

    /// Set the built-in subagent allowlist. An empty iterator disables all
    /// built-in subagents without restricting custom agents.
    pub fn with_included_builtin_agents<I, S>(mut self, agents: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.included_builtin_agents = Some(agents.into_iter().map(Into::into).collect());
        self
    }

    /// Set explicit discovery-only agent roots. See [`Self::directories`].
    pub fn with_directories<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.directories = Some(paths.into_iter().map(Into::into).collect());
        self
    }

    /// Re-supply custom agents on resume.
    pub fn with_definitions<I: IntoIterator<Item = CustomAgentConfig>>(
        mut self,
        agents: I,
    ) -> Self {
        self.definitions = Some(agents.into_iter().collect());
        self
    }

    /// Configure the built-in default agent on resume.
    pub fn with_default(mut self, agent: DefaultAgentConfig) -> Self {
        self.default = Some(agent);
        self
    }

    /// Activate a named custom agent on resume.
    pub fn with_selected(mut self, name: impl Into<String>) -> Self {
        self.selected = Some(name.into());
        self
    }

    /// Set [`Self::local_discovery_only`].
    pub fn with_local_discovery_only(mut self, value: bool) -> Self {
        self.local_discovery_only = Some(value);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionSkillOptions {
    /// Built-in skill names to include in the resumed session. In
    /// [`ClientMode::Empty`](crate::ClientMode::Empty), `None` excludes all
    /// runtime-bundled skills; `Some` opts the named built-ins back in.
    pub included_builtin_skills: Option<Vec<String>>,
    /// When true, enables skills on resume.
    pub enabled: Option<bool>,
    /// **Experimental.** Supplies session-scoped skills from the SDK host.
    ///
    /// The provider is runtime-only and is not serialized. Re-supply it on
    /// each resume because providers are not persisted by the runtime.
    pub provider: Option<Arc<dyn SkillProvider>>,
    /// Explicit discovery-only skill roots, not general filesystem grants.
    /// Available without `enable_config_discovery`, subject to skill/managed disables.
    /// Relative paths resolve once when supplied against the request's effective
    /// `working_directory`; retained roots do not move on cwd changes.
    /// `None` and omission are equivalent; an empty list clears only the explicit SDK contribution.
    pub directories: Option<Vec<PathBuf>>,
    /// Skill names to disable on resume.
    pub disabled_skills: Option<Vec<String>>,
}
impl std::fmt::Debug for SessionSkillOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionSkillOptions")
            .finish_non_exhaustive()
    }
}
impl SessionSkillOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set [`Self::enabled`].
    pub fn with_enabled(mut self, value: bool) -> Self {
        self.enabled = Some(value);
        self
    }

    /// Install an experimental session-scoped [`SkillProvider`].
    ///
    /// The provider is not persisted and must be re-supplied on each resume.
    pub fn with_provider(mut self, provider: Arc<dyn SkillProvider>) -> Self {
        self.provider = Some(provider);
        self
    }

    /// Set skill directory paths passed through to the CLI on resume.
    pub fn with_directories<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.directories = Some(paths.into_iter().map(Into::into).collect());
        self
    }

    /// Set the runtime-bundled skill allowlist on resume.
    pub fn with_included_builtin_skills<I, S>(mut self, names: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.included_builtin_skills = Some(names.into_iter().map(Into::into).collect());
        self
    }

    /// Set the names of skills to disable on resume.
    pub fn with_disabled_skills<I, S>(mut self, names: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.disabled_skills = Some(names.into_iter().map(Into::into).collect());
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionMcpOptions {
    /// Re-supply MCP servers so they remain available after app restart.
    pub servers: Option<IndexMap<String, McpServerConfig>>,
    /// Re-supply the host OAuth client metadata document URL on resume.
    ///
    /// Set this to the same host identity used when the session was created.
    /// When unset, the SDK does not supply a first-party host identity.
    /// OAuth client metadata URL used for MCP authentication.
    pub auth_client_id_metadata_url: Option<String>,
    /// **Experimental.** This option is part of an experimental wire-protocol
    /// surface (SEP-1865) and may change or be removed in a future release.
    ///
    /// Enable MCP Apps (SEP-1865) UI passthrough on resume. See
    /// Defaults to `None` (treated as `false`).
    pub enable_apps: Option<bool>,
    /// Exact MCP server names to disable on resume. This prevents startup and
    /// authentication during a cold resume, but cannot stop resident servers.
    pub disabled_servers: Option<Vec<String>>,
    /// Optional MCP OAuth handler.
    pub on_auth_request: Option<Arc<dyn McpAuthHandler>>,
    /// Controls how MCP OAuth tokens are stored for this session.
    /// Storage mode for MCP OAuth tokens.
    pub oauth_token_storage: Option<String>,
}
impl std::fmt::Debug for SessionMcpOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionMcpOptions").finish_non_exhaustive()
    }
}
impl SessionMcpOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Install an [`McpAuthHandler`] for host-provided MCP OAuth tokens.
    pub fn with_on_auth_request(mut self, handler: Arc<dyn McpAuthHandler>) -> Self {
        self.on_auth_request = Some(handler);
        self
    }

    /// Re-supply MCP server configurations on resume.
    pub fn with_servers(mut self, servers: IndexMap<String, McpServerConfig>) -> Self {
        self.servers = Some(servers);
        self
    }

    /// Set the host OAuth client metadata document URL on resume.
    pub fn with_auth_client_id_metadata_url(mut self, url: impl Into<String>) -> Self {
        self.auth_client_id_metadata_url = Some(url.into());
        self
    }

    /// **Experimental.** This method is part of an experimental wire-protocol
    /// surface (SEP-1865) and may change or be removed in a future release.
    ///
    /// Enable MCP Apps (SEP-1865) UI passthrough on resume. Defaults to
    /// `None` (treated as `false`). See [`Self::enable_apps`].
    pub fn with_enable_apps(mut self, enable: bool) -> Self {
        self.enable_apps = Some(enable);
        self
    }

    /// Set exact MCP server names to disable for this session.
    pub fn with_disabled_servers<I, S>(mut self, names: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.disabled_servers = Some(names.into_iter().map(Into::into).collect());
        self
    }

    /// Set MCP OAuth token storage to `"persistent"` (OS keychain) or
    /// `"in-memory"` (discarded when the session ends).
    /// Empty mode defaults to `"in-memory"` at session creation/resume time.
    pub fn with_oauth_token_storage(mut self, mode: impl Into<String>) -> Self {
        self.oauth_token_storage = Some(mode.into());
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct CreateSessionFilesystemOptions {
    /// Trust the working directory for this session (`"session"`) or persistently
    /// for the user (`"user"`). `None` leaves existing folder trust unchanged.
    pub trust_working_directory: Option<WorkingDirectoryTrust>,
    /// Opts in to capturing file changes from the first turn for session rewind
    /// and cumulative session diff.
    pub track_file_changes: Option<bool>,
    /// Additional directories the agent may access beyond the working directory.
    /// Relative paths resolve against the session working directory. Re-supply
    /// them when resuming a session.
    pub additional_directories: Option<Vec<PathBuf>>,
}
impl std::fmt::Debug for CreateSessionFilesystemOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CreateSessionFilesystemOptions")
            .finish_non_exhaustive()
    }
}
impl CreateSessionFilesystemOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Trust the working directory for this session (`"session"`) or user (`"user"`).
    pub fn with_trust_working_directory(mut self, trust: WorkingDirectoryTrust) -> Self {
        self.trust_working_directory = Some(trust);
        self
    }

    /// Opt in to capturing file changes from the first turn for session rewind
    /// and cumulative session diff.
    pub fn with_track_file_changes(mut self, enable: bool) -> Self {
        self.track_file_changes = Some(enable);
        self
    }

    /// Set directories the agent may access beyond the working directory.
    pub fn with_additional_directories<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.additional_directories = Some(paths.into_iter().map(Into::into).collect());
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionStateOptions {
    /// When true, skips embedding retrieval on resume.
    pub skip_embedding_retrieval: Option<bool>,
    /// Controls how the embedding cache is stored for this session.
    pub embedding_cache_storage: Option<String>,
    /// When true, enables the session store on resume.
    pub enable_cross_session_store: Option<bool>,
    /// Re-supply infinite session configuration on resume.
    pub infinite_sessions: Option<InfiniteSessionConfig>,
    /// Per-session configuration for the runtime memory feature on resume.
    pub memory: Option<MemoryConfiguration>,
    /// **Experimental.** Stores memories in the app instead of the built-in
    /// memory backends. Used only when [`memory`](Self::memory) is enabled.
    ///
    /// The store is not persisted and must be re-supplied on each resume.
    pub memory_store: Option<Arc<dyn MemoryStore>>,
    /// Custom session filesystem provider. Required on resume when the
    /// [`Client`](crate::Client) was started with
    /// [`ClientOptions::session_fs`](crate::ClientOptions::session_fs).
    /// Session-scoped filesystem provider; re-supply on resume.
    pub session_fs_provider: Option<Arc<dyn SessionFsProvider>>,
}
impl std::fmt::Debug for SessionStateOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionStateOptions")
            .finish_non_exhaustive()
    }
}
impl SessionStateOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Install a [`SessionFsProvider`] backing the resumed session's
    /// filesystem. See [`Self::session_fs_provider`].
    pub fn with_session_fs_provider(mut self, provider: Arc<dyn SessionFsProvider>) -> Self {
        self.session_fs_provider = Some(provider);
        self
    }

    /// Set embedding cache storage mode on resume.
    pub fn with_embedding_cache_storage(
        mut self,
        embedding_cache_storage: impl Into<String>,
    ) -> Self {
        self.embedding_cache_storage = Some(embedding_cache_storage.into());
        self
    }

    /// Set [`Self::skip_embedding_retrieval`].
    pub fn with_skip_embedding_retrieval(mut self, value: bool) -> Self {
        self.skip_embedding_retrieval = Some(value);
        self
    }

    /// Set [`Self::enable_cross_session_store`].
    pub fn with_enable_cross_session_store(mut self, value: bool) -> Self {
        self.enable_cross_session_store = Some(value);
        self
    }

    /// Re-supply infinite session configuration on resume.
    pub fn with_infinite_sessions(mut self, config: InfiniteSessionConfig) -> Self {
        self.infinite_sessions = Some(config);
        self
    }

    /// Install an experimental [`MemoryStore`] on resume; it is not persisted and must be re-supplied on each resume.
    ///
    /// Requires memory enabled with [`Self::with_memory`]; otherwise session setup
    /// fails before any request is sent. Not supported for cloud sessions.
    pub fn with_memory_store(mut self, store: Arc<dyn MemoryStore>) -> Self {
        self.memory_store = Some(store);
        self
    }

    /// Configure the runtime memory feature for the resumed session.
    pub fn with_memory(mut self, memory: MemoryConfiguration) -> Self {
        self.memory = Some(memory);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct CreateSessionInstructionOptions {
    /// Organization-level custom instructions to apply to this session.
    pub organization_text: Option<String>,
    /// Invalidate the runtime process-wide instruction discovery cache before
    /// constructing this new session. `None` or `Some(false)` retains cache reuse.
    ///
    /// Available only on creation configs, not resume configs. Other sessions in
    /// this runtime may observe updated instructions on later turns or discovery.
    /// This does not watch files or override instruction-loading policy.
    pub refresh_custom_instructions: Option<bool>,
    /// When true, enables on-demand instruction discovery for this session.
    pub on_demand_discovery: Option<bool>,
    /// Explicit discovery-only instruction roots using existing SDK formats.
    /// No general filesystem grant. Available without `enable_config_discovery`,
    /// subject to instruction/managed disables. Relative paths resolve once when supplied
    /// against the request's effective `working_directory`; retained roots do not move.
    /// `None` and omission are equivalent; an empty list clears only the explicit SDK contribution.
    pub directories: Option<Vec<PathBuf>>,
    /// Whether to skip loading custom-instruction sources for this session.
    /// Applied via `session.options.update` after create/resume. Defaults to
    /// `true` in [`crate::ClientMode::Empty`] when unset.
    pub skip_custom_instructions: Option<bool>,
}
impl std::fmt::Debug for CreateSessionInstructionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CreateSessionInstructionOptions")
            .finish_non_exhaustive()
    }
}
impl CreateSessionInstructionOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set [`Self::organization_text`].
    pub fn with_organization_text(mut self, instructions: impl Into<String>) -> Self {
        self.organization_text = Some(instructions.into());
        self
    }

    /// Set [`Self::refresh_custom_instructions`] for this new session.
    pub fn with_refresh_custom_instructions(mut self, refresh: bool) -> Self {
        self.refresh_custom_instructions = Some(refresh);
        self
    }

    /// Set [`Self::on_demand_discovery`].
    pub fn with_on_demand_discovery(mut self, value: bool) -> Self {
        self.on_demand_discovery = Some(value);
        self
    }

    /// Set additional directories to search for custom instruction files.
    /// Forwarded to the CLI on session create; not the same as
    /// [`with_skill_directories`](SessionSkillOptions::with_directories).
    pub fn with_directories<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.directories = Some(paths.into_iter().map(Into::into).collect());
        self
    }

    /// Set [`Self::skip_custom_instructions`].
    pub fn with_skip_custom_instructions(mut self, value: bool) -> Self {
        self.skip_custom_instructions = Some(value);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionHookOptions {
    /// Enables repository-origin file hooks from `.github/hooks/` and trusted
    /// repository-configured roots, not explicit `hook_directories` or callback hooks.
    pub enable_repository_hooks: Option<bool>,
    /// Explicit discovery-only file-hook roots, separate from callback `hooks`.
    /// No general filesystem grant. Available without `enable_config_discovery` or
    /// `enable_file_hooks` (repository-origin hooks only), subject to hook/managed disables.
    /// Relative paths resolve once when supplied against the request's effective
    /// `working_directory`; retained roots do not move on cwd changes.
    /// `None` and omission are equivalent; an empty list clears only the explicit SDK contribution.
    pub directories: Option<Vec<PathBuf>>,
    /// Enable session hooks on resume.
    pub has_handlers: Option<bool>,
    /// Session hook handler. Re-supply on resume.
    pub handlers: Option<Arc<dyn SessionHooks>>,
}
impl std::fmt::Debug for SessionHookOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionHookOptions").finish_non_exhaustive()
    }
}
impl SessionHookOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Install a [`SessionHooks`] handler. Automatically enables the
    /// wire-level `hooks` flag on session resumption.
    pub fn with_handlers(mut self, hooks: Arc<dyn SessionHooks>) -> Self {
        self.handlers = Some(hooks);
        self
    }

    /// Set [`Self::enable_repository_hooks`].
    pub fn with_enable_repository_hooks(mut self, value: bool) -> Self {
        self.enable_repository_hooks = Some(value);
        self
    }

    /// Set explicit discovery-only file-hook roots. See [`Self::directories`].
    pub fn with_directories<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.directories = Some(paths.into_iter().map(Into::into).collect());
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionGitOptions {
    /// When true, allows host Git operations on resume.
    pub enable_host_git_operations: Option<bool>,
    /// Enable or disable Git coauthor trailers.
    pub coauthor_enabled: Option<bool>,
}
impl std::fmt::Debug for SessionGitOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionGitOptions").finish_non_exhaustive()
    }
}
impl SessionGitOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set [`Self::enable_host_git_operations`].
    pub fn with_enable_host_git_operations(mut self, value: bool) -> Self {
        self.enable_host_git_operations = Some(value);
        self
    }

    /// Set [`Self::coauthor_enabled`].
    pub fn with_coauthor_enabled(mut self, value: bool) -> Self {
        self.coauthor_enabled = Some(value);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionGitHubOptions {
    /// Configuration for the built-in GitHub MCP server.
    ///
    /// `disable_form_deferral` only applies to that server and only has an
    /// effect when MCP Apps and form-backed GitHub tools are enabled.
    pub mcp: Option<GitHubMcpToolConfig>,
    /// Re-supply provider-scoped CAPI session options on resume.
    ///
    /// Use this to opt out of the default WebSocket transport for CAPI
    /// Responses API calls, equivalent to setting
    /// `COPILOT_CLI_DISABLE_WEBSOCKET_RESPONSES`.
    pub capi: Option<CapiSessionOptions>,
    /// Enables or disables internal session telemetry for this session.
    ///
    /// When `Some(false)`, disables session telemetry. `Some(true)` enables it
    /// for eligible GitHub-authenticated sessions, including BYOK.
    /// `None` preserves a resident session's choice; otherwise defaults to enabled.
    /// Host and account policies still control uploads. This is
    /// independent of [`ClientObservabilityOptions::telemetry`](crate::ClientObservabilityOptions::telemetry).
    pub telemetry_enabled: Option<bool>,
    /// Per-session GitHub token on resume. See
    /// [`SessionGitHubOptions::token`].
    pub token: Option<String>,
    /// Rotating GitHub token provider on resume. See
    /// [`SessionGitHubOptions::token_provider`].
    pub token_provider: Option<Arc<dyn GitHubTokenProvider>>,
    /// Opt-in flag injected on resume. See
    /// [`SessionGitHubOptions::fetch_managed_settings`]. Re-supply on resume so
    /// the runtime re-applies the managed-settings self-fetch after a CLI
    /// process restart. Set via
    /// [`with_enable_managed_settings`](Self::with_fetch_managed_settings).
    pub fetch_managed_settings: Option<bool>,
    /// Re-supplies that a human drives this session so a session loaded from
    /// storage keeps the declaration. It is not persisted and is ignored if the
    /// session is already active in the runtime. Set via
    /// [`with_interactive_user`](Self::with_interactive_user).
    pub interactive_user: Option<bool>,
}
impl std::fmt::Debug for SessionGitHubOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionGitHubOptions")
            .finish_non_exhaustive()
    }
}
impl SessionGitHubOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the built-in GitHub MCP server configuration.
    pub fn with_mcp(mut self, config: GitHubMcpToolConfig) -> Self {
        self.mcp = Some(config);
        self
    }

    /// Re-supply provider-scoped CAPI session options on resume.
    pub fn with_capi(mut self, capi: CapiSessionOptions) -> Self {
        self.capi = Some(capi);
        self
    }

    /// Enable or disable internal session telemetry on resume.
    ///
    /// See [`Self::telemetry_enabled`] for default and BYOK behavior.
    pub fn with_telemetry_enabled(mut self, enable: bool) -> Self {
        self.telemetry_enabled = Some(enable);
        self
    }

    /// Set the per-session GitHub token on resume. See
    /// [`SessionGitHubOptions::token`] for distinction from the
    /// client-level token.
    pub fn with_token(mut self, token: impl Into<String>) -> Self {
        self.token = Some(token.into());
        self
    }

    /// Install a rotating GitHub token provider for the resumed session.
    ///
    /// The provider must return the remaining lifetime in seconds when its callback
    /// completes. More than 3600 seconds must still remain after token validation;
    /// validation time counts against the lifetime. Production GitHub tokens
    /// typically last eight hours. Mutually exclusive with
    /// [`with_github_token`](Self::with_token).
    pub fn with_token_provider(mut self, provider: Arc<dyn GitHubTokenProvider>) -> Self {
        self.token_provider = Some(provider);
        self
    }

    /// Opt the runtime into self-fetching enterprise managed settings on resume.
    /// See [`Self::fetch_managed_settings`].
    pub fn with_fetch_managed_settings(mut self, enabled: bool) -> Self {
        self.fetch_managed_settings = Some(enabled);
        self
    }

    /// Re-supply that a human drives this session.
    pub fn with_interactive_user(mut self, interactive_user: bool) -> Self {
        self.interactive_user = Some(interactive_user);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionPluginOptions {
    /// Open Plugin directory paths passed through to the CLI on resume.
    pub directories: Option<Vec<PathBuf>>,
}
impl std::fmt::Debug for SessionPluginOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionPluginOptions")
            .finish_non_exhaustive()
    }
}
impl SessionPluginOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set Open Plugin directory paths passed through to the CLI on resume.
    pub fn with_directories<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.directories = Some(paths.into_iter().map(Into::into).collect());
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionPermissionOptions {
    /// Sandbox configuration applied when the session is constructed, before MCP servers start.
    /// Same shape as the `sandboxConfig` option of session options update;
    /// an identical later update is a no-op.
    pub sandbox: Option<SandboxConfig>,
    /// Permission mode applied before resumed pending work continues and
    /// before the resume returns. If it is refused (for example by managed
    /// policy), continued work runs under the mode
    /// [`Session::permission_mode_result`](crate::session::Session::permission_mode_result)
    /// reports. See [`SessionPermissionOptions::mode`]. Set via
    /// [`with_permission_mode`](Self::with_mode).
    pub mode: Option<PermissionMode>,
    /// Optional permission-request handler. See
    /// [`SessionPermissionOptions::on_request`].
    pub on_request: Option<Arc<dyn PermissionHandler>>,
    /// Permission policy composed with the request handler.
    pub(crate) policy: Option<crate::permission::Policy>,
}
impl std::fmt::Debug for SessionPermissionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionPermissionOptions")
            .finish_non_exhaustive()
    }
}
impl SessionPermissionOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Install a [`PermissionHandler`] for the resumed session.
    pub fn with_on_request(mut self, handler: Arc<dyn PermissionHandler>) -> Self {
        self.on_request = Some(handler);
        self
    }

    /// Auto-approve every permission request on the resumed session. See
    /// [`SessionPermissionOptions::approve_all_permissions`].
    pub fn approve_all_permissions(mut self) -> Self {
        self.policy = Some(crate::permission::Policy::ApproveAll);
        self
    }

    /// Auto-deny every permission request on the resumed session. See
    /// [`SessionPermissionOptions::deny_all_permissions`].
    pub fn deny_all_permissions(mut self) -> Self {
        self.policy = Some(crate::permission::Policy::DenyAll);
        self
    }

    /// Apply a closure-based permission policy on the resumed session.
    /// See [`SessionPermissionOptions::approve_permissions_if`].
    pub fn approve_permissions_if<F>(mut self, predicate: F) -> Self
    where
        F: Fn(&crate::types::PermissionRequestData) -> bool + Send + Sync + 'static,
    {
        self.policy = Some(crate::permission::Policy::Predicate(Arc::new(predicate)));
        self
    }

    /// Set the sandbox configuration applied during session construction, before MCP servers start.
    pub fn with_sandbox(mut self, config: SandboxConfig) -> Self {
        self.sandbox = Some(config);
        self
    }

    /// Set the permission mode applied before resumed work continues.
    pub fn with_mode(mut self, permission_mode: PermissionMode) -> Self {
        self.mode = Some(permission_mode);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionExecutionOptions {
    /// **Experimental.** Limits applied to this session's current accounting window.
    pub limits: Option<SessionLimitsConfig>,
    /// **Experimental.** Patch resident Autopilot policy on warm resume.
    /// Omitted fields preserve resident values. Re-supply on cold resume.
    pub autopilot: Option<AutopilotOptions>,
}
impl std::fmt::Debug for SessionExecutionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionExecutionOptions")
            .finish_non_exhaustive()
    }
}
impl SessionExecutionOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// **Experimental.** Set limits for this session's current accounting window.
    pub fn with_limits(mut self, limits: SessionLimitsConfig) -> Self {
        self.limits = Some(limits);
        self
    }

    /// **Experimental.** Patch Autopilot policy without changing the session mode.
    pub fn with_autopilot(mut self, options: AutopilotOptions) -> Self {
        self.autopilot = Some(options);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionExperimentOptions {
    /// Feature-flag values resolved by the host to apply on resume.
    ///
    /// Explicit feature flags.
    pub flags: Option<HashMap<String, bool>>,
    /// ExP assignment ("flight") data injected on resume. See
    /// [`SessionExperimentOptions::assignments`]. Re-supply on resume so the runtime
    /// re-applies the assignments after a CLI process restart. Set via
    /// [`with_exp_assignments`](Self::with_assignments).
    #[doc(hidden)]
    pub assignments: Option<CopilotExpAssignmentResponse>,
    /// Controls whether the session enables experimental features.
    ///
    /// Defaults to `false` in [`crate::ClientMode::Empty`] when unset;
    /// in `copilot-cli` mode, leaving this unset lets the runtime decide.
    pub enable_experimental_mode: Option<bool>,
}
impl std::fmt::Debug for SessionExperimentOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionExperimentOptions")
            .finish_non_exhaustive()
    }
}
impl SessionExperimentOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set [`enable_experimental_mode`](Self::enable_experimental_mode).
    pub fn with_enable_experimental_mode(mut self, enable_experimental_mode: bool) -> Self {
        self.enable_experimental_mode = Some(enable_experimental_mode);
        self
    }

    /// Re-supply feature-flag values resolved by the host on resume.
    pub fn with_flags(mut self, feature_flags: HashMap<String, bool>) -> Self {
        self.flags = Some(feature_flags);
        self
    }

    /// Inject ExP assignment ("flight") data on resume. See
    /// [`SessionExperimentOptions::with_assignments`]. Re-supply the assignments on
    /// resume so the runtime re-applies them after a CLI process restart.
    #[doc(hidden)]
    pub fn with_assignments(mut self, assignments: CopilotExpAssignmentResponse) -> Self {
        self.assignments = Some(assignments);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct ResumeSessionCanvasOptions {
    /// Canvas declarations this connection provides to the runtime.
    pub definitions: Option<Vec<CanvasDeclaration>>,
    /// Provider-side canvas lifecycle handler. See
    /// [`SessionCanvasOptions::handler`].
    pub handler: Option<Arc<dyn CanvasHandler>>,
    /// Open canvas instances the caller knows were open before this resume.
    pub open: Option<Vec<OpenCanvasInstance>>,
    /// Request canvas renderer tools for this connection.
    pub request_renderer: Option<bool>,
    /// Stable identity for a host/SDK connection that supplies built-in
    /// canvases, so they rehydrate against a stable extension id on resume.
    pub provider: Option<CanvasProviderIdentity>,
}
impl std::fmt::Debug for ResumeSessionCanvasOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResumeSessionCanvasOptions")
            .finish_non_exhaustive()
    }
}
impl ResumeSessionCanvasOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Re-supply canvas declarations on resume.
    pub fn with_definitions<I: IntoIterator<Item = CanvasDeclaration>>(
        mut self,
        canvases: I,
    ) -> Self {
        self.definitions = Some(canvases.into_iter().collect());
        self
    }

    /// Install the provider-side [`CanvasHandler`] for the resumed session.
    pub fn with_handler(mut self, handler: Arc<dyn CanvasHandler>) -> Self {
        self.handler = Some(handler);
        self
    }

    /// Seed open canvas instances that were visible before resuming.
    pub fn with_open<I: IntoIterator<Item = OpenCanvasInstance>>(
        mut self,
        open_canvases: I,
    ) -> Self {
        self.open = Some(open_canvases.into_iter().collect());
        self
    }

    /// Request host canvas renderer tools for this connection on resume.
    pub fn with_request_renderer(mut self, request: bool) -> Self {
        self.request_renderer = Some(request);
        self
    }

    /// Set the canvas provider identity for this connection on resume so
    /// host-supplied canvases rehydrate against a stable extension id.
    pub fn with_provider(mut self, canvas_provider: CanvasProviderIdentity) -> Self {
        self.provider = Some(canvas_provider);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionInstructionOptions {
    /// Organization-level custom instructions to apply on resume.
    pub organization_text: Option<String>,
    /// When true, enables on-demand instruction discovery on resume.
    pub on_demand_discovery: Option<bool>,
    /// Explicit discovery-only instruction roots using existing SDK formats.
    /// No general filesystem grant. Available without `enable_config_discovery`,
    /// subject to instruction/managed disables. Relative paths resolve once when supplied
    /// against the request's effective `working_directory`; retained roots do not move.
    /// `None` and omission are equivalent; an empty list clears only the explicit SDK contribution.
    pub directories: Option<Vec<PathBuf>>,
    /// Skip automatic custom instruction loading.
    pub skip_custom_instructions: Option<bool>,
}
impl std::fmt::Debug for SessionInstructionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionInstructionOptions")
            .finish_non_exhaustive()
    }
}
impl SessionInstructionOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set [`Self::organization_text`].
    pub fn with_organization_text(mut self, instructions: impl Into<String>) -> Self {
        self.organization_text = Some(instructions.into());
        self
    }

    /// Set [`Self::on_demand_discovery`].
    pub fn with_on_demand_discovery(mut self, value: bool) -> Self {
        self.on_demand_discovery = Some(value);
        self
    }

    /// Set additional directories to search for custom instruction files
    /// on resume. Forwarded to the CLI; not the same as
    /// [`with_skill_directories`](SessionSkillOptions::with_directories).
    pub fn with_directories<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.directories = Some(paths.into_iter().map(Into::into).collect());
        self
    }

    /// Set [`Self::skip_custom_instructions`].
    pub fn with_skip_custom_instructions(mut self, value: bool) -> Self {
        self.skip_custom_instructions = Some(value);
        self
    }
}
/// Grouped session settings. Unset values retain runtime defaults.
#[derive(Clone, Default)]
#[non_exhaustive]
pub struct SessionFilesystemOptions {
    /// Opts in to capturing file changes for session rewind and cumulative
    /// session diff when the resumed session has a valid baseline. Earlier
    /// untracked changes cannot be reconstructed.
    pub track_file_changes: Option<bool>,
    /// Additional directories the agent may access on resume. Relative paths
    /// resolve against the session working directory.
    pub additional_directories: Option<Vec<PathBuf>>,
}
impl std::fmt::Debug for SessionFilesystemOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SessionFilesystemOptions")
            .finish_non_exhaustive()
    }
}
impl SessionFilesystemOptions {
    /// Construct an empty option group.
    pub fn new() -> Self {
        Self::default()
    }

    /// Opt in to capturing file changes for session rewind and cumulative
    /// session diff when the resumed session has a valid baseline.
    pub fn with_track_file_changes(mut self, enable: bool) -> Self {
        self.track_file_changes = Some(enable);
        self
    }

    /// Set directories the agent may access beyond the working directory on resume.
    pub fn with_additional_directories<I, P>(mut self, paths: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.additional_directories = Some(paths.into_iter().map(Into::into).collect());
        self
    }
}

/// Options for creating a session, grouped by the capability they configure.
///
/// Group setters retain existing values; direct field assignment also accepts
/// optional values from upstream callers. Callbacks stay client-side, while
/// serializable group fields are sent as nested protocol-v4 options.
///
/// ```
/// use copilot_sdk::CreateSessionOptions;
/// let mut options = CreateSessionOptions::default()
///     .with_events(|events| events.with_streaming(true))
///     .with_inference(|inference| inference.with_reasoning_effort("high"));
/// options.enable_config_discovery = Some(false);
/// ```
#[derive(Clone)]
#[non_exhaustive]
pub struct CreateSessionOptions {
    /// Custom session ID. When unset, the CLI generates one.
    pub session_id: Option<SessionId>,
    /// Name the session at creation, as `session.name.set` would, and validated
    /// the same way: non-empty, no leading or trailing whitespace, at most 100
    /// UTF-16 units, no control characters, and no double quotes. A name also
    /// makes the session persist from the start, so it can be listed and resumed
    /// even if it never runs a turn. Local sessions only: cloud creation rejects
    /// it.
    pub name: Option<String>,
    /// Model to use (e.g. `"gpt-5.4"`, `"claude-sonnet-4"`).
    pub model: Option<String>,
    /// Application name sent as `User-Agent` context.
    pub client_name: Option<String>,
    /// Custom system message configuration.
    pub system_message: Option<SystemMessageConfig>,
    /// Enables session-scoped MCP diagnostic capture. Diagnostics are off by
    /// default. Debug and trace entries can contain MCP payloads, tool arguments,
    /// paths, and server stderr, so hosts must not upload or export them
    /// automatically.
    pub diagnostics: Option<DiagnosticsConfiguration>,
    /// Configures the startup-only experimental runtime lifecycle diagnostic
    /// read and persistence surfaces.
    pub(crate) lifecycle_diagnostics: Option<LifecycleDiagnosticsConfiguration>,
    /// Working directory for the session. Tool operations resolve
    /// relative paths against this directory.
    pub working_directory: Option<PathBuf>,
    /// Creates a remote session in the cloud instead of a local session.
    /// The optional repository is associated with the cloud session.
    pub cloud: Option<CloudSessionOptions>,
    /// System-message transform. When set, the SDK injects the matching
    /// `action: "transform"` sections into the system message and routes
    /// `systemMessage.transform` RPC callbacks to it during the session.
    /// Use [`with_system_message_transform`](Self::with_system_message_transform) to install one.
    pub system_message_transform: Option<Arc<dyn SystemMessageTransform>>,
    /// Inference options for this session.
    pub inference: SessionInferenceOptions,
    /// Policy options for this session.
    pub policy: SessionPolicyOptions,
    /// Events options for this session.
    pub events: SessionEventOptions,
    /// Interaction options for this session.
    pub interaction: SessionInteractionOptions,
    /// Tools options for this session.
    pub tools: SessionToolOptions,
    /// Canvases options for this session.
    pub canvases: SessionCanvasOptions,
    /// Extensions options for this session.
    pub extensions: SessionExtensionOptions,
    /// Agents options for this session.
    pub agents: SessionAgentOptions,
    /// Skills options for this session.
    pub skills: SessionSkillOptions,
    /// Mcp options for this session.
    pub mcp: SessionMcpOptions,
    /// Override the default configuration directory location.
    pub config_directory: Option<PathBuf>,
    /// Enables runtime discovery of supported configuration. Explicitly supplied
    /// configuration takes precedence; explicit feature roots remain independent.
    pub enable_config_discovery: Option<bool>,
    /// Filesystem options for this session.
    pub filesystem: CreateSessionFilesystemOptions,
    /// State options for this session.
    pub state: SessionStateOptions,
    /// Instructions options for this session.
    pub instructions: CreateSessionInstructionOptions,
    /// Hooks options for this session.
    pub hooks: SessionHookOptions,
    /// Git options for this session.
    pub git: SessionGitOptions,
    /// GitHub options for this session.
    pub github: SessionGitHubOptions,
    /// Plugins options for this session.
    pub plugins: SessionPluginOptions,
    /// Permissions options for this session.
    pub permissions: SessionPermissionOptions,
    /// Execution options for this session.
    pub execution: SessionExecutionOptions,
    /// Experiments options for this session.
    pub experiments: SessionExperimentOptions,
}

impl std::fmt::Debug for CreateSessionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CreateSessionOptions")
            .finish_non_exhaustive()
    }
}

impl Default for CreateSessionOptions {
    /// All wire-level "request" flags and handler fields start unset.
    /// Install a [`PermissionHandler`] via
    /// [`SessionPermissionOptions::with_on_request`] and
    /// the SDK derives `requestPermission: true` on the wire at
    /// [`Client::create_session`](crate::Client::create_session) time.
    fn default() -> Self {
        Self {
            session_id: None,
            name: None,
            model: None,
            client_name: None,
            system_message: None,
            diagnostics: None,
            lifecycle_diagnostics: None,
            working_directory: None,
            cloud: None,
            system_message_transform: None,
            inference: Default::default(),
            policy: Default::default(),
            events: Default::default(),
            interaction: Default::default(),
            tools: Default::default(),
            canvases: Default::default(),
            extensions: Default::default(),
            agents: Default::default(),
            skills: Default::default(),
            mcp: Default::default(),
            config_directory: None,
            enable_config_discovery: None,
            filesystem: Default::default(),
            state: Default::default(),
            instructions: Default::default(),
            hooks: Default::default(),
            git: Default::default(),
            github: Default::default(),
            plugins: Default::default(),
            permissions: Default::default(),
            execution: Default::default(),
            experiments: Default::default(),
        }
    }
}

/// Runtime-only bundle drained out of a [`CreateSessionOptions`] or
/// [`ResumeSessionOptions`] by [`CreateSessionOptions::into_wire`] /
/// [`ResumeSessionOptions::into_wire`]. Holds the trait-object handlers,
/// session-fs provider, and slash commands so the wire payload struct
/// stays a pure data shape.
pub(crate) struct SessionOptionsRuntime {
    pub permission_handler: Option<Arc<dyn PermissionHandler>>,
    pub permission_policy: Option<crate::permission::Policy>,
    pub elicitation_handler: Option<Arc<dyn ElicitationHandler>>,
    pub mcp_auth_handler: Option<Arc<dyn McpAuthHandler>>,
    pub user_input_handler: Option<Arc<dyn UserInputHandler>>,
    pub exit_plan_mode_handler: Option<Arc<dyn ExitPlanModeHandler>>,
    pub auto_mode_switch_handler: Option<Arc<dyn AutoModeSwitchHandler>>,
    pub shell_authentication_handler: Option<Arc<dyn ShellAuthenticationHandler>>,
    pub hooks_handler: Option<Arc<dyn SessionHooks>>,
    pub system_message_transform: Option<Arc<dyn SystemMessageTransform>>,
    pub tool_handlers: ToolHandlerMap,
    pub canvas_handler: Option<Arc<dyn CanvasHandler>>,
    pub session_fs_provider: Option<Arc<dyn SessionFsProvider>>,
    pub skill_provider: Option<Arc<dyn SkillProvider>>,
    pub memory_store: Option<Arc<dyn MemoryStore>>,
    pub bearer_token_providers: HashMap<String, Arc<dyn BearerTokenProvider>>,
    pub github_token_provider: Option<Arc<dyn GitHubTokenProvider>>,
    pub commands: Option<Vec<CommandDefinition>>,
}

impl CreateSessionOptions {
    /// Consume this config to produce the [`SessionCreateWire`] payload
    /// for `session.create` and a [`SessionOptionsRuntime`] bundle holding
    /// the runtime-only fields (handlers, transforms, providers).
    ///
    /// Wire-format flags are derived from handler presence and the policy
    /// field. Tool handler Arcs are shared until wire serialization completes;
    /// other runtime fields are moved into the returned runtime, avoiding deep
    /// `Vec<Tool>` / `IndexMap<String, Value>` clones.
    ///
    /// [`SessionCreateWire`]: crate::wire::SessionCreateWire
    pub(crate) fn into_wire(
        mut self,
        session_id: Option<SessionId>,
    ) -> Result<(crate::wire::SessionCreateWire, SessionOptionsRuntime), crate::Error> {
        if self.interaction.auto_mode_recovery
            && self.interaction.on_auto_mode_switch_request.is_none()
        {
            return Err(crate::Error::with_message(
                crate::ErrorKind::InvalidConfig,
                "auto_mode_recovery requires auto_mode_switch_handler",
            ));
        }
        if self.github.token.is_some() && self.github.token_provider.is_some() {
            return Err(crate::Error::with_message(
                crate::ErrorKind::InvalidConfig,
                "github.token and github.token_provider are mutually exclusive",
            ));
        }
        let permission_active =
            self.permissions.on_request.is_some() || self.permissions.policy.is_some();
        let request_user_input = self.interaction.on_user_input_request.is_some();
        let request_exit_plan_mode = self.interaction.on_exit_plan_mode_request.is_some();
        let request_auto_mode_switch = self.interaction.on_auto_mode_switch_request.is_some();
        let request_shell_authentication =
            self.interaction.on_shell_authentication_request.is_some();
        let request_elicitation = self.interaction.on_elicitation_request.is_some();
        let hooks_flag = self.hooks.handlers.is_some();
        let has_skill_provider = self.skills.provider.is_some();
        let has_memory_store = self.state.memory_store.is_some();

        let tool_handlers =
            collect_tool_handlers(self.tools.definitions.as_deref().unwrap_or_default())?;

        let wire_commands = self.interaction.commands.as_ref().map(|cmds| {
            cmds.iter()
                .map(|c| crate::wire::CommandWireDefinition {
                    name: c.name.clone(),
                    description: c.description.clone().unwrap_or_default(),
                })
                .collect()
        });
        let wire_canvases = self.canvases.definitions.clone();
        let canvas_handler = self.canvases.handler.clone();
        let bearer_token_providers = prepare_bearer_token_providers(&mut self.inference.providers);

        let wire = crate::wire::SessionCreateWire {
            session_id,
            name: self.name,
            model: self.model,
            client_name: self.client_name,
            system_message: self.system_message,
            diagnostics: self.diagnostics,
            lifecycle_diagnostics: self.lifecycle_diagnostics,
            working_directory: self.working_directory,
            remote_session: None,
            cloud: self.cloud,
            inference: crate::wire::SessionInferenceWire {
                allow_multiple_tool_calls: self.inference.allow_multiple_tool_calls,
                reasoning_effort: self.inference.reasoning_effort,
                reasoning_summary: self.inference.reasoning_summary,
                context_tier: self.inference.context_tier,
                default_provider: self.inference.default_provider,
                providers: self.inference.providers,
                models: self.inference.models,
                enable_citations: self.inference.enable_citations,
                capabilities: self.inference.capabilities,
            },
            policy: crate::wire::SessionPolicyWire {
                allowed_models: self.policy.allowed_models,
                excluded_managed_settings: self.policy.excluded_managed_settings,
                enforce_managed_model_defaults: self.policy.enforce_managed_model_defaults,
                managed_settings: self.policy.managed_settings,
            },
            events: crate::wire::SessionEventWire {
                streaming: self.events.streaming,
                include_sub_agent_streaming_events: self.events.include_sub_agent_streaming_events,
            },
            interaction: crate::wire::SessionInteractionWire {
                ask_user_variant: self.interaction.ask_user_variant,
                request_user_input,
                request_exit_plan_mode,
                request_auto_mode_switch,
                request_auto_mode_recovery: self.interaction.auto_mode_recovery.then_some(true),
                request_elicitation,
                request_shell_authentication: request_shell_authentication.then_some(true),
                commands: wire_commands,
            },
            tools: crate::wire::SessionToolWire {
                definitions: self.tools.definitions,
                available_tools: self.tools.available_tools,
                excluded_tools: self.tools.excluded_tools,
                large_output: self.tools.large_output,
                search: self.tools.search,
                image_generation: self.tools.image_generation,
            },
            canvases: crate::wire::SessionCreateCanvasWire {
                definitions: wire_canvases,
                request_renderer: self.canvases.request_renderer,
                provider: self.canvases.provider,
            },
            extensions: crate::wire::SessionExtensionWire {
                enabled: self.extensions.enabled,
                sdk_path: self.extensions.sdk_path,
                identity: self.extensions.identity,
            },
            agents: crate::wire::SessionAgentWire {
                excluded_builtin_agents: self.agents.excluded_builtin_agents,
                included_builtin_agents: self.agents.included_builtin_agents,
                directories: self.agents.directories,
                definitions: self.agents.definitions,
                local_discovery_only: self.agents.local_discovery_only,
                default: self.agents.default,
                selected: self.agents.selected,
            },
            mcp: crate::wire::SessionMcpWire {
                oauth_token_storage: self.mcp.oauth_token_storage,
                servers: self.mcp.servers,
                auth_client_id_metadata_url: self.mcp.auth_client_id_metadata_url,
                env_value_mode: "direct",
                enable_apps: self.mcp.enable_apps.unwrap_or(false),
                disabled_servers: self.mcp.disabled_servers,
            },
            state: crate::wire::SessionStateWire {
                embedding_cache_storage: self.state.embedding_cache_storage,
                skip_embedding_retrieval: self.state.skip_embedding_retrieval,
                enable_cross_session_store: self.state.enable_cross_session_store,
                has_memory_store: has_memory_store.then_some(true),
                infinite_sessions: self.state.infinite_sessions,
                memory: self.state.memory,
            },
            config_directory: self.config_directory,
            enable_config_discovery: self.enable_config_discovery,
            filesystem: crate::wire::SessionCreateFilesystemWire {
                trust_working_directory: self.filesystem.trust_working_directory,
                track_file_changes: self.filesystem.track_file_changes,
                additional_directories: self.filesystem.additional_directories,
            },
            instructions: crate::wire::SessionCreateInstructionWire {
                organization_text: self.instructions.organization_text,
                refresh_custom_instructions: self.instructions.refresh_custom_instructions,
                on_demand_discovery: self.instructions.on_demand_discovery,
                directories: self.instructions.directories,
            },
            hooks: crate::wire::SessionHookWire {
                enable_repository_hooks: self.hooks.enable_repository_hooks,
                has_handlers: hooks_flag,
                directories: self.hooks.directories,
            },
            git: crate::wire::SessionGitWire {
                enable_host_git_operations: self.git.enable_host_git_operations,
            },
            skills: crate::wire::SessionSkillWire {
                enabled: self.skills.enabled,
                has_provider: has_skill_provider.then_some(true),
                directories: self.skills.directories,
                disabled_skills: self.skills.disabled_skills,
            },
            permissions: crate::wire::SessionPermissionWire {
                request: permission_active,
                sandbox: self.permissions.sandbox,
                mode: self.permissions.mode,
            },
            github: crate::wire::SessionGitHubWire {
                telemetry_forwarding: None,
                mcp: self.github.mcp,
                capi: self.github.capi,
                telemetry_enabled: self.github.telemetry_enabled,
                token: self.github.token,
                token_provider_registration_id: None,
                fetch_managed_settings: self.github.fetch_managed_settings,
                interactive_user: self.github.interactive_user,
            },
            plugins: crate::wire::SessionPluginWire {
                directories: self.plugins.directories,
            },
            execution: crate::wire::SessionExecutionWire {
                limits: self.execution.limits,
                autopilot: self.execution.autopilot,
            },
            experiments: crate::wire::SessionExperimentWire {
                flags: self.experiments.flags,
                assignments: self.experiments.assignments,
                enable_experimental_mode: self.experiments.enable_experimental_mode,
            },
        };

        let runtime = SessionOptionsRuntime {
            permission_handler: self.permissions.on_request,
            permission_policy: self.permissions.policy,
            elicitation_handler: self.interaction.on_elicitation_request,
            mcp_auth_handler: self.mcp.on_auth_request,
            user_input_handler: self.interaction.on_user_input_request,
            exit_plan_mode_handler: self.interaction.on_exit_plan_mode_request,
            auto_mode_switch_handler: self.interaction.on_auto_mode_switch_request,
            shell_authentication_handler: self.interaction.on_shell_authentication_request,
            hooks_handler: self.hooks.handlers,
            system_message_transform: self.system_message_transform,
            tool_handlers,
            canvas_handler,
            session_fs_provider: self.state.session_fs_provider,
            skill_provider: self.skills.provider,
            memory_store: self.state.memory_store,
            bearer_token_providers,
            github_token_provider: self.github.token_provider,
            commands: self.interaction.commands,
        };

        Ok((wire, runtime))
    }

    /// Install a [`SystemMessageTransform`]. The SDK injects the matching
    /// `action: "transform"` sections into the system message and routes
    /// `systemMessage.transform` RPC callbacks to it during the session.
    pub fn with_system_message_transform(
        mut self,
        transform: Arc<dyn SystemMessageTransform>,
    ) -> Self {
        self.system_message_transform = Some(transform);
        self
    }

    /// Set a custom session ID (when unset, the CLI generates one).
    pub fn with_session_id(mut self, id: impl Into<SessionId>) -> Self {
        self.session_id = Some(id.into());
        self
    }

    /// Set [`Self::name`]. The runtime rejects a name that is empty, padded with
    /// whitespace, longer than 100 UTF-16 units, or contains control characters
    /// or double quotes, so `create_session` fails rather than creating an
    /// unnamed or unrenameable session.
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Set the model identifier (e.g. `"claude-sonnet-4"`).
    pub fn with_model(mut self, model: impl Into<String>) -> Self {
        self.model = Some(model.into());
        self
    }

    /// Set the application name sent as `User-Agent` context.
    pub fn with_client_name(mut self, name: impl Into<String>) -> Self {
        self.client_name = Some(name.into());
        self
    }

    /// Set a custom system message configuration.
    pub fn with_system_message(mut self, system_message: SystemMessageConfig) -> Self {
        self.system_message = Some(system_message);
        self
    }

    /// Set the session-scoped MCP diagnostic level.
    pub fn with_diagnostics(mut self, diagnostics: DiagnosticsConfiguration) -> Self {
        self.diagnostics = Some(diagnostics);
        self
    }

    /// Set the per-session working directory. Tool operations resolve
    /// relative paths against this directory.
    pub fn with_working_directory(mut self, dir: impl Into<PathBuf>) -> Self {
        self.working_directory = Some(dir.into());
        self
    }

    /// Create a remote session in the cloud instead of a local session.
    pub fn with_cloud(mut self, cloud: CloudSessionOptions) -> Self {
        self.cloud = Some(cloud);
        self
    }
    /// Update the inference option group.
    pub fn with_inference(
        mut self,
        configure: impl FnOnce(SessionInferenceOptions) -> SessionInferenceOptions,
    ) -> Self {
        self.inference = configure(self.inference);
        self
    }

    /// Update the policy option group.
    pub fn with_policy(
        mut self,
        configure: impl FnOnce(SessionPolicyOptions) -> SessionPolicyOptions,
    ) -> Self {
        self.policy = configure(self.policy);
        self
    }

    /// Update the events option group.
    pub fn with_events(
        mut self,
        configure: impl FnOnce(SessionEventOptions) -> SessionEventOptions,
    ) -> Self {
        self.events = configure(self.events);
        self
    }

    /// Update the interaction option group.
    pub fn with_interaction(
        mut self,
        configure: impl FnOnce(SessionInteractionOptions) -> SessionInteractionOptions,
    ) -> Self {
        self.interaction = configure(self.interaction);
        self
    }

    /// Update the tools option group.
    pub fn with_tools(
        mut self,
        configure: impl FnOnce(SessionToolOptions) -> SessionToolOptions,
    ) -> Self {
        self.tools = configure(self.tools);
        self
    }

    /// Update the canvases option group.
    pub fn with_canvases(
        mut self,
        configure: impl FnOnce(SessionCanvasOptions) -> SessionCanvasOptions,
    ) -> Self {
        self.canvases = configure(self.canvases);
        self
    }

    /// Update the extensions option group.
    pub fn with_extensions(
        mut self,
        configure: impl FnOnce(SessionExtensionOptions) -> SessionExtensionOptions,
    ) -> Self {
        self.extensions = configure(self.extensions);
        self
    }

    /// Update the agents option group.
    pub fn with_agents(
        mut self,
        configure: impl FnOnce(SessionAgentOptions) -> SessionAgentOptions,
    ) -> Self {
        self.agents = configure(self.agents);
        self
    }

    /// Update the skills option group.
    pub fn with_skills(
        mut self,
        configure: impl FnOnce(SessionSkillOptions) -> SessionSkillOptions,
    ) -> Self {
        self.skills = configure(self.skills);
        self
    }

    /// Update the mcp option group.
    pub fn with_mcp(
        mut self,
        configure: impl FnOnce(SessionMcpOptions) -> SessionMcpOptions,
    ) -> Self {
        self.mcp = configure(self.mcp);
        self
    }

    /// Override the default configuration directory location.
    pub fn with_config_directory(mut self, directory: impl Into<PathBuf>) -> Self {
        self.config_directory = Some(directory.into());
        self
    }

    /// Enables runtime discovery of supported configuration. Explicitly supplied
    /// configuration takes precedence; explicit feature roots remain independent.
    pub fn with_enable_config_discovery(mut self, enable: bool) -> Self {
        self.enable_config_discovery = Some(enable);
        self
    }

    /// Update the filesystem option group.
    pub fn with_filesystem(
        mut self,
        configure: impl FnOnce(CreateSessionFilesystemOptions) -> CreateSessionFilesystemOptions,
    ) -> Self {
        self.filesystem = configure(self.filesystem);
        self
    }

    /// Update the state option group.
    pub fn with_state(
        mut self,
        configure: impl FnOnce(SessionStateOptions) -> SessionStateOptions,
    ) -> Self {
        self.state = configure(self.state);
        self
    }

    /// Update the instructions option group.
    pub fn with_instructions(
        mut self,
        configure: impl FnOnce(CreateSessionInstructionOptions) -> CreateSessionInstructionOptions,
    ) -> Self {
        self.instructions = configure(self.instructions);
        self
    }

    /// Update the hooks option group.
    pub fn with_hooks(
        mut self,
        configure: impl FnOnce(SessionHookOptions) -> SessionHookOptions,
    ) -> Self {
        self.hooks = configure(self.hooks);
        self
    }

    /// Update the git option group.
    pub fn with_git(
        mut self,
        configure: impl FnOnce(SessionGitOptions) -> SessionGitOptions,
    ) -> Self {
        self.git = configure(self.git);
        self
    }

    /// Update the github option group.
    pub fn with_github(
        mut self,
        configure: impl FnOnce(SessionGitHubOptions) -> SessionGitHubOptions,
    ) -> Self {
        self.github = configure(self.github);
        self
    }

    /// Update the plugins option group.
    pub fn with_plugins(
        mut self,
        configure: impl FnOnce(SessionPluginOptions) -> SessionPluginOptions,
    ) -> Self {
        self.plugins = configure(self.plugins);
        self
    }

    /// Update the permissions option group.
    pub fn with_permissions(
        mut self,
        configure: impl FnOnce(SessionPermissionOptions) -> SessionPermissionOptions,
    ) -> Self {
        self.permissions = configure(self.permissions);
        self
    }

    /// Update the execution option group.
    pub fn with_execution(
        mut self,
        configure: impl FnOnce(SessionExecutionOptions) -> SessionExecutionOptions,
    ) -> Self {
        self.execution = configure(self.execution);
        self
    }

    /// Update the experiments option group.
    pub fn with_experiments(
        mut self,
        configure: impl FnOnce(SessionExperimentOptions) -> SessionExperimentOptions,
    ) -> Self {
        self.experiments = configure(self.experiments);
        self
    }
}
///
/// See [`CreateSessionOptions`] for the construction patterns (chained `with_*`
/// builder vs. direct field assignment for `Option<T>` pass-through) and
/// the note on snake_case vs. camelCase field naming. This config is not
/// itself serializable — call `ResumeSessionOptions::into_wire`
/// (crate-private) to produce the wire payload.
#[derive(Clone)]
#[non_exhaustive]
pub struct ResumeSessionOptions {
    /// ID of the session to resume.
    pub session_id: SessionId,
    /// Model to use for this session (e.g. `"gpt-5.4"`, `"claude-sonnet-4"`).
    /// Can change the model when resuming.
    pub model: Option<String>,
    /// Application name sent as User-Agent context.
    pub client_name: Option<String>,
    /// Re-supply the system message so the agent retains workspace context
    /// across CLI process restarts.
    pub system_message: Option<SystemMessageConfig>,
    /// Updates session-scoped MCP diagnostics. Leave this unset on a resident
    /// resume to preserve the current diagnostic level.
    pub diagnostics: Option<DiagnosticsConfiguration>,
    /// Must be omitted on resident resume unless it equals the active startup
    /// lifecycle diagnostics configuration.
    pub(crate) lifecycle_diagnostics: Option<LifecycleDiagnosticsConfiguration>,
    /// Per-session working directory on resume.
    pub working_directory: Option<PathBuf>,
    /// When `true`, the runtime does not emit the `session.resume` event for
    /// this resume. Useful for reconnecting to a session without triggering
    /// resume-related side effects. Most callers should leave this unset
    /// (the default, same as `false`) to receive the resume notification.
    /// Sent directly as `suppressResumeEvent`.
    pub suppress_resume_event: Option<bool>,
    /// When `true`, instructs the runtime to continue any tool calls or
    /// permission requests that were pending when the previous connection
    /// was dropped. To hand off a session from one process to another, end
    /// the first client with [`Client::stop`] before resuming the session
    /// elsewhere. [`Client::force_stop`] does not wait for the runtime to
    /// save the session, so when it terminates a runtime the client started,
    /// pending work can be lost.
    /// When omitted or `false` (the default), work still pending on resume
    /// is treated as interrupted; completed tool results already recorded by
    /// the runtime are preserved.
    ///
    /// When `true`, pending permission requests and external tool calls wait for
    /// caller-supplied results via the corresponding low-level RPC methods. Resuming
    /// does not re-emit restored permission requests or re-invoke permission handlers
    /// or already-dispatched external tool callbacks.
    /// If a pending call was recorded as SDK-handler-backed and resume registers a
    /// handler for that tool, the abandoned callback is interrupted, never replayed.
    /// Handlerless calls remain pending. To supply an abandoned callback's result
    /// manually, resume without a handler for it. Older requests without recorded
    /// callback ownership also remain pending.
    ///
    /// [`Client::stop`]: crate::Client::stop
    /// [`Client::force_stop`]: crate::Client::force_stop
    pub continue_pending_work: Option<bool>,
    /// Permit recovery of a damaged transcript on resume. Defaults to `true`
    /// in all modes when unset. Set `false` to reject recovery.
    pub allow_transcript_recovery: Option<bool>,
    /// System-message transform. See [`CreateSessionOptions::system_message_transform`].
    pub system_message_transform: Option<Arc<dyn SystemMessageTransform>>,
    /// Inference options for this session.
    pub inference: SessionInferenceOptions,
    /// Policy options for this session.
    pub policy: SessionPolicyOptions,
    /// Events options for this session.
    pub events: SessionEventOptions,
    /// Interaction options for this session.
    pub interaction: SessionInteractionOptions,
    /// Tools options for this session.
    pub tools: SessionToolOptions,
    /// Canvases options for this session.
    pub canvases: ResumeSessionCanvasOptions,
    /// Extensions options for this session.
    pub extensions: SessionExtensionOptions,
    /// Agents options for this session.
    pub agents: SessionAgentOptions,
    /// Skills options for this session.
    pub skills: SessionSkillOptions,
    /// Mcp options for this session.
    pub mcp: SessionMcpOptions,
    /// Override the default configuration directory location.
    pub config_directory: Option<PathBuf>,
    /// Enables runtime discovery of supported configuration. Explicitly supplied
    /// configuration takes precedence; explicit feature roots remain independent.
    pub enable_config_discovery: Option<bool>,
    /// State options for this session.
    pub state: SessionStateOptions,
    /// Instructions options for this session.
    pub instructions: SessionInstructionOptions,
    /// Hooks options for this session.
    pub hooks: SessionHookOptions,
    /// Git options for this session.
    pub git: SessionGitOptions,
    /// GitHub options for this session.
    pub github: SessionGitHubOptions,
    /// Plugins options for this session.
    pub plugins: SessionPluginOptions,
    /// Permissions options for this session.
    pub permissions: SessionPermissionOptions,
    /// Filesystem options for this session.
    pub filesystem: SessionFilesystemOptions,
    /// Execution options for this session.
    pub execution: SessionExecutionOptions,
    /// Experiments options for this session.
    pub experiments: SessionExperimentOptions,
}

impl std::fmt::Debug for ResumeSessionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResumeSessionOptions")
            .finish_non_exhaustive()
    }
}

impl ResumeSessionOptions {
    pub(crate) fn validate_auto_mode_recovery(&self) -> Result<(), crate::Error> {
        if self.interaction.auto_mode_recovery
            && self.interaction.on_auto_mode_switch_request.is_none()
        {
            return Err(crate::Error::with_message(
                crate::ErrorKind::InvalidConfig,
                "auto_mode_recovery requires auto_mode_switch_handler",
            ));
        }
        Ok(())
    }

    /// Consume this config to produce the [`SessionResumeWire`] payload
    /// for `session.resume` and a [`SessionOptionsRuntime`] bundle holding
    /// the runtime-only fields (handlers, transforms, providers).
    ///
    /// See [`CreateSessionOptions::into_wire`] for the design rationale.
    ///
    /// [`SessionResumeWire`]: crate::wire::SessionResumeWire
    pub(crate) fn into_wire(
        mut self,
    ) -> Result<(crate::wire::SessionResumeWire, SessionOptionsRuntime), crate::Error> {
        self.validate_auto_mode_recovery()?;
        if self.github.token.is_some() && self.github.token_provider.is_some() {
            return Err(crate::Error::with_message(
                crate::ErrorKind::InvalidConfig,
                "github.token and github.token_provider are mutually exclusive",
            ));
        }
        let permission_active =
            self.permissions.on_request.is_some() || self.permissions.policy.is_some();
        let request_user_input = self.interaction.on_user_input_request.is_some();
        let request_exit_plan_mode = self.interaction.on_exit_plan_mode_request.is_some();
        let request_auto_mode_switch = self.interaction.on_auto_mode_switch_request.is_some();
        let request_shell_authentication =
            self.interaction.on_shell_authentication_request.is_some();
        let request_elicitation = self.interaction.on_elicitation_request.is_some();
        let hooks_flag = self.hooks.handlers.is_some();
        let has_skill_provider = self.skills.provider.is_some();
        let has_memory_store = self.state.memory_store.is_some();

        let tool_handlers =
            collect_tool_handlers(self.tools.definitions.as_deref().unwrap_or_default())?;

        let wire_commands = self.interaction.commands.as_ref().map(|cmds| {
            cmds.iter()
                .map(|c| crate::wire::CommandWireDefinition {
                    name: c.name.clone(),
                    description: c.description.clone().unwrap_or_default(),
                })
                .collect()
        });
        let wire_canvases = self.canvases.definitions.clone();
        let canvas_handler = self.canvases.handler.clone();
        let bearer_token_providers = prepare_bearer_token_providers(&mut self.inference.providers);

        let wire = crate::wire::SessionResumeWire {
            session_id: self.session_id,
            model: self.model,
            client_name: self.client_name,
            system_message: self.system_message,
            diagnostics: self.diagnostics,
            lifecycle_diagnostics: self.lifecycle_diagnostics,
            working_directory: self.working_directory,
            remote_session: None,
            suppress_resume_event: self.suppress_resume_event,
            continue_pending_work: self.continue_pending_work,
            allow_transcript_recovery: self.allow_transcript_recovery,
            inference: crate::wire::SessionInferenceWire {
                allow_multiple_tool_calls: self.inference.allow_multiple_tool_calls,
                reasoning_effort: self.inference.reasoning_effort,
                reasoning_summary: self.inference.reasoning_summary,
                context_tier: self.inference.context_tier,
                default_provider: self.inference.default_provider,
                providers: self.inference.providers,
                models: self.inference.models,
                enable_citations: self.inference.enable_citations,
                capabilities: self.inference.capabilities,
            },
            policy: crate::wire::SessionPolicyWire {
                allowed_models: self.policy.allowed_models,
                excluded_managed_settings: self.policy.excluded_managed_settings,
                enforce_managed_model_defaults: self.policy.enforce_managed_model_defaults,
                managed_settings: self.policy.managed_settings,
            },
            events: crate::wire::SessionEventWire {
                streaming: self.events.streaming,
                include_sub_agent_streaming_events: self.events.include_sub_agent_streaming_events,
            },
            interaction: crate::wire::SessionInteractionWire {
                ask_user_variant: self.interaction.ask_user_variant,
                request_user_input,
                request_exit_plan_mode,
                request_auto_mode_switch,
                request_auto_mode_recovery: self.interaction.auto_mode_recovery.then_some(true),
                request_elicitation,
                request_shell_authentication: request_shell_authentication.then_some(true),
                commands: wire_commands,
            },
            tools: crate::wire::SessionToolWire {
                definitions: self.tools.definitions,
                available_tools: self.tools.available_tools,
                excluded_tools: self.tools.excluded_tools,
                large_output: self.tools.large_output,
                search: self.tools.search,
                image_generation: self.tools.image_generation,
            },
            canvases: crate::wire::SessionResumeCanvasWire {
                definitions: wire_canvases,
                open: self.canvases.open,
                request_renderer: self.canvases.request_renderer,
                provider: self.canvases.provider,
            },
            extensions: crate::wire::SessionExtensionWire {
                enabled: self.extensions.enabled,
                sdk_path: self.extensions.sdk_path,
                identity: self.extensions.identity,
            },
            agents: crate::wire::SessionAgentWire {
                excluded_builtin_agents: self.agents.excluded_builtin_agents,
                included_builtin_agents: self.agents.included_builtin_agents,
                directories: self.agents.directories,
                definitions: self.agents.definitions,
                local_discovery_only: self.agents.local_discovery_only,
                default: self.agents.default,
                selected: self.agents.selected,
            },
            mcp: crate::wire::SessionMcpWire {
                oauth_token_storage: self.mcp.oauth_token_storage,
                servers: self.mcp.servers,
                auth_client_id_metadata_url: self.mcp.auth_client_id_metadata_url,
                env_value_mode: "direct",
                enable_apps: self.mcp.enable_apps.unwrap_or(false),
                disabled_servers: self.mcp.disabled_servers,
            },
            state: crate::wire::SessionStateWire {
                embedding_cache_storage: self.state.embedding_cache_storage,
                skip_embedding_retrieval: self.state.skip_embedding_retrieval,
                enable_cross_session_store: self.state.enable_cross_session_store,
                has_memory_store: has_memory_store.then_some(true),
                infinite_sessions: self.state.infinite_sessions,
                memory: self.state.memory,
            },
            config_directory: self.config_directory,
            enable_config_discovery: self.enable_config_discovery,
            instructions: crate::wire::SessionResumeInstructionWire {
                organization_text: self.instructions.organization_text,
                on_demand_discovery: self.instructions.on_demand_discovery,
                directories: self.instructions.directories,
            },
            hooks: crate::wire::SessionHookWire {
                enable_repository_hooks: self.hooks.enable_repository_hooks,
                has_handlers: hooks_flag,
                directories: self.hooks.directories,
            },
            git: crate::wire::SessionGitWire {
                enable_host_git_operations: self.git.enable_host_git_operations,
            },
            skills: crate::wire::SessionSkillWire {
                enabled: self.skills.enabled,
                has_provider: has_skill_provider.then_some(true),
                directories: self.skills.directories,
                disabled_skills: self.skills.disabled_skills,
            },
            permissions: crate::wire::SessionPermissionWire {
                request: permission_active,
                sandbox: self.permissions.sandbox,
                mode: self.permissions.mode,
            },
            github: crate::wire::SessionGitHubWire {
                telemetry_forwarding: None,
                mcp: self.github.mcp,
                capi: self.github.capi,
                telemetry_enabled: self.github.telemetry_enabled,
                token: self.github.token,
                token_provider_registration_id: None,
                fetch_managed_settings: self.github.fetch_managed_settings,
                interactive_user: self.github.interactive_user,
            },
            plugins: crate::wire::SessionPluginWire {
                directories: self.plugins.directories,
            },
            filesystem: crate::wire::SessionResumeFilesystemWire {
                track_file_changes: self.filesystem.track_file_changes,
                additional_directories: self.filesystem.additional_directories,
            },
            execution: crate::wire::SessionExecutionWire {
                limits: self.execution.limits,
                autopilot: self.execution.autopilot,
            },
            experiments: crate::wire::SessionExperimentWire {
                flags: self.experiments.flags,
                assignments: self.experiments.assignments,
                enable_experimental_mode: self.experiments.enable_experimental_mode,
            },
        };

        let runtime = SessionOptionsRuntime {
            permission_handler: self.permissions.on_request,
            permission_policy: self.permissions.policy,
            elicitation_handler: self.interaction.on_elicitation_request,
            mcp_auth_handler: self.mcp.on_auth_request,
            user_input_handler: self.interaction.on_user_input_request,
            exit_plan_mode_handler: self.interaction.on_exit_plan_mode_request,
            auto_mode_switch_handler: self.interaction.on_auto_mode_switch_request,
            shell_authentication_handler: self.interaction.on_shell_authentication_request,
            hooks_handler: self.hooks.handlers,
            system_message_transform: self.system_message_transform,
            tool_handlers,
            canvas_handler,
            session_fs_provider: self.state.session_fs_provider,
            skill_provider: self.skills.provider,
            memory_store: self.state.memory_store,
            bearer_token_providers,
            github_token_provider: self.github.token_provider,
            commands: self.interaction.commands,
        };

        Ok((wire, runtime))
    }

    /// Construct a `ResumeSessionOptions` with the given session ID and all
    /// other fields left unset. Combine with `.with_*` builders or struct
    /// update syntax (`..ResumeSessionOptions::new(id)`) to populate the
    /// fields you need.
    pub fn new(session_id: SessionId) -> Self {
        Self {
            session_id,
            model: None,
            client_name: None,
            system_message: None,
            diagnostics: None,
            lifecycle_diagnostics: None,
            working_directory: None,
            suppress_resume_event: None,
            continue_pending_work: None,
            allow_transcript_recovery: None,
            system_message_transform: None,
            inference: Default::default(),
            policy: Default::default(),
            events: Default::default(),
            interaction: Default::default(),
            tools: Default::default(),
            canvases: Default::default(),
            extensions: Default::default(),
            agents: Default::default(),
            skills: Default::default(),
            mcp: Default::default(),
            config_directory: None,
            enable_config_discovery: None,
            state: Default::default(),
            instructions: Default::default(),
            hooks: Default::default(),
            git: Default::default(),
            github: Default::default(),
            plugins: Default::default(),
            permissions: Default::default(),
            filesystem: Default::default(),
            execution: Default::default(),
            experiments: Default::default(),
        }
    }

    /// Install a [`SystemMessageTransform`].
    pub fn with_system_message_transform(
        mut self,
        transform: Arc<dyn SystemMessageTransform>,
    ) -> Self {
        self.system_message_transform = Some(transform);
        self
    }

    /// Set the model identifier to switch to on resume (e.g. `"claude-sonnet-4"`).
    pub fn with_model(mut self, model: impl Into<String>) -> Self {
        self.model = Some(model.into());
        self
    }

    /// Set the application name sent as `User-Agent` context.
    pub fn with_client_name(mut self, name: impl Into<String>) -> Self {
        self.client_name = Some(name.into());
        self
    }

    /// Re-supply the system message so the agent retains workspace context
    /// across CLI process restarts.
    pub fn with_system_message(mut self, system_message: SystemMessageConfig) -> Self {
        self.system_message = Some(system_message);
        self
    }

    /// Set the session-scoped MCP diagnostic level on resume.
    pub fn with_diagnostics(mut self, diagnostics: DiagnosticsConfiguration) -> Self {
        self.diagnostics = Some(diagnostics);
        self
    }

    /// Set the per-session working directory on resume.
    pub fn with_working_directory(mut self, dir: impl Into<PathBuf>) -> Self {
        self.working_directory = Some(dir.into());
        self
    }

    /// Skip the `session.resume` event for this resume when `suppress` is
    /// `true`. See [`Self::suppress_resume_event`].
    pub fn with_suppress_resume_event(mut self, suppress: bool) -> Self {
        self.suppress_resume_event = Some(suppress);
        self
    }

    /// When `true`, instructs the runtime to continue any tool calls or
    /// permission requests that were pending when the previous connection
    /// was dropped. To hand off a session from one process to another, end
    /// the first client with [`Client::stop`](crate::Client::stop), not
    /// [`Client::force_stop`](crate::Client::force_stop), which can lose
    /// pending work the runtime has not saved yet.
    /// When `false` (the default), pending work is treated as interrupted on
    /// resume; already-recorded tool results are preserved.
    ///
    /// See [`Self::continue_pending_work`] for pending-request recovery requirements.
    pub fn with_continue_pending_work(mut self, continue_pending: bool) -> Self {
        self.continue_pending_work = Some(continue_pending);
        self
    }

    /// Set [`Self::allow_transcript_recovery`].
    pub fn with_allow_transcript_recovery(mut self, allow: bool) -> Self {
        self.allow_transcript_recovery = Some(allow);
        self
    }
    /// Update the inference option group.
    pub fn with_inference(
        mut self,
        configure: impl FnOnce(SessionInferenceOptions) -> SessionInferenceOptions,
    ) -> Self {
        self.inference = configure(self.inference);
        self
    }

    /// Update the policy option group.
    pub fn with_policy(
        mut self,
        configure: impl FnOnce(SessionPolicyOptions) -> SessionPolicyOptions,
    ) -> Self {
        self.policy = configure(self.policy);
        self
    }

    /// Update the events option group.
    pub fn with_events(
        mut self,
        configure: impl FnOnce(SessionEventOptions) -> SessionEventOptions,
    ) -> Self {
        self.events = configure(self.events);
        self
    }

    /// Update the interaction option group.
    pub fn with_interaction(
        mut self,
        configure: impl FnOnce(SessionInteractionOptions) -> SessionInteractionOptions,
    ) -> Self {
        self.interaction = configure(self.interaction);
        self
    }

    /// Update the tools option group.
    pub fn with_tools(
        mut self,
        configure: impl FnOnce(SessionToolOptions) -> SessionToolOptions,
    ) -> Self {
        self.tools = configure(self.tools);
        self
    }

    /// Update the canvases option group.
    pub fn with_canvases(
        mut self,
        configure: impl FnOnce(ResumeSessionCanvasOptions) -> ResumeSessionCanvasOptions,
    ) -> Self {
        self.canvases = configure(self.canvases);
        self
    }

    /// Update the extensions option group.
    pub fn with_extensions(
        mut self,
        configure: impl FnOnce(SessionExtensionOptions) -> SessionExtensionOptions,
    ) -> Self {
        self.extensions = configure(self.extensions);
        self
    }

    /// Update the agents option group.
    pub fn with_agents(
        mut self,
        configure: impl FnOnce(SessionAgentOptions) -> SessionAgentOptions,
    ) -> Self {
        self.agents = configure(self.agents);
        self
    }

    /// Update the skills option group.
    pub fn with_skills(
        mut self,
        configure: impl FnOnce(SessionSkillOptions) -> SessionSkillOptions,
    ) -> Self {
        self.skills = configure(self.skills);
        self
    }

    /// Update the mcp option group.
    pub fn with_mcp(
        mut self,
        configure: impl FnOnce(SessionMcpOptions) -> SessionMcpOptions,
    ) -> Self {
        self.mcp = configure(self.mcp);
        self
    }

    /// Override the default configuration directory location.
    pub fn with_config_directory(mut self, directory: impl Into<PathBuf>) -> Self {
        self.config_directory = Some(directory.into());
        self
    }

    /// Enables runtime discovery of supported configuration. Explicitly supplied
    /// configuration takes precedence; explicit feature roots remain independent.
    pub fn with_enable_config_discovery(mut self, enable: bool) -> Self {
        self.enable_config_discovery = Some(enable);
        self
    }

    /// Update the state option group.
    pub fn with_state(
        mut self,
        configure: impl FnOnce(SessionStateOptions) -> SessionStateOptions,
    ) -> Self {
        self.state = configure(self.state);
        self
    }

    /// Update the instructions option group.
    pub fn with_instructions(
        mut self,
        configure: impl FnOnce(SessionInstructionOptions) -> SessionInstructionOptions,
    ) -> Self {
        self.instructions = configure(self.instructions);
        self
    }

    /// Update the hooks option group.
    pub fn with_hooks(
        mut self,
        configure: impl FnOnce(SessionHookOptions) -> SessionHookOptions,
    ) -> Self {
        self.hooks = configure(self.hooks);
        self
    }

    /// Update the git option group.
    pub fn with_git(
        mut self,
        configure: impl FnOnce(SessionGitOptions) -> SessionGitOptions,
    ) -> Self {
        self.git = configure(self.git);
        self
    }

    /// Update the github option group.
    pub fn with_github(
        mut self,
        configure: impl FnOnce(SessionGitHubOptions) -> SessionGitHubOptions,
    ) -> Self {
        self.github = configure(self.github);
        self
    }

    /// Update the plugins option group.
    pub fn with_plugins(
        mut self,
        configure: impl FnOnce(SessionPluginOptions) -> SessionPluginOptions,
    ) -> Self {
        self.plugins = configure(self.plugins);
        self
    }

    /// Update the permissions option group.
    pub fn with_permissions(
        mut self,
        configure: impl FnOnce(SessionPermissionOptions) -> SessionPermissionOptions,
    ) -> Self {
        self.permissions = configure(self.permissions);
        self
    }

    /// Update the filesystem option group.
    pub fn with_filesystem(
        mut self,
        configure: impl FnOnce(SessionFilesystemOptions) -> SessionFilesystemOptions,
    ) -> Self {
        self.filesystem = configure(self.filesystem);
        self
    }

    /// Update the execution option group.
    pub fn with_execution(
        mut self,
        configure: impl FnOnce(SessionExecutionOptions) -> SessionExecutionOptions,
    ) -> Self {
        self.execution = configure(self.execution);
        self
    }

    /// Update the experiments option group.
    pub fn with_experiments(
        mut self,
        configure: impl FnOnce(SessionExperimentOptions) -> SessionExperimentOptions,
    ) -> Self {
        self.experiments = configure(self.experiments);
        self
    }
}

/// Controls how the system message is constructed.
///
/// Use `mode: "append"` (default) to add content after the built-in system
/// message, `"replace"` to substitute it entirely, or `"customize"` for
/// section-level overrides.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct SystemMessageConfig {
    /// How content is applied: `"append"` (default), `"replace"`, or `"customize"`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<String>,
    /// Content string to append or replace.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,
    /// Section-level overrides (used with `mode: "customize"`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sections: Option<HashMap<String, SectionOverride>>,
}

impl SystemMessageConfig {
    /// Construct an empty [`SystemMessageConfig`]; all fields default to
    /// unset.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the application mode: `"append"` (default), `"replace"`, or
    /// `"customize"`.
    pub fn with_mode(mut self, mode: impl Into<String>) -> Self {
        self.mode = Some(mode.into());
        self
    }

    /// Set the system message content (used by `"append"` and `"replace"`
    /// modes).
    pub fn with_content(mut self, content: impl Into<String>) -> Self {
        self.content = Some(content.into());
        self
    }

    /// Set the section-level overrides (used with `mode: "customize"`).
    pub fn with_sections(mut self, sections: HashMap<String, SectionOverride>) -> Self {
        self.sections = Some(sections);
        self
    }
}

/// An override operation for a single system message section.
///
/// Used within [`SystemMessageConfig::sections`] when `mode` is `"customize"`.
/// The `action` field determines the operation: `"replace"`, `"remove"`,
/// `"append"`, `"prepend"`, `"preserve"`, or `"transform"`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SectionOverride {
    /// Override action: `"replace"`, `"remove"`, `"append"`, `"prepend"`,
    /// `"preserve"`, or `"transform"`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub action: Option<String>,
    /// Content for the override operation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,
}

/// Response from `session.create`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateSessionResult {
    /// The CLI-assigned session ID.
    pub session_id: SessionId,
    /// Workspace directory for the session (infinite sessions).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace_path: Option<PathBuf>,
    /// Remote session URL, if the session is running remotely.
    #[serde(default, alias = "remote_url")]
    pub remote_url: Option<String>,
    /// Capabilities negotiated with the CLI for this session.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<SessionCapabilities>,
    /// Outcome of the requested [`SessionPermissionOptions::mode`].
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub permission_mode_result: Option<PermissionsSetModeResult>,
}

/// Details of transcript repair planned during resume.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TranscriptRecovery {
    /// Planned backup path; the backup is written on the next append.
    pub planned_backup_path: String,
    /// One-based physical line numbers removed from the transcript.
    pub invalid_line_numbers: Vec<u32>,
    /// Whether a valid session.start event was moved to the beginning.
    pub session_start_moved: bool,
}

/// Response from `session.resume`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ResumeSessionResult {
    /// The CLI-assigned session ID. Older runtimes may omit this on resume.
    #[serde(default)]
    pub session_id: Option<SessionId>,
    /// Workspace directory for the session (infinite sessions).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub workspace_path: Option<PathBuf>,
    /// Remote session URL, if the session is running remotely.
    #[serde(default, alias = "remote_url")]
    pub remote_url: Option<String>,
    /// Capabilities negotiated with the CLI for this session.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<SessionCapabilities>,
    /// Canvas instances already open when the session was resumed.
    #[serde(
        default,
        alias = "openCanvasInstances",
        skip_serializing_if = "Option::is_none"
    )]
    pub open_canvases: Option<Vec<OpenCanvasInstance>>,
    /// Recovery performed in memory while loading the session.
    #[serde(default)]
    pub transcript_recovery: Option<TranscriptRecovery>,
    /// Outcome of the requested [`SessionPermissionOptions::mode`].
    #[serde(default)]
    pub permission_mode_result: Option<PermissionsSetModeResult>,
}

/// Severity level for [`Session::log`](crate::session::Session::log) messages.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LogLevel {
    /// Informational message (default).
    #[default]
    Info,
    /// Warning message.
    Warning,
    /// Error message.
    Error,
}

/// Options for [`Session::log`](crate::session::Session::log).
///
/// Pass `None` to `log` for defaults (info level, persisted to the session
/// event log on disk).
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LogOptions {
    /// Log severity. `None` lets the server pick (defaults to `info`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub level: Option<LogLevel>,
    /// When `Some(true)`, the message is transient and not persisted to the
    /// session event log on disk. `None` lets the server pick.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ephemeral: Option<bool>,
}

impl LogOptions {
    /// Set [`level`](Self::level).
    pub fn with_level(mut self, level: LogLevel) -> Self {
        self.level = Some(level);
        self
    }

    /// Set [`ephemeral`](Self::ephemeral).
    pub fn with_ephemeral(mut self, ephemeral: bool) -> Self {
        self.ephemeral = Some(ephemeral);
        self
    }
}

/// Options for [`Session::set_model`](crate::session::Session::set_model).
///
/// Pass `None` to `set_model` to switch model without any overrides.
#[derive(Debug, Clone, Default)]
pub struct SetModelOptions {
    /// Reasoning effort for the new model (e.g. `"low"`, `"medium"`,
    /// `"high"`, `"xhigh"`, `"max"`).
    pub reasoning_effort: Option<String>,
    /// Reasoning summary mode for the new model. Use
    /// [`ReasoningSummary::None`] to suppress summary output regardless of
    /// whether reasoning is enabled.
    pub reasoning_summary: Option<ReasoningSummary>,
    /// Explicit context window tier for the new model. Leave unset to use
    /// normal model behavior with no explicit tier.
    pub context_tier: Option<ContextTier>,
    /// Override individual model capabilities resolved by the runtime. Only
    /// fields set on the override are applied; the rest fall back to the
    /// runtime-resolved values for the model.
    pub model_capabilities: Option<crate::generated::api_types::ModelCapabilitiesOverride>,
    /// Auto routing preference to stage atomically with selecting the `auto`
    /// model.
    ///
    /// Leave as `None` to leave the current preference alone. The runtime
    /// rejects this option when the model is anything other than `auto`; use
    /// [`Session::set_auto_tier`](crate::session::Session::set_auto_tier) to
    /// change the preference without changing the selected model.
    ///
    /// For Copilot Fusion, prefer [`AutoTierPreference::Fusion`]. The
    /// `balance`, `hydrafusion` and `hydrafusion-max` identifiers it replaces
    /// remain accepted but are deprecated.
    pub auto_tier: Option<AutoTierPreference>,
}

/// Auto routing preference requested alongside a model switch.
///
/// **Experimental.** Part of an experimental Auto routing surface and may change
/// or be removed in a future release.
///
/// This is a three-state choice. Leaving [`SetModelOptions::auto_tier`] as
/// `None` leaves the current preference alone, which is different from
/// [`AutoTierPreference::Reset`], which actively resets it.
///
/// New ways to choose a preference can be added as variants, so a `match` on
/// this enum needs a wildcard arm.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum AutoTierPreference {
    /// Route using a specific tier.
    Tier(AutoTier),
    /// Return to the provider's default Auto routing.
    Reset,
    /// Route using a Copilot Fusion tier, matching the CLI's `--fusion-tier`.
    /// Valid when the model is `fusion` or `auto`; the runtime resolves the
    /// tier to the Auto routing preference it stands for.
    Fusion(FusionTier),
}

impl SetModelOptions {
    /// Set [`reasoning_effort`](Self::reasoning_effort).
    pub fn with_reasoning_effort(mut self, effort: impl Into<String>) -> Self {
        self.reasoning_effort = Some(effort.into());
        self
    }

    /// Set [`reasoning_summary`](Self::reasoning_summary).
    pub fn with_reasoning_summary(mut self, summary: ReasoningSummary) -> Self {
        self.reasoning_summary = Some(summary);
        self
    }

    /// Set [`context_tier`](Self::context_tier).
    pub fn with_context_tier(mut self, tier: ContextTier) -> Self {
        self.context_tier = Some(tier);
        self
    }

    /// Set [`model_capabilities`](Self::model_capabilities).
    pub fn with_model_capabilities(
        mut self,
        caps: crate::generated::api_types::ModelCapabilitiesOverride,
    ) -> Self {
        self.model_capabilities = Some(caps);
        self
    }

    /// Set [`auto_tier`](Self::auto_tier) to a specific routing tier.
    pub fn with_auto_tier(mut self, tier: AutoTier) -> Self {
        self.auto_tier = Some(AutoTierPreference::Tier(tier));
        self
    }

    /// Set [`auto_tier`](Self::auto_tier) to return to the provider's default
    /// Auto routing.
    pub fn with_reset_auto_tier(mut self) -> Self {
        self.auto_tier = Some(AutoTierPreference::Reset);
        self
    }

    /// Set [`auto_tier`](Self::auto_tier) to a Copilot Fusion tier, matching
    /// the CLI's `--fusion-tier`.
    pub fn with_fusion_tier(mut self, tier: FusionTier) -> Self {
        self.auto_tier = Some(AutoTierPreference::Fusion(tier));
        self
    }
}

/// Response from the top-level `ping` RPC.
///
/// The `protocol_version` field is the most commonly-inspected piece —
/// see [`Client::verify_protocol_version`].
///
/// [`Client::verify_protocol_version`]: crate::Client::verify_protocol_version
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PingResponse {
    /// The message echoed back by the CLI.
    #[serde(default)]
    pub message: String,
    /// ISO 8601 timestamp when the ping was processed.
    #[serde(default)]
    pub timestamp: String,
    /// The protocol version negotiated by the CLI, if reported.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub protocol_version: Option<u32>,
}

/// Line range for file attachments.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AttachmentLineRange {
    /// First line (1-based).
    pub start: u32,
    /// Last line (inclusive).
    pub end: u32,
}

/// Cursor position within a file selection.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AttachmentSelectionPosition {
    /// Line number (0-based).
    pub line: u32,
    /// Character offset (0-based).
    pub character: u32,
}

/// Range of selected text within a file.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AttachmentSelectionRange {
    /// Start position.
    pub start: AttachmentSelectionPosition,
    /// End position.
    pub end: AttachmentSelectionPosition,
}

/// Type of GitHub reference attachment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum GitHubReferenceType {
    /// GitHub issue.
    Issue,
    /// GitHub pull request.
    Pr,
    /// GitHub discussion.
    Discussion,
}

/// Pointer to a GitHub repository (owner/name plus optional numeric id).
///
/// Used by the GitHub-anchored [`Attachment`] variants. Mirrors the field
/// shape of the generated `GitHubRepoRef`, but defined locally so it can
/// derive `Eq` for use inside the `Attachment` enum.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GitHubRepoPointer {
    /// Numeric GitHub repository id.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<i64>,
    /// Repository name (without owner).
    pub name: String,
    /// Repository owner login (user or organization).
    pub owner: String,
}

/// One side (head or base) of a GitHub single-file diff.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GitHubFileDiffSide {
    /// Repository-relative path to the file.
    pub path: String,
    /// Git ref (branch, tag, or commit SHA) the file is read at.
    pub r#ref: String,
    /// Repository the file lives in.
    pub repo: GitHubRepoPointer,
}

/// One side (head or base) of a GitHub tree comparison.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GitHubTreeComparisonSide {
    /// Repository the revision belongs to.
    pub repo: GitHubRepoPointer,
    /// Git revision (branch, tag, or commit SHA).
    pub revision: String,
}

/// Line range covered by a GitHub snippet attachment (1-based, inclusive end).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GitHubSnippetLineRange {
    /// Start line number (1-based).
    pub start: i64,
    /// End line number (1-based, inclusive).
    pub end: i64,
}

/// An attachment included with a user message.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(
    tag = "type",
    rename_all = "camelCase",
    rename_all_fields = "camelCase"
)]
#[non_exhaustive]
pub enum Attachment {
    /// A file path, optionally with a line range.
    File {
        /// Absolute path to the file.
        path: PathBuf,
        /// Label shown in the UI.
        #[serde(skip_serializing_if = "Option::is_none")]
        display_name: Option<String>,
        /// Optional line range to focus on.
        #[serde(skip_serializing_if = "Option::is_none")]
        line_range: Option<AttachmentLineRange>,
    },
    /// A directory path.
    Directory {
        /// Absolute path to the directory.
        path: PathBuf,
        /// Label shown in the UI.
        #[serde(skip_serializing_if = "Option::is_none")]
        display_name: Option<String>,
    },
    /// A text selection within a file.
    Selection {
        /// Path to the file containing the selection.
        file_path: PathBuf,
        /// The selected text content.
        text: String,
        /// Label shown in the UI.
        #[serde(skip_serializing_if = "Option::is_none")]
        display_name: Option<String>,
        /// Character range of the selection.
        selection: AttachmentSelectionRange,
    },
    /// Raw binary data (e.g. an image).
    Blob {
        /// Base64-encoded data.
        data: String,
        /// MIME type of the data.
        mime_type: String,
        /// Label shown in the UI.
        #[serde(skip_serializing_if = "Option::is_none")]
        display_name: Option<String>,
    },
    /// Context captured from an extension-owned canvas.
    #[serde(rename = "extension_context")]
    ExtensionContext {
        /// ISO 8601 timestamp when the context was captured.
        captured_at: String,
        /// Extension that owns the canvas.
        extension_id: String,
        /// Canvas declaration identifier when the context is bound to a canvas.
        #[serde(skip_serializing_if = "Option::is_none")]
        canvas_id: Option<String>,
        /// Open canvas instance identifier when the context is bound to a canvas.
        #[serde(skip_serializing_if = "Option::is_none")]
        instance_id: Option<String>,
        /// Human-readable context title.
        title: String,
        /// Extension-defined structured context payload.
        #[serde(skip_serializing_if = "Option::is_none")]
        payload: Option<Value>,
    },
    /// A reference to a GitHub issue, PR, or discussion.
    #[serde(rename = "github_reference")]
    GitHubReference {
        /// Issue/PR/discussion number.
        number: u64,
        /// Title of the referenced item.
        title: String,
        /// Kind of reference.
        reference_type: GitHubReferenceType,
        /// Current state (e.g. "open", "closed").
        state: String,
        /// URL to the referenced item.
        url: String,
    },
    /// A pointer to a GitHub commit.
    #[serde(rename = "github_commit")]
    GitHubCommit {
        /// First line of the commit message.
        message: String,
        /// Full commit SHA.
        oid: String,
        /// Repository the commit belongs to.
        repo: GitHubRepoPointer,
        /// URL to the commit on GitHub.
        url: String,
    },
    /// A pointer to a GitHub release.
    #[serde(rename = "github_release")]
    GitHubRelease {
        /// Human-readable release name.
        name: String,
        /// Repository the release belongs to.
        repo: GitHubRepoPointer,
        /// Git tag the release is anchored to.
        tag_name: String,
        /// URL to the release on GitHub.
        url: String,
    },
    /// A pointer to a GitHub Actions job.
    #[serde(rename = "github_actions_job")]
    GitHubActionsJob {
        /// Terminal conclusion of the job when finished (e.g. "success",
        /// "failure", "cancelled"). Absent for in-progress jobs.
        #[serde(skip_serializing_if = "Option::is_none")]
        conclusion: Option<String>,
        /// Job id within the workflow run.
        job_id: i64,
        /// Display name of the job.
        job_name: String,
        /// Repository the workflow run belongs to.
        repo: GitHubRepoPointer,
        /// URL to the job on GitHub.
        url: String,
        /// Display name of the workflow the job ran in.
        workflow_name: String,
    },
    /// A pointer to a GitHub repository.
    #[serde(rename = "github_repository")]
    GitHubRepository {
        /// Short description of the repository.
        #[serde(skip_serializing_if = "Option::is_none")]
        description: Option<String>,
        /// Git ref this attachment is anchored at (branch, tag, or commit).
        /// When absent the default branch is implied.
        #[serde(skip_serializing_if = "Option::is_none")]
        r#ref: Option<String>,
        /// Repository pointer.
        repo: GitHubRepoPointer,
        /// URL to the repository on GitHub.
        url: String,
    },
    /// A pointer to a single-file diff. At least one of `head` and `base` is present.
    #[serde(rename = "github_file_diff")]
    GitHubFileDiff {
        /// File location on the base side of the diff. Absent for additions.
        #[serde(skip_serializing_if = "Option::is_none")]
        base: Option<GitHubFileDiffSide>,
        /// File location on the head side of the diff. Absent for deletions.
        #[serde(skip_serializing_if = "Option::is_none")]
        head: Option<GitHubFileDiffSide>,
        /// URL to the diff on GitHub (e.g. a commit, compare, or PR-file URL).
        url: String,
    },
    /// A pointer to a comparison between two git revisions.
    #[serde(rename = "github_tree_comparison")]
    GitHubTreeComparison {
        /// Base side of the comparison.
        base: GitHubTreeComparisonSide,
        /// Head side of the comparison.
        head: GitHubTreeComparisonSide,
        /// URL to the comparison on GitHub.
        url: String,
    },
    /// A generic GitHub URL reference.
    #[serde(rename = "github_url")]
    GitHubUrl {
        /// URL to the GitHub resource.
        url: String,
    },
    /// A pointer to a file in a GitHub repository at a specific ref.
    #[serde(rename = "github_file")]
    GitHubFile {
        /// Repository-relative path to the file.
        path: String,
        /// Git ref the file is read at (branch, tag, or commit SHA).
        r#ref: String,
        /// Repository the file lives in.
        repo: GitHubRepoPointer,
        /// URL to the file on GitHub.
        url: String,
    },
    /// A pointer to a line range inside a file in a GitHub repository.
    #[serde(rename = "github_snippet")]
    GitHubSnippet {
        /// Line range the snippet covers.
        line_range: GitHubSnippetLineRange,
        /// Repository-relative path to the file.
        path: String,
        /// Git ref the file is read at (branch, tag, or commit SHA).
        r#ref: String,
        /// Repository the file lives in.
        repo: GitHubRepoPointer,
        /// URL to the snippet on GitHub (with line anchor).
        url: String,
    },
}

impl Attachment {
    /// Returns the display name, if set.
    pub fn display_name(&self) -> Option<&str> {
        match self {
            Self::File { display_name, .. }
            | Self::Directory { display_name, .. }
            | Self::Selection { display_name, .. }
            | Self::Blob { display_name, .. } => display_name.as_deref(),
            Self::GitHubReference { .. }
            | Self::GitHubCommit { .. }
            | Self::GitHubRelease { .. }
            | Self::GitHubActionsJob { .. }
            | Self::GitHubRepository { .. }
            | Self::GitHubFileDiff { .. }
            | Self::GitHubTreeComparison { .. }
            | Self::GitHubUrl { .. }
            | Self::GitHubFile { .. }
            | Self::GitHubSnippet { .. }
            | Self::ExtensionContext { .. } => None,
        }
    }

    /// Returns a human-readable label, deriving one from the path if needed.
    pub fn label(&self) -> Option<String> {
        if let Some(display_name) = self
            .display_name()
            .map(str::trim)
            .filter(|name| !name.is_empty())
        {
            return Some(display_name.to_string());
        }

        match self {
            Self::GitHubReference { number, title, .. } => Some(if title.trim().is_empty() {
                format!("#{}", number)
            } else {
                title.trim().to_string()
            }),
            Self::ExtensionContext { title, .. } if !title.trim().is_empty() => {
                Some(title.trim().to_string())
            }
            _ => self.derived_display_name(),
        }
    }

    /// Ensure `display_name` is populated when the variant supports one.
    pub fn ensure_display_name(&mut self) {
        if self
            .display_name()
            .map(str::trim)
            .is_some_and(|name| !name.is_empty())
        {
            return;
        }

        let Some(derived_display_name) = self.derived_display_name() else {
            return;
        };

        match self {
            Self::File { display_name, .. }
            | Self::Directory { display_name, .. }
            | Self::Selection { display_name, .. }
            | Self::Blob { display_name, .. } => *display_name = Some(derived_display_name),
            Self::GitHubReference { .. }
            | Self::GitHubCommit { .. }
            | Self::GitHubRelease { .. }
            | Self::GitHubActionsJob { .. }
            | Self::GitHubRepository { .. }
            | Self::GitHubFileDiff { .. }
            | Self::GitHubTreeComparison { .. }
            | Self::GitHubUrl { .. }
            | Self::GitHubFile { .. }
            | Self::GitHubSnippet { .. }
            | Self::ExtensionContext { .. } => {}
        }
    }

    fn derived_display_name(&self) -> Option<String> {
        match self {
            Self::File { path, .. } | Self::Directory { path, .. } => {
                Some(attachment_name_from_path(path))
            }
            Self::Selection { file_path, .. } => Some(attachment_name_from_path(file_path)),
            Self::Blob { .. } => Some("attachment".to_string()),
            Self::GitHubReference { .. }
            | Self::GitHubCommit { .. }
            | Self::GitHubRelease { .. }
            | Self::GitHubActionsJob { .. }
            | Self::GitHubRepository { .. }
            | Self::GitHubFileDiff { .. }
            | Self::GitHubTreeComparison { .. }
            | Self::GitHubUrl { .. }
            | Self::GitHubFile { .. }
            | Self::GitHubSnippet { .. }
            | Self::ExtensionContext { .. } => None,
        }
    }
}

fn attachment_name_from_path(path: &Path) -> String {
    path.file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| {
            let full = path.to_string_lossy();
            if full.is_empty() {
                "attachment".to_string()
            } else {
                full.into_owned()
            }
        })
}

/// Normalize a list of attachments so every entry has a `display_name`.
pub fn ensure_attachment_display_names(attachments: &mut [Attachment]) {
    for attachment in attachments {
        attachment.ensure_display_name();
    }
}

/// Provenance of a message sent through `session.send`.
///
/// Source is independent of delivery mode. Leaving [`MessageOptions::source`]
/// unset omits the field and preserves the runtime's default for user messages.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum MessageSource {
    /// A message from a human user.
    User,
    /// An automated message from the integrating application.
    System,
    /// A message from the agent with this opaque sender ID.
    Agent(String),
    /// A turn sent on behalf of the automation with this opaque ID, such as a
    /// recurring check-in the user configured. The turn defines the session's
    /// task like a user message but carries no human authority. The ID must not
    /// be empty: the runtime does not treat `automation-` alone as an
    /// automation turn.
    Automation(String),
}

impl std::fmt::Display for MessageSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::User => f.write_str("user"),
            Self::System => f.write_str("system"),
            Self::Agent(id) => write!(f, "agent-{id}"),
            Self::Automation(id) => write!(f, "automation-{id}"),
        }
    }
}

impl Serialize for MessageSource {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

impl<'de> Deserialize<'de> for MessageSource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = String::deserialize(deserializer)?;
        match value.as_str() {
            "user" => Ok(Self::User),
            "system" => Ok(Self::System),
            value => value
                .strip_prefix("agent-")
                .map(|id| Self::Agent(id.to_owned()))
                .or_else(|| {
                    value
                        .strip_prefix("automation-")
                        .map(|id| Self::Automation(id.to_owned()))
                })
                .ok_or_else(|| {
                    serde::de::Error::custom(
                        "expected user, system, agent-<id>, or automation-<id>",
                    )
                }),
        }
    }
}

/// Message delivery mode for [`MessageOptions::mode`].
///
/// Controls how a prompt is delivered relative to in-flight session work.
/// Wire values: `"enqueue"` and `"immediate"`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
#[non_exhaustive]
pub enum DeliveryMode {
    /// Queue the prompt behind any in-flight work (default).
    Enqueue,
    /// Interrupt the session and run the prompt immediately.
    Immediate,
}

/// The UI mode the agent is in for a given turn, used by
/// [`MessageOptions::agent_mode`].
///
/// Wire values: `"interactive"`, `"plan"`, `"autopilot"`, `"shell"`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
#[non_exhaustive]
pub enum AgentMode {
    /// The agent is responding interactively to the user.
    Interactive,
    /// The agent is preparing a plan before making changes.
    Plan,
    /// The agent is working autonomously toward task completion.
    Autopilot,
    /// The agent is in shell-focused UI mode.
    Shell,
}

/// Options for sending a user message to the agent.
///
/// Used by both [`Session::send`](crate::session::Session::send) and
/// [`Session::send_and_wait`](crate::session::Session::send_and_wait); the
/// `wait_timeout` field is honored only by `send_and_wait` and is ignored by
/// `send`.
///
/// `MessageOptions` is `#[non_exhaustive]` and constructed via [`MessageOptions::new`]
/// plus the `with_*` chain so future fields can land without breaking callers.
/// For the trivial case, both `&str` and `String` implement `Into<MessageOptions>`,
/// so:
///
/// ```no_run
/// # use copilot_sdk::session::Session;
/// # async fn run(session: Session) -> Result<(), copilot_sdk::Error> {
/// session.send("hello").await?;
/// # Ok(()) }
/// ```
///
/// is equivalent to:
///
/// ```no_run
/// # use copilot_sdk::session::Session;
/// # use copilot_sdk::types::MessageOptions;
/// # async fn run(session: Session) -> Result<(), copilot_sdk::Error> {
/// session.send(MessageOptions::new("hello")).await?;
/// # Ok(()) }
/// ```
#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct MessageOptions {
    /// Per-run JSON Schema. Independent sends and subagents do not inherit it.
    /// Immediate steering must not specify a schema. Streaming events remain text.
    pub response_schema: Option<Value>,
    /// The user prompt to send.
    pub prompt: String,
    /// Optional message provenance. When `None`, the field is omitted,
    /// preserving the runtime's default for user messages.
    pub source: Option<MessageSource>,
    /// Optional message delivery mode for this turn.
    ///
    /// Controls whether the prompt is queued behind in-flight work
    /// ([`DeliveryMode::Enqueue`], default) or interrupts the session and
    /// runs immediately ([`DeliveryMode::Immediate`]).
    pub mode: Option<DeliveryMode>,
    /// Optional UI mode the agent was in when this message was sent
    /// (for example [`AgentMode::Plan`] or [`AgentMode::Autopilot`]).
    /// Defaults to the session's current mode when `None`.
    pub agent_mode: Option<AgentMode>,
    /// Optional attachments to include with the message.
    pub attachments: Option<Vec<Attachment>>,
    /// Maximum time to wait for the session to go idle. Honored only by
    /// `send_and_wait`. Defaults to 60 seconds when unset.
    pub wait_timeout: Option<Duration>,
    /// Custom HTTP headers to include in outbound model requests for this
    /// turn. When `None` or empty, no `requestHeaders` field is sent on
    /// the wire.
    pub request_headers: Option<HashMap<String, String>>,
    /// W3C Trace Context `traceparent` header for this turn.
    ///
    /// Per-turn override that takes precedence over
    /// [`ClientObservabilityOptions::on_get_trace_context`](crate::ClientObservabilityOptions::on_get_trace_context).
    /// When `None`, the SDK falls back to the provider (if configured)
    /// before omitting the field.
    pub traceparent: Option<String>,
    /// W3C Trace Context `tracestate` header for this turn.
    ///
    /// Per-turn override paired with [`traceparent`](Self::traceparent).
    pub tracestate: Option<String>,
    /// If provided, this is shown in the timeline instead of `prompt`.
    pub display_prompt: Option<String>,
    /// Opaque caller-chosen ID for the turn this message starts.
    ///
    /// The runtime copies it unchanged into
    /// `_meta["com.github.copilot/turnId"]` on every MCP `tools/call` made
    /// while serving that turn, including calls from sub-agents and Autopilot
    /// continuations. When a running turn consumes the message as visible user
    /// steering, the value (or its absence) replaces the turn's ID from the
    /// next model call onward. The `user.message` event for the message
    /// echoes it as `clientTurnId` and is persisted like other events; the
    /// value stamped on MCP calls is held in memory only and is not restored
    /// on resume.
    pub client_turn_id: Option<String>,
}

impl MessageOptions {
    /// Build a new `MessageOptions` with just a prompt.
    pub fn new(prompt: impl Into<String>) -> Self {
        Self {
            prompt: prompt.into(),
            response_schema: None,
            source: None,
            mode: None,
            agent_mode: None,
            attachments: None,
            wait_timeout: None,
            request_headers: None,
            traceparent: None,
            tracestate: None,
            display_prompt: None,
            client_turn_id: None,
        }
    }

    /// Set the message provenance without changing its delivery mode.
    pub fn with_source(mut self, source: MessageSource) -> Self {
        self.source = Some(source);
        self
    }

    /// Request provider-native structured output for this run.
    pub fn with_response_schema(mut self, schema: Value) -> Self {
        self.response_schema = Some(schema);
        self
    }

    /// Set the message delivery mode for this turn.
    ///
    /// Pass [`DeliveryMode::Immediate`] to interrupt the session and run
    /// the prompt now; the default ([`DeliveryMode::Enqueue`]) queues the
    /// prompt behind in-flight work.
    pub fn with_mode(mut self, mode: DeliveryMode) -> Self {
        self.mode = Some(mode);
        self
    }

    /// Set the per-message agent UI mode for this turn.
    ///
    /// When `None`, the session's current mode is used.
    pub fn with_agent_mode(mut self, agent_mode: AgentMode) -> Self {
        self.agent_mode = Some(agent_mode);
        self
    }

    /// Attach files / selections / blobs to the message.
    pub fn with_attachments(mut self, attachments: Vec<Attachment>) -> Self {
        self.attachments = Some(attachments);
        self
    }

    /// Override the default 60-second wait timeout for `send_and_wait`.
    pub fn with_wait_timeout(mut self, timeout: Duration) -> Self {
        self.wait_timeout = Some(timeout);
        self
    }

    /// Set custom HTTP headers for outbound model requests for this turn.
    pub fn with_request_headers(mut self, headers: HashMap<String, String>) -> Self {
        self.request_headers = Some(headers);
        self
    }

    /// Set both `traceparent` and `tracestate` from a [`TraceContext`].
    /// Either field may remain `None` if the [`TraceContext`] has no value
    /// for it. Use [`with_traceparent`](Self::with_traceparent) or
    /// [`with_tracestate`](Self::with_tracestate) to set them individually.
    pub fn with_trace_context(mut self, ctx: TraceContext) -> Self {
        self.traceparent = ctx.traceparent;
        self.tracestate = ctx.tracestate;
        self
    }

    /// Set the W3C `traceparent` header for this turn.
    pub fn with_traceparent(mut self, traceparent: impl Into<String>) -> Self {
        self.traceparent = Some(traceparent.into());
        self
    }

    /// Set the W3C `tracestate` header for this turn.
    pub fn with_tracestate(mut self, tracestate: impl Into<String>) -> Self {
        self.tracestate = Some(tracestate.into());
        self
    }

    /// Set the display prompt shown in the timeline instead of `prompt`.
    pub fn with_display_prompt(mut self, display_prompt: impl Into<String>) -> Self {
        self.display_prompt = Some(display_prompt.into());
        self
    }

    /// Set the opaque caller-chosen ID for the turn this message starts.
    ///
    /// See [`client_turn_id`](Self::client_turn_id).
    pub fn with_client_turn_id(mut self, client_turn_id: impl Into<String>) -> Self {
        self.client_turn_id = Some(client_turn_id.into());
        self
    }
}

impl From<&str> for MessageOptions {
    fn from(prompt: &str) -> Self {
        Self::new(prompt)
    }
}

impl From<String> for MessageOptions {
    fn from(prompt: String) -> Self {
        Self::new(prompt)
    }
}

impl From<&String> for MessageOptions {
    fn from(prompt: &String) -> Self {
        Self::new(prompt.clone())
    }
}

/// Response from [`Client::get_status`](crate::Client::get_status).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct GetStatusResponse {
    /// Package version (e.g. `"1.0.0"`).
    pub version: String,
    /// Protocol version for SDK compatibility.
    pub protocol_version: u32,
    /// Cached error-reporting policy, when supplied by the runtime.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub host_diagnostics: Option<crate::rpc::HostDiagnostics>,
}

/// Response from [`Client::get_auth_status`](crate::Client::get_auth_status).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct GetAuthStatusResponse {
    /// Whether the user is authenticated.
    pub is_authenticated: bool,
    /// Authentication type (e.g. `"user"`, `"env"`, `"gh-cli"`, `"hmac"`,
    /// `"api-key"`, `"token"`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auth_type: Option<String>,
    /// GitHub host URL.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub host: Option<String>,
    /// User login name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub login: Option<String>,
    /// Human-readable status message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status_message: Option<String>,
}

/// Wrapper for session event notifications received from the CLI.
///
/// The CLI sends these as JSON-RPC notifications on the `session.event` method.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionEventNotification {
    /// The session this event belongs to.
    pub session_id: SessionId,
    /// The event payload.
    pub event: SessionEvent,
}

/// A single event in a session's timeline.
///
/// Events form a linked chain via `parent_id`. The `event_type` string
/// identifies the kind (e.g. `"assistant.message_delta"`, `"session.idle"`,
/// `"tool.execution_start"`). Event-specific payload is in `data` as
/// untyped JSON.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionEvent {
    /// Unique event ID (UUID v4).
    pub id: String,
    /// ISO 8601 timestamp.
    pub timestamp: String,
    /// ID of the preceding event in the chain.
    pub parent_id: Option<String>,
    /// Transient events that are not persisted to disk.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ephemeral: Option<bool>,
    /// Sub-agent instance identifier. Absent for events emitted by the
    /// root/main agent and for session-level events.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_id: Option<String>,
    /// Debug timestamp: when the CLI received this event (ms since epoch).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub debug_cli_received_at_ms: Option<i64>,
    /// Debug timestamp: when the event was forwarded over WebSocket.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub debug_ws_forwarded_at_ms: Option<i64>,
    /// Event type string (e.g. `"assistant.message"`, `"session.idle"`).
    #[serde(rename = "type")]
    pub event_type: String,
    /// Event-specific data. Structure depends on `event_type`.
    pub data: Value,
}

/// An event payload with experimental programmatic tool-call provenance.
///
/// Deserialize [`SessionEvent::data`] into this wrapper around
/// [`PermissionRequestedData`](crate::session_events::PermissionRequestedData),
/// [`ToolExecutionStartData`](crate::session_events::ToolExecutionStartData), or
/// [`ToolExecutionCompleteData`](crate::session_events::ToolExecutionCompleteData)
/// to retain provenance without changing those payloads' existing struct literals
/// or exhaustive destructuring.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ProgrammaticToolCallData<T> {
    /// The existing event payload.
    #[serde(flatten)]
    pub data: T,
    /// Host-owned ID of the `run_program` invocation that owns this child call or permission.
    /// Absent for ordinary tool calls and permissions.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub program_tool_call_id: Option<String>,
}

/// An experimental typed session event retaining programmatic tool-call provenance.
///
/// Deserialize the whole wire event into this projection before converting to
/// [`TypedSessionEvent`](crate::session_events::TypedSessionEvent), whose legacy
/// payload structs cannot retain program ownership. Serialization restores the
/// ownership field inside `data`, preserving the session-event wire format.
/// Nest in [`SessionEventWithMetadata`] to also retain journal checkpoint markers.
#[derive(Debug, Clone)]
pub struct ProgrammaticToolCallEvent {
    /// The existing typed event envelope and payload.
    pub event: crate::session_events::TypedSessionEvent,
    /// Host-owned ID of the program invocation owning this call or permission.
    /// Absent for ordinary events.
    pub program_tool_call_id: Option<String>,
}

impl Serialize for ProgrammaticToolCallEvent {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut wire =
            serde_json::to_value(&self.event).map_err(<S::Error as serde::ser::Error>::custom)?;
        if self.event.parent_id.is_none() {
            wire["parentId"] = Value::Null;
        }
        if let Some(parent_id) = &self.program_tool_call_id {
            let data = wire
                .get_mut("data")
                .and_then(Value::as_object_mut)
                .ok_or_else(|| {
                    <S::Error as serde::ser::Error>::custom(
                        "program-owned event data must be an object",
                    )
                })?;
            data.insert(
                "programToolCallId".to_owned(),
                Value::String(parent_id.clone()),
            );
        }
        wire.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ProgrammaticToolCallEvent {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let mut wire = Value::deserialize(deserializer)?;
        let program_tool_call_id = wire
            .get_mut("data")
            .and_then(Value::as_object_mut)
            .and_then(|data| data.remove("programToolCallId"))
            .map(serde_json::from_value::<Option<String>>)
            .transpose()
            .map_err(<D::Error as serde::de::Error>::custom)?
            .flatten();
        let event = serde_json::from_value(wire).map_err(<D::Error as serde::de::Error>::custom)?;
        Ok(Self {
            event,
            program_tool_call_id,
        })
    }
}

/// An event-log record with optional journal replay metadata.
///
/// Deserialize replay-view records into this wrapper to retain their checkpoint
/// marker without changing the construction of [`SessionEvent`] or
/// [`TypedSessionEvent`](crate::session_events::TypedSessionEvent).
/// `T` defaults to [`SessionEvent`] and may also be a typed event.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionEventWithMetadata<T = SessionEvent> {
    /// The existing event envelope and payload.
    #[serde(flatten)]
    pub event: T,
    /// Whether this record is synthetic replay state in a linked journal head.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub checkpoint: Option<bool>,
}

/// An event-log page retaining checkpoint markers on replay-view records.
///
/// Returned by [`read`](crate::rpc::SessionRpcEventLog::read).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EventsReadResultWithMetadata {
    /// Opaque cursor to pass unchanged to the next read in the same view.
    pub cursor: String,
    /// Whether the cursor was accepted or restarted at a retained boundary.
    pub cursor_status: crate::rpc::EventsCursorStatus,
    /// Events and their optional replay metadata, in creation order.
    pub events: Vec<SessionEventWithMetadata>,
    /// Whether more events remain in the selected direction.
    pub has_more: bool,
}

impl crate::generated::api_types::TaskAnalyticsClassifyTrajectoryRequest {
    /// Builds a trajectory classification request from recorded session
    /// events, such as the result of [`Session::get_events`](crate::session::Session::get_events).
    ///
    /// Typed events can normalize or discard JSON fields. For exact archive
    /// identity across retries, assign [`Session::get_raw_events`](crate::session::Session::get_raw_events)
    /// directly to the request's `events` field instead.
    ///
    /// Other request fields use their defaults and can be set on the result.
    ///
    /// <div class="warning">
    ///
    /// **Experimental.** This API is part of an experimental wire-protocol surface
    /// and may change or be removed in future SDK or CLI releases.
    ///
    /// </div>
    pub fn from_session_events(events: &[SessionEvent]) -> Result<Self, crate::Error> {
        Ok(Self {
            events: events
                .iter()
                .map(serde_json::to_value)
                .collect::<Result<_, _>>()?,
            ..Default::default()
        })
    }
}

impl SessionEvent {
    /// Parse the string `event_type` into a typed [`SessionEventType`](crate::session_events::SessionEventType) enum.
    ///
    /// Returns `SessionEventType::Unknown` for unrecognized event types,
    /// ensuring forward compatibility with newer CLI versions.
    pub fn parsed_type(&self) -> crate::generated::SessionEventType {
        use serde::de::IntoDeserializer;
        let deserializer: serde::de::value::StrDeserializer<'_, serde::de::value::Error> =
            self.event_type.as_str().into_deserializer();
        crate::generated::SessionEventType::deserialize(deserializer)
            .unwrap_or(crate::generated::SessionEventType::Unknown)
    }

    /// Deserialize the event `data` field into a typed struct.
    ///
    /// Returns `None` if deserialization fails (e.g. unknown event type
    /// or schema mismatch). Prefer typed data accessors for specific
    /// event types where you need strongly-typed field access.
    pub fn typed_data<T: serde::de::DeserializeOwned>(&self) -> Option<T> {
        serde_json::from_value(self.data.clone()).ok()
    }

    /// Read the UI capabilities in a `capabilities.changed` event.
    pub fn capabilities_changed_data(
        &self,
    ) -> Option<crate::session_events::CapabilitiesChangedData> {
        if self.event_type != "capabilities.changed" {
            return None;
        }
        self.typed_data()
    }

    /// `model_call` errors are transient — the CLI agent loop continues
    /// after them and may succeed on the next turn. These should not be
    /// treated as session-ending errors.
    pub fn is_transient_error(&self) -> bool {
        self.event_type == "session.error"
            && self.data.get("errorType").and_then(|v| v.as_str()) == Some("model_call")
    }
}

/// A request from the CLI to invoke a client-defined tool.
///
/// Built from an `external_tool.requested` session event. The SDK sends the
/// handler's result back with `session.tools.handlePendingToolCall`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct ToolInvocation {
    /// Session that owns this tool call.
    pub session_id: SessionId,
    /// Unique ID for this tool call, used to correlate the response.
    pub tool_call_id: String,
    /// Name of the tool being invoked.
    pub tool_name: String,
    /// Tool arguments as JSON.
    pub arguments: Value,
    /// Snapshot of the session's currently initialized tools.
    ///
    /// The SDK populates this only when the invocation targets the built-in
    /// tool-search tool (`tool_search_tool`), so a tool-search override can
    /// rank/filter the live catalog — including MCP tools configured in
    /// settings — without issuing its own RPC. `None` for every other tool
    /// invocation. This field is not part of the wire protocol.
    #[serde(skip)]
    pub available_tools: Option<Vec<CurrentToolMetadata>>,
    /// W3C Trace Context `traceparent` header propagated from the CLI's
    /// `execute_tool` span. Pass through to OpenTelemetry-aware code so
    /// child spans created inside the handler are parented to the CLI
    /// span. `None` when the CLI has no trace context for this call.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub traceparent: Option<String>,
    /// W3C Trace Context `tracestate` paired with
    /// [`traceparent`](Self::traceparent).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tracestate: Option<String>,
    /// **Experimental.** Sub-agent instance that issued this call (the
    /// `agentId` of its session events). `None` for a root-agent call, and for
    /// every call when the runtime does not attribute sub-agent tool calls.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub agent_id: Option<String>,
    /// **Experimental.** Runtime request id of this call: the key
    /// `session.tools.handlePendingToolCall` and `session.tools.listPendingRequests`
    /// use. A root-agent call stays answerable after the session is resumed with
    /// pending work continued; a sub-agent's call can be answered only while that
    /// sub-agent is still running.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub request_id: Option<RequestId>,
}

impl ToolInvocation {
    /// Deserialize this invocation's [`arguments`](Self::arguments) into a
    /// strongly-typed parameter struct.
    ///
    /// Idiomatic way to extract typed parameters when implementing
    /// [`ToolHandler`](crate::tool::ToolHandler) directly. Equivalent to
    /// `serde_json::from_value(invocation.arguments.clone())` with the SDK's
    /// error type.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use copilot_sdk::{Error, types::ToolInvocation, ToolResult};
    /// # use serde::Deserialize;
    /// # #[derive(Deserialize)] struct MyParams { city: String }
    /// # async fn example(inv: ToolInvocation) -> Result<ToolResult, Error> {
    /// let params: MyParams = inv.params()?;
    /// // …use `inv.session_id` / `inv.tool_call_id` alongside `params`…
    /// # let _ = params; Ok(ToolResult::Text(String::new()))
    /// # }
    /// ```
    pub fn params<P: serde::de::DeserializeOwned>(&self) -> Result<P, crate::Error> {
        serde_json::from_value(self.arguments.clone()).map_err(crate::Error::from)
    }

    /// Returns the propagated [`TraceContext`] for this invocation, or
    /// [`TraceContext::default()`] when the CLI sent no headers.
    pub fn trace_context(&self) -> TraceContext {
        TraceContext {
            traceparent: self.traceparent.clone(),
            tracestate: self.tracestate.clone(),
        }
    }
}

/// Binary content returned by a tool.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolBinaryResult {
    /// Base64-encoded binary data.
    pub data: String,
    /// MIME type for the binary data.
    pub mime_type: String,
    /// Type identifier for the binary result.
    pub r#type: String,
    /// Optional description shown alongside the binary result.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

/// Expanded tool result with metadata for the LLM and session log.
///
/// This type is `#[non_exhaustive]`: it mirrors a growing wire shape, so
/// construct it via [`ToolResultExpanded::new`] plus the `with_*` chain
/// rather than a struct literal, allowing new fields to land without
/// breaking callers.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub struct ToolResultExpanded {
    /// Result text sent back to the LLM.
    pub text_result_for_llm: String,
    /// `"success"` or `"failure"`.
    pub result_type: String,
    /// Binary payloads sent back to the LLM.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub binary_results_for_llm: Option<Vec<ToolBinaryResult>>,
    /// Optional log message for the session timeline.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_log: Option<String>,
    /// Error message, if the tool failed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
    /// Tool-specific telemetry emitted with the result.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tool_telemetry: Option<HashMap<String, Value>>,
    /// Names of tools returned by a tool-search tool.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tool_references: Option<Vec<String>>,
}

impl ToolResultExpanded {
    /// Construct an expanded result with the required `text_result_for_llm`
    /// and `result_type` (`"success"` or `"failure"`). All optional metadata
    /// fields start unset; populate them with the `with_*` builders.
    pub fn new(text_result_for_llm: impl Into<String>, result_type: impl Into<String>) -> Self {
        Self {
            text_result_for_llm: text_result_for_llm.into(),
            result_type: result_type.into(),
            binary_results_for_llm: None,
            session_log: None,
            error: None,
            tool_telemetry: None,
            tool_references: None,
        }
    }

    /// Set the binary payloads returned to the LLM.
    pub fn with_binary_results(mut self, results: Vec<ToolBinaryResult>) -> Self {
        self.binary_results_for_llm = Some(results);
        self
    }

    /// Set the log message for the session timeline.
    pub fn with_session_log(mut self, session_log: impl Into<String>) -> Self {
        self.session_log = Some(session_log.into());
        self
    }

    /// Set the error message, marking the tool as failed.
    pub fn with_error(mut self, error: impl Into<String>) -> Self {
        self.error = Some(error.into());
        self
    }

    /// Set the tool-specific telemetry emitted with the result.
    pub fn with_tool_telemetry(mut self, telemetry: HashMap<String, Value>) -> Self {
        self.tool_telemetry = Some(telemetry);
        self
    }

    /// Set the names of tools returned by a tool-search tool.
    pub fn with_tool_references<I, S>(mut self, references: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.tool_references = Some(references.into_iter().map(Into::into).collect());
        self
    }
}

/// Result of a tool invocation — either a plain text string or an expanded result.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
#[non_exhaustive]
pub enum ToolResult {
    /// Simple text result passed directly to the LLM.
    Text(String),
    /// Structured result with metadata.
    Expanded(ToolResultExpanded),
}

/// JSON-RPC response wrapper for a tool result, sent back to the CLI.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolResultResponse {
    /// The tool result payload.
    pub result: ToolResult,
}

/// Metadata for a persisted session, returned by `session.list`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionMetadata {
    /// The session's unique identifier.
    pub session_id: SessionId,
    /// ISO 8601 timestamp when the session was created.
    pub start_time: String,
    /// ISO 8601 timestamp of the last modification.
    pub modified_time: String,
    /// Agent-generated session summary.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    /// Whether the session is running remotely.
    pub is_remote: bool,
    /// Name of the client that created the session (for example `github/cli`).
    /// `None` when unknown.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_name: Option<String>,
    /// Name of the client that most recently ran the session: the client that
    /// created it, or a client that resumed it and then ran it. Resuming a
    /// session only to observe it does not change this. `None` when unknown,
    /// such as for sessions persisted before this field existed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub last_client_name: Option<String>,
}

/// Response from `session.list`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ListSessionsResponse {
    /// The list of session metadata entries.
    pub sessions: Vec<SessionMetadata>,
}

/// Filter options for [`Client::list_sessions`](crate::Client::list_sessions).
///
/// All fields are optional; unset fields don't constrain the result.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionListFilter {
    /// Filter by exact `cwd` match.
    #[serde(default, skip_serializing_if = "Option::is_none", rename = "cwd")]
    pub working_directory: Option<String>,
    /// Filter by git root path.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub git_root: Option<String>,
    /// Filter by repository in `owner/repo` form.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,
    /// Filter by git branch name.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub branch: Option<String>,
}

/// Response from `session.getMetadata`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GetSessionMetadataResponse {
    /// The session metadata, or `None` if the session was not found.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session: Option<SessionMetadata>,
}

/// Response from `session.getLastId`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GetLastSessionIdResponse {
    /// The most recently updated session ID, or `None` if no sessions exist.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_id: Option<SessionId>,
}

/// Response from `session.getForeground`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GetForegroundSessionResponse {
    /// The current foreground session ID, or `None` if no foreground session.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_id: Option<SessionId>,
}

/// Response from `session.getMessages`.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GetMessagesResponse {
    /// Timeline events for the session.
    pub events: Vec<SessionEvent>,
}

/// Result of an elicitation (interactive UI form) request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ElicitationResult {
    /// User's action: `"accept"`, `"decline"`, or `"cancel"`.
    pub action: String,
    /// Form data submitted by the user (present when action is `"accept"`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<Value>,
}

/// Elicitation display mode.
///
/// New modes may be added by the CLI in future protocol versions; the
/// `Unknown` variant keeps deserialization from failing on unrecognised
/// values so the SDK can still surface the request to callers.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[non_exhaustive]
pub enum ElicitationMode {
    /// Structured form input rendered by the host.
    Form,
    /// Browser redirect to a URL.
    Url,
    /// A mode not yet known to this SDK version.
    #[serde(other)]
    Unknown,
}

/// An incoming elicitation request from the CLI (provider side).
///
/// Received via `elicitation.requested` session event when the session has
/// an [`ElicitationHandler`] installed.
/// The provider should render a form or dialog and return an
/// [`ElicitationResult`].
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ElicitationRequest {
    /// Message describing what information is needed from the user.
    pub message: String,
    /// JSON Schema describing the form fields to present.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub requested_schema: Option<Value>,
    /// Elicitation display mode.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<ElicitationMode>,
    /// The source that initiated the request (e.g. MCP server name).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elicitation_source: Option<String>,
    /// URL to open in the user's browser (url mode only).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
}

/// Context for an offer to recover the active request using Auto.
#[non_exhaustive]
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AutoModeRecoveryContext {
    /// The one-use offer identifier, not a replay token.
    pub request_id: String,
    /// The original explicit model selection.
    pub original_model: String,
    /// Whether this offer permits remembering consent.
    /// Currently true only for per-model rate limits.
    pub allow_always: bool,
    /// The retained interaction identifier, when available.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub interaction_id: Option<String>,
    /// The original user-message identifier, when available.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub originating_message_id: Option<String>,
}

/// Session-level capabilities reported by the CLI after session creation.
///
/// Capabilities indicate which features the CLI host supports for this session.
/// Updated at runtime via `capabilities.changed` events.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionCapabilities {
    /// UI capabilities (elicitation support, etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ui: Option<UiCapabilities>,
}

/// UI-specific capabilities for a session.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UiCapabilities {
    /// Whether the host supports interactive elicitation dialogs.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elicitation: Option<bool>,
    /// **Experimental.** This field is part of an experimental wire-protocol
    /// surface (SEP-1865) and may change or be removed in a future release.
    ///
    /// Whether the runtime has accepted the session's MCP Apps (SEP-1865)
    /// opt-in. `Some(true)` when the consumer set
    /// [`SessionMcpOptions::enable_apps`]
    /// to `true` on create/resume **and** the runtime's `MCP_APPS` feature
    /// flag (or `COPILOT_MCP_APPS=true` env override) is on. Otherwise
    /// absent or `Some(false)`, indicating the runtime silently dropped the
    /// opt-in.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mcp_apps: Option<bool>,
    /// Host-specific canvas capabilities.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub canvases: Option<bool>,
}

/// Options for the [`SessionUi::input`](crate::session::SessionUi::input) convenience method.
#[derive(Debug, Clone, Default)]
pub struct UiInputOptions<'a> {
    /// Title label for the input field.
    pub title: Option<&'a str>,
    /// Descriptive text shown below the field.
    pub description: Option<&'a str>,
    /// Minimum character length.
    pub min_length: Option<u64>,
    /// Maximum character length.
    pub max_length: Option<u64>,
    /// Semantic format hint.
    pub format: Option<InputFormat>,
    /// Default value pre-populated in the field.
    pub default: Option<&'a str>,
}

/// Semantic format hints for text input fields.
#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum InputFormat {
    /// Email address.
    Email,
    /// URI.
    Uri,
    /// Calendar date.
    Date,
    /// Date and time.
    DateTime,
}

impl InputFormat {
    /// Returns the JSON Schema format string for this variant.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Email => "email",
            Self::Uri => "uri",
            Self::Date => "date",
            Self::DateTime => "date-time",
        }
    }
}

/// Re-exports of generated protocol types that are part of the SDK's
/// public API surface. The canonical definitions live in
/// [`crate::rpc`]; they live here so the crate-root
/// `pub use types::*` surfaces them alongside hand-written SDK types.
pub use crate::generated::api_types::{
    Model, ModelBilling, ModelBillingTokenPrices, ModelBillingTokenPricesLongContext,
    ModelCapabilities, ModelCapabilitiesLimits, ModelCapabilitiesLimitsVision,
    ModelCapabilitiesSupports, ModelList, ModelPolicy, PermissionDecision,
    PermissionDecisionApproveOnce, PermissionDecisionContext, PermissionDecisionOutcome,
    PermissionDecisionReject, PermissionDecisionSource, PermissionDecisionSurface,
    PermissionDecisionUserNotAvailable, PermissionResponseCapability,
};

/// Permission categories the CLI may request approval for.
///
/// Wire values are the lower-kebab strings the CLI sends as the `kind`
/// discriminator on a permission request. Marked `#[non_exhaustive]`
/// because the CLI may add new kinds; matches must include a `_` arm.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[non_exhaustive]
pub enum PermissionRequestKind {
    /// Run a shell command.
    Shell,
    /// Write to a file.
    Write,
    /// Read a file.
    Read,
    /// Open a URL.
    Url,
    /// Invoke an MCP server tool.
    Mcp,
    /// Invoke a client-defined custom tool.
    CustomTool,
    /// Update agent memory.
    Memory,
    /// Run a hook callback.
    Hook,
    /// Unrecognized kind. The original wire string is available in
    /// [`PermissionRequestData::extra`] under the `kind` key.
    #[serde(other)]
    Unknown,
}

/// Data sent by the CLI for permission-related events.
///
/// Used for both the `permission.request` RPC call (which expects a response)
/// and `permission.requested` notifications (fire-and-forget). Contains the
/// full params object.
///
/// Convert handler data into the original request for managed-policy review.
/// The borrowed conversion leaves the handler data unchanged:
///
/// ```
/// use copilot_sdk::PermissionRequestData;
/// use copilot_sdk::rpc::SessionPermissionsManagedReviewRequiredParams;
/// use serde_json::json;
///
/// let data = PermissionRequestData {
///     extra: json!({
///         "requestId": "permission-1",
///         "permissionRequest": {"kind": "write", "path": "/workspace/report.txt"}
///     }),
///     ..Default::default()
/// };
/// let params = SessionPermissionsManagedReviewRequiredParams {
///     request: (&data).into(),
/// };
/// assert_eq!(params.request, json!({"kind": "write", "path": "/workspace/report.txt"}));
/// println!("{}", params.request);
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PermissionRequestData {
    /// The permission category being requested. `None` means the CLI did
    /// not include a `kind` field. Use this to branch on common cases
    /// (shell, write, etc.) without parsing [`extra`](Self::extra).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub kind: Option<PermissionRequestKind>,
    /// The originating tool-call ID, if this permission request is tied
    /// to a specific tool invocation.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tool_call_id: Option<String>,
    /// Whether managed policy requires an explicit human decision.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub managed_approval_required: Option<bool>,
    /// Whether managed settings are enabled for this session.
    #[serde(default, skip_serializing_if = "is_false")]
    pub managed_settings_enabled: bool,
    /// The full permission event params from the CLI, including the request ID
    /// and nested permission request. The shape varies by permission type and
    /// CLI version, so we preserve it as `Value`.
    #[serde(flatten)]
    pub extra: Value,
}

impl From<&PermissionRequestData> for Value {
    /// Copies the original request, without the permission event envelope.
    ///
    /// Legacy callbacks without `permissionRequest` retain their full raw data.
    fn from(data: &PermissionRequestData) -> Self {
        data.extra
            .get("permissionRequest")
            .unwrap_or(&data.extra)
            .clone()
    }
}

impl From<PermissionRequestData> for Value {
    /// Moves the original request, without the permission event envelope.
    ///
    /// Legacy callbacks without `permissionRequest` retain their full raw data.
    fn from(data: PermissionRequestData) -> Self {
        match data.extra {
            Value::Object(mut fields) => fields
                .remove("permissionRequest")
                .unwrap_or(Value::Object(fields)),
            value => value,
        }
    }
}

impl PermissionRequestData {
    /// Returns the derived user-facing prompt supplied by newer runtimes.
    ///
    /// The raw request retains managed-policy provenance. Only `commands`,
    /// `read`, `write`, `url`, and `workflow` prompts can clear that human
    /// requirement after Assisted approval. For missing or other prompt kinds,
    /// keep the raw marker authoritative.
    ///
    /// `Ok(None)` indicates an older runtime without the derived prompt.
    /// Unknown future prompt kinds return a deserialization error.
    pub fn prompt_request(
        &self,
    ) -> Result<Option<crate::session_events::PermissionPromptRequest>, serde_json::Error> {
        self.extra
            .get("promptRequest")
            .cloned()
            .map(serde_json::from_value)
            .transpose()
    }

    /// Returns the canonical runtime-owned Assisted Permissions assessment.
    ///
    /// This explains the assisted review only; `permission.completed` remains
    /// authoritative for the final permission result.
    ///
    /// `Ok(None)` indicates an older runtime or legacy direct permission
    /// callback without the canonical assessment.
    pub fn assisted_approval_assessment(
        &self,
    ) -> Result<Option<crate::session_events::AssistedApprovalAssessment>, serde_json::Error> {
        self.extra
            .get("assistedApprovalAssessment")
            .cloned()
            .map(serde_json::from_value)
            .transpose()
    }
}

/// Data sent by the CLI with an `exitPlanMode.request` RPC call.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExitPlanModeData {
    /// Markdown summary of the plan presented to the user.
    #[serde(default)]
    pub summary: String,
    /// Full plan content (e.g. the plan.md body), if available.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub plan_content: Option<String>,
    /// Allowed exit actions (e.g. "interactive", "autopilot", "autopilot_fleet").
    #[serde(default)]
    pub actions: Vec<String>,
    /// Which action the CLI recommends, defaults to "autopilot".
    #[serde(default = "default_recommended_action")]
    pub recommended_action: String,
}

fn default_recommended_action() -> String {
    "autopilot".to_string()
}

impl Default for ExitPlanModeData {
    fn default() -> Self {
        Self {
            summary: String::new(),
            plan_content: None,
            actions: Vec::new(),
            recommended_action: default_recommended_action(),
        }
    }
}

#[cfg(test)]
mod customization_directory_tests;

#[cfg(test)]
mod tests;

#[cfg(test)]
mod permission_builder_tests;
