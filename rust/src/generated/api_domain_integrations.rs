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

/// Canvas action invocation parameters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasActionInvokeRequest {
    /// Action name to invoke
    pub action_name: String,
    /// Action input
    pub input: Option<serde_json::Value>,
    /// Open canvas instance identifier
    pub instance_id: String,
}
crate::record_serde::wire_record!(CanvasActionInvokeRequest { action_name: "actionName", input: "input" if is_none, instance_id: "instanceId" });

/// Canvas action invocation result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasActionInvokeResult {
    /// Provider-supplied action result
    pub result: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(CanvasActionInvokeResult { result: "result" if is_none });

/// Canvas close parameters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasCloseRequest {
    /// Open canvas instance identifier
    pub instance_id: String,
}
crate::record_serde::wire_record!(CanvasCloseRequest {
    instance_id: "instanceId"
});

/// Host capabilities
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasHostContextCapabilities {
    /// Whether canvas rendering is supported
    pub canvases: Option<bool>,
}
crate::record_serde::wire_record!(CanvasHostContextCapabilities { canvases: "canvases" if is_none });

/// Host context supplied by the runtime.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasHostContext {
    /// Host capabilities
    pub capabilities: Option<CanvasHostContextCapabilities>,
}
crate::record_serde::wire_record!(CanvasHostContext { capabilities: "capabilities" if is_none });

/// Declared canvases available in this session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasList {
    /// Declared canvases available in this session
    pub canvases: Vec<DiscoveredCanvas>,
}
crate::record_serde::wire_record!(CanvasList {
    canvases: "canvases"
});

/// Live open-canvas snapshot.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasListOpenResult {
    /// Currently open canvas instances
    pub open_canvases: Vec<OpenCanvasInstance>,
}
crate::record_serde::wire_record!(CanvasListOpenResult {
    open_canvases: "openCanvases"
});

/// Canvas open parameters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasOpenRequest {
    /// Provider-local canvas identifier
    pub canvas_id: String,
    /// Owning provider identifier. Optional when the canvasId is unique across providers; required to disambiguate when multiple providers register the same canvasId.
    pub extension_id: Option<String>,
    /// Canvas open input
    pub input: Option<serde_json::Value>,
    /// Caller-supplied stable instance identifier
    pub instance_id: String,
}
crate::record_serde::wire_record!(CanvasOpenRequest { canvas_id: "canvasId", extension_id: "extensionId" if is_none, input: "input" if is_none, instance_id: "instanceId" });

/// Session context supplied by the runtime.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasSessionContext {
    /// Active session working directory, when known.
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(CanvasSessionContext { working_directory: "workingDirectory" if is_none });

/// Canvas close parameters sent to the provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasProviderCloseRequest {
    /// Target session identifier
    pub session_id: SessionId,
    /// Owning provider identifier
    pub extension_id: String,
    /// Provider-local canvas identifier
    pub canvas_id: String,
    /// Canvas instance identifier
    pub instance_id: String,
    /// Host context supplied by the runtime.
    pub host: Option<CanvasHostContext>,
    /// Session context supplied by the runtime.
    pub session: Option<CanvasSessionContext>,
}
crate::record_serde::wire_record!(CanvasProviderCloseRequest { session_id: "sessionId", extension_id: "extensionId", canvas_id: "canvasId", instance_id: "instanceId", host: "host" if is_none, session: "session" if is_none });

/// Canvas action invocation parameters sent to the provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasProviderInvokeActionRequest {
    /// Target session identifier
    pub session_id: SessionId,
    /// Owning provider identifier
    pub extension_id: String,
    /// Provider-local canvas identifier
    pub canvas_id: String,
    /// Canvas instance identifier
    pub instance_id: String,
    /// Action name to invoke
    pub action_name: String,
    /// Action input
    pub input: Option<serde_json::Value>,
    /// Host context supplied by the runtime.
    pub host: Option<CanvasHostContext>,
    /// Session context supplied by the runtime.
    pub session: Option<CanvasSessionContext>,
}
crate::record_serde::wire_record!(CanvasProviderInvokeActionRequest { session_id: "sessionId", extension_id: "extensionId", canvas_id: "canvasId", instance_id: "instanceId", action_name: "actionName", input: "input" if is_none, host: "host" if is_none, session: "session" if is_none });

/// Canvas open parameters sent to the provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasProviderOpenRequest {
    /// Target session identifier
    pub session_id: SessionId,
    /// Owning provider identifier
    pub extension_id: String,
    /// Provider-local canvas identifier
    pub canvas_id: String,
    /// Stable caller-supplied canvas instance identifier
    pub instance_id: String,
    /// Canvas open input
    pub input: Option<serde_json::Value>,
    /// Host context supplied by the runtime.
    pub host: Option<CanvasHostContext>,
    /// Session context supplied by the runtime.
    pub session: Option<CanvasSessionContext>,
}
crate::record_serde::wire_record!(CanvasProviderOpenRequest { session_id: "sessionId", extension_id: "extensionId", canvas_id: "canvasId", instance_id: "instanceId", input: "input" if is_none, host: "host" if is_none, session: "session" if is_none });

/// Canvas open result returned by the provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasProviderOpenResult {
    /// Provider-supplied status text
    pub status: Option<String>,
    /// Provider-supplied title
    pub title: Option<String>,
    /// URL for web-rendered canvases
    pub url: Option<String>,
}
crate::record_serde::wire_record!(CanvasProviderOpenResult { status: "status" if is_none, title: "title" if is_none, url: "url" if is_none });

/// Internal canvas provider registration parameters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasProviderRegisterRequest {
    /// Canvas contributions supplied by the provider
    pub canvases: Vec<serde_json::Value>,
    /// Connection identifier for callback routing
    pub connection_id: String,
    /// Provider metadata supplied by the host
    pub info: serde_json::Value,
}
crate::record_serde::wire_record!(CanvasProviderRegisterRequest {
    canvases: "canvases",
    connection_id: "connectionId",
    info: "info"
});

/// Internal canvas provider unregistration parameters.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasProviderUnregisterRequest {
    /// Connection identifier to unregister
    pub connection_id: String,
}
crate::record_serde::wire_record!(CanvasProviderUnregisterRequest {
    connection_id: "connectionId"
});

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
    /// Exact authenticated local attachment identifier. Omit for the SDK connection's owned session.
    pub attach_id: Option<i64>,
    /// Open canvas instance identifier
    pub instance_id: String,
}
crate::record_serde::wire_record!(CanvasSourceCloseRequest { attach_id: "attachId" if is_none, instance_id: "instanceId" });

/// Semantic digest of a strictly parsed and schema-validated JSON MCP card. Both URL-backed and embedded cards are canonicalised with RFC 8785 JSON Canonicalization Scheme, encoded as UTF-8, and hashed with SHA-256.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CardDigest {
    /// Digest algorithm and canonical representation
    #[serde(deserialize_with = "CardDigest::deserialize_algorithm")]
    pub algorithm: CardDigestAlgorithm,
    /// SHA-256 digest of the RFC 8785 canonical UTF-8 bytes, encoded as exactly 64 lowercase hexadecimal characters.
    pub value: String,
}

impl CardDigest {
    fn deserialize_algorithm<'de, D>(deserializer: D) -> Result<CardDigestAlgorithm, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "sha256-rfc8785" {
            return Err(serde::de::Error::unknown_variant(
                &value,
                &["sha256-rfc8785"],
            ));
        }
        <CardDigestAlgorithm>::deserialize(serde::de::value::StringDeserializer::<D::Error>::new(
            value,
        ))
    }
}

/// A presented handle was not accepted. Handles are runtime-instance scoped, TTL-bound, and single-use, so each way of failing is reported distinctly.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogHandleRejectedError {
    /// Which kind of handle was presented.
    pub handle_type: CatalogHandleType,
    /// Discriminator: a handle was rejected
    pub kind: CatalogHandleRejectedErrorKind,
    /// Human-readable explanation, safe to surface. Never contains the handle itself, nor a query, URL, or secret.
    pub message: String,
    /// Why the handle was rejected.
    pub reason: CatalogHandleRejectionReason,
}
crate::record_serde::wire_record!(CatalogHandleRejectedError {
    handle_type: "handleType",
    kind: "kind",
    message: "message",
    reason: "reason"
});

/// The candidate is discoverable but cannot be installed. `application/ai-skill` resolves here, because it stays searchable while remaining typed non-installable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogNotInstallableError {
    /// Discriminator: the candidate cannot be installed
    pub kind: CatalogNotInstallableErrorKind,
    /// Human-readable explanation, safe to surface. Never contains a query, URL, handle, or secret.
    pub message: String,
    /// Why the candidate cannot be installed.
    pub reason: CatalogNotInstallableReason,
}
crate::record_serde::wire_record!(CatalogNotInstallableError {
    kind: "kind",
    message: "message",
    reason: "reason"
});

/// No transport this runtime can use is available for the requested server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CatalogUnavailableTransportError {
    /// Discriminator: no usable transport is available
    pub kind: CatalogUnavailableTransportErrorKind,
    /// Human-readable explanation, safe to surface. Never contains a query, URL, handle, or secret.
    pub message: String,
    /// Why no transport could be offered.
    pub reason: CatalogUnavailableTransportReason,
}
crate::record_serde::wire_record!(CatalogUnavailableTransportError {
    kind: "kind",
    message: "message",
    reason: "reason"
});

/// Concrete configuration file containing an MCP server declaration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSourceFile {
    /// RFC 6901 JSON Pointer to the server declaration, when known
    pub json_pointer: Option<String>,
    /// Canonical file URI for the configuration document
    pub uri: String,
}
crate::record_serde::wire_record!(McpSourceFile { json_pointer: "jsonPointer" if is_none, uri: "uri" });

/// Plugin identity associated with an MCP server declaration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSourcePlugin {
    /// Canonical plugin identity
    pub id: String,
    /// Human-readable plugin name, when available
    pub name: Option<String>,
    /// Plugin version, when available
    pub version: Option<String>,
}
crate::record_serde::wire_record!(McpSourcePlugin { id: "id", name: "name" if is_none, version: "version" if is_none });

/// Canonical identity and location of the effective MCP server declaration. The declaration is uniquely addressed by this source id together with the discovered server name.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSourceRef {
    /// Open semantic editability identifier. Known values are editable and read-only.
    pub editability: String,
    /// Configuration file location, when the declaration is file-backed.
    pub file: Option<McpSourceFile>,
    /// Opaque stable identity for the configuration source. Clients must not parse this value.
    pub id: String,
    /// Open source-kind identifier. Known values include user, workspace, invocation, plugin, builtin, and device-registry.
    pub kind: String,
    /// Plugin identity, when the declaration is plugin-provided.
    pub plugin: Option<McpSourcePlugin>,
}
crate::record_serde::wire_record!(McpSourceRef { editability: "editability", file: "file" if is_none, id: "id", kind: "kind", plugin: "plugin" if is_none });

/// MCP server discovered by `mcp.discover`, with config source, optional plugin source, transport type, and enabled state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DiscoveredMcpServer {
    /// Effective serializable configuration, present only when includeConfiguration is true. Authentication remains owned by the runtime; callers must not log secret configuration values.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub configuration: Option<serde_json::Value>,
    /// Canonical identity and location of the effective server declaration.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub effective_source: Option<McpSourceRef>,
    /// Whether the server is enabled (not in the disabled list)
    pub enabled: bool,
    /// Server name (config key)
    pub name: String,
    /// Configuration source: user, workspace, plugin, or builtin
    pub source: McpServerSource,
    /// Plugin name that provided this server, when source is plugin.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_plugin: Option<String>,
    /// Plugin version that provided this server, when source is plugin.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_plugin_version: Option<String>,
    /// Server transport type: stdio, http, sse (deprecated), or memory
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#type: Option<DiscoveredMcpServerType>,
}

impl std::fmt::Debug for DiscoveredMcpServer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiscoveredMcpServer")
            .field(
                "configuration",
                &self.configuration.as_ref().map(|_| "<redacted>"),
            )
            .field("effective_source", &self.effective_source)
            .field("enabled", &self.enabled)
            .field("name", &self.name)
            .field("source", &self.source)
            .field("source_plugin", &self.source_plugin)
            .field("source_plugin_version", &self.source_plugin_version)
            .field("type", &self.r#type)
            .finish()
    }
}

/// Discovered extension metadata, including source-qualified ID, name, discovery source, status, and optional process ID.
///
/// Expanded projection for the existing `Client::call` API. It includes fields added after `Extension` was published.
#[derive(Debug, Clone, Default)]
#[non_exhaustive]
pub struct ExtensionDetails {
    /// Source-qualified ID (e.g., 'project:my-ext', 'user:auth-helper', 'plugin:my-plugin:my-ext')
    pub id: String,
    /// Runtime-owned process log path, when available.
    pub log_path: Option<String>,
    /// Absolute executable module path discovered by the runtime.
    pub module_path: Option<String>,
    /// Extension name (directory name)
    pub name: String,
    /// Process ID if the extension is running
    pub pid: Option<i64>,
    /// Discovery source: project (.github/extensions/), user (~/.copilot/extensions/), plugin (installed plugin), or session (session-state/`<id>`/extensions/)
    pub source: ExtensionSource,
    /// Current status: running, dormant, disabled, failed, or starting
    pub status: ExtensionStatus,
}
crate::record_serde::wire_record!(ExtensionDetails { id: "id", log_path: "logPath" if is_none, module_path: "modulePath" if is_none, name: "name", pid: "pid" if is_none, source: "source", status: "status" });

/// Slim input shape for extension_context attachments; identity fields are runtime-derived.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExtensionContextPushInput {
    /// Caller-supplied JSON payload (required, may be null but not undefined)
    pub payload: serde_json::Value,
    /// Human-readable composer pill label
    pub title: String,
    /// Attachment type discriminator
    pub r#type: ExtensionContextPushInputType,
}
crate::record_serde::wire_record!(ExtensionContextPushInput {
    payload: "payload",
    title: "title",
    r#type: "type"
});

/// Opaque integrator-owned process launch profile for one extension entrypoint.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExtensionLaunchProfile {
    /// Opaque integrator-defined arguments passed to the executable. The runtime does not append the extension entrypoint.
    pub args: Vec<String>,
    /// Opaque integrator-defined environment variables. Runtime-owned COPILOT_SDK_PATH, SESSION_ID, and COPILOT_EXTENSION_PARENT_PID values take precedence.
    pub env: HashMap<String, String>,
    /// Executable used to launch the extension entrypoint.
    pub executable: String,
}
crate::record_serde::wire_record!(ExtensionLaunchProfile {
    args: "args",
    env: "env",
    executable: "executable"
});

/// A discovered extension entrypoint that the registered integrator may classify and resolve to an opaque launch profile.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExtensionLaunchProviderResolveRequest {
    /// Source-qualified extension identifier.
    pub id: String,
    /// Absolute path to the discovered extension entrypoint.
    pub module_path: String,
    /// Human-readable extension name.
    pub name: String,
    /// Discovery source for the extension entrypoint.
    pub source: ExtensionSource,
}
crate::record_serde::wire_record!(ExtensionLaunchProviderResolveRequest {
    id: "id",
    module_path: "modulePath",
    name: "name",
    source: "source"
});

/// The launch profile for a supported entrypoint. Omit launch when the provider does not support the entrypoint.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExtensionLaunchProviderResolveResult {
    /// Opaque launch profile, omitted when this provider does not support the entrypoint.
    pub launch: Option<ExtensionLaunchProfile>,
}
crate::record_serde::wire_record!(ExtensionLaunchProviderResolveResult { launch: "launch" if is_none });

/// Runtime support and session-context standalone executable extension policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExtensionManagedPolicy {
    /// Executable entries discovered from this session's runtime-verified eligible plugins. Under plugin_only, independently launched targets must match these entry paths; an external package, a caller-supplied plugin label, or a synthetic transport bootstrap does not establish eligibility. Empty while pending or when no extension controller is configured.
    pub eligible_plugin_entries: Vec<EligiblePluginExtensionEntry>,
    /// Extension policy enforcement contract version. Version 1 includes standalone discovery, launch admission, and live-policy revocation.
    pub enforcement_version: i64,
    /// Whether standalone extensions are allowed, prohibited by plugin-only policy, or pending unresolved managed policy.
    pub standalone: StandaloneExtensionAdmission,
}
crate::record_serde::wire_record!(ExtensionManagedPolicy {
    eligible_plugin_entries: "eligiblePluginEntries",
    enforcement_version: "enforcementVersion",
    standalone: "standalone"
});

/// Source-qualified extension identifier to disable for the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExtensionsDisableRequest {
    /// Source-qualified extension ID to disable
    pub id: String,
}
crate::record_serde::wire_record!(ExtensionsDisableRequest { id: "id" });

/// Source-qualified extension identifier to enable for the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExtensionsEnableRequest {
    /// Source-qualified extension ID to enable
    pub id: String,
}
crate::record_serde::wire_record!(ExtensionsEnableRequest { id: "id" });

/// The configuration-change alternative for the transportChoices entry at the same index. Only the selected alternative is applied; entries are not cumulative. The payload stays behind the runtime boundary.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPlanConfigurationChange {
    /// Names of the configuration fields the change would set, without their values.
    pub changed_fields: Vec<String>,
    /// Configuration key the change applies to.
    pub config_key: String,
    /// Whether the change would create a new entry or modify an existing one.
    pub operation: McpPlanConfigurationOperation,
    /// Scope the change would be written to.
    pub scope: McpPlanScope,
    /// Secret placeholders the written configuration would reference. The constrained placeholder type cannot carry a literal secret value.
    pub secret_references: Vec<String>,
}
crate::record_serde::wire_record!(McpPlanConfigurationChange {
    changed_fields: "changedFields",
    config_key: "configKey",
    operation: "operation",
    scope: "scope",
    secret_references: "secretReferences"
});

/// Final remote configuration, not a template. The producer refuses external-value
/// expansion before presenting this review. Receipt-owned secrets appear only as
/// `${installation-secret:<id>}` references whose `<id>` matches a reviewed
/// `${secret:<id>}` placeholder; values are never included.
#[derive(Debug, Clone, Default)]
pub struct McpInstallationRemoteConfiguration {
    /// Configured headers, excluding separately authorised OAuth tokens. Values may
    /// contain owned secret references, never secret values.
    pub headers: HashMap<String, String>,
    /// Configured tool selection, not permission to invoke those tools.
    pub tools: Vec<String>,
    /// Transport in the effective persisted remote configuration.
    pub transport: McpPlanRemoteTransport,
    /// Exact resolved endpoint, without templates or secret placeholders.
    pub url: String,
}
crate::record_serde::wire_record!(McpInstallationRemoteConfiguration {
    headers: "headers",
    tools: "tools",
    transport: "transport",
    url: "url"
});

/// Normalised identity of the MCP server a plan targets, independent of how the card spelled it.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPlanResourceIdentity {
    /// Canonical, normalised name of the server, for example `io.github.owner/server`.
    pub canonical_name: String,
    /// Registry identifier of the server, when it came from a registry.
    pub registry_id: Option<String>,
    /// Local configuration key the server would be recorded under.
    pub server_name: String,
    /// Version advertised by the card, when it declares one.
    pub version: Option<String>,
}
crate::record_serde::wire_record!(McpPlanResourceIdentity { canonical_name: "canonicalName", registry_id: "registryId" if is_none, server_name: "serverName", version: "version" if is_none });

/// One Registry string-valued configuration entry for the selected transport.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpInstallationInput {
    /// Exact category declared by the selected choice.
    pub category: McpPlanValueCategory,
    /// Exact key declared by the selected choice.
    pub key: String,
    /// Explicit non-secret value. Secret placeholders use a separate input channel.
    pub value: String,
}
crate::record_serde::wire_record!(McpInstallationInput {
    category: "category",
    key: "key",
    value: "value"
});

/// Outcome of evaluating the planned server against registry and enterprise policy. Evaluation is read-only.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPlanPolicyResult {
    /// What policy decided for this server.
    pub decision: McpPlanPolicyDecision,
    /// Human-readable explanation, safe to surface. Never contains a query, URL, handle, or secret.
    pub reason: Option<String>,
    /// Which authority produced the decision.
    pub source: McpPlanPolicySource,
}
crate::record_serde::wire_record!(McpPlanPolicyResult { decision: "decision", reason: "reason" if is_none, source: "source" });

/// Provenance of the exact validated JSON MCP card content bound privately to a completed plan and its opaque handle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPlanProvenance {
    /// Authority associated with the validated card, without path, query, or credentials. Inert untrusted data.
    pub authority: String,
    /// Semantic digest of the exact validated JSON content bound to the plan handle.
    pub card_digest: CardDigest,
    /// JSON MCP media type the validated card was interpreted as.
    pub media_type: McpServerCardMediaType,
    /// ISO 8601 timestamp at which the runtime completed strict parsing and schema validation of the card content.
    pub validated_at: String,
}
crate::record_serde::wire_record!(McpPlanProvenance {
    authority: "authority",
    card_digest: "cardDigest",
    media_type: "mediaType",
    validated_at: "validatedAt"
});

/// One non-secret scalar value a transport choice needs before it can be applied.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpPlanRequiredValueScalar {
    /// Where the value is applied when the server is launched.
    pub category: McpPlanValueCategory,
    /// Default supplied by the card, when the value can be resolved without input. Presence is the authoritative indication that a default exists. Inert untrusted data.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_value: Option<String>,
    /// Human-readable explanation from the card. Inert untrusted text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Whether the value may be supplied more than once.
    pub is_repeated: bool,
    /// Key the value is supplied under. Inert untrusted data.
    pub key: String,
    /// Discriminator: this required value uses a scalar type.
    #[serde(deserialize_with = "McpPlanRequiredValueScalar::deserialize_kind")]
    pub kind: McpPlanRequiredValueScalarKind,
    /// Whether the value must be present for the plan to be applicable.
    pub required: bool,
    /// Human-readable label from the card. Inert untrusted text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    /// Scalar type the value must conform to.
    pub value_type: McpPlanScalarValueType,
}

impl McpPlanRequiredValueScalar {
    fn deserialize_kind<'de, D>(deserializer: D) -> Result<McpPlanRequiredValueScalarKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "scalar" {
            return Err(serde::de::Error::unknown_variant(&value, &["scalar"]));
        }
        <McpPlanRequiredValueScalarKind>::deserialize(serde::de::value::StringDeserializer::<
            D::Error,
        >::new(value))
    }
}

/// One enumerated non-secret value a transport choice needs before it can be applied. The permitted values are structurally required.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpPlanRequiredValueEnum {
    /// Where the value is applied when the server is launched.
    pub category: McpPlanValueCategory,
    /// Default supplied by the card, when the value can be resolved without input. Presence is the authoritative indication that a default exists. Inert untrusted data.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_value: Option<String>,
    /// Human-readable explanation from the card. Inert untrusted text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Non-empty permitted value set. Inert untrusted data.
    pub enum_values: Vec<String>,
    /// Whether the value may be supplied more than once.
    pub is_repeated: bool,
    /// Key the value is supplied under. Inert untrusted data.
    pub key: String,
    /// Discriminator: this required value uses a fixed enumeration.
    #[serde(deserialize_with = "McpPlanRequiredValueEnum::deserialize_kind")]
    pub kind: McpPlanRequiredValueEnumKind,
    /// Whether the value must be present for the plan to be applicable.
    pub required: bool,
    /// Human-readable label from the card. Inert untrusted text.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    /// Discriminator: the value must be one of `enumValues`.
    #[serde(deserialize_with = "McpPlanRequiredValueEnum::deserialize_value_type")]
    pub value_type: McpPlanEnumValueType,
}

impl McpPlanRequiredValueEnum {
    fn deserialize_kind<'de, D>(deserializer: D) -> Result<McpPlanRequiredValueEnumKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "enum" {
            return Err(serde::de::Error::unknown_variant(&value, &["enum"]));
        }
        <McpPlanRequiredValueEnumKind>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }

    fn deserialize_value_type<'de, D>(deserializer: D) -> Result<McpPlanEnumValueType, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "enum" {
            return Err(serde::de::Error::unknown_variant(&value, &["enum"]));
        }
        <McpPlanEnumValueType>::deserialize(serde::de::value::StringDeserializer::<D::Error>::new(
            value,
        ))
    }
}

/// A secret a transport choice needs, referenced by placeholder. No secret value ever appears in a plan, and the placeholder resolves against the keychain only when a plan is applied.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPlanSecretPlaceholder {
    /// Key the secret is supplied under. Inert untrusted data.
    pub key: String,
    /// The runtime-assigned `${secret:<id>}` placeholder written into configuration in place of the value.
    pub placeholder: String,
    /// Human-readable label from the card. Inert untrusted text.
    pub title: Option<String>,
}
crate::record_serde::wire_record!(McpPlanSecretPlaceholder { key: "key", placeholder: "placeholder", title: "title" if is_none });

/// An eligible local-package transport choice. Package identity is required and a remote endpoint cannot be represented.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpPlanTransportChoicePackage {
    /// Stable identifier for this choice within the plan, used to select it when the plan is applied.
    pub choice_id: String,
    /// Discriminator: this choice runs a local package
    #[serde(deserialize_with = "McpPlanTransportChoicePackage::deserialize_install_method")]
    pub install_method: McpPlanPackageInstallMethod,
    /// Package identifier. Inert untrusted data.
    pub package_identifier: String,
    /// Packaging ecosystem, for example `oci` or `npm`.
    pub package_type: String,
    /// Typed values this choice requires, excluding secrets.
    pub required_values: Vec<McpPlanRequiredValue>,
    /// Secrets this choice requires, referenced by placeholder only.
    pub secret_placeholders: Vec<McpPlanSecretPlaceholder>,
    /// Local process transport this package choice would use.
    pub transport: McpPlanPackageTransport,
}

impl McpPlanTransportChoicePackage {
    fn deserialize_install_method<'de, D>(
        deserializer: D,
    ) -> Result<McpPlanPackageInstallMethod, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "package" {
            return Err(serde::de::Error::unknown_variant(&value, &["package"]));
        }
        <McpPlanPackageInstallMethod>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// An eligible remote-endpoint transport choice. The endpoint is required and package identity cannot be represented.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpPlanTransportChoiceRemote {
    /// Stable identifier for this choice within the plan, used to select it when the plan is applied.
    pub choice_id: String,
    /// Endpoint URL. Inert untrusted data.
    pub endpoint: String,
    /// Discriminator: this choice connects to a remote endpoint
    #[serde(deserialize_with = "McpPlanTransportChoiceRemote::deserialize_install_method")]
    pub install_method: McpPlanRemoteInstallMethod,
    /// Typed values this choice requires, excluding secrets.
    pub required_values: Vec<McpPlanRequiredValue>,
    /// Secrets this choice requires, referenced by placeholder only.
    pub secret_placeholders: Vec<McpPlanSecretPlaceholder>,
    /// Endpoint transport this remote choice would use.
    pub transport: McpPlanRemoteTransport,
}

impl McpPlanTransportChoiceRemote {
    fn deserialize_install_method<'de, D>(
        deserializer: D,
    ) -> Result<McpPlanRemoteInstallMethod, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "remote" {
            return Err(serde::de::Error::unknown_variant(&value, &["remote"]));
        }
        <McpPlanRemoteInstallMethod>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// Where a plan would be written.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPlanTarget {
    /// Configuration key the server would be recorded under within that scope.
    pub config_key: String,
    /// Configuration scope the plan targets.
    pub scope: McpPlanScope,
}
crate::record_serde::wire_record!(McpPlanTarget {
    config_key: "configKey",
    scope: "scope"
});

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpInstallationReviewInstall {
    /// Exact reviewed installation action.
    pub action: McpInstallationReviewInstallAction,
    /// Catalogue identity retained from the bound candidate when available.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub catalogue: Option<InstallationCatalogueIdentity>,
    /// Original catalogue trust metadata, not a verification claim.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub catalogue_trust: Option<serde_json::Value>,
    /// The configuration change for the selected alternative only.
    pub configuration_change: McpPlanConfigurationChange,
    /// Complete effective remote configuration for final installation review.
    /// Earlier private selection reviews and package choices omit this field.
    /// The owned remote resource requires it before issuing confirmation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub effective_configuration: Option<McpInstallationRemoteConfiguration>,
    /// Identity from the retained plan, not caller display text.
    pub identity: McpPlanResourceIdentity,
    /// Non-secret values supplied for this selected alternative.
    pub inputs: Vec<McpInstallationInput>,
    /// Policy decision bound to this plan.
    pub policy: McpPlanPolicyResult,
    /// Original source identity and content commitment.
    pub provenance: McpPlanProvenance,
    /// Explicit reviewed backend selection; no backend is accessed when no secrets are supplied.
    pub secret_storage: McpInstallationSecretStorage,
    /// Only the selected alternative is applied.
    pub selected_choice: McpPlanTransportChoice,
    /// Exact reviewed placeholders supplied separately. Never secret values.
    pub supplied_secrets: Vec<String>,
    /// Exact reviewed user-scope destination.
    pub target: McpPlanTarget,
}

#[derive(Debug, Clone, Default)]
pub struct McpInstallationReviewUninstall {
    /// Exact reviewed installation action.
    pub action: McpInstallationReviewUninstallAction,
    /// Identity from the installed receipt.
    pub identity: McpPlanResourceIdentity,
    /// Receipt-owned installation being removed.
    pub installation_id: String,
    /// Exact planner-owned secret slots to remove, excluding shared OAuth grants.
    pub owned_secret_count: i64,
    /// Current removal policy, independent of permission to activate the server.
    pub policy: McpPlanPolicyResult,
    /// Shared profile authentication is deliberately retained, not pending cleanup.
    pub preserves_shared_authentication: bool,
    /// Source identity and content commitment retained by the installed receipt.
    pub provenance: McpPlanProvenance,
    /// Whether uninstall restores a protected pre-install configuration.
    pub restores_previous_configuration: bool,
    /// Exact destination, checked for intervening changes before mutation.
    pub target: McpPlanTarget,
}
crate::record_serde::wire_record!(McpInstallationReviewUninstall {
    action: "action",
    identity: "identity",
    installation_id: "installationId",
    owned_secret_count: "ownedSecretCount",
    policy: "policy",
    preserves_shared_authentication: "preservesSharedAuthentication",
    provenance: "provenance",
    restores_previous_configuration: "restoresPreviousConfiguration",
    target: "target"
});

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InstallationReviewMcp {
    /// Reviewed resource discriminator.
    pub resource: InstallationReviewMcpResource,
    /// The exact MCP action and its reviewed changes.
    pub review: McpInstallationReview,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InstallationReviewSkill {
    /// Reviewed resource discriminator.
    pub resource: InstallationReviewSkillResource,
    /// The exact verified Skill action and its reviewed files.
    pub review: SkillInstallationReview,
}

/// Review for installing a verified Agent Plugin.
#[derive(Debug, Clone, Default)]
pub struct InstallationReviewPluginReview {
    /// Exact reviewed Agent Plugin installation action.
    pub action: InstallationReviewPluginReviewAction,
    /// Catalogue identity retained from the bound candidate, or verified data for a reference.
    pub catalogue: InstallationCatalogueIdentity,
    /// Manifest description when present.
    pub description: Option<String>,
    /// Reviewed files and digests.
    pub files: Vec<PluginInstallationFileReview>,
    /// Installing never grants immediate use; the plugin is registered disabled.
    pub installs_disabled: bool,
    /// Complete verified manifest content. Planning refuses with review-too-large
    /// when this exceeds 262144 UTF-8 bytes; it is never truncated.
    pub manifest_content: String,
    /// Relative path of the verified plugin manifest.
    pub manifest_path: String,
    /// MCP servers the verified files declare, exactly as an install of these files loads
    /// them. Omitted when the plugin declares none.
    pub mcp_servers: Option<Vec<PluginInstallationMcpServerReview>>,
    /// Installed plugin name, derived from the verified manifest exactly as a direct install derives it.
    pub name: String,
    /// Exact verified source identity.
    pub source: PluginInstallationSource,
    /// Exact install location without an absolute host path.
    pub target: PluginInstallationLocation,
    /// Total reviewed payload size in bytes.
    pub total_bytes: i64,
    /// Manifest version when present.
    pub version: Option<String>,
}
crate::record_serde::wire_record!(InstallationReviewPluginReview { action: "action", catalogue: "catalogue", description: "description" if is_none, files: "files", installs_disabled: "installsDisabled", manifest_content: "manifestContent", manifest_path: "manifestPath", mcp_servers: "mcpServers" if is_none, name: "name", source: "source", target: "target", total_bytes: "totalBytes", version: "version" if is_none });

#[derive(Debug, Clone, Default)]
pub struct InstallationReviewPlugin {
    /// Reviewed resource discriminator.
    pub resource: InstallationReviewPluginResource,
    /// The exact verified Agent Plugin action and its reviewed files.
    pub review: InstallationReviewPluginReview,
}
crate::record_serde::wire_record!(InstallationReviewPlugin {
    resource: "resource",
    review: "review"
});

/// One connection-owned, expiring request for a trusted host's explicit user decision.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InstallationConfirmationRequest {
    /// Opaque one-use challenge. Return unchanged; never log or persist.
    pub confirmation_id: String,
    /// Original plan expiry as an ISO 8601 timestamp. Confirmation never extends it.
    pub expires_at: String,
    /// Random identifier of this installation operation, not a plan handle.
    pub operation_id: String,
    /// Original engine-resolved selector for a bound operation, never a dispatch default.
    /// Bound MCP confirmation always includes it; correlate it with the original pending action.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub policy_session_id: Option<String>,
    /// Resource-specific review to present before collecting the user's decision.
    pub review: InstallationReview,
    /// Opaque commitment to the exact review and inputs. Return unchanged; never log.
    pub review_fingerprint: String,
}

/// A response is meaningful only on the connection and request that issued its challenge.
#[derive(Debug, Clone, Default)]
pub struct InstallationConfirmationResponse {
    /// Exact challenge from the request.
    pub confirmation_id: String,
    /// Fresh explicit user decision. There is no default.
    pub decision: InstallationDecision,
    /// Exact review commitment from the request.
    pub review_fingerprint: String,
}
crate::record_serde::wire_record!(InstallationConfirmationResponse {
    confirmation_id: "confirmationId",
    decision: "decision",
    review_fingerprint: "reviewFingerprint"
});

/// A JSON document to validate and compose.
#[derive(Debug, Clone, Default)]
pub struct McpAdditionalConfigSourceJson {
    /// The source format: inline JSON or a host file.
    pub kind: McpAdditionalConfigSourceJsonKind,
    /// Raw JSON text.
    pub value: String,
}
crate::record_serde::wire_record!(McpAdditionalConfigSourceJson {
    kind: "kind",
    value: "value"
});

/// A document read from the host filesystem.
#[derive(Debug, Clone, Default)]
pub struct McpAdditionalConfigSourceFile {
    /// The source format: inline JSON or a host file.
    pub kind: McpAdditionalConfigSourceFileKind,
    /// Absolute path resolved by the caller.
    pub path: String,
}
crate::record_serde::wire_record!(McpAdditionalConfigSourceFile {
    kind: "kind",
    path: "path"
});

/// Applies exactly one previously prepared operation on its original connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpApplyInstallRequest {
    /// Capabilities required by the original prepared operation.
    pub contract: CatalogClientContract,
    /// Runtime-issued ID already returned by prepareInstall, never reused or rebound.
    pub operation_id: String,
    /// Same existing attached or privately borrowed session as preparation.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(McpApplyInstallRequest {
    contract: "contract",
    operation_id: "operationId",
    policy_session_id: "policySessionId"
});

/// One-use application of the exact retained removal plan.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpApplyUninstallRequest {
    /// Required authenticated bound installation capabilities.
    pub contract: CatalogClientContract,
    /// Opaque original removal plan, consumed once.
    pub plan_handle: String,
    /// Same existing selected session as removal preparation.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(McpApplyUninstallRequest {
    contract: "contract",
    plan_handle: "planHandle",
    policy_session_id: "policySessionId"
});

/// MCP server, tool name, and arguments to invoke from an MCP App view.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsCallToolRequest {
    /// MCP tools/call request metadata, forwarded separately from tool arguments. Host-owned progress tokens and protocol metadata take precedence.
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Tool arguments
    pub arguments: Option<HashMap<String, serde_json::Value>>,
    /// **Required.** Server whose ui:// view issued the request. Per SEP-1865 ('callable by the app from this server only'), the call is rejected when this differs from `serverName`, and rejected outright when missing.
    pub origin_server_name: String,
    /// MCP server hosting the tool
    pub server_name: String,
    /// MCP tool name
    pub tool_name: String,
}
crate::record_serde::wire_record!(McpAppsCallToolRequest { meta: "_meta" if is_none, arguments: "arguments" if is_none, origin_server_name: "originServerName", server_name: "serverName", tool_name: "toolName" });

/// MCP server to diagnose MCP Apps wiring for.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsDiagnoseRequest {
    /// MCP server to probe
    pub server_name: String,
}
crate::record_serde::wire_record!(McpAppsDiagnoseRequest {
    server_name: "serverName"
});

/// Diagnostic snapshot of MCP Apps wiring for the named server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsDiagnoseResult {
    /// Capability negotiation snapshot
    pub capability: McpAppsDiagnoseCapability,
    /// What the server returned for this session
    pub server: McpAppsDiagnoseServer,
}
crate::record_serde::wire_record!(McpAppsDiagnoseResult {
    capability: "capability",
    server: "server"
});

/// Current host context advertised to MCP App guests.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsHostContext {
    /// Current host context
    pub context: McpAppsHostContextDetails,
}
crate::record_serde::wire_record!(McpAppsHostContext { context: "context" });

/// MCP server to list app-callable tools for.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsListToolsRequest {
    /// **Required.** Server whose ui:// view issued the request. Per SEP-1865 ('callable by the app from this server only'), the call is rejected when this differs from `serverName`, and rejected outright when missing.
    pub origin_server_name: String,
    /// MCP server hosting the app
    pub server_name: String,
}
crate::record_serde::wire_record!(McpAppsListToolsRequest {
    origin_server_name: "originServerName",
    server_name: "serverName"
});

/// App-callable tools from the named MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsListToolsResult {
    /// App-callable tools from the server
    pub tools: Vec<HashMap<String, serde_json::Value>>,
}
crate::record_serde::wire_record!(McpAppsListToolsResult { tools: "tools" });

/// MCP server and resource URI to fetch.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsReadResourceRequest {
    /// Name of the MCP server hosting the resource
    pub server_name: String,
    /// Resource URI (typically ui://...)
    pub uri: String,
}
crate::record_serde::wire_record!(McpAppsReadResourceRequest {
    server_name: "serverName",
    uri: "uri"
});

/// Resource contents returned by the MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsReadResourceResult {
    /// Resource contents returned by the server
    pub contents: Vec<McpAppsResourceContent>,
}
crate::record_serde::wire_record!(McpAppsReadResourceResult {
    contents: "contents"
});

/// Host context advertised to MCP App guests
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsSetHostContextDetails {
    /// Display modes the host supports
    pub available_display_modes: Option<Vec<McpAppsSetHostContextDetailsAvailableDisplayMode>>,
    /// Current display mode (SEP-1865)
    pub display_mode: Option<McpAppsSetHostContextDetailsDisplayMode>,
    /// BCP-47 locale, e.g. 'en-US'
    pub locale: Option<String>,
    /// Platform type for responsive design
    pub platform: Option<McpAppsSetHostContextDetailsPlatform>,
    /// UI theme preference per SEP-1865
    pub theme: Option<McpAppsSetHostContextDetailsTheme>,
    /// IANA timezone, e.g. 'America/New_York'
    pub time_zone: Option<String>,
    /// Host application identifier
    pub user_agent: Option<String>,
}
crate::record_serde::wire_record!(McpAppsSetHostContextDetails { available_display_modes: "availableDisplayModes" if is_none, display_mode: "displayMode" if is_none, locale: "locale" if is_none, platform: "platform" if is_none, theme: "theme" if is_none, time_zone: "timeZone" if is_none, user_agent: "userAgent" if is_none });

/// Host context to advertise to MCP App guests.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpAppsSetHostContextRequest {
    /// Host context advertised to MCP App guests
    pub context: McpAppsSetHostContextDetails,
}
crate::record_serde::wire_record!(McpAppsSetHostContextRequest { context: "context" });

/// Invokes a tool on a session MCP server through the server's permission policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpCallToolRequest {
    /// MCP tool arguments.
    pub arguments: serde_json::Value,
    /// Registered MCP server name.
    pub server_name: String,
    /// Unprefixed MCP tool name.
    pub tool_name: String,
}
crate::record_serde::wire_record!(McpCallToolRequest {
    arguments: "arguments",
    server_name: "serverName",
    tool_name: "toolName"
});

/// The requestId previously passed to executeSampling that should be cancelled.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpCancelSamplingExecutionParams {
    /// The requestId previously passed to executeSampling that should be cancelled
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(McpCancelSamplingExecutionParams {
    request_id: "requestId"
});

/// Indicates whether an in-flight sampling execution with the given requestId was found and cancelled.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpCancelSamplingExecutionResult {
    /// True if an in-flight execution with the given requestId was found and signalled to cancel. False when no such execution is in flight (already completed, never started, or cancelled by another caller).
    pub cancelled: bool,
}
crate::record_serde::wire_record!(McpCancelSamplingExecutionResult {
    cancelled: "cancelled"
});

/// Apply the host's Computer Use platform overlay without starting a server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpComputerUseApplyConfigurationRequest {
    /// Disabled servers after invocation precedence.
    pub disabled_servers: Vec<String>,
    /// Explicitly enabled servers.
    pub enabled_servers: Vec<String>,
    /// Complete server map, including provenance and fields unknown to older clients.
    pub servers: serde_json::Value,
}
crate::record_serde::wire_record!(McpComputerUseApplyConfigurationRequest {
    disabled_servers: "disabledServers",
    enabled_servers: "enabledServers",
    servers: "servers"
});

/// Effective configuration after the existing platform overlay.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpComputerUseApplyConfigurationResult {
    /// Effective disabled server names.
    pub disabled_servers: Vec<String>,
    /// Server map with only the platform overlay's changes.
    pub servers: serde_json::Value,
}
crate::record_serde::wire_record!(McpComputerUseApplyConfigurationResult {
    disabled_servers: "disabledServers",
    servers: "servers"
});

/// MCP server name and configuration to add to user configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigAddRequest {
    /// MCP server configuration (stdio process or remote HTTP/SSE)
    pub config: serde_json::Value,
    /// Unique name for the MCP server
    pub name: String,
}
crate::record_serde::wire_record!(McpConfigAddRequest {
    config: "config",
    name: "name"
});

/// Extensible [`McpConfigAddRequest`], including inputs added after it was published.
///
/// Required inputs are [`McpConfigAddOptions::new`] arguments; optional inputs have fluent setters.
/// Input-only: it serialises to the flat wire request and is not deserialisable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct McpConfigAddOptions {
    #[serde(flatten)]
    legacy: McpConfigAddRequest,
    #[serde(skip_serializing_if = "Option::is_none")]
    config_dir: Option<String>,
}

impl McpConfigAddOptions {
    /// Creates options with the required inputs.
    pub fn new(name: impl Into<String>, config: serde_json::Value) -> Self {
        Self {
            legacy: McpConfigAddRequest {
                name: name.into(),
                config,
            },
            config_dir: None,
        }
    }

    /// Configuration root for the user MCP document. Omission retains the connection's root; overrides require a trusted host or direct SDK caller.
    pub fn config_dir(mut self, value: impl Into<String>) -> Self {
        self.config_dir = Some(value.into());
        self
    }
}

/// Validates and combines additional MCP documents in source order.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpConfigComposeAdditionalRequest {
    /// Documents in low-to-high precedence order.
    pub sources: Vec<McpAdditionalConfigSource>,
}
crate::record_serde::wire_record!(McpConfigComposeAdditionalRequest { sources: "sources" });

/// Validated additional MCP servers in source precedence order. Runtime-owned filtering remains outside the serialized document.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigComposeAdditionalResult {
    /// Server names mapped to their validated document configuration.
    pub mcp_servers: HashMap<String, serde_json::Value>,
}
crate::record_serde::wire_record!(McpConfigComposeAdditionalResult {
    mcp_servers: "mcpServers"
});

/// MCP server names to disable for new sessions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigDisableRequest {
    /// Names of MCP servers to disable. Each server is added to the persisted disabled list so new sessions skip it. Already-disabled names are ignored. Active sessions keep their current connections until they end.
    pub names: Vec<String>,
}
crate::record_serde::wire_record!(McpConfigDisableRequest { names: "names" });

/// Extensible [`McpConfigDisableRequest`], including inputs added after it was published.
///
/// Required inputs are [`McpConfigDisableOptions::new`] arguments; optional inputs have fluent setters.
/// Input-only: it serialises to the flat wire request and is not deserialisable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct McpConfigDisableOptions {
    #[serde(flatten)]
    legacy: McpConfigDisableRequest,
    #[serde(skip_serializing_if = "Option::is_none")]
    config_dir: Option<String>,
}

impl McpConfigDisableOptions {
    /// Creates options with the required inputs.
    pub fn new(names: Vec<String>) -> Self {
        Self {
            legacy: McpConfigDisableRequest { names },
            config_dir: None,
        }
    }

    /// Optional configuration root for user settings. Omission retains the connection's root; overrides require a trusted host or direct SDK caller.
    pub fn config_dir(mut self, value: impl Into<String>) -> Self {
        self.config_dir = Some(value.into());
        self
    }
}

/// MCP server names to enable for new sessions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigEnableRequest {
    /// Names of MCP servers to enable. Each server is removed from the persisted disabled list so new sessions spawn it. Unknown or already-enabled names are ignored.
    pub names: Vec<String>,
}
crate::record_serde::wire_record!(McpConfigEnableRequest { names: "names" });

/// Extensible [`McpConfigEnableRequest`], including inputs added after it was published.
///
/// Required inputs are [`McpConfigEnableOptions::new`] arguments; optional inputs have fluent setters.
/// Input-only: it serialises to the flat wire request and is not deserialisable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct McpConfigEnableOptions {
    #[serde(flatten)]
    legacy: McpConfigEnableRequest,
    #[serde(skip_serializing_if = "Option::is_none")]
    config_dir: Option<String>,
}

impl McpConfigEnableOptions {
    /// Creates options with the required inputs.
    pub fn new(names: Vec<String>) -> Self {
        Self {
            legacy: McpConfigEnableRequest { names },
            config_dir: None,
        }
    }

    /// Optional configuration root for user settings. Omission retains the connection's root; overrides require a trusted host or direct SDK caller.
    pub fn config_dir(mut self, value: impl Into<String>) -> Self {
        self.config_dir = Some(value.into());
        self
    }
}

/// Selects an explicit user MCP configuration root without changing the connection owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpConfigForConfigDirRequest {
    /// Configuration root containing the user MCP document.
    pub config_dir: String,
}
crate::record_serde::wire_record!(McpConfigForConfigDirRequest {
    config_dir: "configDir"
});

/// User-configured MCP servers, keyed by server name.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigList {
    /// All MCP servers from user config, keyed by name
    pub servers: HashMap<String, serde_json::Value>,
}
crate::record_serde::wire_record!(McpConfigList { servers: "servers" });

/// Optional configuration root for user MCP servers. Omission retains the connection's root.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigListRequest {
    /// Configuration root containing the user MCP document. Only trusted hosts or direct SDK callers can override it.
    pub config_dir: Option<String>,
}
crate::record_serde::wire_record!(McpConfigListRequest { config_dir: "configDir" if is_none });

/// Read-only MCP configuration composition from explicit source inventories. The result grants no execution authority.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpConfigMergeSourcesRequest {
    /// Additional MCP configuration, merged after the other sources.
    pub additional_config: Option<serde_json::Value>,
    /// Host-bundled plugin records, below user and workspace configuration.
    pub builtin_plugins: Option<Vec<serde_json::Value>>,
    /// Configuration root for the user MCP document.
    pub config_dir: String,
    /// Include workspace files only where the runtime confirms folder trust.
    pub include_workspace_sources: Option<bool>,
    /// Resolved installed, live, and explicit plugin records in precedence order.
    pub installed_plugins: Option<Vec<serde_json::Value>>,
    /// Repository boundary for upward workspace discovery.
    pub repo_root: Option<String>,
    /// Working directory for workspace discovery and relative source references.
    pub working_directory: String,
}
crate::record_serde::wire_record!(McpConfigMergeSourcesRequest { additional_config: "additionalConfig" if is_none, builtin_plugins: "builtinPlugins" if is_none, config_dir: "configDir", include_workspace_sources: "includeWorkspaceSources" if is_none, installed_plugins: "installedPlugins" if is_none, repo_root: "repoRoot" if is_none, working_directory: "workingDirectory" });

/// The composed MCP configuration and all omitted-source diagnostics.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpConfigMergeSourcesResult {
    /// The complete merged configuration, including source provenance.
    pub config: serde_json::Value,
    /// Registry servers disabled because their MSIX container is unavailable.
    pub odr_msix_disabled_server_names: Vec<String>,
    /// Server names discovered through the on-device registry.
    pub odr_server_names: Vec<String>,
    /// Invalid user entries omitted while valid siblings remained.
    #[doc(hidden)]
    pub(crate) user_config_errors: Vec<McpConfigSourceDiagnostic>,
    /// Workspace files or entries that did not load.
    #[doc(hidden)]
    pub(crate) workspace_errors: Vec<McpConfigSourceDiagnostic>,
}
crate::record_serde::wire_record!(McpConfigMergeSourcesResult {
    config: "config",
    odr_msix_disabled_server_names: "odrMsixDisabledServerNames",
    odr_server_names: "odrServerNames",
    user_config_errors: "userConfigErrors",
    workspace_errors: "workspaceErrors"
});

/// MCP server name to remove from user configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigRemoveRequest {
    /// OAuth Client ID Metadata Document URL whose persisted credentials should also be removed.
    pub auth_client_id_metadata_url: Option<String>,
    /// Name of the MCP server to remove
    pub name: String,
}
crate::record_serde::wire_record!(McpConfigRemoveRequest { auth_client_id_metadata_url: "authClientIdMetadataUrl" if is_none, name: "name" });

/// Extensible [`McpConfigRemoveRequest`], including inputs added after it was published.
///
/// Required inputs are [`McpConfigRemoveOptions::new`] arguments; optional inputs have fluent setters.
/// Input-only: it serialises to the flat wire request and is not deserialisable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct McpConfigRemoveOptions {
    #[serde(flatten)]
    legacy: McpConfigRemoveRequest,
    #[serde(skip_serializing_if = "Option::is_none")]
    config_dir: Option<String>,
}

impl McpConfigRemoveOptions {
    /// Creates options with the required inputs.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            legacy: McpConfigRemoveRequest {
                name: name.into(),
                auth_client_id_metadata_url: None,
            },
            config_dir: None,
        }
    }

    /// OAuth Client ID Metadata Document URL whose persisted credentials should also be removed.
    pub fn auth_client_id_metadata_url(mut self, value: impl Into<String>) -> Self {
        self.legacy.auth_client_id_metadata_url = Some(value.into());
        self
    }

    /// Configuration root for the user MCP document and credentials. Omission retains the connection's root; overrides require a trusted host or direct SDK caller.
    pub fn config_dir(mut self, value: impl Into<String>) -> Self {
        self.config_dir = Some(value.into());
        self
    }
}

/// MCP server name and replacement configuration to write to user configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigUpdateRequest {
    /// MCP server configuration (stdio process or remote HTTP/SSE)
    pub config: serde_json::Value,
    /// Name of the MCP server to update
    pub name: String,
}
crate::record_serde::wire_record!(McpConfigUpdateRequest {
    config: "config",
    name: "name"
});

/// Effective MCP configuration with optional live observations from matching already materialized servers.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfiguredServerList {
    /// Effective configured MCP servers.
    pub servers: Vec<McpConfiguredServer>,
}
crate::record_serde::wire_record!(McpConfiguredServerList { servers: "servers" });

/// Credential-free authentication identity used to configure GitHub MCP.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpConfigureGitHubRequest {
    /// Credential-free authentication identity for GitHub MCP configuration. The field retains its legacy opaque schema for compatibility.
    #[doc(hidden)]
    pub(crate) auth_info: serde_json::Value,
}
crate::record_serde::wire_record!(McpConfigureGitHubRequest {
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
pub struct McpConfigureGitHubResult {
    /// Whether GitHub MCP configuration changed.
    pub changed: bool,
}
crate::record_serde::wire_record!(McpConfigureGitHubResult { changed: "changed" });

/// Fresh user MCP server names, including invalid entries but excluding installation-owned entries. This read does not change the validated configuration cache.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpConfigUserServerNamesResult {
    /// User-owned persisted server names, including entries that validation skips.
    pub names: Vec<String>,
}
crate::record_serde::wire_record!(McpConfigUserServerNamesResult { names: "names" });

/// Name of the MCP server to disable for the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpDisableRequest {
    /// Required for an owned installation; omission preserves only manual-server behaviour.
    pub expected_installation_id: Option<String>,
    /// Name of the MCP server to disable
    pub server_name: String,
}
crate::record_serde::wire_record!(McpDisableRequest { expected_installation_id: "expectedInstallationId" if is_none, server_name: "serverName" });

/// Optional working directory used as context for MCP server discovery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpDiscoverRequest {
    /// Whether to include canonical effectiveSource metadata for each discovered server. Callers must opt in so protocol-3 clients retain the legacy closed response shape.
    pub include_effective_source: Option<bool>,
    /// Working directory used as context for discovery (e.g., plugin resolution)
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(McpDiscoverRequest { include_effective_source: "includeEffectiveSource" if is_none, working_directory: "workingDirectory" if is_none });

/// Read-only invocation options for effective MCP discovery. User and repository settings remain unchanged.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpDiscoveryOptions {
    /// Additional MCP configuration, as inline JSON or @file references, merged in order after the persisted and plugin sources. Relative file references resolve against workingDirectory.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub additional_config: Option<Vec<String>>,
    /// Invocation tool allowlist. Together with excludedTools, it controls whether shell-replaceable GitHub MCP tools remain necessary.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub available_tools: Option<Vec<String>>,
    /// Disable the runtime's built-in MCP server names for this invocation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disable_builtin_servers: Option<bool>,
    /// Server names explicitly disabled for this invocation. These disables take precedence over enabledServers.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disabled_servers: Option<Vec<String>>,
    /// Enable the full GitHub MCP tool surface for this invocation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enable_all_github_tools: Option<bool>,
    /// Server names explicitly enabled for this invocation, overriding persisted disabled names but not explicit invocation disables or runtime policy.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled_servers: Option<Vec<String>>,
    /// Invocation tool exclusions. These exclusions take precedence over availableTools.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_tools: Option<Vec<String>>,
    /// Experimental feature opt-in for this invocation. Defaults to the effective user and trusted repository setting.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub experimental: Option<bool>,
    /// Additional GitHub MCP tools selected for this invocation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub github_tools: Option<Vec<String>>,
    /// Additional GitHub MCP toolsets selected for this invocation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub github_toolsets: Option<Vec<String>>,
}

impl std::fmt::Debug for McpDiscoveryOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("McpDiscoveryOptions")
            .field(
                "additional_config",
                &self.additional_config.as_ref().map(|_| "<redacted>"),
            )
            .field("available_tools", &self.available_tools)
            .field("disable_builtin_servers", &self.disable_builtin_servers)
            .field("disabled_servers", &self.disabled_servers)
            .field("enable_all_github_tools", &self.enable_all_github_tools)
            .field("enabled_servers", &self.enabled_servers)
            .field("excluded_tools", &self.excluded_tools)
            .field("experimental", &self.experimental)
            .field("github_tools", &self.github_tools)
            .field("github_toolsets", &self.github_toolsets)
            .finish()
    }
}

/// Extensible [`McpDiscoverRequest`], including inputs added after it was published.
///
/// Required inputs are [`McpDiscoverOptions::new`] arguments; optional inputs have fluent setters.
/// Input-only: it serialises to the flat wire request and is not deserialisable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct McpDiscoverOptions {
    #[serde(flatten)]
    legacy: McpDiscoverRequest,
    #[serde(skip_serializing_if = "Option::is_none")]
    include_configuration: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    options: Option<McpDiscoveryOptions>,
    #[serde(skip_serializing_if = "Option::is_none")]
    plugin_directories: Option<Vec<String>>,
}

impl McpDiscoverOptions {
    /// Creates options with the required inputs.
    pub fn new() -> Self {
        Self {
            legacy: McpDiscoverRequest {
                working_directory: None,
                include_effective_source: None,
            },
            include_configuration: None,
            options: None,
            plugin_directories: None,
        }
    }

    /// Working directory used as context for discovery (e.g., plugin resolution)
    pub fn working_directory(mut self, value: impl Into<String>) -> Self {
        self.legacy.working_directory = Some(value.into());
        self
    }

    /// Whether to include canonical effectiveSource metadata for each discovered server. Callers must opt in so protocol-3 clients retain the legacy closed response shape.
    pub fn include_effective_source(mut self, value: bool) -> Self {
        self.legacy.include_effective_source = Some(value);
        self
    }

    /// Include each server's effective serializable configuration and structured diagnostics. Omitted or false preserves the existing response shape exactly.
    pub fn include_configuration(mut self, value: bool) -> Self {
        self.include_configuration = Some(value);
        self
    }

    /// Read-only invocation options layered over runtime-owned configuration. These options never assert account, workspace trust, or managed policy.
    pub fn options(mut self, value: McpDiscoveryOptions) -> Self {
        self.options = Some(value);
        self
    }

    /// External plugin directories mounted for this discovery only. Relative paths resolve against workingDirectory. The runtime resolves trust and policy; these directories do not become trusted built-ins or change settings.
    pub fn plugin_directories(mut self, value: Vec<String>) -> Self {
        self.plugin_directories = Some(value);
        self
    }
}

impl Default for McpDiscoverOptions {
    fn default() -> Self {
        Self::new()
    }
}

/// A nonfatal discovery problem. Paths and messages are untrusted display text and can contain private configuration data.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpDiscoveryDiagnostic {
    /// Problem detail for display. Clients must sanitize terminal control characters before printing it.
    pub message: String,
    /// Configuration path when the problem concerns a file.
    pub path: Option<String>,
    /// Machine-readable reason, such as invalid-server, malformed, unreadable, timeout, or unavailable.
    pub reason: String,
    /// Source of the diagnostic, such as user, workspace, plugin, builtin, auth, or policy.
    pub source: String,
}
crate::record_serde::wire_record!(McpDiscoveryDiagnostic { message: "message", path: "path" if is_none, reason: "reason", source: "source" });

/// MCP servers discovered from user, workspace, plugin, and built-in sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpDiscoverResult {
    /// Discovery warnings, present only when includeConfiguration is true. Empty when discovery has no warnings.
    pub diagnostics: Option<Vec<McpDiscoveryDiagnostic>>,
    /// MCP servers discovered from all sources
    pub servers: Vec<DiscoveredMcpServer>,
}
crate::record_serde::wire_record!(McpDiscoverResult { diagnostics: "diagnostics" if is_none, servers: "servers" });

/// Discovered configuration for a registered in-process MCP bridge.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpDiscoveryMemoryConfig {
    /// Configuration warnings recorded while loading the server.
    pub config_warnings: Option<Vec<String>>,
    /// Controls whether tools can be loaded on demand.
    pub defer_tools: Option<McpServerConfigDeferTools>,
    /// Whether secret masking is disabled for calls to this server.
    pub disable_secret_masking: Option<bool>,
    /// Whether persisted tool snapshots are disabled.
    pub disable_tool_cache: Option<bool>,
    /// Optional human-readable server name.
    pub display_name: Option<String>,
    /// Event types this server receives as Copilot notifications.
    pub events: Option<Vec<String>>,
    /// Tool names excluded after the include filter is applied.
    pub exclude_tools: Option<Vec<String>>,
    /// Content filtering mode to apply to this server's tools.
    pub filter_mapping: Option<serde_json::Value>,
    /// Whether this server is a built-in fallback.
    pub is_default_server: Option<bool>,
    /// Copilot notification types this server may send to the host.
    pub notifications: Option<Vec<String>>,
    /// Set to `true` to use default OIDC settings.
    pub oidc: Option<serde_json::Value>,
    /// Telemetry-obfuscation policy for this server's tools.
    pub safe_for_telemetry: Option<serde_json::Value>,
    /// Indicates a registered in-process MCP bridge, not a serializable server instance.
    pub server_instance: bool,
    /// Milliseconds this server may spend connecting before the CLI warns that it is taking longer than expected. Presentation only: it does not change how long the connection is allowed to take.
    pub slow_connection_threshold_ms: Option<i64>,
    /// The origin of this server configuration.
    pub source: Option<McpServerSource>,
    /// Source file path recorded while loading the config.
    pub source_path: Option<String>,
    /// Plugin that provided this server.
    pub source_plugin: Option<String>,
    /// Whether the providing plugin uses the Open Plugin Spec.
    pub source_plugin_spec: Option<bool>,
    /// Version of the plugin that provided this server.
    pub source_plugin_version: Option<String>,
    /// Timeout in milliseconds for tool discovery and tool calls.
    pub timeout: Option<i64>,
    /// Tools to include. Defaults to all tools if not specified.
    pub tools: Option<Vec<String>>,
    /// In-process MCP transport.
    pub r#type: McpDiscoveryMemoryConfigType,
}
crate::record_serde::wire_record!(McpDiscoveryMemoryConfig { config_warnings: "configWarnings" if is_none, defer_tools: "deferTools" if is_none, disable_secret_masking: "disableSecretMasking" if is_none, disable_tool_cache: "disableToolCache" if is_none, display_name: "displayName" if is_none, events: "events" if is_none, exclude_tools: "excludeTools" if is_none, filter_mapping: "filterMapping" if is_none, is_default_server: "isDefaultServer" if is_none, notifications: "notifications" if is_none, oidc: "oidc" if is_none, safe_for_telemetry: "safeForTelemetry" if is_none, server_instance: "serverInstance", slow_connection_threshold_ms: "slowConnectionThresholdMs" if is_none, source: "source" if is_none, source_path: "sourcePath" if is_none, source_plugin: "sourcePlugin" if is_none, source_plugin_spec: "sourcePluginSpec" if is_none, source_plugin_version: "sourcePluginVersion" if is_none, timeout: "timeout" if is_none, tools: "tools" if is_none, r#type: "type" });

/// Name of the MCP server to enable for the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpEnableRequest {
    /// Exact receipt identity for explicit owned activation in this session.
    pub expected_installation_id: Option<String>,
    /// Name of the MCP server to enable
    pub server_name: String,
}
crate::record_serde::wire_record!(McpEnableRequest { expected_installation_id: "expectedInstallationId" if is_none, server_name: "serverName" });

/// Raw MCP CreateMessageRequest params, as received in the `sampling.requested` event. Treated as opaque at the schema layer; the runtime converts the embedded MCP messages into the OpenAI chat-completion shape internally.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpExecuteSamplingRequest {}

/// Identifiers and raw MCP CreateMessageRequest params used to run a sampling inference.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpExecuteSamplingParams {
    /// The original MCP JSON-RPC request ID (string or number). Used by the runtime to correlate the inference with the originating MCP request for telemetry; this is distinct from `requestId` (which is the schema-level cancellation handle).
    pub mcp_request_id: serde_json::Value,
    /// Raw MCP CreateMessageRequest params, as received in the `sampling.requested` event. Treated as opaque at the schema layer; the runtime converts the embedded MCP messages into the OpenAI chat-completion shape internally.
    pub request: McpExecuteSamplingRequest,
    /// Caller-provided unique identifier for this sampling execution. Use this same ID with cancelSamplingExecution to cancel the in-flight call. Must be unique within the session for the lifetime of the call.
    pub request_id: RequestId,
    /// Name of the MCP server that initiated the sampling request
    pub server_name: String,
}
crate::record_serde::wire_record!(McpExecuteSamplingParams {
    mcp_request_id: "mcpRequestId",
    request: "request",
    request_id: "requestId",
    server_name: "serverName"
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
pub struct McpExternalClientExchangeRequest {
    /// The operation and its serializable arguments.
    pub request: serde_json::Value,
}
crate::record_serde::wire_record!(McpExternalClientExchangeRequest { request: "request" });

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

/// One invocation's GitHub MCP tool overrides.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpGitHubInvocationToolOptions {
    /// Additional tools.
    pub additional_tools: Option<Vec<String>>,
    /// Additional toolsets.
    pub additional_toolsets: Option<Vec<String>>,
    /// Override form deferral when the shared tool resolver supports it.
    pub disable_form_deferral: Option<bool>,
    /// Expose all supported tools.
    pub enable_all_tools: Option<bool>,
    /// Select the insiders endpoint.
    pub enable_insiders_mode: Option<bool>,
}
crate::record_serde::wire_record!(McpGitHubInvocationToolOptions { additional_tools: "additionalTools" if is_none, additional_toolsets: "additionalToolsets" if is_none, disable_form_deferral: "disableFormDeferral" if is_none, enable_all_tools: "enableAllTools" if is_none, enable_insiders_mode: "enableInsidersMode" if is_none });

/// Feature populations for a sessionless capability inspection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpInvocationAudience {
    /// First launch time in Unix milliseconds.
    pub first_launch_at_millis: Option<f64>,
    /// Whether experimental capabilities are enabled.
    pub is_experimental: bool,
    /// Whether this installation belongs to staff.
    pub is_staff: bool,
    /// Whether this installation belongs to GitHub staff.
    pub is_staff_github: Option<bool>,
    /// Whether this installation belongs to Microsoft staff.
    pub is_staff_microsoft: Option<bool>,
    /// Whether team capabilities are enabled.
    pub is_team: bool,
    /// Whether streamer mode is active.
    pub streamer_mode: bool,
}
crate::record_serde::wire_record!(McpInvocationAudience { first_launch_at_millis: "firstLaunchAtMillis" if is_none, is_experimental: "isExperimental", is_staff: "isStaff", is_staff_github: "isStaffGithub" if is_none, is_staff_microsoft: "isStaffMicrosoft" if is_none, is_team: "isTeam", streamer_mode: "streamerMode" });

/// The session tool filters that decide whether the session still has a shell to run `gh` with.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpShouldExcludeGitHubToolsRequest {
    /// The session's tool allowlist, when it set one. Omitted means the session constrains nothing this way.
    pub available_tools: Option<Vec<String>>,
    /// The session's tool denylist, when it set one. Omitted means the session constrains nothing this way.
    pub excluded_tools: Option<Vec<String>>,
    /// How the allowlist and denylist combine when both are set. Omitted uses the default excluded precedence; supply the session's explicit Update selection when present.
    pub tool_filter_precedence: Option<OptionsUpdateToolFilterPrecedence>,
}
crate::record_serde::wire_record!(McpShouldExcludeGitHubToolsRequest { available_tools: "availableTools" if is_none, excluded_tools: "excludedTools" if is_none, tool_filter_precedence: "toolFilterPrecedence" if is_none });

/// Resolve the authentication-derived built-in without a session or MCP connection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpGitHubResolveInvocationRequest {
    /// Existing launch populations; account identity comes only from the connection.
    #[doc(hidden)]
    pub(crate) audience: McpInvocationAudience,
    /// Effective disabled servers, after enable overrides.
    pub disabled_servers: Vec<String>,
    /// Invocation tool overrides, above settings.
    #[doc(hidden)]
    pub(crate) flags: Option<McpGitHubInvocationToolOptions>,
    /// Client integration identifier for the secret-free descriptor.
    pub integration_id: String,
    /// Tool filters used by the session this invocation describes.
    #[doc(hidden)]
    pub(crate) tool_filters: Option<McpShouldExcludeGitHubToolsRequest>,
    /// Effective user and trusted repository settings.
    pub user_settings: serde_json::Value,
    /// Repository context for host detection and shell availability.
    pub working_directory: String,
}
crate::record_serde::wire_record!(McpGitHubResolveInvocationRequest { audience: "audience", disabled_servers: "disabledServers", flags: "flags" if is_none, integration_id: "integrationId", tool_filters: "toolFilters" if is_none, user_settings: "userSettings", working_directory: "workingDirectory" });

/// A secret-free GitHub descriptor, an ordinary signed-out result, or a bounded degraded result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpGitHubResolveInvocationResult {
    /// Built-in descriptor without authentication headers; absent when unavailable.
    pub server: Option<serde_json::Value>,
    /// Whether the existing two-second resolution budget expired.
    pub timed_out: bool,
    /// Resolution failure detail, distinct from an ordinary signed-out result.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(McpGitHubResolveInvocationResult { server: "server" if is_none, timed_out: "timedOut", warning: "warning" if is_none });

/// Repository and server state for GitHub MCP exposure.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpGitHubWorkspace {
    /// Server names already disabled by the host.
    pub already_disabled: Vec<String>,
    /// The repository has an Azure DevOps remote.
    pub has_azure_dev_ops: bool,
    /// The repository has a GitHub remote.
    pub has_github: bool,
    /// A user configured the GitHub MCP server.
    pub user_configured_github_mcp: bool,
}
crate::record_serde::wire_record!(McpGitHubWorkspace {
    already_disabled: "alreadyDisabled",
    has_azure_dev_ops: "hasAzureDevOps",
    has_github: "hasGithub",
    user_configured_github_mcp: "userConfiguredGithubMcp"
});

#[derive(Debug, Clone, Default)]
pub struct McpHeadersHandlePendingHeadersRefreshRequestHeaders {
    /// Headers to overlay onto the MCP request. Dynamic headers override static config headers but do not replace SDK-managed request headers.
    pub headers: HashMap<String, String>,
    /// Headers-refresh response variant discriminator.
    pub kind: McpHeadersHandlePendingHeadersRefreshRequestHeadersKind,
    /// Optional lifetime in milliseconds for these returned headers. The runtime clamps its configured cache lifetime to this value.
    pub ttl_ms: Option<i64>,
}
crate::record_serde::wire_record!(McpHeadersHandlePendingHeadersRefreshRequestHeaders { headers: "headers", kind: "kind", ttl_ms: "ttlMs" if is_none });

#[derive(Debug, Clone, Default)]
pub struct McpHeadersHandlePendingHeadersRefreshRequestNone {
    /// Headers-refresh response variant discriminator.
    pub kind: McpHeadersHandlePendingHeadersRefreshRequestNoneKind,
}
crate::record_serde::wire_record!(McpHeadersHandlePendingHeadersRefreshRequestNone {
    kind: "kind"
});

#[derive(Debug, Clone, Default)]
pub struct McpHeadersHandlePendingHeadersRefreshRequestError {
    /// Headers-refresh response variant discriminator.
    pub kind: McpHeadersHandlePendingHeadersRefreshRequestErrorKind,
    /// Host credential broker failure, denial, or revocation reason.
    pub message: String,
}
crate::record_serde::wire_record!(McpHeadersHandlePendingHeadersRefreshRequestError {
    kind: "kind",
    message: "message"
});

/// MCP headers refresh request id and the host response.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpHeadersHandlePendingHeadersRefreshRequestRequest {
    /// Headers refresh request identifier from mcp.headers_refresh_required
    pub request_id: RequestId,
    /// Host response: supply dynamic headers or decline this refresh.
    pub result: McpHeadersHandlePendingHeadersRefreshRequest,
}

/// Indicates whether the pending MCP headers refresh response was accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpHeadersHandlePendingHeadersRefreshRequestResult {
    /// Whether the response was accepted. False if the request was unknown, timed out, or already resolved.
    pub success: bool,
}
crate::record_serde::wire_record!(McpHeadersHandlePendingHeadersRefreshRequestResult {
    success: "success"
});

/// One callback routed only to the engine's original observer.
#[derive(Debug, Clone, Default)]
pub struct McpIdeClientCallback {
    /// Unique live-client identity, independent of server name and graph token.
    pub callback_id: String,
    /// Tool, protocol, cancellation, or release operation as JSON.
    pub request_json: String,
    /// Canonical session that owns this client.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(McpIdeClientCallback {
    callback_id: "callbackId",
    request_json: "requestJson",
    session_id: "sessionId"
});

/// Serialized response from the original host's live IDE client.
#[derive(Debug, Clone, Default)]
pub struct McpIdeClientCallbackResult {
    /// The backing client's result as JSON.
    pub result_json: String,
}
crate::record_serde::wire_record!(McpIdeClientCallbackResult {
    result_json: "resultJson"
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
pub(crate) struct McpIdeClientRequest {
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
pub struct McpIdeClientResult {
    /// Whether this operation changed or addressed the current registration.
    pub applied: bool,
    /// Opaque token returned only after successful registration.
    pub token: Option<String>,
}
crate::record_serde::wire_record!(McpIdeClientResult { applied: "applied", token: "token" if is_none });

/// IDE metadata for an existing local MCP endpoint on the runtime host.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpIdeConfig {
    /// Client process identifier sent to the IDE.
    pub client_pid: i64,
    /// Client package version used by the MCP initialize request.
    pub client_version: String,
    /// IDE display name.
    pub ide_name: String,
    /// Lock file whose removal ends the live connection.
    pub lock_file_path: String,
    /// Client parent process identifier sent to the IDE.
    pub parent_pid: i64,
    /// IDE process identifier.
    pub pid: i64,
    /// Absolute workspace path on the runtime host.
    pub workspace_folder: String,
}
crate::record_serde::wire_record!(McpIdeConfig {
    client_pid: "clientPid",
    client_version: "clientVersion",
    ide_name: "ideName",
    lock_file_path: "lockFilePath",
    parent_pid: "parentPid",
    pid: "pid",
    workspace_folder: "workspaceFolder"
});

/// Already-confirmed durable work must be reconciled before new mutations or inventory.
#[derive(Debug, Clone, Default)]
pub struct McpInstallationManagementOutcomeRecoveryRequired {
    /// Installation management outcome discriminator.
    pub kind: McpInstallationManagementOutcomeRecoveryRequiredKind,
}
crate::record_serde::wire_record!(McpInstallationManagementOutcomeRecoveryRequired {
    kind: "kind"
});

/// Inert, runtime-owned admission. The operation ID is known before confirmation or effects.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPreparedInstall {
    /// Original plan expiry in Unix epoch milliseconds; preparation does not extend it.
    pub expires_at_epoch_ms: i64,
    /// Original connection-owned operation, known before the first confirmation callback.
    pub operation_id: String,
}
crate::record_serde::wire_record!(McpPreparedInstall {
    expires_at_epoch_ms: "expiresAtEpochMs",
    operation_id: "operationId"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationManagementOutcomeInstallPrepared {
    /// Installation management outcome discriminator.
    pub kind: McpInstallationManagementOutcomeInstallPreparedKind,
    /// Known original operation identity, returned before callback or effects.
    pub operation: McpPreparedInstall,
}
crate::record_serde::wire_record!(McpInstallationManagementOutcomeInstallPrepared {
    kind: "kind",
    operation: "operation"
});

/// Durable configuration ownership is distinct from session-specific usability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpInstallationSummary {
    /// Catalogue identity retained from the installed plan when available.
    pub catalogue: Option<InstallationCatalogueIdentity>,
    /// Exact alternative retained in the installing receipt.
    pub choice_id: String,
    /// Identity retained from the validated original plan.
    pub identity: McpPlanResourceIdentity,
    /// Exact durable installation receipt identity.
    pub installation_id: String,
    /// ISO 8601 wall-clock installation time when available.
    pub installed_at: Option<String>,
    /// Original installing operation, not a fresh management operation.
    pub operation_id: String,
    /// Ownership or setup state, never inferred proof of tool usability.
    pub state: McpInstallationState,
}
crate::record_serde::wire_record!(McpInstallationSummary { catalogue: "catalogue" if is_none, choice_id: "choiceId", identity: "identity", installation_id: "installationId", installed_at: "installedAt" if is_none, operation_id: "operationId", state: "state" });

#[derive(Debug, Clone, Default)]
pub struct McpInstallationManagementOutcomeListed {
    /// Owned receipts visible to the selected account and host.
    pub installations: Vec<McpInstallationSummary>,
    /// Installation management outcome discriminator.
    pub kind: McpInstallationManagementOutcomeListedKind,
}
crate::record_serde::wire_record!(McpInstallationManagementOutcomeListed {
    installations: "installations",
    kind: "kind"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationManagementOutcomeRecovered {
    /// Freshly inspected receipts after successful durable reconciliation.
    pub installations: Vec<McpInstallationSummary>,
    /// Installation management outcome discriminator.
    pub kind: McpInstallationManagementOutcomeRecoveredKind,
}
crate::record_serde::wire_record!(McpInstallationManagementOutcomeRecovered {
    installations: "installations",
    kind: "kind"
});

/// Exact inert removal plan. No configuration or credentials have changed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpUninstallPlan {
    /// Original wall-clock expiry in milliseconds. Applying never renews it.
    pub expires_at_epoch_ms: i64,
    /// Original owned receipt being removed.
    pub installation: McpInstallationSummary,
    /// The original operation, inspectable and cancellable on this same connection.
    pub operation_id: String,
    /// Exact configured input slots owned by this installation, never shared OAuth tokens.
    pub owned_secret_count: i64,
    /// One-use original connection and authority-bound plan handle.
    pub plan_handle: String,
    /// Shared authentication is deliberately retained; revocation is a separate action.
    pub preserves_shared_authentication: bool,
    /// Whether removal restores a protected earlier configuration.
    pub restores_previous_configuration: bool,
}
crate::record_serde::wire_record!(McpUninstallPlan {
    expires_at_epoch_ms: "expiresAtEpochMs",
    installation: "installation",
    operation_id: "operationId",
    owned_secret_count: "ownedSecretCount",
    plan_handle: "planHandle",
    preserves_shared_authentication: "preservesSharedAuthentication",
    restores_previous_configuration: "restoresPreviousConfiguration"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationManagementOutcomeUninstallPlanned {
    /// Installation management outcome discriminator.
    pub kind: McpInstallationManagementOutcomeUninstallPlannedKind,
    /// Original owned removal plan and operation.
    pub plan: McpUninstallPlan,
}
crate::record_serde::wire_record!(McpInstallationManagementOutcomeUninstallPlanned {
    kind: "kind",
    plan: "plan"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationOperationStatusPreparing {
    /// Whether cancellation has been requested, not proof that a write was undone.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    /// Original operation progress discriminator.
    pub phase: McpInstallationOperationStatusPreparingPhase,
}
crate::record_serde::wire_record!(McpInstallationOperationStatusPreparing {
    cancellation_requested: "cancellationRequested",
    operation_id: "operationId",
    phase: "phase"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationOperationStatusPrepared {
    /// Whether the inert prepared operation was asked to cancel.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    /// Original operation progress discriminator.
    pub phase: McpInstallationOperationStatusPreparedPhase,
}
crate::record_serde::wire_record!(McpInstallationOperationStatusPrepared {
    cancellation_requested: "cancellationRequested",
    operation_id: "operationId",
    phase: "phase"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationOperationStatusAwaitingConfirmation {
    /// Whether the pending human callback was asked to cancel.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    /// Original operation progress discriminator.
    pub phase: McpInstallationOperationStatusAwaitingConfirmationPhase,
}
crate::record_serde::wire_record!(McpInstallationOperationStatusAwaitingConfirmation {
    cancellation_requested: "cancellationRequested",
    operation_id: "operationId",
    phase: "phase"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationOperationStatusRevalidating {
    /// Whether source or authority revalidation was asked to cancel.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    /// Original operation progress discriminator.
    pub phase: McpInstallationOperationStatusRevalidatingPhase,
}
crate::record_serde::wire_record!(McpInstallationOperationStatusRevalidating {
    cancellation_requested: "cancellationRequested",
    operation_id: "operationId",
    phase: "phase"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationOperationStatusApplying {
    /// Whether applying was asked to cancel; already-started effects retain their lease.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    /// Original operation progress discriminator.
    pub phase: McpInstallationOperationStatusApplyingPhase,
}
crate::record_serde::wire_record!(McpInstallationOperationStatusApplying {
    cancellation_requested: "cancellationRequested",
    operation_id: "operationId",
    phase: "phase"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationOutcomeInstalled {
    /// Durable installation succeeded but final transaction cleanup remains.
    pub cleanup_pending: bool,
    /// Receipt identity produced by the confirmed transaction.
    pub installation: McpInstallationSummary,
    /// Terminal installation outcome discriminator.
    pub kind: McpInstallationOutcomeInstalledKind,
}
crate::record_serde::wire_record!(McpInstallationOutcomeInstalled {
    cleanup_pending: "cleanupPending",
    installation: "installation",
    kind: "kind"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationOutcomeUninstalled {
    /// Durable removal succeeded but final cleanup remains.
    pub cleanup_pending: bool,
    /// Exact removed receipt identity.
    pub installation_id: String,
    /// Terminal installation outcome discriminator.
    pub kind: McpInstallationOutcomeUninstalledKind,
    /// Original removal operation.
    pub operation_id: String,
    /// Any grants in the incumbent shared OAuth store remain unowned and retained.
    pub preserved_shared_authentication: bool,
    /// Exact owned input slots removed, excluding shared OAuth credentials.
    pub removed_owned_secrets: i64,
    /// Whether protected pre-install configuration was restored.
    pub restored_previous_configuration: bool,
}
crate::record_serde::wire_record!(McpInstallationOutcomeUninstalled {
    cleanup_pending: "cleanupPending",
    installation_id: "installationId",
    kind: "kind",
    operation_id: "operationId",
    preserved_shared_authentication: "preservedSharedAuthentication",
    removed_owned_secrets: "removedOwnedSecrets",
    restored_previous_configuration: "restoredPreviousConfiguration"
});

/// The durable transaction was aborted or fully compensated.
#[derive(Debug, Clone, Default)]
pub struct McpInstallationOutcomeRolledBack {
    /// Terminal installation outcome discriminator.
    pub kind: McpInstallationOutcomeRolledBackKind,
    /// Original connection-owned operation.
    pub operation_id: String,
    /// Cause of the fully aborted or compensated operation.
    pub reason: McpInstallationFailureReason,
}
crate::record_serde::wire_record!(McpInstallationOutcomeRolledBack {
    kind: "kind",
    operation_id: "operationId",
    reason: "reason"
});

/// A write may have completed. Recover and inspect durable state before retrying.
#[derive(Debug, Clone, Default)]
pub struct McpInstallationOutcomeRecoveryRequired {
    /// Terminal installation outcome discriminator.
    pub kind: McpInstallationOutcomeRecoveryRequiredKind,
    /// Operation whose durable result must be recovered and inspected.
    pub operation_id: String,
}
crate::record_serde::wire_record!(McpInstallationOutcomeRecoveryRequired {
    kind: "kind",
    operation_id: "operationId"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationOutcomeDeclined {
    /// Terminal installation outcome discriminator.
    pub kind: McpInstallationOutcomeDeclinedKind,
    /// Original operation explicitly declined by the user.
    pub operation_id: String,
}
crate::record_serde::wire_record!(McpInstallationOutcomeDeclined {
    kind: "kind",
    operation_id: "operationId"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationOutcomeCancelled {
    /// Terminal installation outcome discriminator.
    pub kind: McpInstallationOutcomeCancelledKind,
    /// Original operation cancelled before a terminal application result.
    pub operation_id: String,
}
crate::record_serde::wire_record!(McpInstallationOutcomeCancelled {
    kind: "kind",
    operation_id: "operationId"
});

#[derive(Debug, Clone, Default)]
pub struct McpInstallationOutcomeRefused {
    /// Terminal installation outcome discriminator.
    pub kind: McpInstallationOutcomeRefusedKind,
    /// Present once an operation has been allocated; never a plan handle.
    pub operation_id: Option<String>,
    /// Specific bounded refusal, never a success-shaped fallback.
    pub reason: McpInstallationFailureReason,
}
crate::record_serde::wire_record!(McpInstallationOutcomeRefused { kind: "kind", operation_id: "operationId" if is_none, reason: "reason" });

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpInstallationOperationStatusCompleted {
    /// Whether cancellation was requested before the terminal result.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    /// Immutable terminal receipt.
    pub outcome: McpInstallationOutcome,
    /// Original operation progress discriminator.
    pub phase: McpInstallationOperationStatusCompletedPhase,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpInstallationManagementOutcomeOperation {
    /// Installation management outcome discriminator.
    pub kind: McpInstallationManagementOutcomeOperationKind,
    /// Original-connection operation snapshot.
    pub operation: McpInstallationOperationStatus,
}

#[derive(Debug, Clone, Default)]
pub struct McpInstallationManagementOutcomeRefused {
    /// Installation management outcome discriminator.
    pub kind: McpInstallationManagementOutcomeRefusedKind,
    /// Specific bounded refusal.
    pub reason: McpInstallationFailureReason,
}
crate::record_serde::wire_record!(McpInstallationManagementOutcomeRefused {
    kind: "kind",
    reason: "reason"
});

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpInstallationManagementResultOutcome {
    /// A handled management outcome with an honoured contract.
    pub kind: McpInstallationManagementResultOutcomeKind,
    /// Capabilities actually honoured for this request.
    pub negotiated: CatalogNegotiatedContract,
    /// Observed management outcome.
    pub outcome: McpInstallationManagementOutcome,
}

/// Existing-operation control. A new session selector is deliberately not accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpInstallationOperationRequest {
    /// Original installation wire capability; new-work authentication is not reacquired.
    pub contract: CatalogClientContract,
    /// Exact runtime-issued operation ID on the original connection.
    pub operation_id: String,
}
crate::record_serde::wire_record!(McpInstallationOperationRequest {
    contract: "contract",
    operation_id: "operationId"
});

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpInstallationResultOutcome {
    /// A handled operation outcome with an honoured contract.
    pub kind: McpInstallationResultOutcomeKind,
    /// Capabilities actually honoured for this request.
    pub negotiated: CatalogNegotiatedContract,
    /// Terminal result of the original operation.
    pub outcome: McpInstallationOutcome,
}

/// A request-local value for one exact reviewed placeholder. Never logged or persisted in a plan.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpInstallationSecret {
    /// Exact placeholder from the selected choice, not a caller-chosen backend identifier.
    pub placeholder: String,
    /// Fresh explicit secret value. It is omitted from confirmation reviews and telemetry.
    pub value: String,
}
crate::record_serde::wire_record!(McpInstallationSecret {
    placeholder: "placeholder",
    value: "value"
});

/// New-work inventory or recovery request under an explicitly selected existing session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpInstallationsRequest {
    /// Required authenticated bound installation contract.
    pub contract: CatalogClientContract,
    /// Existing selected local session on this connection.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(McpInstallationsRequest {
    contract: "contract",
    policy_session_id: "policySessionId"
});

/// A normalised, inert description of what installing an MCP server would involve. Carries no raw card, no install specification, and no secret value.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpInstallPlan {
    /// Alternative configuration changes, with exactly one entry for each transportChoices entry in the same order. Only the entry for the subsequently selected transport applies; these are not cumulative writes. Payloads remain behind the runtime boundary.
    pub configuration_changes: Vec<McpPlanConfigurationChange>,
    /// Normalised identity of the server the plan would install.
    pub identity: McpPlanResourceIdentity,
    /// Opaque, runtime-instance scoped, TTL-bound, single-use handle for this plan. Rejected when stale, replayed, or presented to a different runtime instance. Never logged.
    pub plan_handle: String,
    /// ISO 8601 timestamp after which the plan handle is stale and will be rejected. Abandoning a plan needs no call: an unused handle simply expires, so cancellation before commit is side-effect free.
    pub plan_handle_expires_at: String,
    /// Outcome of evaluating the server against registry and enterprise policy.
    pub policy: McpPlanPolicyResult,
    /// Origin and semantic digest of the exact validated JSON MCP card content bound to this plan.
    pub provenance: McpPlanProvenance,
    /// Identifier of the choice the runtime would pick by default. Omitted when there is no eligible transport, or when the runtime expresses no preference.
    pub recommended_transport_choice_id: Option<String>,
    /// Whether applying this plan would require an MCP reload to take effect. Planning itself never reloads.
    pub reload_required: bool,
    /// True only when every eligible transport choice needs additional values or secrets. False means at least one choice needs no additional configuration, not that every choice is ready. A later apply operation must validate the selected choice's own inputs, secrets and policy after explicit confirmation.
    pub requires_interactive_configuration: bool,
    /// Configuration scope and key the plan would write to.
    pub target: McpPlanTarget,
    /// Every eligible transport, so a host can present an explicit choice. A completed plan always has at least one; when none is eligible, planning returns `CatalogUnavailableTransportError` instead.
    pub transport_choices: Vec<McpPlanTransportChoice>,
}
crate::record_serde::wire_record!(McpInstallPlan { configuration_changes: "configurationChanges", identity: "identity", plan_handle: "planHandle", plan_handle_expires_at: "planHandleExpiresAt", policy: "policy", provenance: "provenance", recommended_transport_choice_id: "recommendedTransportChoiceId" if is_none, reload_required: "reloadRequired", requires_interactive_configuration: "requiresInteractiveConfiguration", target: "target", transport_choices: "transportChoices" });

/// Server name to check running status for.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpIsServerRunningRequest {
    /// Name of the MCP server to check
    pub server_name: String,
}
crate::record_serde::wire_record!(McpIsServerRunningRequest {
    server_name: "serverName"
});

/// Whether the named MCP server is running.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpIsServerRunningResult {
    /// True if the server has an active client and transport.
    pub running: bool,
}
crate::record_serde::wire_record!(McpIsServerRunningResult { running: "running" });

/// Names of the MCP servers the runtime supplies itself.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpListBuiltinServerNamesResult {
    /// Every built-in server name. A host disabling the built-ins wholesale suppresses exactly this set.
    pub names: Vec<String>,
}
crate::record_serde::wire_record!(McpListBuiltinServerNamesResult { names: "names" });

/// Server name whose tool list should be returned.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpListToolsRequest {
    /// Name of the connected MCP server whose tools to list.
    pub server_name: String,
}
crate::record_serde::wire_record!(McpListToolsRequest {
    server_name: "serverName"
});

/// Tools exposed by the connected MCP server. Throws when the server is not connected.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpListToolsResult {
    /// Tools exposed by the server.
    pub tools: Vec<McpTools>,
}
crate::record_serde::wire_record!(McpListToolsResult { tools: "tools" });

/// Full host-driven authentication for an arbitrary, possibly unsaved, MCP URL.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthAuthenticateRequest {
    /// Authorization timeout in milliseconds. Defaults to five minutes.
    pub authorization_timeout_ms: Option<i64>,
    /// Fail with browser_required when silent authentication cannot complete.
    pub browserless: Option<bool>,
    /// Text displayed after a successful loopback callback.
    pub callback_success_message: Option<String>,
    /// Client metadata document URL. Omission uses the session identity.
    pub client_id_metadata_url: Option<String>,
    /// Display name for new dynamic client registrations.
    pub client_name: Option<String>,
    /// Delay before the runtime offers device authorization.
    pub device_code_offer_timeout_ms: Option<i64>,
    /// Override the device authorization poll interval in milliseconds.
    pub device_code_poll_interval_ms: Option<i64>,
    /// Correlation identity reserved through prepareFlow.
    pub flow_id: String,
    /// Start device authorization instead of a browser flow.
    pub force_device_code: Option<bool>,
    /// Bypass cached grants for explicit account selection.
    pub force_reauth: Option<bool>,
    /// Preserve previously granted scopes during additional consent.
    pub include_existing_token_scopes: Option<bool>,
    /// Preserve replacement policy when another pending flow owns consent.
    pub preserve_pending_flow_replacement_flags: Option<bool>,
    /// Fixed callback port. A busy port fails without fallback.
    pub redirect_port: Option<i64>,
    /// Raw protected-resource metadata already fetched by the host.
    pub resource_metadata: Option<String>,
    /// Proxy fallback when the session has not published an egress entry.
    pub sandbox_proxy: Option<String>,
    /// MCP endpoint, without a requirement for saved configuration.
    pub server_url: String,
    /// Use environment session claims for a hostless compatibility call, while retaining this requester's egress authority.
    pub sessionless: Option<bool>,
    /// Let the host present the authorization URL without a runtime browser launch.
    pub skip_browser_open: Option<bool>,
    /// Reject the cached access token but permit a refresh grant.
    pub skip_cached_access_token: Option<bool>,
    /// Static OAuth client settings for this attempt.
    #[doc(hidden)]
    pub(crate) static_client_config: Option<McpOauthFlowStaticClientConfig>,
    /// Credential owner. Omission uses the authoritative session store.
    #[doc(hidden)]
    pub(crate) store: Option<McpOauthFlowStore>,
    /// Challenge captured by the host, including an empty challenge.
    pub www_authenticate_params: Option<McpOauthWWWAuthenticateParams>,
}
crate::record_serde::wire_record!(McpOauthAuthenticateRequest { authorization_timeout_ms: "authorizationTimeoutMs" if is_none, browserless: "browserless" if is_none, callback_success_message: "callbackSuccessMessage" if is_none, client_id_metadata_url: "clientIdMetadataUrl" if is_none, client_name: "clientName" if is_none, device_code_offer_timeout_ms: "deviceCodeOfferTimeoutMs" if is_none, device_code_poll_interval_ms: "deviceCodePollIntervalMs" if is_none, flow_id: "flowId", force_device_code: "forceDeviceCode" if is_none, force_reauth: "forceReauth" if is_none, include_existing_token_scopes: "includeExistingTokenScopes" if is_none, preserve_pending_flow_replacement_flags: "preservePendingFlowReplacementFlags" if is_none, redirect_port: "redirectPort" if is_none, resource_metadata: "resourceMetadata" if is_none, sandbox_proxy: "sandboxProxy" if is_none, server_url: "serverUrl", sessionless: "sessionless" if is_none, skip_browser_open: "skipBrowserOpen" if is_none, skip_cached_access_token: "skipCachedAccessToken" if is_none, static_client_config: "staticClientConfig" if is_none, store: "store" if is_none, www_authenticate_params: "wwwAuthenticateParams" if is_none });

/// Token projection for the existing host-delegated provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthAuthenticateResult {
    /// Access token from the runtime-owned store.
    pub access_token: String,
    /// Remaining lifetime in seconds when the issuer supplied an expiry.
    pub expires_in: Option<i64>,
    /// Token scheme. Defaults to Bearer.
    pub token_type: String,
}
crate::record_serde::wire_record!(McpOauthAuthenticateResult { access_token: "accessToken", expires_in: "expiresIn" if is_none, token_type: "tokenType" });

/// Identifies the MCP server whose persisted OAuth credentials were updated.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthAuthenticationStateChangedRequest {
    /// Whether the target session must mint a session-scoped access token instead of reusing a shared access token persisted by another session.
    pub refresh_session_token: Option<bool>,
    /// Name of the MCP server whose OAuth credentials were updated. Omit only when the host cannot identify the server.
    pub server_name: Option<String>,
}
crate::record_serde::wire_record!(McpOauthAuthenticationStateChangedRequest { refresh_session_token: "refreshSessionToken" if is_none, server_name: "serverName" if is_none });

/// Authorization URL presented by the original SDK requester.
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthAuthorizationUrlCallback {
    /// Authorization URL that the requester must present.
    pub authorization_url: String,
    /// Identity reserved by the original requester.
    pub flow_id: String,
    /// Session that owns the authentication attempt.
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(McpOauthAuthorizationUrlCallback {
    authorization_url: "authorizationUrl",
    flow_id: "flowId",
    session_id: "sessionId"
});

/// Targets only the original prepared/applying owned login on this exact session requester.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthCancelLoginRequest {
    /// The same authoritative installation identity supplied during preparation.
    pub expected_installation_id: String,
    /// Runtime-issued login handle known before the effectful login request begins.
    pub login_id: String,
}
crate::record_serde::wire_record!(McpOauthCancelLoginRequest {
    expected_installation_id: "expectedInstallationId",
    login_id: "loginId"
});

/// Honest terminal cancellation result; persistence or recovery failures remain RPC errors.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthCancelLoginResult {
    /// True after cancellation settles, false when the original login already connected successfully.
    pub cancelled: bool,
}
crate::record_serde::wire_record!(McpOauthCancelLoginResult {
    cancelled: "cancelled"
});

/// A remote MCP server whose authorization server should be asked about Dynamic Client Registration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthCheckDcrSupportRequest {
    /// Raw protected-resource metadata text from the server's `WWW-Authenticate` challenge. Omit to discover the authorization server from the server URL instead.
    pub resource_metadata_text: Option<String>,
    /// Proxy the request must travel through. Omit to use the session's own egress settings.
    pub sandbox_proxy: Option<String>,
    /// URL of the remote MCP server that answered 401.
    pub server_url: String,
    /// Locally owned session whose egress policy and sandbox proxy constrain the request. Omitted or nonlocal session IDs provide no local session egress state.
    pub session_id: Option<SessionId>,
}
crate::record_serde::wire_record!(McpOauthCheckDcrSupportRequest { resource_metadata_text: "resourceMetadataText" if is_none, sandbox_proxy: "sandboxProxy" if is_none, server_url: "serverUrl", session_id: "sessionId" if is_none });

/// Whether the server's authorization server registers clients dynamically.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthCheckDcrSupportResult {
    /// Authorization server the metadata pointed at. Absent when none could be discovered.
    pub authorization_server_url: Option<String>,
    /// Whether the authorization server advertises a client registration endpoint.
    pub supports_dcr: bool,
}
crate::record_serde::wire_record!(McpOauthCheckDcrSupportResult { authorization_server_url: "authorizationServerUrl" if is_none, supports_dcr: "supportsDcr" });

/// Host-delivered callback for a runtime-managed MCP OAuth login.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthCompleteRequest {
    /// Opaque identifier returned by session.mcp.oauth.login for the pending external callback.
    pub authorization_id: String,
    /// Full externally visible HTTPS callback URL received by the host, including the authorization response query parameters. Applications behind a reverse proxy must reconstruct the public URL rather than passing an internal proxy URL.
    pub callback_url: String,
}
crate::record_serde::wire_record!(McpOauthCompleteRequest {
    authorization_id: "authorizationId",
    callback_url: "callbackUrl"
});

/// Dispose an explicit host store without altering the session authorizer's owner.
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthDisposeStoreRequest {
    /// Host-selected identity of the explicit store.
    pub store_id: String,
}
crate::record_serde::wire_record!(McpOauthDisposeStoreRequest {
    store_id: "storeId"
});

/// An OAuth challenge from a remote MCP server whose protected-resource metadata should be read.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthFetchProtectedResourceMetadataRequest {
    /// Request headers the configured server sends, as a serialized JSON object. Omit to send none.
    pub headers_json: Option<String>,
    /// URL of the remote MCP server that answered 401.
    pub request_url: String,
    /// Metadata URL named in the server's `WWW-Authenticate` challenge. Omit to try conventional metadata endpoints derived from the request URL. It is server-controlled, so it passes the same egress policy check as the request URL.
    pub resource_metadata_url: Option<String>,
    /// Proxy the request must travel through. Omit to use the session's own egress settings.
    pub sandbox_proxy: Option<String>,
    /// Locally owned session whose egress policy and sandbox proxy constrain the request. Omitted or nonlocal session IDs provide no local session egress state.
    pub session_id: Option<SessionId>,
    /// User agent to send. Omit to use the runtime's own.
    pub user_agent: Option<String>,
}
crate::record_serde::wire_record!(McpOauthFetchProtectedResourceMetadataRequest { headers_json: "headersJson" if is_none, request_url: "requestUrl", resource_metadata_url: "resourceMetadataUrl" if is_none, sandbox_proxy: "sandboxProxy" if is_none, session_id: "sessionId" if is_none, user_agent: "userAgent" if is_none });

/// Raw protected-resource metadata from the explicit metadata URL or conventional discovery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthFetchProtectedResourceMetadataResult {
    /// Metadata document exactly as the server returned it, unparsed. Absent when no document could be read from the explicit metadata URL or, when omitted, the conventional metadata endpoints.
    pub metadata_text: Option<String>,
}
crate::record_serde::wire_record!(McpOauthFetchProtectedResourceMetadataResult { metadata_text: "metadataText" if is_none });

/// Correlation identity reserved before an effectful authentication request.
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthFlowIdentityRequest {
    /// Host-chosen identity, unique within this session requester.
    pub flow_id: String,
}
crate::record_serde::wire_record!(McpOauthFlowIdentityRequest { flow_id: "flowId" });

/// Progress from an OAuth attempt to its original SDK requester.
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthFlowStatusCallback {
    /// Identity reserved by the original requester.
    pub flow_id: String,
    /// Status detail for the host UI, or null when absent.
    pub message: Option<String>,
    /// Serialized device authorization details, or null when absent.
    pub payload_json: Option<String>,
    /// Session that owns the authentication attempt.
    pub session_id: SessionId,
    /// Current OAuth flow status.
    pub status: String,
}
crate::record_serde::wire_record!(McpOauthFlowStatusCallback {
    flow_id: "flowId",
    message: "message",
    payload_json: "payloadJson",
    session_id: "sessionId",
    status: "status"
});

#[derive(Debug, Clone, Default)]
pub struct McpOauthPendingRequestResponseToken {
    /// Access token acquired by the SDK host
    pub access_token: String,
    /// Token lifetime in seconds, if known.
    pub expires_in: Option<i64>,
    /// OAuth response variant discriminator.
    pub kind: McpOauthPendingRequestResponseTokenKind,
    /// OAuth token type. Defaults to bearer when omitted.
    pub token_type: Option<String>,
}
crate::record_serde::wire_record!(McpOauthPendingRequestResponseToken { access_token: "accessToken", expires_in: "expiresIn" if is_none, kind: "kind", token_type: "tokenType" if is_none });

#[derive(Debug, Clone, Default)]
pub struct McpOauthPendingRequestResponseCancelled {
    /// OAuth response variant discriminator.
    pub kind: McpOauthPendingRequestResponseCancelledKind,
}
crate::record_serde::wire_record!(McpOauthPendingRequestResponseCancelled { kind: "kind" });

/// Delegate noninteractive refresh to the original runtime authorization operation.
/// Valid only for a Refresh request advertising runtimeRefreshSupported.
#[derive(Debug, Clone, Default)]
pub struct McpOauthPendingRequestResponseRuntimeRefresh {
    /// OAuth response variant discriminator.
    pub kind: McpOauthPendingRequestResponseRuntimeRefreshKind,
}
crate::record_serde::wire_record!(McpOauthPendingRequestResponseRuntimeRefresh { kind: "kind" });

/// Pending MCP OAuth request ID and host token, cancellation, or advertised runtime refresh response.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpOauthHandlePendingRequest {
    /// OAuth request identifier from the mcp.oauth_required event
    pub request_id: RequestId,
    /// Host response to the pending OAuth request.
    pub result: McpOauthPendingRequestResponse,
}

/// Indicates whether the pending MCP OAuth response was accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthHandlePendingResult {
    /// Whether the response was accepted. False if the request was unknown, timed out, or already resolved.
    pub success: bool,
}
crate::record_serde::wire_record!(McpOauthHandlePendingResult { success: "success" });

/// An authorization server URL to classify.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthIsMicrosoftAuthorizationServerRequest {
    /// Authorization server URL, typically the one `mcp.oauth.checkDcrSupport` discovered.
    pub authorization_server_url: String,
}
crate::record_serde::wire_record!(McpOauthIsMicrosoftAuthorizationServerRequest {
    authorization_server_url: "authorizationServerUrl"
});

/// Whether the URL is a Microsoft Entra ID authorization server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthIsMicrosoftAuthorizationServerResult {
    /// Whether the host is one Microsoft Entra ID serves authorization from.
    pub is_microsoft_authorization_server: bool,
}
crate::record_serde::wire_record!(McpOauthIsMicrosoftAuthorizationServerResult {
    is_microsoft_authorization_server: "isMicrosoftAuthorizationServer"
});

/// Remote MCP server name and optional overrides controlling reauthentication, OAuth client display name, callback handling, and static OAuth client selection.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthLoginRequest {
    /// Optional override for the body text shown on the OAuth loopback callback success page. When omitted, the runtime applies a neutral fallback; callers driving interactive auth should pass surface-specific copy telling the user where to return.
    pub callback_success_message: Option<String>,
    /// Optional OAuth client ID override for this login. When set, the runtime uses this pre-registered static client instead of dynamic client registration.
    pub client_id: Option<String>,
    /// Optional override for the OAuth client display name shown on the consent screen. Applies to newly registered dynamic clients only — existing registrations keep the name they were created with. When omitted, the runtime applies a neutral fallback; callers driving interactive auth should pass their own surface-specific label so the consent screen matches the product the user sees.
    pub client_name: Option<String>,
    /// Optional OAuth client secret override for this login. The runtime treats this as an ephemeral host-owned secret, uses it for this authentication attempt and does not persist it.
    pub client_secret: Option<String>,
    /// Exact owned receipt identity. Owned login never uses an implicit helper session.
    pub expected_installation_id: Option<String>,
    /// When true, clears any cached OAuth token for the server and runs a full new authorization. Use when the user explicitly wants to switch accounts or believes their session is stuck.
    pub force_reauth: Option<bool>,
    /// Optional OAuth grant type override for this login. Defaults to the server configuration, or authorization_code when no grant type is specified.
    pub grant_type: Option<McpOauthLoginGrantType>,
    /// Required for owned login. Consumes the exact prepareLogin handle once.
    /// Set forceReauth and display options during preparation, not consumption.
    pub login_id: Option<String>,
    /// Optional override indicating whether the static OAuth client is public. When false, the runtime treats it as confidential and uses the per-login clientSecret if provided, otherwise retrieving the client secret from the MCP OAuth secret store.
    pub public_client: Option<bool>,
    /// Optional externally visible HTTPS redirect URI for a host-managed callback endpoint. When supplied, the runtime still owns discovery, PKCE, token exchange, persistence, and reconnect, but does not bind a loopback listener or terminate HTTPS. The URI must not contain query parameters or a fragment and must be registered for the selected CIMD, DCR, or static OAuth client.
    pub redirect_uri: Option<String>,
    /// Name of the remote MCP server to authenticate
    pub server_name: String,
}
crate::record_serde::wire_record!(McpOauthLoginRequest { callback_success_message: "callbackSuccessMessage" if is_none, client_id: "clientId" if is_none, client_name: "clientName" if is_none, client_secret: "clientSecret" if is_none, expected_installation_id: "expectedInstallationId" if is_none, force_reauth: "forceReauth" if is_none, grant_type: "grantType" if is_none, login_id: "loginId" if is_none, public_client: "publicClient" if is_none, redirect_uri: "redirectUri" if is_none, server_name: "serverName" });

/// OAuth authorization URL the caller should open, or empty when cached tokens already authenticated the server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthLoginResult {
    /// Opaque authorization identifier returned only for a host-managed redirect URI. The runtime also sends it as the OAuth state value, so the callback endpoint can read state and pass it with the full callback URL to session.mcp.oauth.complete.
    pub authorization_id: Option<String>,
    /// URL the caller should open in a browser to complete OAuth. Omitted when cached tokens were still valid and no browser interaction was needed — the server is already reconnected in that case. For the default loopback flow, the runtime starts its listener before returning. With redirectUri, the host receives the callback and completes it through session.mcp.oauth.complete. The runtime continues the flow in the background and signals completion via session.mcp_server_status_changed.
    pub authorization_url: Option<String>,
    /// Runtime-issued owned flow identity; never a server name or installation operation ID.
    pub login_id: Option<String>,
    /// Explicit outcome for owned sign-in. Manual callers retain their legacy response shape.
    pub status: Option<McpOwnedOauthLoginStatus>,
}
crate::record_serde::wire_record!(McpOauthLoginResult { authorization_id: "authorizationId" if is_none, authorization_url: "authorizationUrl" if is_none, login_id: "loginId" if is_none, status: "status" if is_none });

/// Effect-free preparation bound to the existing local session, requester and installation, with frozen options.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthPrepareLoginRequest {
    /// Text shown on the loopback callback page after successful authorisation.
    pub callback_success_message: Option<String>,
    /// Display name used by the incumbent OAuth client-registration flow.
    pub client_name: Option<String>,
    /// Exact installation identity from owned inventory, never a server-name alias.
    pub expected_installation_id: String,
    /// Request a new authorisation rather than accepting a usable cached grant.
    pub force_reauth: Option<bool>,
    /// Name recorded by the authoritative owned installation receipt.
    pub server_name: String,
}
crate::record_serde::wire_record!(McpOauthPrepareLoginRequest { callback_success_message: "callbackSuccessMessage" if is_none, client_name: "clientName" if is_none, expected_installation_id: "expectedInstallationId", force_reauth: "forceReauth" if is_none, server_name: "serverName" });

/// An inert runtime-issued login handle. Preparation alone performs no activation or OAuth work.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthPrepareLoginResult {
    /// Original expiry, not extended by consumption, retries or cancellation.
    pub expires_at: String,
    /// Retain with the original requester and use for one login or cancellation.
    pub login_id: String,
}
crate::record_serde::wire_record!(McpOauthPrepareLoginResult {
    expires_at: "expiresAt",
    login_id: "loginId"
});

/// Remote MCP server name for a passive OAuth status probe.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthProbeRequest {
    /// Exact owned receipt identity; probing never activates a dormant installation.
    pub expected_installation_id: Option<String>,
    /// Name of the configured remote MCP server to probe.
    pub server_name: String,
}
crate::record_serde::wire_record!(McpOauthProbeRequest { expected_installation_id: "expectedInstallationId" if is_none, server_name: "serverName" });

#[derive(Debug, Clone, Default)]
pub struct McpOauthProbeResultNoAuthRequired {
    /// HTTP response returned by the server.
    pub http_response: McpOauthHttpResponse,
    /// Probe outcome variant discriminator.
    pub status: McpOauthProbeResultNoAuthRequiredStatus,
}
crate::record_serde::wire_record!(McpOauthProbeResultNoAuthRequired {
    http_response: "httpResponse",
    status: "status"
});

#[derive(Debug, Clone, Default)]
pub struct McpOauthProbeResultAuthenticated {
    /// HTTP response returned by the server.
    pub http_response: McpOauthHttpResponse,
    /// Probe outcome variant discriminator.
    pub status: McpOauthProbeResultAuthenticatedStatus,
}
crate::record_serde::wire_record!(McpOauthProbeResultAuthenticated {
    http_response: "httpResponse",
    status: "status"
});

#[derive(Debug, Clone, Default)]
pub struct McpOauthProbeResultNeedsAuth {
    /// HTTP 401 or 403 response returned by the server.
    pub http_response: McpOauthHttpResponse,
    /// Why authentication is needed.
    pub reason: McpOauthProbeNeedsAuthReason,
    /// Probe outcome variant discriminator.
    pub status: McpOauthProbeResultNeedsAuthStatus,
    /// Parsed WWW-Authenticate challenge parameters, when present and parseable.
    pub www_authenticate_params: Option<McpOauthWWWAuthenticateParams>,
}
crate::record_serde::wire_record!(McpOauthProbeResultNeedsAuth { http_response: "httpResponse", reason: "reason", status: "status", www_authenticate_params: "wwwAuthenticateParams" if is_none });

#[derive(Debug, Clone, Default)]
pub struct McpOauthProbeResultFailed {
    /// Human-readable probe failure detail.
    pub error: String,
    /// HTTP response returned by the server, when the probe reached the server and captured the complete response.
    pub http_response: Option<McpOauthHttpResponse>,
    /// Probe outcome variant discriminator.
    pub status: McpOauthProbeResultFailedStatus,
}
crate::record_serde::wire_record!(McpOauthProbeResultFailed { error: "error", http_response: "httpResponse" if is_none, status: "status" });

/// Pending MCP OAuth request id to respond to.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthRespondRequest {
    /// OAuth request identifier from the mcp.oauth_required event
    pub request_id: RequestId,
}
crate::record_serde::wire_record!(McpOauthRespondRequest {
    request_id: "requestId"
});

/// Indicates whether the pending MCP OAuth response was accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpOauthRespondResult {
    /// Whether the response was accepted. False if the request was unknown, timed out, or already resolved.
    pub success: bool,
}
crate::record_serde::wire_record!(McpOauthRespondResult { success: "success" });

/// Persist a confidential client's static secret without exposing token-store CRUD.
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthSaveStaticClientSecretRequest {
    /// Secret to persist in the selected credential owner.
    pub secret: String,
    /// Preserve hostless credential and account selection.
    pub sessionless: Option<bool>,
    /// Credential owner. Omission uses the authoritative session store.
    #[doc(hidden)]
    pub(crate) store: Option<McpOauthFlowStore>,
    /// Runtime OAuth credential key.
    pub store_key: String,
}
crate::record_serde::wire_record!(McpOauthSaveStaticClientSecretRequest { secret: "secret", sessionless: "sessionless" if is_none, store: "store" if is_none, store_key: "storeKey" });

/// An OAuth client a server configuration pins, without its secret.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthStaticClientConfig {
    /// Configured OAuth client identifier.
    pub client_id: String,
    /// Grant type, carried only when the configuration pins the headless `client_credentials` flow. Absent for the default interactive authorization-code flow.
    pub grant_type: Option<McpServerConfigHttpOauthGrantType>,
    /// Whether the configuration declares a public client. Absent when the configuration does not say, which the flow reads as a public client.
    pub public_client: Option<bool>,
    /// Configured OAuth scopes joined into one space-delimited scope value. Absent when the configuration specifies no scopes.
    pub scope: Option<String>,
}
crate::record_serde::wire_record!(McpOauthStaticClientConfig { client_id: "clientId", grant_type: "grantType" if is_none, public_client: "publicClient" if is_none, scope: "scope" if is_none });

/// A remote MCP server configuration whose pinned OAuth client should be read.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthStaticClientConfigRequest {
    /// The server configuration to read. A configuration that pins no client answers nothing.
    pub config: serde_json::Value,
}
crate::record_serde::wire_record!(McpOauthStaticClientConfigRequest { config: "config" });

/// The OAuth client the configuration pins, when it pins one.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthStaticClientConfigResult {
    /// The pinned client. Absent when the configuration pins no non-empty client id, so the flow should register one dynamically.
    #[doc(hidden)]
    pub(crate) client_config: Option<McpOauthStaticClientConfig>,
}
crate::record_serde::wire_record!(McpOauthStaticClientConfigResult { client_config: "clientConfig" if is_none });

/// A narrow token-validity query against the chosen credential owner.
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthStoredGrantRequest {
    /// Preserve hostless credential and account selection.
    pub sessionless: Option<bool>,
    /// Static client identity required for refresh, when configured.
    #[doc(hidden)]
    pub(crate) static_client_config: Option<McpOauthFlowStaticClientConfig>,
    /// Credential owner. Omission uses the authoritative session store.
    #[doc(hidden)]
    pub(crate) store: Option<McpOauthFlowStore>,
    /// Runtime OAuth credential key.
    pub store_key: String,
}
crate::record_serde::wire_record!(McpOauthStoredGrantRequest { sessionless: "sessionless" if is_none, static_client_config: "staticClientConfig" if is_none, store: "store" if is_none, store_key: "storeKey" });

/// Derive a credential key through the selected account and store owner.
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthStoreKeyRequest {
    /// Static OAuth client identity, when configured.
    pub client_id: Option<String>,
    /// Metadata document identity when no static client is configured.
    pub client_id_metadata_url: Option<String>,
    /// MCP endpoint whose credentials the key identifies.
    pub server_url: String,
    /// Preserve hostless credential and account selection.
    pub sessionless: Option<bool>,
    /// Credential owner. Omission uses the authoritative session store.
    #[doc(hidden)]
    pub(crate) store: Option<McpOauthFlowStore>,
}
crate::record_serde::wire_record!(McpOauthStoreKeyRequest { client_id: "clientId" if is_none, client_id_metadata_url: "clientIdMetadataUrl" if is_none, server_url: "serverUrl", sessionless: "sessionless" if is_none, store: "store" if is_none });

/// Tokenless initialize handshake for a possibly unsaved remote MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpOauthTestConnectionRequest {
    /// Headers sent during the handshake.
    pub headers: Option<HashMap<String, String>>,
    /// Proxy fallback when no session egress entry exists.
    pub sandbox_proxy: Option<String>,
    /// Transport. Omission selects Streamable HTTP.
    #[doc(hidden)]
    pub(crate) r#type: Option<McpOauthTestConnectionTransport>,
    /// Remote endpoint to initialize without an OAuth token.
    pub url: String,
}
crate::record_serde::wire_record!(McpOauthTestConnectionRequest { headers: "headers" if is_none, sandbox_proxy: "sandboxProxy" if is_none, r#type: "type" if is_none, url: "url" });

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

/// A computed MCP install plan. Nothing has been applied: the plan describes what installing would change, and the plan handle is what a later apply operation would consume.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPlanInstallPlanned {
    /// Discriminator: a plan was computed and nothing was changed
    pub kind: McpPlanInstallPlannedKind,
    /// Protocol version and capabilities the runtime honoured.
    pub negotiated: CatalogNegotiatedContract,
    /// The normalised plan.
    pub plan: McpInstallPlan,
}
crate::record_serde::wire_record!(McpPlanInstallPlanned {
    kind: "kind",
    negotiated: "negotiated",
    plan: "plan"
});

/// Plan from a candidate returned by a previous catalog search.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpPlanInstallSourceCandidate {
    /// Single-use candidate handle. Consumed by this call, so a replay of the same handle is rejected.
    pub candidate_handle: String,
    /// Discriminator: plan from a previously returned candidate
    #[serde(deserialize_with = "McpPlanInstallSourceCandidate::deserialize_kind")]
    pub kind: McpPlanInstallSourceCandidateKind,
    /// The runtime- or authority-minted `searchId` returned with the search that produced this candidate. Planning verifies the private correlation and atomically consumes a matching candidate before downstream work, including attempts that subsequently fail or report unavailable. A mismatched search does not consume the candidate. It identifies a search rather than a person and must never be joined with user identity to re-identify anyone.
    pub search_id: String,
}

impl McpPlanInstallSourceCandidate {
    fn deserialize_kind<'de, D>(
        deserializer: D,
    ) -> Result<McpPlanInstallSourceCandidateKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "candidate" {
            return Err(serde::de::Error::unknown_variant(&value, &["candidate"]));
        }
        <McpPlanInstallSourceCandidateKind>::deserialize(serde::de::value::StringDeserializer::<
            D::Error,
        >::new(value))
    }
}

/// An MCP server card to be retrieved from a URL through the runtime's hardened fetch boundary.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpServerCardUrl {
    /// Discriminator: the card is URL-backed, and carries no embedded data
    #[serde(deserialize_with = "McpServerCardUrl::deserialize_kind")]
    pub kind: McpServerCardUrlKind,
    /// Media type the card is expected to conform to.
    pub media_type: McpServerCardMediaType,
    /// Card URL. Retrieved only through the runtime's hardened boundary, with scheme, credential, address-range, redirect, timeout, and response-size controls applied. Never logged.
    pub url: String,
}

impl McpServerCardUrl {
    fn deserialize_kind<'de, D>(deserializer: D) -> Result<McpServerCardUrlKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "url" {
            return Err(serde::de::Error::unknown_variant(&value, &["url"]));
        }
        <McpServerCardUrlKind>::deserialize(serde::de::value::StringDeserializer::<D::Error>::new(
            value,
        ))
    }
}

/// An MCP server card supplied inline as an inert document.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpServerCardEmbedded {
    /// The card document verbatim, treated as inert untrusted bytes. The runtime parses and validates it; the host is not expected to interpret it. Never logged.
    pub data: String,
    /// Discriminator: the card is embedded, and carries no URL
    #[serde(deserialize_with = "McpServerCardEmbedded::deserialize_kind")]
    pub kind: McpServerCardEmbeddedKind,
    /// Media type the card is expected to conform to.
    pub media_type: McpServerCardMediaType,
}

impl McpServerCardEmbedded {
    fn deserialize_kind<'de, D>(deserializer: D) -> Result<McpServerCardEmbeddedKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "embedded" {
            return Err(serde::de::Error::unknown_variant(&value, &["embedded"]));
        }
        <McpServerCardEmbeddedKind>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// Plan from a card supplied directly by the caller, without a preceding search.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpPlanInstallSourceCard {
    /// The card to plan from: exactly one of a URL or embedded data.
    pub card: McpServerCardReference,
    /// Discriminator: plan from a caller-supplied card
    #[serde(deserialize_with = "McpPlanInstallSourceCard::deserialize_kind")]
    pub kind: McpPlanInstallSourceCardKind,
}

impl McpPlanInstallSourceCard {
    fn deserialize_kind<'de, D>(deserializer: D) -> Result<McpPlanInstallSourceCardKind, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        if value != "card" {
            return Err(serde::de::Error::unknown_variant(&value, &["card"]));
        }
        <McpPlanInstallSourceCardKind>::deserialize(
            serde::de::value::StringDeserializer::<D::Error>::new(value),
        )
    }
}

/// A side-effect-free request for an MCP install plan. Computing a plan never writes configuration, stores a secret, or reloads MCP servers.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpPlanInstallRequest {
    /// Protocol version and capabilities the caller requires.
    pub contract: CatalogClientContract,
    /// The same existing attached session that owns the original catalogue candidate.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub policy_session_id: Option<String>,
    /// Configuration scope the plan targets. Defaults to user scope when omitted.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub scope: Option<McpPlanScope>,
    /// What to plan: either a candidate handle from a previous search, or a card supplied directly.
    pub source: McpPlanInstallSource,
}

/// Read-only preparation of one owned removal under fresh selected-session authority.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPlanUninstallRequest {
    /// Required authenticated bound installation capabilities.
    pub contract: CatalogClientContract,
    /// Exact receipt to inspect, not a server-name guess.
    pub installation_id: String,
    /// Existing selected session on the original connection.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(McpPlanUninstallRequest {
    contract: "contract",
    installation_id: "installationId",
    policy_session_id: "policySessionId"
});

/// Side-effect-free preparation of one original bound remote MCP choice.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpPrepareInstallRequest {
    /// Exact selected alternative from that plan.
    pub choice_id: String,
    /// Required bound catalogue and confirmed remote installation capabilities.
    pub contract: CatalogClientContract,
    /// Declared non-secret values. Non-empty only when the caller requires
    /// `mcp-configured-remote-installation`; omitted values use the card default.
    pub inputs: Vec<McpInstallationInput>,
    /// Original single-use bound plan, never a client-authored configuration.
    pub plan_handle: String,
    /// An existing local session attached to this connection, not permission to attach one.
    pub policy_session_id: String,
    /// One entry per declared secret placeholder of the selected choice. Non-empty
    /// only when the caller requires `mcp-configured-remote-installation`.
    pub secrets: Vec<McpInstallationSecret>,
    /// The trusted host presents this choice alongside the exact secret placeholders.
    pub secret_storage: McpInstallationSecretStorage,
    /// The exact original source, used transiently only after confirmation.
    pub source: McpServerCardReference,
}

/// MCP server, prompt name, and optional string-valued arguments.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPromptsGetRequest {
    /// String-valued arguments to pass to the prompt
    pub arguments: Option<HashMap<String, String>>,
    /// The programmatic name of the prompt
    pub prompt_name: String,
    /// Name of the MCP server hosting the prompt
    pub server_name: String,
}
crate::record_serde::wire_record!(McpPromptsGetRequest { arguments: "arguments" if is_none, prompt_name: "promptName", server_name: "serverName" });

/// Prompt messages returned by the MCP server without sending them to the model.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPromptsGetResult {
    /// MCP result metadata
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Server-provided non-standard result fields
    pub additional_properties: Option<HashMap<String, serde_json::Value>>,
    /// Description of the prompt
    pub description: Option<String>,
    /// Ordered prompt messages
    pub messages: Vec<McpPromptMessage>,
}
crate::record_serde::wire_record!(McpPromptsGetResult { meta: "_meta" if is_none, additional_properties: "additionalProperties" if is_none, description: "description" if is_none, messages: "messages" });

/// MCP server whose prompts to enumerate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPromptsListRequest {
    /// Opaque MCP pagination cursor from a prior `nextCursor` value
    pub cursor: Option<String>,
    /// Name of the MCP server whose prompts to enumerate
    pub server_name: String,
}
crate::record_serde::wire_record!(McpPromptsListRequest { cursor: "cursor" if is_none, server_name: "serverName" });

/// One page of prompts advertised by the named MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpPromptsListResult {
    /// MCP result metadata
    pub meta: Option<HashMap<String, serde_json::Value>>,
    /// Server-provided non-standard result fields
    pub additional_properties: Option<HashMap<String, serde_json::Value>>,
    /// Opaque cursor for the next page, if the server has more prompts
    pub next_cursor: Option<String>,
    /// Prompts advertised by the server
    pub prompts: Vec<McpPrompt>,
}
crate::record_serde::wire_record!(McpPromptsListResult { meta: "_meta" if is_none, additional_properties: "additionalProperties" if is_none, next_cursor: "nextCursor" if is_none, prompts: "prompts" });

/// Registration parameters for an external MCP client.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpRegisterExternalClientRequest {
    /// In-process MCP Client instance. Marked internal: cannot be serialized across the JSON-RPC boundary.
    #[doc(hidden)]
    pub(crate) client: serde_json::Value,
    /// In-process server config (MCPServerConfig) paired with the in-process client/transport. Marked internal alongside its companions.
    #[doc(hidden)]
    pub(crate) config: serde_json::Value,
    /// Logical server name for the external client
    pub server_name: String,
    /// In-process MCP Transport instance. Marked internal: cannot be serialized across the JSON-RPC boundary.
    #[doc(hidden)]
    pub(crate) transport: serde_json::Value,
}
crate::record_serde::wire_record!(McpRegisterExternalClientRequest {
    client: "client",
    config: "config",
    server_name: "serverName",
    transport: "transport"
});

/// Registry search to abandon.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpRegistryCancelRequest {
    /// Request ID from `mcp.registry.allocateRequestId` that the search uses.
    pub request_id: i64,
}
crate::record_serde::wire_record!(McpRegistryCancelRequest {
    request_id: "requestId"
});

/// Whether the cancel reached a live search.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpRegistryCancelResult {
    /// True when the cancel stopped a running search. False for unknown or reclaimed IDs, completed or canceled searches, and unused reservations. The cancel releases an unused reservation.
    pub canceled: bool,
}
crate::record_serde::wire_record!(McpRegistryCancelResult {
    canceled: "canceled"
});

/// Request id naming a cancellable registry search.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpRegistryRequestIdResult {
    /// Request ID for `mcp.registry.search` and `mcp.registry.cancel`. It serves one search. Allocation can reclaim unused IDs at the 1,024-ID reservation limit.
    pub request_id: i64,
}
crate::record_serde::wire_record!(McpRegistryRequestIdResult {
    request_id: "requestId"
});

/// Registry search terms, the credential to search under, and the request id that makes the search cancellable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpRegistrySearchRequest {
    /// The credential the search runs under, carried opaquely. Hosts usually send credential-free `AuthIdentity`; a `token` identity can be resolved only when it embeds a token, while `env` and `gh-cli` identities can use a token embedded in the request first.
    pub auth_info: serde_json::Value,
    /// Maximum number of servers to return.
    pub limit: i64,
    /// Free-text query. Omitted or empty asks the registry for its top servers rather than searching. A value that is not a string is refused.
    pub query: Option<String>,
    /// Repository used for the policy lookup, as `owner/name`. The policy selects the registry URL and whether the user token goes to the registry. The registry receives this repository only when the policy entry lists it as required context. A value that is not a string is refused.
    pub repository: Option<String>,
    /// Request ID from `mcp.registry.allocateRequestId`. The search refuses unknown, reclaimed, or canceled IDs and IDs that another search or request already uses.
    pub request_id: i64,
}
crate::record_serde::wire_record!(McpRegistrySearchRequest { auth_info: "authInfo", limit: "limit", query: "query" if is_none, repository: "repository" if is_none, request_id: "requestId" });

/// Servers selected from the registry response.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpRegistrySearchResult {
    /// The server objects, carried opaquely. The runtime follows pages, keeps the newest entry per server name, cuts the list to `limit`, and sorts an empty-query result by GitHub stars. Each server object remains unchanged because the registry owns that shape.
    pub servers: serde_json::Value,
}
crate::record_serde::wire_record!(McpRegistrySearchResult { servers: "servers" });

/// An MCP server whose saved disable replaces its session-only override.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpReleaseSessionDisableRequest {
    /// Name of the MCP server whose disable the host saved.
    pub server_name: String,
}
crate::record_serde::wire_record!(McpReleaseSessionDisableRequest {
    server_name: "serverName"
});

/// In-process MCP reload configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpReloadConfig {
    pub active_git_hub_token: Option<String>,
    /// Server names the CLI enabled for this session via `--enable-mcp-server`.
    pub cli_enabled_servers: Option<Vec<String>>,
    pub config_filter: Option<serde_json::Value>,
    /// Caller-resolved settings disables, below CLI enables and explicit session disables.
    pub configured_disabled_servers: Option<Vec<String>>,
    pub disabled_servers: Option<Vec<String>>,
    pub enabled_servers: Option<Vec<String>>,
    pub force_restart: Option<bool>,
    pub github_mcp_tool_options: Option<serde_json::Value>,
    pub github_mcp_user_override: Option<bool>,
    pub include_workspace_sources: Option<bool>,
    pub mcp3p_enabled: Option<bool>,
    pub mcp_servers: HashMap<String, serde_json::Value>,
    /// Reload reason for the runtime-owned registry policy check.
    pub policy_check_trigger: Option<String>,
    pub secret_store: Option<serde_json::Value>,
    pub use_cached_tool_snapshots: Option<bool>,
}
crate::record_serde::wire_record!(McpReloadConfig { active_git_hub_token: "activeGitHubToken" if is_none, cli_enabled_servers: "cliEnabledServers" if is_none, config_filter: "configFilter" if is_none, configured_disabled_servers: "configuredDisabledServers" if is_none, disabled_servers: "disabledServers" if is_none, enabled_servers: "enabledServers" if is_none, force_restart: "forceRestart" if is_none, github_mcp_tool_options: "githubMcpToolOptions" if is_none, github_mcp_user_override: "githubMcpUserOverride" if is_none, include_workspace_sources: "includeWorkspaceSources" if is_none, mcp3p_enabled: "mcp3pEnabled" if is_none, mcp_servers: "mcpServers", policy_check_trigger: "policyCheckTrigger" if is_none, secret_store: "secretStore" if is_none, use_cached_tool_snapshots: "useCachedToolSnapshots" if is_none });

/// Opaque MCP reload configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpReloadWithConfigRequest {
    /// Opaque runtime MCP reload configuration. Marked internal: an in-process runtime shape (reloadMcpServers throws over the wire).
    #[doc(hidden)]
    pub(crate) config: serde_json::Value,
}
crate::record_serde::wire_record!(McpReloadWithConfigRequest { config: "config" });

/// Indicates whether the auto-managed `github` MCP server was removed (false when nothing to remove).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpRemoveGitHubResult {
    /// True when the auto-managed `github` MCP server was removed; false when no removal happened (e.g. user has explicitly configured a `github` server, or the server was not registered).
    pub removed: bool,
}
crate::record_serde::wire_record!(McpRemoveGitHubResult { removed: "removed" });

/// Resolve GitHub MCP exposure and optionally build a secretless server descriptor.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpResolveGitHubConfigRequest {
    /// Existing host auth context for an explicit session credential.
    pub auth_context_id: Option<String>,
    /// Pinned identity metadata for a host-owned registration.
    pub auth_info: Option<serde_json::Value>,
    /// Opaque selector for an existing host credential owner, admitted on this connection.
    pub auth_owner_id: Option<String>,
    /// Build a descriptor using the caller's current account.
    pub build_config: Option<bool>,
    /// The runtime's shell and gh availability decision.
    pub exclude_gh_replaceable_tools: Option<bool>,
    /// Enable the FIDES capability.
    pub fides_ifc_enabled: Option<bool>,
    /// Invocation options, with precedence over settings.
    pub flags: Option<GitHubMcpOptions>,
    /// Explicit descriptor options, when the host already resolved exposure.
    pub options: Option<GitHubMcpOptions>,
    /// Session override to validate and merge after invocation options.
    pub session: Option<serde_json::Value>,
    /// Persisted user settings in their original shape.
    pub settings: Option<serde_json::Value>,
    /// Workspace state for automatic ADO restriction.
    pub workspace: Option<McpGitHubWorkspace>,
}
crate::record_serde::wire_record!(McpResolveGitHubConfigRequest { auth_context_id: "authContextId" if is_none, auth_info: "authInfo" if is_none, auth_owner_id: "authOwnerId" if is_none, build_config: "buildConfig" if is_none, exclude_gh_replaceable_tools: "excludeGhReplaceableTools" if is_none, fides_ifc_enabled: "fidesIfcEnabled" if is_none, flags: "flags" if is_none, options: "options" if is_none, session: "session" if is_none, settings: "settings" if is_none, workspace: "workspace" if is_none });

/// Effective GitHub MCP configuration and override attribution.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpResolveGitHubConfigResult {
    /// Secretless descriptor. Omitted when not requested or not authenticated.
    pub config_json: Option<String>,
    /// Effective options after source precedence and capability policy.
    #[doc(hidden)]
    pub(crate) options: GitHubMcpOptions,
    /// True when the workspace requires web_search-only exposure.
    pub restrict_to_ado_web_search: bool,
    /// Validated session options, if present.
    pub session: Option<serde_json::Value>,
    /// Override attribution: none, flag, settings, or both.
    pub source: String,
    /// True when a source explicitly widens exposure.
    pub user_overrode: bool,
}
crate::record_serde::wire_record!(McpResolveGitHubConfigResult { config_json: "configJson" if is_none, options: "options", restrict_to_ado_web_search: "restrictToAdoWebSearch", session: "session" if is_none, source: "source", user_overrode: "userOverrode" });

/// MCP server whose resources to enumerate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResourcesListRequest {
    /// Opaque MCP pagination cursor from a prior `nextCursor` value
    pub cursor: Option<String>,
    /// Name of the MCP server whose resources to enumerate
    pub server_name: String,
}
crate::record_serde::wire_record!(McpResourcesListRequest { cursor: "cursor" if is_none, server_name: "serverName" });

/// One page of resources advertised by the named MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResourcesListResult {
    /// Opaque cursor for the next page, if the server has more resources
    pub next_cursor: Option<String>,
    /// Resources advertised by the server (proxied MCP `resources/list`)
    pub resources: Vec<McpResource>,
}
crate::record_serde::wire_record!(McpResourcesListResult { next_cursor: "nextCursor" if is_none, resources: "resources" });

/// MCP server whose resource templates to enumerate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResourcesListTemplatesRequest {
    /// Opaque MCP pagination cursor from a prior `nextCursor` value
    pub cursor: Option<String>,
    /// Name of the MCP server whose resource templates to enumerate
    pub server_name: String,
}
crate::record_serde::wire_record!(McpResourcesListTemplatesRequest { cursor: "cursor" if is_none, server_name: "serverName" });

/// One page of resource templates advertised by the named MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResourcesListTemplatesResult {
    /// Opaque cursor for the next page, if the server has more resource templates
    pub next_cursor: Option<String>,
    /// Resource templates advertised by the server (proxied MCP `resources/templates/list`)
    pub resource_templates: Vec<McpResourceTemplate>,
}
crate::record_serde::wire_record!(McpResourcesListTemplatesResult { next_cursor: "nextCursor" if is_none, resource_templates: "resourceTemplates" });

/// MCP server and resource URI to fetch.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResourcesReadRequest {
    /// Name of the MCP server hosting the resource
    pub server_name: String,
    /// Resource URI
    pub uri: String,
}
crate::record_serde::wire_record!(McpResourcesReadRequest {
    server_name: "serverName",
    uri: "uri"
});

/// Resource contents returned by the MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpResourcesReadResult {
    /// Resource contents returned by the server
    pub contents: Vec<McpResourceContent>,
}
crate::record_serde::wire_record!(McpResourcesReadResult {
    contents: "contents"
});

/// Server name and optional replacement configuration for an individual MCP server restart. Omit `config` for a config-free restart-by-name of an already-configured server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpRestartServerRequest {
    /// Replacement MCP server configuration (stdio process or remote HTTP/SSE). Omit to restart the server with its already-registered configuration (config-free restart-by-name).
    pub config: Option<serde_json::Value>,
    /// Exact receipt identity for an explicit owned restart; configuration overrides are refused.
    pub expected_installation_id: Option<String>,
    /// Name of the MCP server to restart
    pub server_name: String,
}
crate::record_serde::wire_record!(McpRestartServerRequest { config: "config" if is_none, expected_installation_id: "expectedInstallationId" if is_none, server_name: "serverName" });

/// Per-field MCP telemetry-obfuscation policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSafeForTelemetryFields {
    /// Whether MCP tool input names may be included in telemetry without obfuscation.
    pub inputs_names: bool,
    /// Whether the MCP tool name may be included in telemetry without obfuscation.
    pub name: bool,
}
crate::record_serde::wire_record!(McpSafeForTelemetryFields {
    inputs_names: "inputsNames",
    name: "name"
});

/// Outcome of an MCP sampling execution: success result, failure error, or cancellation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSamplingExecutionResult {
    /// Outcome of the sampling inference. 'success' produced a response; 'failure' encountered an error (including agent-side rejection by content filter or criteria); 'cancelled' the caller cancelled this execution via cancelSamplingExecution.
    pub action: McpSamplingExecutionAction,
    /// Error description, present when action='failure'.
    pub error: Option<String>,
    /// MCP CreateMessageResult payload (with optional 'tools' extension), present when action='success'. Treated as opaque at the schema layer; consumers should construct/consume it per the MCP CreateMessageResult shape.
    pub result: Option<McpExecuteSamplingResult>,
}
crate::record_serde::wire_record!(McpSamplingExecutionResult { action: "action", error: "error" if is_none, result: "result" if is_none });

/// An MCP configuration credential to remove from the host's secret store.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSecretDeleteRequest {
    /// Expected mcp-secrets directory. Omit to use the runtime configuration root.
    pub config_dir: Option<String>,
    /// Secret reference to remove.
    pub secret_id: String,
}
crate::record_serde::wire_record!(McpSecretDeleteRequest { config_dir: "configDir" if is_none, secret_id: "secretId" });

/// A legacy MCP server whose configuration credentials must be removed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSecretDeleteServerRequest {
    /// Expected mcp-secrets directory. Omit to use the runtime configuration root.
    pub config_dir: Option<String>,
    /// Server name used by the legacy credential index.
    pub server_name: String,
}
crate::record_serde::wire_record!(McpSecretDeleteServerRequest { config_dir: "configDir" if is_none, server_name: "serverName" });

/// A legacy MCP configuration credential to persist in the host's secret store.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpSecretSaveRequest {
    /// Expected mcp-secrets directory. Omit to use the runtime configuration root.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config_dir: Option<String>,
    /// Secret reference used by an MCP server configuration.
    pub secret_id: String,
    /// Credential value to persist.
    pub value: String,
}

impl std::fmt::Debug for McpSecretSaveRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("McpSecretSaveRequest")
            .field("config_dir", &self.config_dir)
            .field("secret_id", &self.secret_id)
            .field("value", &"<redacted>")
            .finish()
    }
}

/// Whether the OS keychain accepted the saved MCP credential.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSecretSaveResult {
    /// True for keychain storage; false for the existing file backend.
    pub used_keychain: bool,
}
crate::record_serde::wire_record!(McpSecretSaveResult {
    used_keychain: "usedKeychain"
});

/// Authentication settings with optional redirect port configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpServerAuthConfigRedirectPort {
    /// Fixed port for the OAuth redirect callback server.
    pub redirect_port: Option<i32>,
}
crate::record_serde::wire_record!(McpServerAuthConfigRedirectPort { redirect_port: "redirectPort" if is_none });

/// Remote MCP server configuration accessed over HTTP or SSE.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpServerConfigHttp {
    /// Set to `true` to use defaults, or provide an object with additional auth or OIDC settings.
    pub auth: Option<serde_json::Value>,
    /// Configuration warnings recorded while loading the server.
    pub config_warnings: Option<Vec<String>>,
    /// Controls if tools provided by this server can be loaded on demand via tool search (auto) or always included in the initial tool list (never)
    pub defer_tools: Option<McpServerConfigDeferTools>,
    /// Whether secret masking is disabled for calls to this server.
    pub disable_secret_masking: Option<bool>,
    /// Set to true to disable persisted MCP tool snapshots for this server. Live tool discovery is unaffected.
    pub disable_tool_cache: Option<bool>,
    /// Optional human-readable server name.
    pub display_name: Option<String>,
    /// Event types this server receives as Copilot notifications.
    pub events: Option<Vec<String>>,
    /// Tool names excluded after the include filter is applied.
    pub exclude_tools: Option<Vec<String>>,
    /// Content filtering mode to apply to all tools, or a map of tool name to content filtering mode.
    pub filter_mapping: Option<serde_json::Value>,
    /// HTTP headers to include in requests to the remote MCP server.
    pub headers: Option<HashMap<String, String>>,
    /// Dynamic-header refresh cache lifetime in milliseconds.
    pub headers_refresh_ttl_ms: Option<i64>,
    /// Whether this server is a built-in fallback used when the user has not configured their own server.
    pub is_default_server: Option<bool>,
    /// Copilot notification types this server may send to the host.
    pub notifications: Option<Vec<String>>,
    /// OAuth client ID for a pre-registered remote MCP OAuth client.
    pub oauth_client_id: Option<String>,
    /// OAuth grant type to use when authenticating to the remote MCP server.
    pub oauth_grant_type: Option<McpServerConfigHttpOauthGrantType>,
    /// Whether the configured OAuth client is public and does not require a client secret.
    pub oauth_public_client: Option<bool>,
    /// Non-empty array of valid RFC 6749 scope-token strings to request for the statically configured OAuth client when the server challenge omits scope or provides an empty scope. Requires a non-empty oauthClientId. These scopes take precedence over protected-resource metadata.
    pub oauth_scopes: Option<Vec<String>>,
    /// Set to `true` to use defaults, or provide an object with additional auth or OIDC settings.
    pub oidc: Option<serde_json::Value>,
    /// Telemetry-obfuscation policy for this server's tools.
    pub safe_for_telemetry: Option<serde_json::Value>,
    /// Milliseconds this server may spend connecting before the CLI warns that it is taking longer than expected. Presentation only: it does not change how long the connection is allowed to take.
    pub slow_connection_threshold_ms: Option<i64>,
    /// The origin of this server configuration.
    pub source: Option<McpServerSource>,
    /// Source file path recorded while loading the config.
    pub source_path: Option<String>,
    /// Plugin that provided this server.
    pub source_plugin: Option<String>,
    /// Whether the providing plugin uses the Open Plugin Spec.
    pub source_plugin_spec: Option<bool>,
    /// Version of the plugin that provided this server.
    pub source_plugin_version: Option<String>,
    /// Timeout in milliseconds for tool discovery and tool calls.
    pub timeout: Option<i64>,
    /// Tools to include. Defaults to all tools if not specified.
    pub tools: Option<Vec<String>>,
    /// Remote transport type. Defaults to "http" when omitted.
    pub r#type: Option<McpServerConfigHttpType>,
    /// URL of the remote MCP server endpoint.
    pub url: String,
}
crate::record_serde::wire_record!(McpServerConfigHttp { auth: "auth" if is_none, config_warnings: "configWarnings" if is_none, defer_tools: "deferTools" if is_none, disable_secret_masking: "disableSecretMasking" if is_none, disable_tool_cache: "disableToolCache" if is_none, display_name: "displayName" if is_none, events: "events" if is_none, exclude_tools: "excludeTools" if is_none, filter_mapping: "filterMapping" if is_none, headers: "headers" if is_none, headers_refresh_ttl_ms: "headersRefreshTtlMs" if is_none, is_default_server: "isDefaultServer" if is_none, notifications: "notifications" if is_none, oauth_client_id: "oauthClientId" if is_none, oauth_grant_type: "oauthGrantType" if is_none, oauth_public_client: "oauthPublicClient" if is_none, oauth_scopes: "oauthScopes" if is_none, oidc: "oidc" if is_none, safe_for_telemetry: "safeForTelemetry" if is_none, slow_connection_threshold_ms: "slowConnectionThresholdMs" if is_none, source: "source" if is_none, source_path: "sourcePath" if is_none, source_plugin: "sourcePlugin" if is_none, source_plugin_spec: "sourcePluginSpec" if is_none, source_plugin_version: "sourcePluginVersion" if is_none, timeout: "timeout" if is_none, tools: "tools" if is_none, r#type: "type" if is_none, url: "url" });

/// A session-only local IDE endpoint. Starting it never writes persistent configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpServerConfigLocal {
    /// Endpoint headers from IDE discovery.
    pub headers: Option<HashMap<String, String>>,
    /// IDE identity and initialize metadata. The server name must be `ide`.
    pub ide: McpIdeConfig,
    /// Existing Unix socket or Windows named pipe on the runtime host.
    pub socket_path: String,
    /// Local transport discriminator.
    pub r#type: McpServerConfigLocalType,
}
crate::record_serde::wire_record!(McpServerConfigLocal { headers: "headers" if is_none, ide: "ide", socket_path: "socketPath", r#type: "type" });

/// In-process MCP server configuration used by embedded SDK clients.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpServerConfigMemory {
    /// Configuration warnings recorded while loading the server.
    pub config_warnings: Option<Vec<String>>,
    /// Controls whether tools can be loaded on demand.
    pub defer_tools: Option<McpServerConfigDeferTools>,
    /// Whether secret masking is disabled for calls to this server.
    pub disable_secret_masking: Option<bool>,
    /// Whether persisted tool snapshots are disabled.
    pub disable_tool_cache: Option<bool>,
    /// Optional human-readable server name.
    pub display_name: Option<String>,
    /// Event types this server receives as Copilot notifications.
    pub events: Option<Vec<String>>,
    /// Tool names excluded after the include filter is applied.
    pub exclude_tools: Option<Vec<String>>,
    /// Content filtering mode to apply to this server's tools.
    pub filter_mapping: Option<serde_json::Value>,
    /// Whether this server is a built-in fallback.
    pub is_default_server: Option<bool>,
    /// Copilot notification types this server may send to the host.
    pub notifications: Option<Vec<String>>,
    /// Set to `true` to use default OIDC settings.
    pub oidc: Option<serde_json::Value>,
    /// Telemetry-obfuscation policy for this server's tools.
    pub safe_for_telemetry: Option<serde_json::Value>,
    /// In-process MCP server instance. This value cannot cross a JSON-RPC boundary.
    #[doc(hidden)]
    pub(crate) server_instance: serde_json::Value,
    /// Milliseconds this server may spend connecting before the CLI warns that it is taking longer than expected. Presentation only: it does not change how long the connection is allowed to take.
    pub slow_connection_threshold_ms: Option<i64>,
    /// The origin of this server configuration.
    pub source: Option<McpServerSource>,
    /// Source file path recorded while loading the config.
    pub source_path: Option<String>,
    /// Plugin that provided this server.
    pub source_plugin: Option<String>,
    /// Whether the providing plugin uses the Open Plugin Spec.
    pub source_plugin_spec: Option<bool>,
    /// Version of the plugin that provided this server.
    pub source_plugin_version: Option<String>,
    /// Timeout in milliseconds for tool discovery and tool calls.
    pub timeout: Option<i64>,
    /// Tools to include. Defaults to all tools if not specified.
    pub tools: Option<Vec<String>>,
    #[doc(hidden)]
    pub(crate) r#type: McpServerConfigMemoryType,
}
crate::record_serde::wire_record!(McpServerConfigMemory { config_warnings: "configWarnings" if is_none, defer_tools: "deferTools" if is_none, disable_secret_masking: "disableSecretMasking" if is_none, disable_tool_cache: "disableToolCache" if is_none, display_name: "displayName" if is_none, events: "events" if is_none, exclude_tools: "excludeTools" if is_none, filter_mapping: "filterMapping" if is_none, is_default_server: "isDefaultServer" if is_none, notifications: "notifications" if is_none, oidc: "oidc" if is_none, safe_for_telemetry: "safeForTelemetry" if is_none, server_instance: "serverInstance", slow_connection_threshold_ms: "slowConnectionThresholdMs" if is_none, source: "source" if is_none, source_path: "sourcePath" if is_none, source_plugin: "sourcePlugin" if is_none, source_plugin_spec: "sourcePluginSpec" if is_none, source_plugin_version: "sourcePluginVersion" if is_none, timeout: "timeout" if is_none, tools: "tools" if is_none, r#type: "type" });

/// Stdio MCP server configuration launched as a child process.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpServerConfigStdio {
    /// Command-line arguments passed to the Stdio MCP server process.
    pub args: Option<Vec<String>>,
    /// Set to `true` to use defaults, or provide an object with additional auth or OIDC settings.
    pub auth: Option<serde_json::Value>,
    /// Executable command used to start the Stdio MCP server process.
    pub command: String,
    /// Configuration warnings recorded while loading the server.
    pub config_warnings: Option<Vec<String>>,
    /// Working directory for the Stdio MCP server process.
    pub cwd: Option<String>,
    /// Controls if tools provided by this server can be loaded on demand via tool search (auto) or always included in the initial tool list (never)
    pub defer_tools: Option<McpServerConfigDeferTools>,
    /// Whether secret masking is disabled for calls to this server.
    pub disable_secret_masking: Option<bool>,
    /// Set to true to disable persisted MCP tool snapshots for this server. Live tool discovery is unaffected.
    pub disable_tool_cache: Option<bool>,
    /// Optional human-readable server name.
    pub display_name: Option<String>,
    /// Environment variables to pass to the Stdio MCP server process.
    pub env: Option<HashMap<String, String>>,
    /// Event types this server receives as Copilot notifications.
    pub events: Option<Vec<String>>,
    /// Tool names excluded after the include filter is applied.
    pub exclude_tools: Option<Vec<String>>,
    /// Content filtering mode to apply to all tools, or a map of tool name to content filtering mode.
    pub filter_mapping: Option<serde_json::Value>,
    /// Whether this server is a built-in fallback used when the user has not configured their own server.
    pub is_default_server: Option<bool>,
    /// Copilot notification types this server may send to the host.
    pub notifications: Option<Vec<String>>,
    /// Set to `true` to use defaults, or provide an object with additional auth or OIDC settings.
    pub oidc: Option<serde_json::Value>,
    /// Telemetry-obfuscation policy for this server's tools.
    pub safe_for_telemetry: Option<serde_json::Value>,
    /// Milliseconds this server may spend connecting before the CLI warns that it is taking longer than expected. Presentation only: it does not change how long the connection is allowed to take.
    pub slow_connection_threshold_ms: Option<i64>,
    /// The origin of this server configuration.
    pub source: Option<McpServerSource>,
    /// Source file path recorded while loading the config.
    pub source_path: Option<String>,
    /// Plugin that provided this server.
    pub source_plugin: Option<String>,
    /// Whether the providing plugin uses the Open Plugin Spec.
    pub source_plugin_spec: Option<bool>,
    /// Version of the plugin that provided this server.
    pub source_plugin_version: Option<String>,
    /// Timeout in milliseconds for tool discovery and tool calls.
    pub timeout: Option<i64>,
    /// Tools to include. Defaults to all tools if not specified.
    pub tools: Option<Vec<String>>,
    /// Local transport type. Defaults to stdio when omitted.
    pub r#type: Option<McpServerConfigStdioType>,
}
crate::record_serde::wire_record!(McpServerConfigStdio { args: "args" if is_none, auth: "auth" if is_none, command: "command", config_warnings: "configWarnings" if is_none, cwd: "cwd" if is_none, defer_tools: "deferTools" if is_none, disable_secret_masking: "disableSecretMasking" if is_none, disable_tool_cache: "disableToolCache" if is_none, display_name: "displayName" if is_none, env: "env" if is_none, events: "events" if is_none, exclude_tools: "excludeTools" if is_none, filter_mapping: "filterMapping" if is_none, is_default_server: "isDefaultServer" if is_none, notifications: "notifications" if is_none, oidc: "oidc" if is_none, safe_for_telemetry: "safeForTelemetry" if is_none, slow_connection_threshold_ms: "slowConnectionThresholdMs" if is_none, source: "source" if is_none, source_path: "sourcePath" if is_none, source_plugin: "sourcePlugin" if is_none, source_plugin_spec: "sourcePluginSpec" if is_none, source_plugin_version: "sourcePluginVersion" if is_none, timeout: "timeout" if is_none, tools: "tools" if is_none, r#type: "type" if is_none });

/// MCP servers configured for the session, with their connection status and host-level state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpServerList {
    /// Host-level state, omitted when no MCP host is initialized.
    pub host: Option<McpHostState>,
    /// Configured MCP servers
    pub servers: Vec<McpServer>,
}
crate::record_serde::wire_record!(McpServerList { host: "host" if is_none, servers: "servers" });

/// Mode controlling how MCP server env values are resolved (`direct` or `indirect`).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSetEnvValueModeParams {
    /// How environment-variable values supplied to MCP servers are resolved. "direct" passes literal string values; "indirect" treats values as references (e.g. names of environment variables on the host) that the runtime resolves before launch. Defaults to the runtime's startup mode; clients that intentionally launch MCP servers with literal values (e.g. CLI prompt mode and ACP) set this to "direct".
    pub mode: McpSetEnvValueModeDetails,
}
crate::record_serde::wire_record!(McpSetEnvValueModeParams { mode: "mode" });

/// Env-value mode recorded on the session after the update.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpSetEnvValueModeResult {
    /// Mode recorded on the session after the update
    pub mode: McpSetEnvValueModeDetails,
}
crate::record_serde::wire_record!(McpSetEnvValueModeResult { mode: "mode" });

/// Whether the gh-replaceable GitHub MCP tools may be clipped for the session described by the request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpShouldExcludeGitHubToolsResult {
    /// True only when both halves hold: the session's filters still reach the platform shell tool, and the host actually has the `gh` those tools would be replaced by. Feed it straight back as the `excludeGhReplaceableTools` build option.
    pub exclude_gh_replaceable_tools: bool,
}
crate::record_serde::wire_record!(McpShouldExcludeGitHubToolsResult {
    exclude_gh_replaceable_tools: "excludeGhReplaceableTools"
});

/// Server name and optional configuration for an individual MCP server start. Omit `config` for a config-free start-by-name of an already-configured server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpStartServerRequest {
    /// MCP server configuration (stdio, remote HTTP/SSE, or session-only local IDE endpoint). Local IDE endpoints never write persistent configuration. Omit to start with the already-registered configuration.
    pub config: Option<serde_json::Value>,
    /// Exact receipt identity for explicit owned activation in this session.
    pub expected_installation_id: Option<String>,
    /// Name of the MCP server to start
    pub server_name: String,
}
crate::record_serde::wire_record!(McpStartServerRequest { config: "config" if is_none, expected_installation_id: "expectedInstallationId" if is_none, server_name: "serverName" });

/// MCP server startup filtering result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpStartServersResult {
    /// Non-default servers allowed by policy
    pub allowed_servers: Option<Vec<McpAllowedServer>>,
    /// Servers whose connection attempt failed.
    pub failed_servers: Option<Vec<McpFailedServer>>,
    /// Servers filtered out before startup
    pub filtered_servers: Vec<McpFilteredServer>,
}
crate::record_serde::wire_record!(McpStartServersResult { allowed_servers: "allowedServers" if is_none, failed_servers: "failedServers" if is_none, filtered_servers: "filteredServers" });

/// Plugin identity for a scoped MCP server stop.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpStopPluginServersRequest {
    /// Name of the plugin whose owned MCP servers must stop.
    pub plugin_name: String,
}
crate::record_serde::wire_record!(McpStopPluginServersRequest {
    plugin_name: "pluginName"
});

/// Acknowledges completion of the scoped plugin MCP server stop.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct McpStopPluginServersResult {}

/// Server name and optional transport selector for an individual MCP server stop. Omit transport to stop the server by name.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpStopServerRequest {
    /// Exact owned receipt identity. Stop also forgets this session's durable activation.
    pub expected_installation_id: Option<String>,
    /// Name of the MCP server to stop
    pub server_name: String,
    /// Optional transport selector. The only supported value, local-http, requires serverName ide and cannot accompany expectedInstallationId. It cancels pending IDE initialization and removes local IDE state without stopping an ordinary server with the same name. Omit this field for a generic stop by name.
    pub transport: Option<McpServerConfigLocalType>,
}
crate::record_serde::wire_record!(McpStopServerRequest { expected_installation_id: "expectedInstallationId" if is_none, server_name: "serverName", transport: "transport" if is_none });

/// Per-server MCP tool definition token costs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpToolTokenBreakdownResult {
    /// One entry per MCP server with tools in the session's current tool set, in tool order. Empty both when the session has not computed its tool set yet and when the computed tool set contains no MCP tools; the two cases are not distinguished.
    pub servers: Vec<McpServerToolTokenBreakdown>,
}
crate::record_serde::wire_record!(McpToolTokenBreakdownResult { servers: "servers" });

/// Server name identifying the external client to remove.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpUnregisterExternalClientRequest {
    /// Server name of the external client to unregister
    pub server_name: String,
}
crate::record_serde::wire_record!(McpUnregisterExternalClientRequest {
    server_name: "serverName"
});

/// Applies exactly one retained verified Agent Plugin installation plan.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginApplyInstallRequest {
    /// Required authenticated bound catalogue and agent-plugin-confirmed-installation capabilities.
    pub contract: CatalogClientContract,
    /// Opaque original plan, consumed once.
    pub plan_handle: String,
    /// Same existing selected session as planning.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(PluginApplyInstallRequest {
    contract: "contract",
    plan_handle: "planHandle",
    policy_session_id: "policySessionId"
});

/// Locates bundled plugin manifests relative to one installed host directory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginBuiltinDiscoverRequest {
    /// Host module directory used to locate the installed distribution.
    pub base_directory: String,
}
crate::record_serde::wire_record!(PluginBuiltinDiscoverRequest {
    base_directory: "baseDirectory"
});

/// Bundled plugin records and diagnostics from the original manifest scanner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginBuiltinDiscoverResult {
    /// Scanned bundled records in host precedence order.
    pub plugins: Vec<serde_json::Value>,
    /// Manifest and filesystem diagnostics, including a missing required bundle.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(PluginBuiltinDiscoverResult {
    plugins: "plugins",
    warnings: "warnings"
});

/// Identifies a plugin and its configured marketplace without accepting configuration or policy overrides.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginCheckUpdateRequest {
    /// Configured marketplace name.
    pub marketplace: String,
    /// Installed plugin name.
    pub name: String,
}
crate::record_serde::wire_record!(PluginCheckUpdateRequest {
    marketplace: "marketplace",
    name: "name"
});

/// Inspects persisted and live plugins under an explicit effective configuration without changing the existing user-only plugins.list API.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginInspectInventoryRequest {
    /// Trusted caller-composed read configuration, not persisted by inspection.
    pub config_override: serde_json::Value,
    /// Includes pending managed plugins by default. False excludes uninstalled rows before source merging.
    pub include_uninstalled: Option<bool>,
    /// Effective policy for read-only inspection; mutation guards retain their own authority.
    pub managed_settings: Option<serde_json::Value>,
    /// Additional plugin directories in host precedence order.
    pub plugin_directories: Option<Vec<String>>,
    /// Seeds pre-migration enablement from installed records below explicit overrides.
    pub seed_enabled_plugins_from_installed: bool,
    /// Working directory for repository overlay and live marketplace resolution.
    pub working_directory: String,
}
crate::record_serde::wire_record!(PluginInspectInventoryRequest { config_override: "configOverride", include_uninstalled: "includeUninstalled" if is_none, managed_settings: "managedSettings" if is_none, plugin_directories: "pluginDirectories" if is_none, seed_enabled_plugins_from_installed: "seedEnabledPluginsFromInstalled", working_directory: "workingDirectory" });

/// A verified Agent Plugin installation plan was prepared.
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationManagementOutcomeInstallPlanned {
    /// Agent Plugin installation management outcome discriminator.
    pub kind: PluginInstallationManagementOutcomeInstallPlannedKind,
    /// Prepared install plan.
    pub plan: PluginInstallPlan,
}
crate::record_serde::wire_record!(PluginInstallationManagementOutcomeInstallPlanned {
    kind: "kind",
    plan: "plan"
});

/// The request was refused without preparing a plan.
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationManagementOutcomeRefused {
    /// Agent Plugin installation management outcome discriminator.
    pub kind: PluginInstallationManagementOutcomeRefusedKind,
    /// Bounded refusal reason.
    pub reason: PluginInstallationFailureReason,
    /// Whole seconds GitHub asked the caller to wait, present only with source-rate-limited.
    pub retry_after_seconds: Option<i64>,
}
crate::record_serde::wire_record!(PluginInstallationManagementOutcomeRefused { kind: "kind", reason: "reason", retry_after_seconds: "retryAfterSeconds" if is_none });

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PluginInstallationManagementResultOutcome {
    /// Discriminator: handled management outcome.
    pub kind: PluginInstallationManagementResultOutcomeKind,
    /// Capabilities honoured for this request.
    pub negotiated: CatalogNegotiatedContract,
    /// Observed management outcome.
    pub outcome: PluginInstallationManagementOutcome,
}

/// A plugin registered from a verified materialisation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationSummary {
    /// Catalogue identity retained at install time.
    pub catalogue: InstallationCatalogueIdentity,
    /// Whether the plugin is enabled for new sessions. Always false after installation.
    pub enabled: bool,
    /// ISO 8601 wall-clock installation time.
    pub installed_at: String,
    /// Installed plugin name, as plugins.list, plugins.enable and plugins.uninstall identify it.
    pub name: String,
    /// Operation that installed the plugin.
    pub operation_id: String,
    /// Exact retained verified source identity.
    pub source: PluginInstallationSource,
    /// Install location without an absolute host path.
    pub target: PluginInstallationLocation,
}
crate::record_serde::wire_record!(PluginInstallationSummary {
    catalogue: "catalogue",
    enabled: "enabled",
    installed_at: "installedAt",
    name: "name",
    operation_id: "operationId",
    source: "source",
    target: "target"
});

/// The verified plugin was registered disabled.
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationOutcomeInstalled {
    /// Installed plugin summary.
    pub installation: PluginInstallationSummary,
    /// Terminal Agent Plugin installation outcome discriminator.
    pub kind: PluginInstallationOutcomeInstalledKind,
}
crate::record_serde::wire_record!(PluginInstallationOutcomeInstalled {
    installation: "installation",
    kind: "kind"
});

/// The user declined the confirmation request.
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationOutcomeDeclined {
    /// Terminal Agent Plugin installation outcome discriminator.
    pub kind: PluginInstallationOutcomeDeclinedKind,
    /// Original operation identity.
    pub operation_id: String,
}
crate::record_serde::wire_record!(PluginInstallationOutcomeDeclined {
    kind: "kind",
    operation_id: "operationId"
});

/// The confirmation was cancelled before any change.
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationOutcomeCancelled {
    /// Terminal Agent Plugin installation outcome discriminator.
    pub kind: PluginInstallationOutcomeCancelledKind,
    /// Original operation identity.
    pub operation_id: String,
}
crate::record_serde::wire_record!(PluginInstallationOutcomeCancelled {
    kind: "kind",
    operation_id: "operationId"
});

/// The operation was refused and nothing was installed.
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationOutcomeRefused {
    /// Terminal Agent Plugin installation outcome discriminator.
    pub kind: PluginInstallationOutcomeRefusedKind,
    /// Present once an operation has been allocated.
    pub operation_id: Option<String>,
    /// Bounded refusal reason.
    pub reason: PluginInstallationFailureReason,
    /// Whole seconds GitHub asked the caller to wait, present only with source-rate-limited.
    pub retry_after_seconds: Option<i64>,
}
crate::record_serde::wire_record!(PluginInstallationOutcomeRefused { kind: "kind", operation_id: "operationId" if is_none, reason: "reason", retry_after_seconds: "retryAfterSeconds" if is_none });

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PluginInstallationResultOutcome {
    /// Discriminator: handled operation outcome.
    pub kind: PluginInstallationResultOutcomeKind,
    /// Capabilities honoured for this request.
    pub negotiated: CatalogNegotiatedContract,
    /// Terminal operation outcome.
    pub outcome: PluginInstallationOutcome,
}

/// Safe verified Agent Plugin review fields. No raw credential, candidate handle or plan handle.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginInstallationReview {
    /// Exact reviewed Agent Plugin installation action.
    pub action: PluginInstallationReviewAction,
    /// Catalogue identity retained from the bound candidate, or verified data for a reference.
    pub catalogue: InstallationCatalogueIdentity,
    /// Manifest description when present.
    pub description: Option<String>,
    /// Reviewed files and digests.
    pub files: Vec<PluginInstallationFileReview>,
    /// Installing never grants immediate use; the plugin is registered disabled.
    pub installs_disabled: bool,
    /// Complete verified manifest content. Planning refuses with review-too-large
    /// when this exceeds 262144 UTF-8 bytes; it is never truncated.
    pub manifest_content: String,
    /// Relative path of the verified plugin manifest.
    pub manifest_path: String,
    /// MCP servers the verified files declare, exactly as an install of these files loads
    /// them. Omitted when the plugin declares none.
    pub mcp_servers: Option<Vec<PluginInstallationMcpServerReview>>,
    /// Installed plugin name, derived from the verified manifest exactly as a direct install derives it.
    pub name: String,
    /// Exact verified source identity.
    pub source: PluginInstallationSource,
    /// Exact install location without an absolute host path.
    pub target: PluginInstallationLocation,
    /// Total reviewed payload size in bytes.
    pub total_bytes: i64,
    /// Manifest version when present.
    pub version: Option<String>,
}
crate::record_serde::wire_record!(PluginInstallationReview { action: "action", catalogue: "catalogue", description: "description" if is_none, files: "files", installs_disabled: "installsDisabled", manifest_content: "manifestContent", manifest_path: "manifestPath", mcp_servers: "mcpServers" if is_none, name: "name", source: "source", target: "target", total_bytes: "totalBytes", version: "version" if is_none });

/// Result of installing a plugin.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginInstallResult {
    /// Set when the install path is deprecated (e.g. direct repo / URL / local installs). Callers should surface this to end users.
    pub deprecation_warning: Option<String>,
    /// The newly installed plugin's metadata
    pub plugin: InstalledPluginInfo,
    /// Optional post-install message provided by the plugin (e.g. setup instructions)
    pub post_install_message: Option<String>,
    /// Number of skills discovered and installed from the plugin
    pub skills_installed: i64,
    /// Where the completed plugin tree was staged before atomic promotion
    pub staging_mode: Option<PluginInstallStagingMode>,
    /// Warnings raised while reading the plugin's manifest: a field the manifest's schema does not define, which the loader ignores, or the reason the loader rejects the whole manifest. Callers should surface these to end users: a plugin whose skills sit behind an ignored field, or whose manifest is rejected, otherwise installs, lists, and contributes nothing, with no signal to its author. Absent when the manifest raised none.
    pub warnings: Option<Vec<String>>,
}
crate::record_serde::wire_record!(PluginInstallResult { deprecation_warning: "deprecationWarning" if is_none, plugin: "plugin", post_install_message: "postInstallMessage" if is_none, skills_installed: "skillsInstalled", staging_mode: "stagingMode" if is_none, warnings: "warnings" if is_none });

/// One plugin in a host invocation's complete merged inventory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginInvocationInventoryEntry {
    /// Whether an explicit host directory supplies this plugin.
    pub external: bool,
    /// Plugin metadata and effective activation.
    pub plugin: InstalledPluginInfo,
}
crate::record_serde::wire_record!(PluginInvocationInventoryEntry {
    external: "external",
    plugin: "plugin"
});

/// Persisted, live, built-in, and host-directory plugins with canonical shadowing and scan diagnostics.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginInvocationInventoryResult {
    /// Merged inventory in canonical precedence order.
    #[doc(hidden)]
    pub(crate) plugins: Vec<PluginInvocationInventoryEntry>,
    /// Directory scan warnings in source order.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(PluginInvocationInventoryResult {
    plugins: "plugins",
    warnings: "warnings"
});

/// Plugins installed for the session, with their enabled state and version metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginList {
    /// Installed plugins
    pub plugins: Vec<Plugin>,
}
crate::record_serde::wire_record!(PluginList { plugins: "plugins" });

/// Plugins installed in user/global state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginListResult {
    /// Installed plugins
    pub plugins: Vec<InstalledPluginInfo>,
    /// Existing discovery warnings for an invocation-context query. Omitted on legacy calls without workingDirectory or pluginDirectories. Display text can contain untrusted paths and must be sanitized before terminal output.
    pub warnings: Option<Vec<String>>,
}
crate::record_serde::wire_record!(PluginListResult { plugins: "plugins", warnings: "warnings" if is_none });

/// Identifies a marketplace to probe without refreshing its checkout.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginMarketplaceCheckUpdateRequest {
    /// Configured marketplace name.
    pub name: String,
}
crate::record_serde::wire_record!(PluginMarketplaceCheckUpdateRequest { name: "name" });

/// Searches all configured plugin marketplaces, not the Agent Finder catalog.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginMarketplaceSearchRequest {
    /// Refreshes the configured marketplace checkouts before the search. Defaults to false.
    pub force_refresh: Option<bool>,
    /// Case-insensitive substring matched against plugin names and descriptions.
    pub query: String,
}
crate::record_serde::wire_record!(PluginMarketplaceSearchRequest { force_refresh: "forceRefresh" if is_none, query: "query" });

/// Matching plugins across configured marketplaces, with partial failures.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginMarketplaceSearchResult {
    /// Catalog or installation-inventory failures that did not prevent other results.
    pub errors: Vec<PluginMarketplaceSearchError>,
    /// Matching catalog entries in configured marketplace order.
    pub plugins: Vec<PluginMarketplaceSearchEntry>,
}
crate::record_serde::wire_record!(PluginMarketplaceSearchResult {
    errors: "errors",
    plugins: "plugins"
});

/// Side-effect-free planning of one Agent Plugin search candidate through its Agent Finder materialisation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginPlanInstallRequest {
    /// Fresh single-use Agent Plugin candidate handle returned by a bound catalogue search.
    pub candidate_handle: String,
    /// Required authenticated bound catalogue and agent-plugin-confirmed-installation capabilities.
    pub contract: CatalogClientContract,
    /// Existing local session attached to this connection.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(PluginPlanInstallRequest {
    candidate_handle: "candidateHandle",
    contract: "contract",
    policy_session_id: "policySessionId"
});

/// Resolves plugin policy through the requesting connection's original credential owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginPolicyResolveInvocationRequest {
    /// Optional active account identity; credentials still resolve through the original owner.
    pub auth_info: Option<AuthIdentity>,
    /// Reads account and server policy from local caches without network access.
    pub cache_only: bool,
    /// Host client identity supplied to the policy helper.
    pub client_name: String,
    /// Host home directory for policy and cache locations.
    pub home_directory: String,
    /// Host platform in Node platform spelling.
    pub platform: String,
    /// Whether a configured policy helper may run.
    pub policy_helper_enabled: bool,
    /// Retains authoritative source layers for subsequent guarded mutations.
    pub retain_policy: Option<bool>,
    /// Stops on ingestion exceptions instead of reloading the device layer.
    pub strict: bool,
    /// Host user agent supplied to policy retrieval.
    pub user_agent: String,
    /// Working directory used by a directory-sensitive policy helper.
    pub working_directory: String,
}
crate::record_serde::wire_record!(PluginPolicyResolveInvocationRequest { auth_info: "authInfo" if is_none, cache_only: "cacheOnly", client_name: "clientName", home_directory: "homeDirectory", platform: "platform", policy_helper_enabled: "policyHelperEnabled", retain_policy: "retainPolicy" if is_none, strict: "strict", user_agent: "userAgent", working_directory: "workingDirectory" });

/// Resolved plugin policy and authoritative failure posture. Fresh successful resolutions retain the original policy layers for mutation guards; cache-only inspection never changes retained policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginPolicyResolveInvocationResult {
    /// Resolution diagnostics in source order.
    #[doc(hidden)]
    pub(crate) diagnostics: Vec<PluginPolicyDiagnostic>,
    /// Whether this host has a configured helper.
    pub policy_helper_configured: bool,
    /// Whether helper failure requires refusal.
    pub policy_helper_fail_closed: bool,
    /// Whether the helper failed.
    pub policy_helper_load_failed: bool,
    /// Server resolution error, when present.
    pub server_fetch_error: Option<String>,
    /// Whether the server policy could not be resolved.
    pub server_fetch_failed: bool,
    /// Whether required fresh confirmation failed and the caller must refuse.
    pub server_forced_refresh_failure: bool,
    /// Raw server policy for hosts that inspect the account layer alone.
    pub server_response: Option<serde_json::Value>,
    /// Whether no usable credential was available.
    pub server_token_unavailable: bool,
    /// Effective device, server, and helper settings when available.
    pub settings: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(PluginPolicyResolveInvocationResult { diagnostics: "diagnostics", policy_helper_configured: "policyHelperConfigured", policy_helper_fail_closed: "policyHelperFailClosed", policy_helper_load_failed: "policyHelperLoadFailed", server_fetch_error: "serverFetchError" if is_none, server_fetch_failed: "serverFetchFailed", server_forced_refresh_failure: "serverForcedRefreshFailure", server_response: "serverResponse" if is_none, server_token_unavailable: "serverTokenUnavailable", settings: "settings" if is_none });

/// Built-in plugin to activate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsActivateRequest {
    /// Name of a built-in plugin of this server, for example "browser-use".
    pub name: String,
}
crate::record_serde::wire_record!(PluginsActivateRequest { name: "name" });

/// The result of a built-in plugin activation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsActivateResult {
    /// The hook error. Present only when status is failed.
    pub message: Option<String>,
    /// Whether the plugin's sessionStart hooks ran, or why they did not run.
    pub status: PluginActivationStatus,
}
crate::record_serde::wire_record!(PluginsActivateResult { message: "message" if is_none, status: "status" });

/// Resolve managed Browser activation through the connection's original credential owner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginsBuiltinResolveActivationRequest {
    /// Existing launch populations for the policy-helper gate.
    #[doc(hidden)]
    pub(crate) audience: McpInvocationAudience,
    /// Embedding client identity for the policy helper.
    pub client_name: String,
    /// Effective user and trusted repository settings.
    pub user_settings: serde_json::Value,
    /// Working directory for the policy helper.
    pub working_directory: String,
}
crate::record_serde::wire_record!(PluginsBuiltinResolveActivationRequest {
    audience: "audience",
    client_name: "clientName",
    user_settings: "userSettings",
    working_directory: "workingDirectory"
});

/// Browser activation after the managed-over-user whole-plugin policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginsBuiltinResolveActivationResult {
    /// Whether the bundled Browser may contribute MCP servers.
    pub browser_enabled: bool,
    /// Whether policy resolution exceeded the existing two-second budget.
    pub timed_out: bool,
}
crate::record_serde::wire_record!(PluginsBuiltinResolveActivationResult {
    browser_enabled: "browserEnabled",
    timed_out: "timedOut"
});

/// Trusted built-in plugin directories to use for this runtime process.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsBuiltinSetRequest {
    /// Complete replacement set of trusted built-in plugin directories. Every entry must be an absolute local filesystem path no longer than 4096 characters.
    pub paths: Vec<String>,
}
crate::record_serde::wire_record!(PluginsBuiltinSetRequest { paths: "paths" });

/// Plugin names (or specs) to disable, plus the optional working directory the repository-controlled guard is evaluated against.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsDisableRequest {
    /// Plugin names, "plugin@marketplace" specs, or opaque directSourceId values from inventory to disable. Use directSourceId to distinguish same-named direct installations. Unknown targets and session-only plugin-directory mounts are ignored. Direct-install decisions persist on the installation record. Plugin-owned MCP servers are stopped in active sessions immediately; other plugin contributions remain available until each session reloads plugins.
    pub names: Vec<String>,
    /// Working directory whose repository `enabledPlugins` overlay decides whether this mutation is repository-controlled. Hosts that serve sessions across several repositories (the SDK server) should pass the session's directory; otherwise the guard is evaluated against the server process's own working directory, which may belong to a different repository. Defaults to the server's current working directory.
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(PluginsDisableRequest { names: "names", working_directory: "workingDirectory" if is_none });

/// Plugin names (or specs) to enable, plus the optional working directory the repository-controlled guard is evaluated against.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsEnableRequest {
    /// Plugin names, "plugin@marketplace" specs, or opaque directSourceId values from inventory to enable. Use directSourceId to distinguish same-named direct installations. Unknown targets and session-only plugin-directory mounts are ignored. Direct-install decisions persist on the installation record; active sessions must reload plugins to load newly enabled contributions.
    pub names: Vec<String>,
    /// Working directory whose repository `enabledPlugins` overlay decides whether this mutation is repository-controlled. Hosts that serve sessions across several repositories (the SDK server) should pass the session's directory; otherwise the guard is evaluated against the server process's own working directory, which may belong to a different repository. Defaults to the server's current working directory.
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(PluginsEnableRequest { names: "names", working_directory: "workingDirectory" if is_none });

/// Plugin source and optional working directory for relative-path resolution.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsInstallRequest {
    /// Plugin install spec. Accepts the same forms as the CLI: "plugin@marketplace" (marketplace install), "owner/repo" or "owner/repo:subpath" (GitHub direct), an http/https/ssh URL, or a local path. Direct (non-marketplace) installs are deprecated and will produce a deprecationWarning in the result.
    pub source: String,
    /// Working directory used to resolve relative local paths in `source`. Defaults to the server's current working directory.
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(PluginsInstallRequest { source: "source", working_directory: "workingDirectory" if is_none });

/// Optional workspace settings for a plugin catalog or bulk update through the original caller's runtime. Omission retains the server's configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsInvocationRequest {
    /// Effective user and repository settings for this invocation. Runtime-owned managed restrictions still apply.
    pub config_override: Option<serde_json::Value>,
    /// Managed settings supplied by the host for canonical plugin policy evaluation. This value does not select a credential owner.
    pub managed_settings: Option<serde_json::Value>,
    /// Working directory whose repository settings and plugin sources apply to this invocation.
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(PluginsInvocationRequest { config_override: "configOverride" if is_none, managed_settings: "managedSettings" if is_none, working_directory: "workingDirectory" if is_none });

/// Optional read-only plugin discovery context. The runtime resolves account, policy, and workspace trust. This query does not change settings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsListRequest {
    /// External plugin directories mounted for this query only. Relative paths resolve against workingDirectory. These directories do not become trusted built-ins.
    pub plugin_directories: Option<Vec<String>>,
    /// Working directory for repository overlays and live marketplaces. Defaults to the server working directory.
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(PluginsListRequest { plugin_directories: "pluginDirectories" if is_none, working_directory: "workingDirectory" if is_none });

/// Marketplace source and optional working directory for relative-path resolution.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsMarketplacesAddRequest {
    /// Marketplace source. Accepts the same forms as the CLI: "owner/repo" or "owner/repo#ref" (GitHub), an http/https/ssh URL (optionally with #ref), a git scp-style URL (user@host:path), or a local path. The marketplace's own name (from its manifest) is used as the registration key.
    pub source: String,
    /// Working directory used to resolve relative local paths in `source`. Defaults to the server's current working directory.
    pub working_directory: Option<String>,
}
crate::record_serde::wire_record!(PluginsMarketplacesAddRequest { source: "source", working_directory: "workingDirectory" if is_none });

/// Name of the marketplace whose plugin catalog to fetch.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsMarketplacesBrowseRequest {
    /// Marketplace name to browse
    pub name: String,
}
crate::record_serde::wire_record!(PluginsMarketplacesBrowseRequest { name: "name" });

/// Optional marketplace name; omit to refresh all.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsMarketplacesRefreshRequest {
    /// Marketplace name to refresh. When omitted, every registered marketplace is refreshed.
    pub name: Option<String>,
}
crate::record_serde::wire_record!(PluginsMarketplacesRefreshRequest { name: "name" if is_none });

/// Name of the marketplace to remove and an optional force flag.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsMarketplacesRemoveRequest {
    /// When true, also uninstall every plugin sourced from this marketplace. When false (default), removal is a no-op if any plugin from this marketplace is installed and the dependent plugin names are returned in the result.
    pub force: Option<bool>,
    /// Marketplace name to remove
    pub name: String,
}
crate::record_serde::wire_record!(PluginsMarketplacesRemoveRequest { force: "force" if is_none, name: "name" });

/// Resolves active plugin sources with runtime-owned folder trust and managed policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginSourcesResolveRequest {
    /// Configuration root for user settings and persisted installations.
    pub config_dir: String,
    /// Explicit plugin directories in host precedence order.
    pub plugin_directories: Option<Vec<String>>,
    /// Working directory for trusted repository overlays and live marketplaces.
    pub working_directory: String,
}
crate::record_serde::wire_record!(PluginSourcesResolveRequest { config_dir: "configDir", plugin_directories: "pluginDirectories" if is_none, working_directory: "workingDirectory" });

/// Active plugins and the separate persisted inventory, without persisting live directory records.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginSourcesResolveResult {
    /// Live directory records, including inactive records that still own their names.
    pub live_plugins: Option<Vec<serde_json::Value>>,
    /// Whether this pass confirmed the live tier instead of using persisted fallback.
    pub live_tier_known: Option<bool>,
    /// Persisted ambient records, including disabled or shadowed installations.
    pub persisted_plugins: Vec<serde_json::Value>,
    /// Active records, including trusted live marketplaces and explicit directories.
    pub plugins: Vec<serde_json::Value>,
    /// Scanner and live-marketplace warnings for this exact source pass.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(PluginSourcesResolveResult { live_plugins: "livePlugins" if is_none, live_tier_known: "liveTierKnown" if is_none, persisted_plugins: "persistedPlugins", plugins: "plugins", warnings: "warnings" });

/// Optional flags controlling which side effects the reload performs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsReloadRequest {
    /// When true, skip repo-level hooks during the hook reload. Use before folder trust is confirmed; load them post-trust via `sessions.loadDeferredRepoHooks`.
    pub defer_repo_hooks: Option<bool>,
    /// Re-run custom-agent discovery after refreshing plugins. Defaults to true.
    pub reload_custom_agents: Option<bool>,
    /// Re-discover and relaunch subprocess extensions (including plugin-shipped extensions) after refreshing plugins. Defaults to true. Has no effect when the session has no active extension controller (e.g. extensions were not requested for the session).
    pub reload_extensions: Option<bool>,
    /// Re-load user, plugin, and (subject to `deferRepoHooks`) repo hooks. Defaults to true. Has no effect when the host has not registered a hook reloader (e.g. remote sessions).
    pub reload_hooks: Option<bool>,
    /// Reload MCP server connections after refreshing plugins. Defaults to true.
    pub reload_mcp: Option<bool>,
}
crate::record_serde::wire_record!(PluginsReloadRequest { defer_repo_hooks: "deferRepoHooks" if is_none, reload_custom_agents: "reloadCustomAgents" if is_none, reload_extensions: "reloadExtensions" if is_none, reload_hooks: "reloadHooks" if is_none, reload_mcp: "reloadMcp" if is_none });

/// Canonical activation after workspace consent and plugin policy reconciliation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsResolveActivationResult {
    /// Current activation, or null before activation resolves.
    pub snapshot: Option<UIPluginActivationSnapshot>,
    /// Whether the runtime trusts the workspace.
    pub trusted: bool,
    /// Current working directory of the session.
    pub working_directory: String,
    /// Authoritative workspace root, when known.
    pub workspace_root: Option<String>,
}
crate::record_serde::wire_record!(PluginsResolveActivationResult { snapshot: "snapshot" if is_none, trusted: "trusted", working_directory: "workingDirectory", workspace_root: "workspaceRoot" if is_none });

/// Name (or spec) of the plugin to uninstall.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsUninstallRequest {
    /// Stable source identity for a direct (non-marketplace) install. Disambiguates uninstall when multiple installed plugins share the same name.
    pub direct_source_id: Option<String>,
    /// Plugin name or "plugin@marketplace" spec to uninstall. When ambiguous, prefer the fully-qualified spec.
    pub name: String,
}
crate::record_serde::wire_record!(PluginsUninstallRequest { direct_source_id: "directSourceId" if is_none, name: "name" });

/// Name (or spec) of the plugin to update.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsUpdateRequest {
    /// Plugin name or "plugin@marketplace" spec to update.
    pub name: String,
}
crate::record_serde::wire_record!(PluginsUpdateRequest { name: "name" });

/// Per-plugin result from updating all plugins, with versions, skills installed, success flag, and optional error.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginUpdateAllEntry {
    /// Error message (failure only)
    pub error: Option<String>,
    /// Marketplace the plugin came from. Empty string ("") for direct installs.
    pub marketplace: String,
    /// Plugin name that was updated
    pub name: String,
    /// Version after the update, when available
    pub new_version: Option<String>,
    /// Previously installed version, when available
    pub previous_version: Option<String>,
    /// Number of skills installed after the update (success only)
    pub skills_installed: Option<i64>,
    /// Whether the update succeeded for this plugin
    pub success: bool,
    /// Warnings from the updated plugin's manifest, including ignored fields or the reason the loader rejects the plugin. Callers should surface these to end users. Absent when the manifest raised none.
    pub warnings: Option<Vec<String>>,
}
crate::record_serde::wire_record!(PluginUpdateAllEntry { error: "error" if is_none, marketplace: "marketplace", name: "name", new_version: "newVersion" if is_none, previous_version: "previousVersion" if is_none, skills_installed: "skillsInstalled" if is_none, success: "success", warnings: "warnings" if is_none });

/// Result of updating all installed plugins.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginUpdateAllResult {
    /// Per-plugin update results in deterministic order.
    pub results: Vec<PluginUpdateAllEntry>,
}
crate::record_serde::wire_record!(PluginUpdateAllResult { results: "results" });

/// Update availability without installation or checkout mutation. An error means availability is unknown.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginUpdateCheckResult {
    /// Explanation when availability cannot be established.
    pub error: Option<String>,
    /// Installed plugin version, when known.
    pub installed_version: Option<String>,
    /// Cached catalog version, when known.
    pub latest_version: Option<String>,
    /// True when a marketplace has no cached checkout yet.
    pub not_fetched: Option<bool>,
    /// True only when the probe establishes an available update.
    pub update_available: bool,
}
crate::record_serde::wire_record!(PluginUpdateCheckResult { error: "error" if is_none, installed_version: "installedVersion" if is_none, latest_version: "latestVersion" if is_none, not_fetched: "notFetched" if is_none, update_available: "updateAvailable" });

/// Result of updating a single plugin.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginUpdateResult {
    /// Version after the update, when reported by the plugin manifest
    pub new_version: Option<String>,
    /// Version that was previously installed, when available
    pub previous_version: Option<String>,
    /// Number of skills discovered and installed after the update
    pub skills_installed: i64,
    /// Warnings from the updated plugin's manifest, including ignored fields or the reason the loader rejects the plugin. Callers should surface these to end users. Absent when the manifest raised none.
    pub warnings: Option<Vec<String>>,
}
crate::record_serde::wire_record!(PluginUpdateResult { new_version: "newVersion" if is_none, previous_version: "previousVersion" if is_none, skills_installed: "skillsInstalled", warnings: "warnings" if is_none });

/// Applies exactly one retained verified Skill installation plan.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillApplyInstallRequest {
    /// Required authenticated bound catalogue and Skill installation capabilities.
    pub contract: CatalogClientContract,
    /// Opaque original plan, consumed once.
    pub plan_handle: String,
    /// Same existing selected session as planning.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(SkillApplyInstallRequest {
    contract: "contract",
    plan_handle: "planHandle",
    policy_session_id: "policySessionId"
});

/// One-use application of the exact retained Skill removal plan.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillApplyUninstallRequest {
    /// Required authenticated bound installation contract.
    pub contract: CatalogClientContract,
    /// Opaque original removal plan, consumed once.
    pub plan_handle: String,
    /// Same existing selected session as removal preparation.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(SkillApplyUninstallRequest {
    contract: "contract",
    plan_handle: "planHandle",
    policy_session_id: "policySessionId"
});

/// Canonical directory where skills can be discovered or created, with scope, preference, and optional project path.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillDiscoveryPath {
    /// Absolute path of the create/discovery target (may not exist on disk yet)
    pub path: String,
    /// Whether this is the canonical directory to create a new skill in its tier. At most one entry per tier is preferred; the `personal-agents` and `custom` scopes are never preferred.
    pub preferred_for_creation: bool,
    /// The input project path this directory was derived from (only for project scope)
    pub project_path: Option<String>,
    /// Which tier this directory belongs to
    pub scope: SkillDiscoveryScope,
}
crate::record_serde::wire_record!(SkillDiscoveryPath { path: "path", preferred_for_creation: "preferredForCreation", project_path: "projectPath" if is_none, scope: "scope" });

/// Canonical locations where skills can be created so the runtime will recognize them.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillDiscoveryPathList {
    /// Canonical skill create/discovery directories, in priority order
    pub paths: Vec<SkillDiscoveryPath>,
}
crate::record_serde::wire_record!(SkillDiscoveryPathList { paths: "paths" });

/// Already-confirmed durable work must be reconciled before new mutations or inventory.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationManagementOutcomeRecoveryRequired {
    /// Skill installation management outcome discriminator.
    pub kind: SkillInstallationManagementOutcomeRecoveryRequiredKind,
}
crate::record_serde::wire_record!(SkillInstallationManagementOutcomeRecoveryRequired {
    kind: "kind"
});

/// A verified Skill installation plan was prepared.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SkillInstallationManagementOutcomeInstallPlanned {
    /// Skill installation management outcome discriminator.
    pub kind: SkillInstallationManagementOutcomeInstallPlannedKind,
    /// Prepared install plan.
    pub plan: SkillInstallPlan,
}

/// Owned Skill installations were listed.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationManagementOutcomeListed {
    /// Owned Skill installation summaries.
    pub installations: Vec<SkillInstallationSummary>,
    /// Skill installation management outcome discriminator.
    pub kind: SkillInstallationManagementOutcomeListedKind,
}
crate::record_serde::wire_record!(SkillInstallationManagementOutcomeListed {
    installations: "installations",
    kind: "kind"
});

/// Recovery completed and inventory was inspected.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationManagementOutcomeRecovered {
    /// Owned Skill installation summaries after recovery.
    pub installations: Vec<SkillInstallationSummary>,
    /// Skill installation management outcome discriminator.
    pub kind: SkillInstallationManagementOutcomeRecoveredKind,
}
crate::record_serde::wire_record!(SkillInstallationManagementOutcomeRecovered {
    installations: "installations",
    kind: "kind"
});

/// Interrupted work was safely compensated and the pending marker was cleared.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationManagementOutcomeRolledBack {
    /// Skill installation management outcome discriminator.
    pub kind: SkillInstallationManagementOutcomeRolledBackKind,
    /// Original operation identity.
    pub operation_id: String,
    /// Cause of the compensation.
    pub reason: SkillInstallationFailureReason,
}
crate::record_serde::wire_record!(SkillInstallationManagementOutcomeRolledBack {
    kind: "kind",
    operation_id: "operation_id",
    reason: "reason"
});

/// A computed Skill uninstall plan. Nothing has been removed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SkillUninstallPlan {
    /// Original wall-clock expiry as an ISO 8601 timestamp.
    pub expires_at: String,
    /// Owned Skill installation being removed.
    pub installation: SkillInstallationSummary,
    /// Original removal operation identifier returned before confirmation.
    pub operation_id: String,
    /// One-use uninstall plan handle.
    pub plan_handle: String,
    /// Safe review to present before applying removal.
    pub review: SkillInstallationReview,
}

/// An owned Skill uninstall plan was prepared.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SkillInstallationManagementOutcomeUninstallPlanned {
    /// Skill installation management outcome discriminator.
    pub kind: SkillInstallationManagementOutcomeUninstallPlannedKind,
    /// Prepared uninstall plan.
    pub plan: SkillUninstallPlan,
}

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOperationStatusPreparing {
    /// Whether cancellation has been requested.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    pub phase: SkillInstallationOperationStatusPreparingPhase,
}
crate::record_serde::wire_record!(SkillInstallationOperationStatusPreparing {
    cancellation_requested: "cancellationRequested",
    operation_id: "operationId",
    phase: "phase"
});

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOperationStatusPrepared {
    /// Whether cancellation has been requested.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    pub phase: SkillInstallationOperationStatusPreparedPhase,
}
crate::record_serde::wire_record!(SkillInstallationOperationStatusPrepared {
    cancellation_requested: "cancellationRequested",
    operation_id: "operationId",
    phase: "phase"
});

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOperationStatusAwaitingConfirmation {
    /// Whether cancellation has been requested.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    pub phase: SkillInstallationOperationStatusAwaitingConfirmationPhase,
}
crate::record_serde::wire_record!(SkillInstallationOperationStatusAwaitingConfirmation {
    cancellation_requested: "cancellationRequested",
    operation_id: "operationId",
    phase: "phase"
});

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOperationStatusRevalidating {
    /// Whether cancellation has been requested.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    pub phase: SkillInstallationOperationStatusRevalidatingPhase,
}
crate::record_serde::wire_record!(SkillInstallationOperationStatusRevalidating {
    cancellation_requested: "cancellationRequested",
    operation_id: "operationId",
    phase: "phase"
});

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOperationStatusApplying {
    /// Whether cancellation has been requested; already-started effects require recovery.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    pub phase: SkillInstallationOperationStatusApplyingPhase,
}
crate::record_serde::wire_record!(SkillInstallationOperationStatusApplying {
    cancellation_requested: "cancellationRequested",
    operation_id: "operationId",
    phase: "phase"
});

/// The verified Skill was installed disabled.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOutcomeInstalled {
    /// Durable installed Skill summary.
    pub installation: SkillInstallationSummary,
    /// Terminal Skill installation outcome discriminator.
    pub kind: SkillInstallationOutcomeInstalledKind,
}
crate::record_serde::wire_record!(SkillInstallationOutcomeInstalled {
    installation: "installation",
    kind: "kind"
});

/// The owned Skill was removed.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOutcomeUninstalled {
    /// Removed installation identity.
    pub installation_id: String,
    /// Terminal Skill installation outcome discriminator.
    pub kind: SkillInstallationOutcomeUninstalledKind,
    /// Original removal operation identity.
    pub operation_id: String,
}
crate::record_serde::wire_record!(SkillInstallationOutcomeUninstalled {
    installation_id: "installationId",
    kind: "kind",
    operation_id: "operationId"
});

/// The durable transaction was aborted or fully compensated.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOutcomeRolledBack {
    /// Terminal Skill installation outcome discriminator.
    pub kind: SkillInstallationOutcomeRolledBackKind,
    /// Original operation identity.
    pub operation_id: String,
    /// Cause of the fully aborted or compensated operation.
    pub reason: SkillInstallationFailureReason,
}
crate::record_serde::wire_record!(SkillInstallationOutcomeRolledBack {
    kind: "kind",
    operation_id: "operationId",
    reason: "reason"
});

/// A write may have completed. Recover and inspect durable state before retrying.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOutcomeRecoveryRequired {
    /// Terminal Skill installation outcome discriminator.
    pub kind: SkillInstallationOutcomeRecoveryRequiredKind,
    /// Operation whose durable result must be recovered and inspected.
    pub operation_id: String,
}
crate::record_serde::wire_record!(SkillInstallationOutcomeRecoveryRequired {
    kind: "kind",
    operation_id: "operationId"
});

/// The user declined the confirmation request.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOutcomeDeclined {
    /// Terminal Skill installation outcome discriminator.
    pub kind: SkillInstallationOutcomeDeclinedKind,
    /// Original operation identity.
    pub operation_id: String,
}
crate::record_serde::wire_record!(SkillInstallationOutcomeDeclined {
    kind: "kind",
    operation_id: "operationId"
});

/// The operation was cancelled before a terminal mutation.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOutcomeCancelled {
    /// Terminal Skill installation outcome discriminator.
    pub kind: SkillInstallationOutcomeCancelledKind,
    /// Original operation identity.
    pub operation_id: String,
}
crate::record_serde::wire_record!(SkillInstallationOutcomeCancelled {
    kind: "kind",
    operation_id: "operationId"
});

/// The operation was refused without applying changes.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOutcomeRefused {
    /// Terminal Skill installation outcome discriminator.
    pub kind: SkillInstallationOutcomeRefusedKind,
    /// Present once an operation has been allocated.
    pub operation_id: Option<String>,
    /// Bounded refusal reason.
    pub reason: SkillInstallationFailureReason,
    /// Whole seconds GitHub asked the caller to wait, present only with source-rate-limited.
    pub retry_after_seconds: Option<i64>,
}
crate::record_serde::wire_record!(SkillInstallationOutcomeRefused { kind: "kind", operation_id: "operationId" if is_none, reason: "reason", retry_after_seconds: "retryAfterSeconds" if is_none });

/// Original operation progress discriminator.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SkillInstallationOperationStatusCompleted {
    /// Whether cancellation was requested before the terminal result.
    pub cancellation_requested: bool,
    /// Original runtime-issued operation identity.
    pub operation_id: String,
    /// Immutable terminal receipt.
    pub outcome: SkillInstallationOutcome,
    pub phase: SkillInstallationOperationStatusCompletedPhase,
}

/// Original-connection operation snapshot.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SkillInstallationManagementOutcomeOperation {
    /// Skill installation management outcome discriminator.
    pub kind: SkillInstallationManagementOutcomeOperationKind,
    /// Operation status.
    pub operation: SkillInstallationOperationStatus,
}

/// Enablement changed and the selected session was reconciled.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationManagementOutcomeEnabledChanged {
    /// Safe reload or reconciliation diagnostics.
    pub diagnostics: Vec<String>,
    /// Updated installation summary.
    pub installation: SkillInstallationSummary,
    /// Skill installation management outcome discriminator.
    pub kind: SkillInstallationManagementOutcomeEnabledChangedKind,
}
crate::record_serde::wire_record!(SkillInstallationManagementOutcomeEnabledChanged {
    diagnostics: "diagnostics",
    installation: "installation",
    kind: "kind"
});

/// The management request was refused.
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationManagementOutcomeRefused {
    /// Skill installation management outcome discriminator.
    pub kind: SkillInstallationManagementOutcomeRefusedKind,
    /// Bounded refusal reason.
    pub reason: SkillInstallationFailureReason,
    /// Whole seconds GitHub asked the caller to wait, present only with source-rate-limited.
    pub retry_after_seconds: Option<i64>,
}
crate::record_serde::wire_record!(SkillInstallationManagementOutcomeRefused { kind: "kind", reason: "reason", retry_after_seconds: "retryAfterSeconds" if is_none });

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SkillInstallationManagementResultOutcome {
    /// Discriminator: handled management outcome.
    pub kind: SkillInstallationManagementResultOutcomeKind,
    /// Capabilities honoured for this request.
    pub negotiated: CatalogNegotiatedContract,
    /// Observed management outcome.
    pub outcome: SkillInstallationManagementOutcome,
}

/// Existing-operation control. A new session selector is deliberately not accepted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationOperationRequest {
    /// Required authenticated bound Skill installation capability.
    pub contract: CatalogClientContract,
    /// Exact runtime-issued operation ID on the original connection.
    pub operation_id: String,
}
crate::record_serde::wire_record!(SkillInstallationOperationRequest {
    contract: "contract",
    operation_id: "operationId"
});

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SkillInstallationResultOutcome {
    /// Discriminator: handled operation outcome.
    pub kind: SkillInstallationResultOutcomeKind,
    /// Capabilities honoured for this request.
    pub negotiated: CatalogNegotiatedContract,
    /// Terminal operation outcome.
    pub outcome: SkillInstallationOutcome,
}

/// Inventory request under an explicitly selected existing session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillInstallationsRequest {
    /// Required authenticated bound installation contract.
    pub contract: CatalogClientContract,
    /// Existing selected local session on this connection.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(SkillInstallationsRequest {
    contract: "contract",
    policy_session_id: "policySessionId"
});

/// Skills available to the session, with their enabled state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillList {
    /// Available skills
    pub skills: Vec<Skill>,
}
crate::record_serde::wire_record!(SkillList { skills: "skills" });

/// Side-effect-free planning of one verified Agent Finder Skill candidate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillPlanInstallRequest {
    /// Fresh single-use AI skill candidate handle returned by a bound catalogue search.
    pub candidate_handle: String,
    /// Required authenticated bound catalogue and Skill installation capabilities.
    pub contract: CatalogClientContract,
    /// Existing local session attached to this connection.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(SkillPlanInstallRequest {
    candidate_handle: "candidateHandle",
    contract: "contract",
    policy_session_id: "policySessionId"
});

/// Read-only preparation of one owned Skill removal under fresh selected-session authority.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillPlanUninstallRequest {
    /// Required authenticated bound installation contract.
    pub contract: CatalogClientContract,
    /// Exact receipt to inspect.
    pub installation_id: String,
    /// Existing selected local session on this connection.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(SkillPlanUninstallRequest {
    contract: "contract",
    installation_id: "installationId",
    policy_session_id: "policySessionId"
});

/// Authoritative catalog metadata for one SDK-provided skill. The skill's SKILL.md text is fetched separately and lazily.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillProviderDescriptor {
    /// Optional freeform argument hint used by slash-command catalogs.
    pub argument_hint: Option<String>,
    /// Description used in skill catalogs without fetching content.
    pub description: String,
    /// Absolute path to the folder that holds the skill's supporting files (scripts, references, templates, assets), in the session's path conventions: the host OS for local disk, or the registered SessionFs conventions. When the skill is activated the runtime lists the folder's files for the model through the session's filesystem, with the same rules as a file-based skill's folder; it never reads or copies them. On local disk the folder becomes a read-only skill root, like a configured skill directory. It is not shown in skill catalogs.
    pub directory: Option<String>,
    /// Whether model invocation is disabled. Defaults to false.
    pub disable_model_invocation: Option<bool>,
    /// Invocation and display name.
    pub name: String,
    /// Whether users may invoke the skill directly. Defaults to true.
    pub user_invocable: Option<bool>,
}
crate::record_serde::wire_record!(SkillProviderDescriptor { argument_hint: "argumentHint" if is_none, description: "description", directory: "directory" if is_none, disable_model_invocation: "disableModelInvocation" if is_none, name: "name", user_invocable: "userInvocable" if is_none });

/// Catalog metadata returned by an SDK session's skill provider. Catalogs are limited to 1024 descriptors and 1 MiB of aggregate metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SkillProviderListResult {
    /// Skill descriptors in provider order. Invocation names must be unique under case-insensitive comparison.
    pub skills: Vec<SkillProviderDescriptor>,
}
crate::record_serde::wire_record!(SkillProviderListResult { skills: "skills" });

/// Identifies one SDK-provided skill by invocation name.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SkillProviderReadRequest {
    /// Target session identifier
    pub session_id: SessionId,
    /// Invocation name of the skill to read.
    pub name: String,
}
crate::record_serde::wire_record!(SkillProviderReadRequest {
    session_id: "sessionId",
    name: "name"
});

/// SKILL.md content returned by an SDK session's skill provider. YAML frontmatter is optional: fields it omits come from the catalog descriptor, fields it declares must match the descriptor, and `allowed-tools` is read only from frontmatter. Supporting files come from the descriptor's `directory`.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SkillProviderReadResult {
    /// SKILL.md text, with or without YAML frontmatter, or null when the provider has no skill with the requested name. The runtime enforces a 1 MiB UTF-8 byte limit.
    pub markdown: Option<String>,
}
crate::record_serde::wire_record!(SkillProviderReadResult {
    markdown: "markdown"
});

/// One discovered local skill, without its content.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillRemoveSourceEntry {
    /// Discovery directory that owns the skill file.
    pub base_dir: String,
    /// Invocation name used to resolve a removal target.
    pub name: String,
    /// Original discovery source. Removal rejects unsupported or unowned sources.
    pub source: String,
}
crate::record_serde::wire_record!(SkillRemoveSourceEntry {
    base_dir: "baseDir",
    name: "name",
    source: "source"
});

/// Host-owned paths and environment for standalone skill management.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SkillResourceContext {
    /// Settings paths and hook environment for the original host.
    pub context: UserSettingsHostContext,
    /// Workspace directory for repository sources and relative paths.
    pub working_directory: String,
}
crate::record_serde::wire_record!(SkillResourceContext {
    context: "context",
    working_directory: "workingDirectory"
});

/// Loads full local skill records, including disabled records and command metadata.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SkillResourceDiscoverRequest {
    /// Host paths and working directory.
    #[doc(hidden)]
    pub(crate) host: SkillResourceContext,
    /// Explicit plugin directories, appended after persisted and live plugin sources.
    pub plugin_directories: Option<Vec<String>>,
}
crate::record_serde::wire_record!(SkillResourceDiscoverRequest { host: "host", plugin_directories: "pluginDirectories" if is_none });

/// Full local skill inventory and report-and-ignore source diagnostics.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillResourceDiscoverResult {
    /// Sources that failed to load; the remaining inventory is incomplete.
    pub errors: Vec<String>,
    /// Local skill records with invocation identity, content paths, and command metadata.
    pub skills: Vec<serde_json::Value>,
    /// Explicit plugin scan warnings, separate from skill loader diagnostics.
    pub source_warnings: Vec<String>,
    /// Nonfatal loader and manifest warnings.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(SkillResourceDiscoverResult {
    errors: "errors",
    skills: "skills",
    source_warnings: "sourceWarnings",
    warnings: "warnings"
});

/// Registers a skill directory or installs a local or HTTPS skill definition.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SkillResourceInstallRequest {
    /// Host paths and working directory.
    #[doc(hidden)]
    pub(crate) host: SkillResourceContext,
    /// Use the preferred project creation directory instead of personal storage.
    pub project: Option<bool>,
    /// Local file, directory, or HTTPS source.
    pub source: String,
    /// User agent for the existing runtime HTTPS fetcher.
    pub user_agent: Option<String>,
}
crate::record_serde::wire_record!(SkillResourceInstallRequest { host: "host", project: "project" if is_none, source: "source", user_agent: "userAgent" if is_none });

/// The persisted result of a skill source installation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillResourceInstallResult {
    /// Source classification.
    pub kind: SkillResourceInstallKind,
    /// Skill name, absent for a registered directory.
    pub name: Option<String>,
    /// Installed definition path or registered directory.
    pub path: String,
    /// Destination scope, absent for a registered directory.
    pub scope: Option<SkillResourceInstallScope>,
}
crate::record_serde::wire_record!(SkillResourceInstallResult { kind: "kind", name: "name" if is_none, path: "path", scope: "scope" if is_none });

/// Removes a personal or project skill, or unregisters a custom directory.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SkillResourceRemoveRequest {
    /// Host paths and working directory.
    #[doc(hidden)]
    pub(crate) host: SkillResourceContext,
    /// Skill name or registered directory.
    pub target: String,
}
crate::record_serde::wire_record!(SkillResourceRemoveRequest {
    host: "host",
    target: "target"
});

/// The completed skill removal, with conservative disabled-state cleanup.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillResourceRemoveResult {
    /// Removed storage tier.
    pub kind: SkillResourceRemoveKind,
    /// Removed skill name, absent for a registered directory.
    pub name: Option<String>,
    /// Removed definition path or unregistered directory.
    pub path: String,
}
crate::record_serde::wire_record!(SkillResourceRemoveResult { kind: "kind", name: "name" if is_none, path: "path" });

/// Atomically changes only one user's disabled skill entry under explicit host paths.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct SkillResourceSetEnabledRequest {
    /// Desired user-scope enablement.
    pub enabled: bool,
    /// Host paths and working directory.
    #[doc(hidden)]
    pub(crate) host: SkillResourceContext,
    /// Invocation name to enable or disable.
    pub name: String,
}
crate::record_serde::wire_record!(SkillResourceSetEnabledRequest {
    enabled: "enabled",
    host: "host",
    name: "name"
});

/// Skill names to mark as disabled in global configuration, replacing any previous list.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsConfigSetDisabledSkillsRequest {
    /// List of skill names to disable
    pub disabled_skills: Vec<String>,
}
crate::record_serde::wire_record!(SkillsConfigSetDisabledSkillsRequest {
    disabled_skills: "disabledSkills"
});

/// Adds or removes a single skill from the global disabled list, leaving every other entry untouched.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsConfigSetSkillDisabledRequest {
    /// True to disable the skill, false to enable it
    pub disabled: bool,
    /// Name of the skill to add to or remove from the disabled list
    pub name: String,
}
crate::record_serde::wire_record!(SkillsConfigSetSkillDisabledRequest {
    disabled: "disabled",
    name: "name"
});

/// Extensible [`SkillsConfigSetSkillDisabledRequest`], including inputs added after it was published.
///
/// Required inputs are [`SkillsConfigSetSkillDisabledOptions::new`] arguments; optional inputs have fluent setters.
/// Input-only: it serialises to the flat wire request and is not deserialisable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SkillsConfigSetSkillDisabledOptions {
    #[serde(flatten)]
    legacy: SkillsConfigSetSkillDisabledRequest,
    #[serde(skip_serializing_if = "Option::is_none")]
    config_dir: Option<String>,
}

impl SkillsConfigSetSkillDisabledOptions {
    /// Creates options with the required inputs.
    pub fn new(name: impl Into<String>, disabled: bool) -> Self {
        Self {
            legacy: SkillsConfigSetSkillDisabledRequest {
                name: name.into(),
                disabled,
            },
            config_dir: None,
        }
    }

    /// Configuration directory to update. Omit to use the server's configured directory.
    pub fn config_dir(mut self, value: impl Into<String>) -> Self {
        self.config_dir = Some(value.into());
        self
    }
}

/// Name of the skill to disable for the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsDisableRequest {
    /// Name of the skill to disable
    pub name: String,
}
crate::record_serde::wire_record!(SkillsDisableRequest { name: "name" });

/// Optional project paths and additional skill directories to include in discovery.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsDiscoverRequest {
    /// When true, omit skills from the host's global sources (personal, custom, plugin, and built-in), returning only project-scoped skills. For multitenant deployments.
    pub exclude_host_skills: Option<bool>,
    /// Optional skill scan paths to exclude from discovery.
    pub ignored_skills_locations: Option<Vec<String>>,
    /// Optional list of project directory paths to scan for project-scoped skills
    pub project_paths: Option<Vec<String>>,
    /// Explicit discovery-only skill roots, not filesystem grants. Available without ambient discovery, subject to skill/managed disables. Relative paths resolve against the request's effective working directory when supplied. Null and omission are equivalent; an empty array contributes no explicit roots.
    pub skill_directories: Option<Vec<String>>,
}
crate::record_serde::wire_record!(SkillsDiscoverRequest { exclude_host_skills: "excludeHostSkills" if is_none, ignored_skills_locations: "ignoredSkillsLocations" if is_none, project_paths: "projectPaths" if is_none, skill_directories: "skillDirectories" if is_none });

/// Name of the skill to enable for the session.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsEnableRequest {
    /// Name of the skill to enable
    pub name: String,
}
crate::record_serde::wire_record!(SkillsEnableRequest { name: "name" });

/// Persisted enablement update for one owned Skill installation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillSetEnabledRequest {
    /// Required authenticated bound Skill installation capability.
    pub contract: CatalogClientContract,
    /// Persisted enablement value.
    pub enabled: bool,
    /// Exact receipt identity to update.
    pub installation_id: String,
    /// Existing selected local session to reconcile after persistence.
    pub policy_session_id: String,
}
crate::record_serde::wire_record!(SkillSetEnabledRequest {
    contract: "contract",
    enabled: "enabled",
    installation_id: "installationId",
    policy_session_id: "policySessionId"
});

/// Optional project paths to enumerate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsGetDiscoveryPathsRequest {
    /// When true, omit the host's personal and custom skill directories, leaving only project directories. For multitenant deployments.
    pub exclude_host_skills: Option<bool>,
    /// Optional skill scan paths to exclude from discovery.
    pub ignored_skills_locations: Option<Vec<String>>,
    /// Optional list of project directory paths. When omitted or empty, only personal and custom directories are returned.
    pub project_paths: Option<Vec<String>>,
    /// Explicit discovery-only skill roots to enumerate, including configured missing directories. Relative paths resolve against the request's effective working directory when supplied. These are not filesystem grants or preferred creation targets. Null and omission are equivalent.
    pub skill_directories: Option<Vec<String>>,
}
crate::record_serde::wire_record!(SkillsGetDiscoveryPathsRequest { exclude_host_skills: "excludeHostSkills" if is_none, ignored_skills_locations: "ignoredSkillsLocations" if is_none, project_paths: "projectPaths" if is_none, skill_directories: "skillDirectories" if is_none });

/// Skills invoked during this session, ordered by invocation time (most recent last).
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsGetInvokedResult {
    /// Skills invoked during this session, ordered by invocation time (most recent last)
    pub skills: Vec<SkillsInvokedSkill>,
}
crate::record_serde::wire_record!(SkillsGetInvokedResult { skills: "skills" });

/// Installs a user-selected skill source without catalog plan semantics.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsInstallSourceRequest {
    /// Working directory for relative source paths.
    pub cwd: String,
    /// Personal destination for URL and file sources.
    pub personal_skills_dir: String,
    /// Selects project scope. Directory registration requires personal scope.
    pub project: bool,
    /// Project destination for URL and file sources.
    pub project_skills_dir: String,
    /// Current discovery directories. The result returns an updated copy.
    pub skill_directories: Vec<String>,
    /// HTTPS URL, local skill file, or local discovery directory.
    pub source: String,
    /// User agent for runtime-owned HTTPS source requests.
    pub user_agent: String,
}
crate::record_serde::wire_record!(SkillsInstallSourceRequest {
    cwd: "cwd",
    personal_skills_dir: "personalSkillsDir",
    project: "project",
    project_skills_dir: "projectSkillsDir",
    skill_directories: "skillDirectories",
    source: "source",
    user_agent: "userAgent"
});

/// Result of direct source installation. Catalog consent and defaults remain separate.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsInstallSourceResult {
    /// Source acquisition used by this operation.
    pub kind: SkillInstallSourceKind,
    /// Skill name when a file was installed.
    pub name: Option<String>,
    /// Installed skill directory or registered discovery directory.
    pub path: String,
    /// File destination scope. Directory registration has no destination scope.
    pub scope: Option<SkillInstallSourceScope>,
    /// Updated discovery directories for the caller to persist.
    pub skill_directories: Vec<String>,
}
crate::record_serde::wire_record!(SkillsInstallSourceResult { kind: "kind", name: "name" if is_none, path: "path", scope: "scope" if is_none, skill_directories: "skillDirectories" });

/// Diagnostics from reloading skill definitions, with warnings and errors as separate lists.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsLoadDiagnostics {
    /// Errors emitted while loading skills (e.g. skills that failed to load entirely)
    pub errors: Vec<String>,
    /// Warnings emitted while loading skills (e.g. skills that loaded but had issues)
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(SkillsLoadDiagnostics {
    errors: "errors",
    warnings: "warnings"
});

/// Caller-owned inventory and storage roots for direct file removal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsRemoveSourceInventory {
    /// Personal storage roots that the existing removal rules recognize.
    pub personal_skills_dirs: Vec<String>,
    /// Project storage root that the existing removal rules recognize.
    pub project_skills_dir: String,
    /// Local skills discovered before removal.
    pub skills: Vec<SkillRemoveSourceEntry>,
}
crate::record_serde::wire_record!(SkillsRemoveSourceInventory {
    personal_skills_dirs: "personalSkillsDirs",
    project_skills_dir: "projectSkillsDir",
    skills: "skills"
});

/// No directory registration matched. No files or settings changed.
#[derive(Debug, Clone, Default)]
pub struct SkillsRemoveSourceOutcomeNeedsInventory {
    /// Direct source removal or an inventory request without mutation.
    pub kind: SkillsRemoveSourceOutcomeNeedsInventoryKind,
}
crate::record_serde::wire_record!(SkillsRemoveSourceOutcomeNeedsInventory { kind: "kind" });

/// Direct removal result. The caller persists discovery and disabled settings.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsRemoveSourceResult {
    /// Resource removed by this operation.
    pub kind: SkillRemoveSourceKind,
    /// Skill name when a skill file was removed.
    pub name: Option<String>,
    /// Removed skill path or unregistered discovery directory.
    pub path: String,
    /// Updated discovery directories for the caller to persist.
    pub skill_directories: Vec<String>,
}
crate::record_serde::wire_record!(SkillsRemoveSourceResult { kind: "kind", name: "name" if is_none, path: "path", skill_directories: "skillDirectories" });

/// The existing source removal rules resolved and removed the target.
#[derive(Debug, Clone, Default)]
pub struct SkillsRemoveSourceOutcomeRemoved {
    /// Direct source removal or an inventory request without mutation.
    pub kind: SkillsRemoveSourceOutcomeRemovedKind,
    /// Removal result and updated discovery directories.
    pub result: SkillsRemoveSourceResult,
}
crate::record_serde::wire_record!(SkillsRemoveSourceOutcomeRemoved {
    kind: "kind",
    result: "result"
});

/// Removes a user-selected source without changing verified catalog removal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsRemoveSourceRequest {
    /// Working directory for relative removal paths.
    pub cwd: String,
    /// Required for file removal, but not for directory registration removal.
    pub inventory: Option<SkillsRemoveSourceInventory>,
    /// Current discovery directories. Removal returns an updated copy.
    pub skill_directories: Vec<String>,
    /// Local skill name, file path, or registered discovery directory.
    pub target: String,
}
crate::record_serde::wire_record!(SkillsRemoveSourceRequest { cwd: "cwd", inventory: "inventory" if is_none, skill_directories: "skillDirectories", target: "target" });

/// Explicit host approval for account-authenticated MCP endpoints.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpGitHubAuthPrepareRequest {
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
pub(crate) struct McpGitHubAuthPrepareResult {
    /// Remaining ordinary MCP configuration. Approval grants never leave the engine.
    pub additional_config_json: String,
}
crate::record_serde::wire_record!(McpGitHubAuthPrepareResult {
    additional_config_json: "additionalConfigJson"
});

/// Resolved plugin policy and authoritative failure posture. Fresh successful resolutions retain the original policy layers for mutation guards; cache-only inspection never changes retained policy.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginsPolicyResolveForInvocationResult {
    /// Resolution diagnostics in source order.
    #[doc(hidden)]
    pub(crate) diagnostics: Vec<PluginPolicyDiagnostic>,
    /// Whether this host has a configured helper.
    pub policy_helper_configured: bool,
    /// Whether helper failure requires refusal.
    pub policy_helper_fail_closed: bool,
    /// Whether the helper failed.
    pub policy_helper_load_failed: bool,
    /// Server resolution error, when present.
    pub server_fetch_error: Option<String>,
    /// Whether the server policy could not be resolved.
    pub server_fetch_failed: bool,
    /// Whether required fresh confirmation failed and the caller must refuse.
    pub server_forced_refresh_failure: bool,
    /// Raw server policy for hosts that inspect the account layer alone.
    pub server_response: Option<serde_json::Value>,
    /// Whether no usable credential was available.
    pub server_token_unavailable: bool,
    /// Effective device, server, and helper settings when available.
    pub settings: Option<serde_json::Value>,
}
crate::record_serde::wire_record!(PluginsPolicyResolveForInvocationResult { diagnostics: "diagnostics", policy_helper_configured: "policyHelperConfigured", policy_helper_fail_closed: "policyHelperFailClosed", policy_helper_load_failed: "policyHelperLoadFailed", server_fetch_error: "serverFetchError" if is_none, server_fetch_failed: "serverFetchFailed", server_forced_refresh_failure: "serverForcedRefreshFailure", server_response: "serverResponse" if is_none, server_token_unavailable: "serverTokenUnavailable", settings: "settings" if is_none });

/// Persisted, live, built-in, and host-directory plugins with canonical shadowing and scan diagnostics.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginsInspectInventoryResult {
    /// Merged inventory in canonical precedence order.
    #[doc(hidden)]
    pub(crate) plugins: Vec<PluginInvocationInventoryEntry>,
    /// Directory scan warnings in source order.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(PluginsInspectInventoryResult {
    plugins: "plugins",
    warnings: "warnings"
});

/// Plugins installed in user/global state.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsListResult {
    /// Installed plugins
    pub plugins: Vec<InstalledPluginInfo>,
    /// Existing discovery warnings for an invocation-context query. Omitted on legacy calls without workingDirectory or pluginDirectories. Display text can contain untrusted paths and must be sanitized before terminal output.
    pub warnings: Option<Vec<String>>,
}
crate::record_serde::wire_record!(PluginsListResult { plugins: "plugins", warnings: "warnings" if is_none });

/// Result of installing a plugin.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsInstallResult {
    /// Set when the install path is deprecated (e.g. direct repo / URL / local installs). Callers should surface this to end users.
    pub deprecation_warning: Option<String>,
    /// The newly installed plugin's metadata
    pub plugin: InstalledPluginInfo,
    /// Optional post-install message provided by the plugin (e.g. setup instructions)
    pub post_install_message: Option<String>,
    /// Number of skills discovered and installed from the plugin
    pub skills_installed: i64,
    /// Where the completed plugin tree was staged before atomic promotion
    pub staging_mode: Option<PluginInstallStagingMode>,
    /// Warnings raised while reading the plugin's manifest: a field the manifest's schema does not define, which the loader ignores, or the reason the loader rejects the whole manifest. Callers should surface these to end users: a plugin whose skills sit behind an ignored field, or whose manifest is rejected, otherwise installs, lists, and contributes nothing, with no signal to its author. Absent when the manifest raised none.
    pub warnings: Option<Vec<String>>,
}
crate::record_serde::wire_record!(PluginsInstallResult { deprecation_warning: "deprecationWarning" if is_none, plugin: "plugin", post_install_message: "postInstallMessage" if is_none, skills_installed: "skillsInstalled", staging_mode: "stagingMode" if is_none, warnings: "warnings" if is_none });

/// Result of updating a single plugin.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsUpdateResult {
    /// Version after the update, when reported by the plugin manifest
    pub new_version: Option<String>,
    /// Version that was previously installed, when available
    pub previous_version: Option<String>,
    /// Number of skills discovered and installed after the update
    pub skills_installed: i64,
    /// Warnings from the updated plugin's manifest, including ignored fields or the reason the loader rejects the plugin. Callers should surface these to end users. Absent when the manifest raised none.
    pub warnings: Option<Vec<String>>,
}
crate::record_serde::wire_record!(PluginsUpdateResult { new_version: "newVersion" if is_none, previous_version: "previousVersion" if is_none, skills_installed: "skillsInstalled", warnings: "warnings" if is_none });

/// Result of updating all installed plugins.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsUpdateAllResult {
    /// Per-plugin update results in deterministic order.
    pub results: Vec<PluginUpdateAllEntry>,
}
crate::record_serde::wire_record!(PluginsUpdateAllResult { results: "results" });

/// Bundled plugin records and diagnostics from the original manifest scanner.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsBuiltinDiscoverResult {
    /// Scanned bundled records in host precedence order.
    pub plugins: Vec<serde_json::Value>,
    /// Manifest and filesystem diagnostics, including a missing required bundle.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(PluginsBuiltinDiscoverResult {
    plugins: "plugins",
    warnings: "warnings"
});

/// All registered marketplaces, including built-in defaults.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsMarketplacesListResult {
    /// Registered marketplaces
    pub marketplaces: Vec<MarketplaceInfo>,
}
crate::record_serde::wire_record!(PluginsMarketplacesListResult {
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
pub struct PluginsMarketplacesMigrateRetiredDefaultResult {
    /// The marketplace registered by this call. Present only when status is migrated.
    pub marketplace: Option<String>,
    /// Whether the migration changed the profile or why it left the profile unchanged.
    pub status: MarketplaceMigrationStatus,
}
crate::record_serde::wire_record!(PluginsMarketplacesMigrateRetiredDefaultResult { marketplace: "marketplace" if is_none, status: "status" });

/// Result of registering a new marketplace.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsMarketplacesAddResult {
    /// Final name of the marketplace as resolved from its manifest
    pub name: String,
}
crate::record_serde::wire_record!(PluginsMarketplacesAddResult { name: "name" });

/// Outcome of the remove attempt, including dependent-plugin info when applicable.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsMarketplacesRemoveResult {
    /// Names of installed plugins that prevented removal. Populated only when `removed=false`.
    pub dependent_plugins: Option<Vec<String>>,
    /// True when the marketplace was actually removed. False when removal was skipped because the marketplace has dependent plugins and `force` was not set.
    pub removed: bool,
}
crate::record_serde::wire_record!(PluginsMarketplacesRemoveResult { dependent_plugins: "dependentPlugins" if is_none, removed: "removed" });

/// Plugins advertised by the marketplace.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsMarketplacesBrowseResult {
    /// Plugins advertised by the marketplace
    pub plugins: Vec<MarketplacePluginInfo>,
}
crate::record_serde::wire_record!(PluginsMarketplacesBrowseResult { plugins: "plugins" });

/// Result of refreshing one or more marketplace catalogs.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct PluginsMarketplacesRefreshResult {
    /// Per-marketplace refresh results in deterministic order.
    pub results: Vec<MarketplaceRefreshEntry>,
}
crate::record_serde::wire_record!(PluginsMarketplacesRefreshResult { results: "results" });

/// Active plugins and the separate persisted inventory, without persisting live directory records.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct PluginsSourcesResolveResult {
    /// Live directory records, including inactive records that still own their names.
    pub live_plugins: Option<Vec<serde_json::Value>>,
    /// Whether this pass confirmed the live tier instead of using persisted fallback.
    pub live_tier_known: Option<bool>,
    /// Persisted ambient records, including disabled or shadowed installations.
    pub persisted_plugins: Vec<serde_json::Value>,
    /// Active records, including trusted live marketplaces and explicit directories.
    pub plugins: Vec<serde_json::Value>,
    /// Scanner and live-marketplace warnings for this exact source pass.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(PluginsSourcesResolveResult { live_plugins: "livePlugins" if is_none, live_tier_known: "liveTierKnown" if is_none, persisted_plugins: "persistedPlugins", plugins: "plugins", warnings: "warnings" });

/// Full local skill inventory and report-and-ignore source diagnostics.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsResourcesDiscoverResult {
    /// Sources that failed to load; the remaining inventory is incomplete.
    pub errors: Vec<String>,
    /// Local skill records with invocation identity, content paths, and command metadata.
    pub skills: Vec<serde_json::Value>,
    /// Explicit plugin scan warnings, separate from skill loader diagnostics.
    pub source_warnings: Vec<String>,
    /// Nonfatal loader and manifest warnings.
    pub warnings: Vec<String>,
}
crate::record_serde::wire_record!(SkillsResourcesDiscoverResult {
    errors: "errors",
    skills: "skills",
    source_warnings: "sourceWarnings",
    warnings: "warnings"
});

/// The persisted result of a skill source installation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsResourcesInstallResult {
    /// Source classification.
    pub kind: SkillResourceInstallKind,
    /// Skill name, absent for a registered directory.
    pub name: Option<String>,
    /// Installed definition path or registered directory.
    pub path: String,
    /// Destination scope, absent for a registered directory.
    pub scope: Option<SkillResourceInstallScope>,
}
crate::record_serde::wire_record!(SkillsResourcesInstallResult { kind: "kind", name: "name" if is_none, path: "path", scope: "scope" if is_none });

/// The completed skill removal, with conservative disabled-state cleanup.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsResourcesRemoveResult {
    /// Removed storage tier.
    pub kind: SkillResourceRemoveKind,
    /// Removed skill name, absent for a registered directory.
    pub name: Option<String>,
    /// Removed definition path or unregistered directory.
    pub path: String,
}
crate::record_serde::wire_record!(SkillsResourcesRemoveResult { kind: "kind", name: "name" if is_none, path: "path" });

/// Skills discovered across global and project sources.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsDiscoverResult {
    /// Messages for skills that failed to load (e.g. malformed SKILL.md). Empty when host skills are excluded so host-local paths are not disclosed to multitenant callers.
    pub errors: Option<Vec<String>>,
    /// All discovered skills across all sources
    pub skills: Vec<ServerSkill>,
}
crate::record_serde::wire_record!(SkillsDiscoverResult { errors: "errors" if is_none, skills: "skills" });

/// Canonical locations where skills can be created so the runtime will recognize them.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct SkillsGetDiscoveryPathsResult {
    /// Canonical skill create/discovery directories, in priority order
    pub paths: Vec<SkillDiscoveryPath>,
}
crate::record_serde::wire_record!(SkillsGetDiscoveryPathsResult { paths: "paths" });

/// User-configured MCP servers, keyed by server name.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct McpConfigListResult {
    /// All MCP servers from user config, keyed by name
    pub servers: HashMap<String, serde_json::Value>,
}
crate::record_serde::wire_record!(McpConfigListResult { servers: "servers" });

/// Fresh user MCP server names, including invalid entries but excluding installation-owned entries. This read does not change the validated configuration cache.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpConfigUserServerNamesForConfigDirResult {
    /// User-owned persisted server names, including entries that validation skips.
    pub names: Vec<String>,
}
crate::record_serde::wire_record!(McpConfigUserServerNamesForConfigDirResult { names: "names" });

/// Request id naming a cancellable registry search.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpRegistryAllocateRequestIdResult {
    /// Request ID for `mcp.registry.search` and `mcp.registry.cancel`. It serves one search. Allocation can reclaim unused IDs at the 1,024-ID reservation limit.
    pub request_id: i64,
}
crate::record_serde::wire_record!(McpRegistryAllocateRequestIdResult {
    request_id: "requestId"
});

/// A secret-free GitHub descriptor, an ordinary signed-out result, or a bounded degraded result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub(crate) struct McpGitHubResolveForInvocationResult {
    /// Built-in descriptor without authentication headers; absent when unavailable.
    pub server: Option<serde_json::Value>,
    /// Whether the existing two-second resolution budget expired.
    pub timed_out: bool,
    /// Resolution failure detail, distinct from an ordinary signed-out result.
    pub warning: Option<String>,
}
crate::record_serde::wire_record!(McpGitHubResolveForInvocationResult { server: "server" if is_none, timed_out: "timedOut", warning: "warning" if is_none });

/// Extensions discovered from persisted Copilot home state and their effective loading mode. Launch-scoped additional plugins are not included.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct ExtensionsDiscoverResult {
    /// Discovered user and enabled installed-plugin extensions from persisted Copilot home state
    pub extensions: Vec<DiscoveredExtension>,
    /// Effective extension loading mode. Defaults to load_and_augment when unset.
    pub mode: DiscoveredExtensionMode,
}
crate::record_serde::wire_record!(ExtensionsDiscoverResult {
    extensions: "extensions",
    mode: "mode"
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
pub(crate) struct McpSourcePrepareGitHubAuthResult {
    /// Remaining ordinary MCP configuration. Approval grants never leave the engine.
    pub additional_config_json: String,
}
crate::record_serde::wire_record!(McpSourcePrepareGitHubAuthResult {
    additional_config_json: "additionalConfigJson"
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
pub struct SkillProviderListParams {
    /// Target session identifier
    pub session_id: SessionId,
}
crate::record_serde::wire_record!(SkillProviderListParams {
    session_id: "sessionId"
});

/// Canvas open result returned by the provider.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default)]
pub struct CanvasOpenResult {
    /// Provider-supplied status text
    pub status: Option<String>,
    /// Provider-supplied title
    pub title: Option<String>,
    /// URL for web-rendered canvases
    pub url: Option<String>,
}
crate::record_serde::wire_record!(CanvasOpenResult { status: "status" if is_none, title: "title" if is_none, url: "url" if is_none });

/// A standard MCP tool result, or null when the IDE is disconnected.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type McpCallToolResult = Option<McpToolResult>;

/// A runtime-assigned secret placeholder. The identifier is carried once, inside the placeholder, so it cannot contradict a separate secret-id field.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type McpPlanSecretReference = String;

/// Whether the OS keychain accepted the saved MCP credential.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
pub type McpSecretsSaveResult = McpSecretSaveResult;

/// Canonical digest algorithm for a validated MCP card
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CardDigestAlgorithm {
    /// SHA-256 over RFC 8785 canonical JSON encoded as UTF-8.
    #[serde(rename = "sha256-rfc8785")]
    Sha256Rfc8785,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CardDigestAlgorithm {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CardDigestAlgorithm",
            &["sha256-rfc8785", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Sha256Rfc8785,
            _ => Self::Unknown,
        })
    }
}

/// Which kind of opaque handle was presented
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogHandleType {
    /// A search candidate handle.
    #[serde(rename = "candidate")]
    Candidate,
    /// An install plan handle.
    #[serde(rename = "plan")]
    Plan,
    /// A model-safe reference to one retained search candidate.
    #[serde(rename = "selection")]
    Selection,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogHandleType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogHandleType",
            &["candidate", "plan", "selection", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Candidate,
            1 => Self::Plan,
            2 => Self::Selection,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: a handle was rejected
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogHandleRejectedErrorKind {
    #[serde(rename = "handle-rejected")]
    #[default]
    HandleRejected,
}
impl<'de> Deserialize<'de> for CatalogHandleRejectedErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogHandleRejectedErrorKind",
            &["handle-rejected"],
            None,
        )?;
        Ok(Self::HandleRejected)
    }
}

/// Why a presented handle was rejected
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogHandleRejectionReason {
    /// The handle is unparseable or unknown.
    #[serde(rename = "invalid")]
    Invalid,
    /// The handle's time to live has elapsed.
    #[serde(rename = "stale")]
    Stale,
    /// The handle has already been used, and handles are single-use.
    #[serde(rename = "replayed")]
    Replayed,
    /// The handle was issued by a different runtime instance or session.
    #[serde(rename = "foreign")]
    Foreign,
    /// The handle was issued for another catalog operation.
    #[serde(rename = "wrong-kind")]
    WrongKind,
    /// The supplied search identifier does not match the retained candidate.
    #[serde(rename = "search-mismatch")]
    SearchMismatch,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogHandleRejectionReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogHandleRejectionReason",
            &[
                "invalid",
                "stale",
                "replayed",
                "foreign",
                "wrong-kind",
                "search-mismatch",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::Invalid,
            1 => Self::Stale,
            2 => Self::Replayed,
            3 => Self::Foreign,
            4 => Self::WrongKind,
            5 => Self::SearchMismatch,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: the candidate cannot be installed
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogNotInstallableErrorKind {
    #[serde(rename = "not-installable")]
    #[default]
    NotInstallable,
}
impl<'de> Deserialize<'de> for CatalogNotInstallableErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogNotInstallableErrorKind",
            &["not-installable"],
            None,
        )?;
        Ok(Self::NotInstallable)
    }
}

/// Why a discoverable candidate cannot be installed
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogNotInstallableReason {
    /// This kind of resource is not installable through this surface.
    #[serde(rename = "kind-not-installable")]
    KindNotInstallable,
    /// AI skills are discoverable but have no typed importer in this phase.
    #[serde(rename = "ai-skill-not-installable")]
    AiSkillNotInstallable,
    /// Policy forbids installing this candidate.
    #[serde(rename = "policy-forbids")]
    PolicyForbids,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogNotInstallableReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogNotInstallableReason",
            &[
                "kind-not-installable",
                "ai-skill-not-installable",
                "policy-forbids",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::KindNotInstallable,
            1 => Self::AiSkillNotInstallable,
            2 => Self::PolicyForbids,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator: no usable transport is available
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogUnavailableTransportErrorKind {
    #[serde(rename = "unavailable-transport")]
    #[default]
    UnavailableTransport,
}
impl<'de> Deserialize<'de> for CatalogUnavailableTransportErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogUnavailableTransportErrorKind",
            &["unavailable-transport"],
            None,
        )?;
        Ok(Self::UnavailableTransport)
    }
}

/// Why no usable transport could be offered
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum CatalogUnavailableTransportReason {
    /// The card advertises no transport this runtime can use.
    #[serde(rename = "no-eligible-transport")]
    NoEligibleTransport,
    /// Every advertised transport is of a kind this runtime does not implement.
    #[serde(rename = "transport-not-supported")]
    TransportNotSupported,
    /// Eligible remotes could not be enumerated, so no explicit choice can be offered.
    #[serde(rename = "remote-enumeration-unavailable")]
    RemoteEnumerationUnavailable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for CatalogUnavailableTransportReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "CatalogUnavailableTransportReason",
            &[
                "no-eligible-transport",
                "transport-not-supported",
                "remote-enumeration-unavailable",
                "Unknown",
            ],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::NoEligibleTransport,
            1 => Self::TransportNotSupported,
            2 => Self::RemoteEnumerationUnavailable,
            _ => Self::Unknown,
        })
    }
}

/// Server transport type: stdio, http, sse (deprecated), or memory
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum DiscoveredMcpServerType {
    /// Server communicates over stdio with a local child process.
    #[serde(rename = "stdio")]
    Stdio,
    /// Server communicates over streamable HTTP.
    #[serde(rename = "http")]
    Http,
    /// Server communicates over Server-Sent Events (deprecated).
    #[serde(rename = "sse")]
    Sse,
    /// Server is backed by an in-memory runtime implementation.
    #[serde(rename = "memory")]
    Memory,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for DiscoveredMcpServerType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "DiscoveredMcpServerType",
            &["stdio", "http", "sse", "memory", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Stdio,
            1 => Self::Http,
            2 => Self::Sse,
            3 => Self::Memory,
            _ => Self::Unknown,
        })
    }
}

/// Attachment type discriminator
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum ExtensionContextPushInputType {
    #[serde(rename = "extension_context")]
    #[default]
    ExtensionContext,
}
impl<'de> Deserialize<'de> for ExtensionContextPushInputType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "ExtensionContextPushInputType",
            &["extension_context"],
            None,
        )?;
        Ok(Self::ExtensionContext)
    }
}

/// Reviewed resource discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstallationReviewMcpResource {
    #[serde(rename = "mcp")]
    #[default]
    Mcp,
}
impl<'de> Deserialize<'de> for InstallationReviewMcpResource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "InstallationReviewMcpResource",
            &["mcp"],
            None,
        )?;
        Ok(Self::Mcp)
    }
}

/// Exact reviewed installation action.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationReviewInstallAction {
    #[serde(rename = "install")]
    #[default]
    Install,
}
impl<'de> Deserialize<'de> for McpInstallationReviewInstallAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationReviewInstallAction",
            &["install"],
            None,
        )?;
        Ok(Self::Install)
    }
}

/// Whether a planned configuration change would create or modify an entry
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanConfigurationOperation {
    /// Creates a configuration entry that does not exist yet.
    #[serde(rename = "add")]
    Add,
    /// Modifies a configuration entry that already exists.
    #[serde(rename = "update")]
    Update,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanConfigurationOperation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanConfigurationOperation",
            &["add", "update", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Add,
            1 => Self::Update,
            _ => Self::Unknown,
        })
    }
}

/// Configuration scope an MCP install plan targets
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanScope {
    /// The user's own MCP configuration.
    #[serde(rename = "user")]
    User,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanScope",
            &["user", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::User,
            _ => Self::Unknown,
        })
    }
}

/// Transport exposed by a remote endpoint
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanRemoteTransport {
    /// An HTTP endpoint.
    #[serde(rename = "http")]
    Http,
    /// A streamable HTTP endpoint.
    #[serde(rename = "streamable-http")]
    StreamableHttp,
    /// A server-sent events endpoint.
    #[serde(rename = "sse")]
    Sse,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanRemoteTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanRemoteTransport",
            &["http", "streamable-http", "sse", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Http,
            1 => Self::StreamableHttp,
            2 => Self::Sse,
            _ => Self::Unknown,
        })
    }
}

/// Where a required value is applied when the planned server is launched
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanValueCategory {
    /// Set as an environment variable on the launched process.
    #[serde(rename = "environment-variable")]
    EnvironmentVariable,
    /// Passed to the runtime that launches the package.
    #[serde(rename = "runtime-argument")]
    RuntimeArgument,
    /// Passed to the packaged server itself.
    #[serde(rename = "package-argument")]
    PackageArgument,
    /// Sent as a request header to a remote endpoint.
    #[serde(rename = "header")]
    Header,
    /// Substituted into the remote endpoint URL.
    #[serde(rename = "url-variable")]
    UrlVariable,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanValueCategory {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanValueCategory",
            &[
                "environment-variable",
                "runtime-argument",
                "package-argument",
                "header",
                "url-variable",
                "Unknown",
            ],
            Some(5),
        )?;
        Ok(match index {
            0 => Self::EnvironmentVariable,
            1 => Self::RuntimeArgument,
            2 => Self::PackageArgument,
            3 => Self::Header,
            4 => Self::UrlVariable,
            _ => Self::Unknown,
        })
    }
}

/// What policy decided for a planned server
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanPolicyDecision {
    /// Policy permits the server.
    #[serde(rename = "allowed")]
    Allowed,
    /// Policy forbids the server, so the plan cannot be applied.
    #[serde(rename = "blocked")]
    Blocked,
    /// Policy permits the server only after an explicit approval.
    #[serde(rename = "requires-approval")]
    RequiresApproval,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanPolicyDecision {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanPolicyDecision",
            &["allowed", "blocked", "requires-approval", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Allowed,
            1 => Self::Blocked,
            2 => Self::RequiresApproval,
            _ => Self::Unknown,
        })
    }
}

/// Explicit backend selection is part of the final review; failures never switch backends.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationSecretStorage {
    /// The selected operating-system keychain, without fallback to file storage.
    #[serde(rename = "keychain")]
    Keychain,
    /// The explicitly selected private file backend.
    #[serde(rename = "private-file")]
    PrivateFile,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpInstallationSecretStorage {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationSecretStorage",
            &["keychain", "private-file", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Keychain,
            1 => Self::PrivateFile,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for a package-backed transport choice
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanPackageInstallMethod {
    /// Install and run a local package.
    #[serde(rename = "package")]
    Package,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanPackageInstallMethod {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanPackageInstallMethod",
            &["package", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Package,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for a scalar required value
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanRequiredValueScalarKind {
    /// The value uses one scalar type.
    #[serde(rename = "scalar")]
    Scalar,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanRequiredValueScalarKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanRequiredValueScalarKind",
            &["scalar", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Scalar,
            _ => Self::Unknown,
        })
    }
}

/// Scalar type a required value must conform to
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanScalarValueType {
    /// Free text.
    #[serde(rename = "string")]
    String,
    /// A number.
    #[serde(rename = "number")]
    Number,
    /// A boolean.
    #[serde(rename = "boolean")]
    Boolean,
    /// A filesystem path.
    #[serde(rename = "path")]
    Path,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanScalarValueType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanScalarValueType",
            &["string", "number", "boolean", "path", "Unknown"],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::String,
            1 => Self::Number,
            2 => Self::Boolean,
            3 => Self::Path,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for an enumerated required value
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanRequiredValueEnumKind {
    /// The value uses a fixed non-empty enumeration.
    #[serde(rename = "enum")]
    Enum,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanRequiredValueEnumKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanRequiredValueEnumKind",
            &["enum", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Enum,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for an enumerated required value
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanEnumValueType {
    /// One of a fixed, non-empty set of permitted values.
    #[serde(rename = "enum")]
    Enum,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanEnumValueType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanEnumValueType",
            &["enum", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Enum,
            _ => Self::Unknown,
        })
    }
}

/// One non-secret value a transport choice needs, represented as a scalar or enumerated variant so enum values cannot be missing or attached to another type.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpPlanRequiredValue {
    Scalar(McpPlanRequiredValueScalar),
    Enum(McpPlanRequiredValueEnum),
}

/// Transport exposed by a locally launched package
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanPackageTransport {
    /// A locally launched process spoken to over standard input and output.
    #[serde(rename = "stdio")]
    Stdio,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanPackageTransport {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanPackageTransport",
            &["stdio", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Stdio,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for a remote-endpoint transport choice
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanRemoteInstallMethod {
    /// Connect to a remote endpoint.
    #[serde(rename = "remote")]
    Remote,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanRemoteInstallMethod {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanRemoteInstallMethod",
            &["remote", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Remote,
            _ => Self::Unknown,
        })
    }
}

/// One eligible way to run the server, represented as a tagged package or remote variant so package identity and endpoint states cannot contradict the install method.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpPlanTransportChoice {
    Package(McpPlanTransportChoicePackage),
    Remote(McpPlanTransportChoiceRemote),
}

/// Exact reviewed installation action.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationReviewUninstallAction {
    #[serde(rename = "uninstall")]
    #[default]
    Uninstall,
}
impl<'de> Deserialize<'de> for McpInstallationReviewUninstallAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationReviewUninstallAction",
            &["uninstall"],
            None,
        )?;
        Ok(Self::Uninstall)
    }
}

/// Safe MCP review fields. No raw card, retrieval URL, plan handle or secret value.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpInstallationReview {
    Install(McpInstallationReviewInstall),
    Uninstall(McpInstallationReviewUninstall),
}

/// Reviewed resource discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstallationReviewSkillResource {
    #[serde(rename = "skill")]
    #[default]
    Skill,
}
impl<'de> Deserialize<'de> for InstallationReviewSkillResource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "InstallationReviewSkillResource",
            &["skill"],
            None,
        )?;
        Ok(Self::Skill)
    }
}

/// Reviewed resource discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstallationReviewPluginResource {
    #[serde(rename = "plugin")]
    #[default]
    Plugin,
}
impl<'de> Deserialize<'de> for InstallationReviewPluginResource {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "InstallationReviewPluginResource",
            &["plugin"],
            None,
        )?;
        Ok(Self::Plugin)
    }
}

/// Exact reviewed Agent Plugin installation action.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstallationReviewPluginReviewAction {
    #[serde(rename = "install")]
    #[default]
    Install,
}
impl<'de> Deserialize<'de> for InstallationReviewPluginReviewAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "InstallationReviewPluginReviewAction",
            &["install"],
            None,
        )?;
        Ok(Self::Install)
    }
}

/// Only resource kinds with an implemented installation engine have a review variant.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum InstallationReview {
    Mcp(InstallationReviewMcp),
    Skill(InstallationReviewSkill),
    Plugin(InstallationReviewPlugin),
}

/// Explicit user decisions, never inferred from a permission grant or model response.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum InstallationDecision {
    /// The user explicitly approved the exact review on this request.
    #[serde(rename = "confirm")]
    Confirm,
    /// The user declined the reviewed operation.
    #[serde(rename = "decline")]
    Decline,
    /// The user cancelled the pending decision without granting consent.
    #[serde(rename = "cancel")]
    Cancel,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for InstallationDecision {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "InstallationDecision",
            &["confirm", "decline", "cancel", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Confirm,
            1 => Self::Decline,
            2 => Self::Cancel,
            _ => Self::Unknown,
        })
    }
}

/// The source format: inline JSON or a host file.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpAdditionalConfigSourceJsonKind {
    #[serde(rename = "json")]
    #[default]
    Json,
}
impl<'de> Deserialize<'de> for McpAdditionalConfigSourceJsonKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpAdditionalConfigSourceJsonKind",
            &["json"],
            None,
        )?;
        Ok(Self::Json)
    }
}

/// The source format: inline JSON or a host file.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpAdditionalConfigSourceFileKind {
    #[serde(rename = "file")]
    #[default]
    File,
}
impl<'de> Deserialize<'de> for McpAdditionalConfigSourceFileKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpAdditionalConfigSourceFileKind",
            &["file"],
            None,
        )?;
        Ok(Self::File)
    }
}

/// An additional MCP document supplied as JSON text or a host file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpAdditionalConfigSource {
    Json(McpAdditionalConfigSourceJson),
    File(McpAdditionalConfigSourceFile),
}

/// Allowed values for the `McpAppsSetHostContextDetailsAvailableDisplayMode` enumeration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpAppsSetHostContextDetailsAvailableDisplayMode {
    /// Rendered inline within the host conversation surface
    #[serde(rename = "inline")]
    Inline,
    /// Rendered as a fullscreen overlay
    #[serde(rename = "fullscreen")]
    Fullscreen,
    /// Rendered as a picture-in-picture floating panel
    #[serde(rename = "pip")]
    Pip,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpAppsSetHostContextDetailsAvailableDisplayMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpAppsSetHostContextDetailsAvailableDisplayMode",
            &["inline", "fullscreen", "pip", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Inline,
            1 => Self::Fullscreen,
            2 => Self::Pip,
            _ => Self::Unknown,
        })
    }
}

/// Current display mode (SEP-1865)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpAppsSetHostContextDetailsDisplayMode {
    /// Rendered inline within the host conversation surface
    #[serde(rename = "inline")]
    Inline,
    /// Rendered as a fullscreen overlay
    #[serde(rename = "fullscreen")]
    Fullscreen,
    /// Rendered as a picture-in-picture floating panel
    #[serde(rename = "pip")]
    Pip,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpAppsSetHostContextDetailsDisplayMode {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpAppsSetHostContextDetailsDisplayMode",
            &["inline", "fullscreen", "pip", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Inline,
            1 => Self::Fullscreen,
            2 => Self::Pip,
            _ => Self::Unknown,
        })
    }
}

/// Platform type for responsive design
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpAppsSetHostContextDetailsPlatform {
    /// Host runs in a web browser
    #[serde(rename = "web")]
    Web,
    /// Host runs as a desktop application
    #[serde(rename = "desktop")]
    Desktop,
    /// Host runs on a mobile device
    #[serde(rename = "mobile")]
    Mobile,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpAppsSetHostContextDetailsPlatform {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpAppsSetHostContextDetailsPlatform",
            &["web", "desktop", "mobile", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Web,
            1 => Self::Desktop,
            2 => Self::Mobile,
            _ => Self::Unknown,
        })
    }
}

/// UI theme preference per SEP-1865
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpAppsSetHostContextDetailsTheme {
    /// Light UI theme
    #[serde(rename = "light")]
    Light,
    /// Dark UI theme
    #[serde(rename = "dark")]
    Dark,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpAppsSetHostContextDetailsTheme {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpAppsSetHostContextDetailsTheme",
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

/// Controls if tools provided by this server can be loaded on demand via tool search (auto) or always included in the initial tool list (never)
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerConfigDeferTools {
    /// Tools may be deferred under certain conditions
    #[serde(rename = "auto")]
    Auto,
    /// Tools are always included in the initial tool list, even when tool search is enabled.
    #[serde(rename = "never")]
    Never,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerConfigDeferTools {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerConfigDeferTools",
            &["auto", "never", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Auto,
            1 => Self::Never,
            _ => Self::Unknown,
        })
    }
}

/// In-process MCP transport.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpDiscoveryMemoryConfigType {
    #[serde(rename = "memory")]
    #[default]
    Memory,
}
impl<'de> Deserialize<'de> for McpDiscoveryMemoryConfigType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpDiscoveryMemoryConfigType",
            &["memory"],
            None,
        )?;
        Ok(Self::Memory)
    }
}

/// Headers-refresh response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpHeadersHandlePendingHeadersRefreshRequestHeadersKind {
    #[serde(rename = "headers")]
    #[default]
    Headers,
}
impl<'de> Deserialize<'de> for McpHeadersHandlePendingHeadersRefreshRequestHeadersKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpHeadersHandlePendingHeadersRefreshRequestHeadersKind",
            &["headers"],
            None,
        )?;
        Ok(Self::Headers)
    }
}

/// Headers-refresh response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpHeadersHandlePendingHeadersRefreshRequestNoneKind {
    #[serde(rename = "none")]
    #[default]
    None,
}
impl<'de> Deserialize<'de> for McpHeadersHandlePendingHeadersRefreshRequestNoneKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpHeadersHandlePendingHeadersRefreshRequestNoneKind",
            &["none"],
            None,
        )?;
        Ok(Self::None)
    }
}

/// Headers-refresh response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpHeadersHandlePendingHeadersRefreshRequestErrorKind {
    #[serde(rename = "error")]
    #[default]
    Error,
}
impl<'de> Deserialize<'de> for McpHeadersHandlePendingHeadersRefreshRequestErrorKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpHeadersHandlePendingHeadersRefreshRequestErrorKind",
            &["error"],
            None,
        )?;
        Ok(Self::Error)
    }
}

/// Host response: supply dynamic headers or decline this refresh.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpHeadersHandlePendingHeadersRefreshRequest {
    Headers(McpHeadersHandlePendingHeadersRefreshRequestHeaders),
    None(McpHeadersHandlePendingHeadersRefreshRequestNone),
    Error(McpHeadersHandlePendingHeadersRefreshRequestError),
}

/// Bounded refusal categories, without echoing handles, credentials or configuration.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationFailureReason {
    /// The selected choice or request is unsupported or malformed.
    #[serde(rename = "invalid-request")]
    InvalidRequest,
    /// The bounded original-connection operation limit was reached.
    #[serde(rename = "operation-limit")]
    OperationLimit,
    /// The original operation was cancelled.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// Required installation capabilities were omitted.
    #[serde(rename = "capability-required")]
    CapabilityRequired,
    /// The original host cannot receive human confirmation.
    #[serde(rename = "confirmation-unavailable")]
    ConfirmationUnavailable,
    /// The confirmation response is malformed or mismatched.
    #[serde(rename = "confirmation-invalid")]
    ConfirmationInvalid,
    /// Existing authenticated session and host authority is unavailable.
    #[serde(rename = "policy-context-unavailable")]
    PolicyContextUnavailable,
    /// The original authority or policy changed.
    #[serde(rename = "policy-changed")]
    PolicyChanged,
    /// Current managed policy refuses the operation.
    #[serde(rename = "policy-denied")]
    PolicyDenied,
    /// Configuration changed after the reviewed snapshot.
    #[serde(rename = "configuration-changed")]
    ConfigurationChanged,
    /// Installed configuration no longer matches ownership evidence.
    #[serde(rename = "configuration-modified")]
    ConfigurationModified,
    /// No matching owned resource or original operation exists.
    #[serde(rename = "resource-not-found")]
    ResourceNotFound,
    /// The original plan deadline elapsed.
    #[serde(rename = "plan-expired")]
    PlanExpired,
    /// The one-use plan or prepared operation was already consumed.
    #[serde(rename = "plan-replayed")]
    PlanReplayed,
    /// The handle belongs to a different runtime, session or connection.
    #[serde(rename = "foreign-runtime")]
    ForeignRuntime,
    /// Fresh bound planning is required.
    #[serde(rename = "replan-required")]
    ReplanRequired,
    /// Exact original source revalidation is unsupported.
    #[serde(rename = "source-revalidation-unavailable")]
    SourceRevalidationUnavailable,
    /// The source differs from the retained commitment.
    #[serde(rename = "source-changed")]
    SourceChanged,
    /// The original source could not be retrieved safely.
    #[serde(rename = "source-unavailable")]
    SourceUnavailable,
    /// Authoritative Registry interpretation is unavailable.
    #[serde(rename = "registry-unavailable")]
    RegistryUnavailable,
    /// The selected secret backend is unavailable.
    #[serde(rename = "secret-store-unavailable")]
    SecretStoreUnavailable,
    /// Required owned admission and lifecycle support is absent.
    #[serde(rename = "lifecycle-unavailable")]
    LifecycleUnavailable,
    /// A storage operation failed; inspect any allocated operation before retrying.
    #[serde(rename = "write-failed")]
    WriteFailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpInstallationFailureReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationFailureReason",
            &[
                "invalid-request",
                "operation-limit",
                "cancelled",
                "capability-required",
                "confirmation-unavailable",
                "confirmation-invalid",
                "policy-context-unavailable",
                "policy-changed",
                "policy-denied",
                "configuration-changed",
                "configuration-modified",
                "resource-not-found",
                "plan-expired",
                "plan-replayed",
                "foreign-runtime",
                "replan-required",
                "source-revalidation-unavailable",
                "source-changed",
                "source-unavailable",
                "registry-unavailable",
                "secret-store-unavailable",
                "lifecycle-unavailable",
                "write-failed",
                "Unknown",
            ],
            Some(23),
        )?;
        Ok(match index {
            0 => Self::InvalidRequest,
            1 => Self::OperationLimit,
            2 => Self::Cancelled,
            3 => Self::CapabilityRequired,
            4 => Self::ConfirmationUnavailable,
            5 => Self::ConfirmationInvalid,
            6 => Self::PolicyContextUnavailable,
            7 => Self::PolicyChanged,
            8 => Self::PolicyDenied,
            9 => Self::ConfigurationChanged,
            10 => Self::ConfigurationModified,
            11 => Self::ResourceNotFound,
            12 => Self::PlanExpired,
            13 => Self::PlanReplayed,
            14 => Self::ForeignRuntime,
            15 => Self::ReplanRequired,
            16 => Self::SourceRevalidationUnavailable,
            17 => Self::SourceChanged,
            18 => Self::SourceUnavailable,
            19 => Self::RegistryUnavailable,
            20 => Self::SecretStoreUnavailable,
            21 => Self::LifecycleUnavailable,
            22 => Self::WriteFailed,
            _ => Self::Unknown,
        })
    }
}

/// Installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationManagementOutcomeRecoveryRequiredKind {
    #[serde(rename = "recovery-required")]
    #[default]
    RecoveryRequired,
}
impl<'de> Deserialize<'de> for McpInstallationManagementOutcomeRecoveryRequiredKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationManagementOutcomeRecoveryRequiredKind",
            &["recovery-required"],
            None,
        )?;
        Ok(Self::RecoveryRequired)
    }
}

/// Installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationManagementOutcomeInstallPreparedKind {
    #[serde(rename = "install-prepared")]
    #[default]
    InstallPrepared,
}
impl<'de> Deserialize<'de> for McpInstallationManagementOutcomeInstallPreparedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationManagementOutcomeInstallPreparedKind",
            &["install-prepared"],
            None,
        )?;
        Ok(Self::InstallPrepared)
    }
}

/// Configuration ownership and setup observations, distinct from tool permissions.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationState {
    /// Owned configuration exists; inventory alone does not grant activation.
    #[serde(rename = "needs-setup")]
    NeedsSetup,
    /// The selected authorised session reports an active installation.
    #[serde(rename = "active")]
    Active,
    /// The selected server requires explicit sign-in.
    #[serde(rename = "authentication-required")]
    AuthenticationRequired,
    /// The selected server could not be activated.
    #[serde(rename = "activation-failed")]
    ActivationFailed,
    /// Owned configuration no longer matches its receipt.
    #[serde(rename = "configuration-modified")]
    ConfigurationModified,
    /// Confirmed durable work or unsafe evidence requires recovery.
    #[serde(rename = "recovery-required")]
    RecoveryRequired,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpInstallationState {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationState",
            &[
                "needs-setup",
                "active",
                "authentication-required",
                "activation-failed",
                "configuration-modified",
                "recovery-required",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::NeedsSetup,
            1 => Self::Active,
            2 => Self::AuthenticationRequired,
            3 => Self::ActivationFailed,
            4 => Self::ConfigurationModified,
            5 => Self::RecoveryRequired,
            _ => Self::Unknown,
        })
    }
}

/// Installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationManagementOutcomeListedKind {
    #[serde(rename = "listed")]
    #[default]
    Listed,
}
impl<'de> Deserialize<'de> for McpInstallationManagementOutcomeListedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationManagementOutcomeListedKind",
            &["listed"],
            None,
        )?;
        Ok(Self::Listed)
    }
}

/// Installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationManagementOutcomeRecoveredKind {
    #[serde(rename = "recovered")]
    #[default]
    Recovered,
}
impl<'de> Deserialize<'de> for McpInstallationManagementOutcomeRecoveredKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationManagementOutcomeRecoveredKind",
            &["recovered"],
            None,
        )?;
        Ok(Self::Recovered)
    }
}

/// Installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationManagementOutcomeUninstallPlannedKind {
    #[serde(rename = "uninstall-planned")]
    #[default]
    UninstallPlanned,
}
impl<'de> Deserialize<'de> for McpInstallationManagementOutcomeUninstallPlannedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationManagementOutcomeUninstallPlannedKind",
            &["uninstall-planned"],
            None,
        )?;
        Ok(Self::UninstallPlanned)
    }
}

/// Installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationManagementOutcomeOperationKind {
    #[serde(rename = "operation")]
    #[default]
    Operation,
}
impl<'de> Deserialize<'de> for McpInstallationManagementOutcomeOperationKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationManagementOutcomeOperationKind",
            &["operation"],
            None,
        )?;
        Ok(Self::Operation)
    }
}

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOperationStatusPreparingPhase {
    #[serde(rename = "preparing")]
    #[default]
    Preparing,
}
impl<'de> Deserialize<'de> for McpInstallationOperationStatusPreparingPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOperationStatusPreparingPhase",
            &["preparing"],
            None,
        )?;
        Ok(Self::Preparing)
    }
}

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOperationStatusPreparedPhase {
    #[serde(rename = "prepared")]
    #[default]
    Prepared,
}
impl<'de> Deserialize<'de> for McpInstallationOperationStatusPreparedPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOperationStatusPreparedPhase",
            &["prepared"],
            None,
        )?;
        Ok(Self::Prepared)
    }
}

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOperationStatusAwaitingConfirmationPhase {
    #[serde(rename = "awaiting-confirmation")]
    #[default]
    AwaitingConfirmation,
}
impl<'de> Deserialize<'de> for McpInstallationOperationStatusAwaitingConfirmationPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOperationStatusAwaitingConfirmationPhase",
            &["awaiting-confirmation"],
            None,
        )?;
        Ok(Self::AwaitingConfirmation)
    }
}

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOperationStatusRevalidatingPhase {
    #[serde(rename = "revalidating")]
    #[default]
    Revalidating,
}
impl<'de> Deserialize<'de> for McpInstallationOperationStatusRevalidatingPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOperationStatusRevalidatingPhase",
            &["revalidating"],
            None,
        )?;
        Ok(Self::Revalidating)
    }
}

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOperationStatusApplyingPhase {
    #[serde(rename = "applying")]
    #[default]
    Applying,
}
impl<'de> Deserialize<'de> for McpInstallationOperationStatusApplyingPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOperationStatusApplyingPhase",
            &["applying"],
            None,
        )?;
        Ok(Self::Applying)
    }
}

/// Terminal installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOutcomeInstalledKind {
    #[serde(rename = "installed")]
    #[default]
    Installed,
}
impl<'de> Deserialize<'de> for McpInstallationOutcomeInstalledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOutcomeInstalledKind",
            &["installed"],
            None,
        )?;
        Ok(Self::Installed)
    }
}

/// Terminal installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOutcomeUninstalledKind {
    #[serde(rename = "uninstalled")]
    #[default]
    Uninstalled,
}
impl<'de> Deserialize<'de> for McpInstallationOutcomeUninstalledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOutcomeUninstalledKind",
            &["uninstalled"],
            None,
        )?;
        Ok(Self::Uninstalled)
    }
}

/// Terminal installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOutcomeRolledBackKind {
    #[serde(rename = "rolled-back")]
    #[default]
    RolledBack,
}
impl<'de> Deserialize<'de> for McpInstallationOutcomeRolledBackKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOutcomeRolledBackKind",
            &["rolled-back"],
            None,
        )?;
        Ok(Self::RolledBack)
    }
}

/// Terminal installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOutcomeRecoveryRequiredKind {
    #[serde(rename = "recovery-required")]
    #[default]
    RecoveryRequired,
}
impl<'de> Deserialize<'de> for McpInstallationOutcomeRecoveryRequiredKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOutcomeRecoveryRequiredKind",
            &["recovery-required"],
            None,
        )?;
        Ok(Self::RecoveryRequired)
    }
}

/// Terminal installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOutcomeDeclinedKind {
    #[serde(rename = "declined")]
    #[default]
    Declined,
}
impl<'de> Deserialize<'de> for McpInstallationOutcomeDeclinedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOutcomeDeclinedKind",
            &["declined"],
            None,
        )?;
        Ok(Self::Declined)
    }
}

/// Terminal installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOutcomeCancelledKind {
    #[serde(rename = "cancelled")]
    #[default]
    Cancelled,
}
impl<'de> Deserialize<'de> for McpInstallationOutcomeCancelledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOutcomeCancelledKind",
            &["cancelled"],
            None,
        )?;
        Ok(Self::Cancelled)
    }
}

/// Terminal installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOutcomeRefusedKind {
    #[serde(rename = "refused")]
    #[default]
    Refused,
}
impl<'de> Deserialize<'de> for McpInstallationOutcomeRefusedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOutcomeRefusedKind",
            &["refused"],
            None,
        )?;
        Ok(Self::Refused)
    }
}

/// Terminal mutation result. Uncertainty is not approval, rollback or permission to replay.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpInstallationOutcome {
    Installed(McpInstallationOutcomeInstalled),
    Uninstalled(McpInstallationOutcomeUninstalled),
    RolledBack(McpInstallationOutcomeRolledBack),
    RecoveryRequired(McpInstallationOutcomeRecoveryRequired),
    Declined(McpInstallationOutcomeDeclined),
    Cancelled(McpInstallationOutcomeCancelled),
    Refused(McpInstallationOutcomeRefused),
}

/// Original operation progress discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationOperationStatusCompletedPhase {
    #[serde(rename = "completed")]
    #[default]
    Completed,
}
impl<'de> Deserialize<'de> for McpInstallationOperationStatusCompletedPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationOperationStatusCompletedPhase",
            &["completed"],
            None,
        )?;
        Ok(Self::Completed)
    }
}

/// Status snapshot from the original connection, independent of new-work account availability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpInstallationOperationStatus {
    Preparing(McpInstallationOperationStatusPreparing),
    Prepared(McpInstallationOperationStatusPrepared),
    AwaitingConfirmation(McpInstallationOperationStatusAwaitingConfirmation),
    Revalidating(McpInstallationOperationStatusRevalidating),
    Applying(McpInstallationOperationStatusApplying),
    Completed(McpInstallationOperationStatusCompleted),
}

/// Installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationManagementOutcomeRefusedKind {
    #[serde(rename = "refused")]
    #[default]
    Refused,
}
impl<'de> Deserialize<'de> for McpInstallationManagementOutcomeRefusedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationManagementOutcomeRefusedKind",
            &["refused"],
            None,
        )?;
        Ok(Self::Refused)
    }
}

/// Read-only or recovery management result, never permission to activate or replay.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpInstallationManagementOutcome {
    RecoveryRequired(McpInstallationManagementOutcomeRecoveryRequired),
    InstallPrepared(McpInstallationManagementOutcomeInstallPrepared),
    Listed(McpInstallationManagementOutcomeListed),
    Recovered(McpInstallationManagementOutcomeRecovered),
    UninstallPlanned(McpInstallationManagementOutcomeUninstallPlanned),
    Operation(McpInstallationManagementOutcomeOperation),
    Refused(McpInstallationManagementOutcomeRefused),
}

/// A handled management outcome with an honoured contract.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationManagementResultOutcomeKind {
    #[serde(rename = "outcome")]
    #[default]
    Outcome,
}
impl<'de> Deserialize<'de> for McpInstallationManagementResultOutcomeKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationManagementResultOutcomeKind",
            &["outcome"],
            None,
        )?;
        Ok(Self::Outcome)
    }
}

/// Management result with contract receipt, or a typed request/negotiation refusal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpInstallationManagementResult {
    Outcome(McpInstallationManagementResultOutcome),
    NegotiationRefused(CatalogNegotiationRefusedError),
    InvalidRequest(CatalogInvalidRequestError),
}

/// A handled operation outcome with an honoured contract.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpInstallationResultOutcomeKind {
    #[serde(rename = "outcome")]
    #[default]
    Outcome,
}
impl<'de> Deserialize<'de> for McpInstallationResultOutcomeKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpInstallationResultOutcomeKind",
            &["outcome"],
            None,
        )?;
        Ok(Self::Outcome)
    }
}

/// An installation result together with the exact honoured contract, or a negotiation refusal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpInstallationResult {
    Outcome(McpInstallationResultOutcome),
    NegotiationRefused(CatalogNegotiationRefusedError),
    InvalidRequest(CatalogInvalidRequestError),
}

/// OAuth response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthPendingRequestResponseTokenKind {
    #[serde(rename = "token")]
    #[default]
    Token,
}
impl<'de> Deserialize<'de> for McpOauthPendingRequestResponseTokenKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthPendingRequestResponseTokenKind",
            &["token"],
            None,
        )?;
        Ok(Self::Token)
    }
}

/// OAuth response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthPendingRequestResponseCancelledKind {
    #[serde(rename = "cancelled")]
    #[default]
    Cancelled,
}
impl<'de> Deserialize<'de> for McpOauthPendingRequestResponseCancelledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthPendingRequestResponseCancelledKind",
            &["cancelled"],
            None,
        )?;
        Ok(Self::Cancelled)
    }
}

/// OAuth response variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthPendingRequestResponseRuntimeRefreshKind {
    #[serde(rename = "runtimeRefresh")]
    #[default]
    RuntimeRefresh,
}
impl<'de> Deserialize<'de> for McpOauthPendingRequestResponseRuntimeRefreshKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthPendingRequestResponseRuntimeRefreshKind",
            &["runtimeRefresh"],
            None,
        )?;
        Ok(Self::RuntimeRefresh)
    }
}

/// Host response to the pending OAuth request.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpOauthPendingRequestResponse {
    Token(McpOauthPendingRequestResponseToken),
    Cancelled(McpOauthPendingRequestResponseCancelled),
    RuntimeRefresh(McpOauthPendingRequestResponseRuntimeRefresh),
}

/// Why a passive MCP OAuth probe determined authentication is needed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthProbeNeedsAuthReason {
    /// No token was sent and the server requires authentication.
    #[serde(rename = "initial")]
    Initial,
    /// A cached token was sent and rejected.
    #[serde(rename = "refresh")]
    Refresh,
    /// The server returned a 403 insufficient_scope challenge, indicating additional scopes or audience are needed.
    #[serde(rename = "upscope")]
    Upscope,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpOauthProbeNeedsAuthReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthProbeNeedsAuthReason",
            &["initial", "refresh", "upscope", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Initial,
            1 => Self::Refresh,
            2 => Self::Upscope,
            _ => Self::Unknown,
        })
    }
}

/// Probe outcome variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthProbeResultNoAuthRequiredStatus {
    #[serde(rename = "no-auth-required")]
    #[default]
    NoAuthRequired,
}
impl<'de> Deserialize<'de> for McpOauthProbeResultNoAuthRequiredStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthProbeResultNoAuthRequiredStatus",
            &["no-auth-required"],
            None,
        )?;
        Ok(Self::NoAuthRequired)
    }
}

/// Probe outcome variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthProbeResultAuthenticatedStatus {
    #[serde(rename = "authenticated")]
    #[default]
    Authenticated,
}
impl<'de> Deserialize<'de> for McpOauthProbeResultAuthenticatedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthProbeResultAuthenticatedStatus",
            &["authenticated"],
            None,
        )?;
        Ok(Self::Authenticated)
    }
}

/// Probe outcome variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthProbeResultNeedsAuthStatus {
    #[serde(rename = "needs-auth")]
    #[default]
    NeedsAuth,
}
impl<'de> Deserialize<'de> for McpOauthProbeResultNeedsAuthStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthProbeResultNeedsAuthStatus",
            &["needs-auth"],
            None,
        )?;
        Ok(Self::NeedsAuth)
    }
}

/// Probe outcome variant discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpOauthProbeResultFailedStatus {
    #[serde(rename = "failed")]
    #[default]
    Failed,
}
impl<'de> Deserialize<'de> for McpOauthProbeResultFailedStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpOauthProbeResultFailedStatus",
            &["failed"],
            None,
        )?;
        Ok(Self::Failed)
    }
}

/// Passive MCP OAuth probe result. `authenticated` means the server accepted the probe request while an OAuth-origin access token was attached; it does not prove the server required or independently validated that token. The probe does not make a second unauthenticated request. Failed is an expected probe-domain outcome; JSON-RPC errors are reserved for API-call failures.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpOauthProbeResult {
    NoAuthRequired(McpOauthProbeResultNoAuthRequired),
    Authenticated(McpOauthProbeResultAuthenticated),
    NeedsAuth(McpOauthProbeResultNeedsAuth),
    Failed(McpOauthProbeResultFailed),
}

/// OAuth grant type to use when authenticating to the remote MCP server.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerConfigHttpOauthGrantType {
    /// Interactive browser-based authorization code flow with PKCE.
    #[serde(rename = "authorization_code")]
    AuthorizationCode,
    /// Headless client credentials flow using the configured OAuth client.
    #[serde(rename = "client_credentials")]
    ClientCredentials,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerConfigHttpOauthGrantType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerConfigHttpOauthGrantType",
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
pub(crate) enum McpOauthTestConnectionResult {
    Connected(McpOauthTestConnectionResultConnected),
    NeedsAuth(McpOauthTestConnectionResultNeedsAuth),
    Failed(McpOauthTestConnectionResultFailed),
}

/// Discriminator: a plan was computed and nothing was changed
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanInstallPlannedKind {
    #[serde(rename = "planned")]
    #[default]
    Planned,
}
impl<'de> Deserialize<'de> for McpPlanInstallPlannedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanInstallPlannedKind",
            &["planned"],
            None,
        )?;
        Ok(Self::Planned)
    }
}

/// Discriminator for a candidate-backed install-plan source
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanInstallSourceCandidateKind {
    /// Plan from a candidate returned by catalog search.
    #[serde(rename = "candidate")]
    Candidate,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanInstallSourceCandidateKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanInstallSourceCandidateKind",
            &["candidate", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Candidate,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for a URL-backed MCP server card
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerCardUrlKind {
    /// Retrieve the card from its URL.
    #[serde(rename = "url")]
    Url,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerCardUrlKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerCardUrlKind",
            &["url", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Url,
            _ => Self::Unknown,
        })
    }
}

/// Discriminator for an embedded MCP server card
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerCardEmbeddedKind {
    /// Use the embedded card document.
    #[serde(rename = "embedded")]
    Embedded,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerCardEmbeddedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerCardEmbeddedKind",
            &["embedded", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Embedded,
            _ => Self::Unknown,
        })
    }
}

/// A card supplied directly by the caller. Exactly one of a URL or embedded data, encoded structurally so neither both nor neither can be expressed.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpServerCardReference {
    Url(McpServerCardUrl),
    Embedded(McpServerCardEmbedded),
}

/// Discriminator for a caller-supplied-card install-plan source
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpPlanInstallSourceCardKind {
    /// Plan directly from a caller-supplied card.
    #[serde(rename = "card")]
    Card,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpPlanInstallSourceCardKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpPlanInstallSourceCardKind",
            &["card", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Card,
            _ => Self::Unknown,
        })
    }
}

/// What an install plan is computed from: a candidate handle from a previous search, or a card supplied directly.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpPlanInstallSource {
    Candidate(McpPlanInstallSourceCandidate),
    Card(McpPlanInstallSourceCard),
}

/// Outcome of an mcp.planInstall call: either a normalised plan, or one typed refusal. Nothing is written in either case.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum McpPlanInstallResult {
    Planned(McpPlanInstallPlanned),
    NegotiationRefused(CatalogNegotiationRefusedError),
    HandleRejected(CatalogHandleRejectedError),
    InvalidRequest(CatalogInvalidRequestError),
    AuthenticationRequired(CatalogAuthenticationRequiredError),
    PolicyRejected(CatalogPolicyRejectedError),
    NetworkFailure(CatalogNetworkFailureError),
    UnsafeRetrieval(CatalogUnsafeRetrievalError),
    MalformedCard(CatalogMalformedCardError),
    ContractViolation(CatalogContractViolationError),
    UnavailableTransport(CatalogUnavailableTransportError),
    NotInstallable(CatalogNotInstallableError),
    Unavailable(CatalogUnavailableError),
}

/// Remote transport type. Defaults to "http" when omitted.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerConfigHttpType {
    /// Streamable HTTP transport.
    #[serde(rename = "http")]
    Http,
    /// Server-Sent Events transport.
    #[serde(rename = "sse")]
    Sse,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerConfigHttpType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerConfigHttpType",
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

/// Local transport for a session-owned IDE endpoint.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerConfigLocalType {
    /// Existing Unix socket or Windows named pipe.
    #[serde(rename = "local-http")]
    LocalHttp,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerConfigLocalType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerConfigLocalType",
            &["local-http", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::LocalHttp,
            _ => Self::Unknown,
        })
    }
}

/// In-process MCP transport type.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub(crate) enum McpServerConfigMemoryType {
    #[serde(rename = "memory")]
    Memory,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerConfigMemoryType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerConfigMemoryType",
            &["memory", "Unknown"],
            Some(1),
        )?;
        Ok(match index {
            0 => Self::Memory,
            _ => Self::Unknown,
        })
    }
}

/// Local MCP transport type.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum McpServerConfigStdioType {
    /// Legacy alias for the local stdio transport.
    #[serde(rename = "local")]
    Local,
    /// Server communicates over stdio with a local child process.
    #[serde(rename = "stdio")]
    Stdio,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for McpServerConfigStdioType {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "McpServerConfigStdioType",
            &["local", "stdio", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Local,
            1 => Self::Stdio,
            _ => Self::Unknown,
        })
    }
}

/// The result of a built-in plugin activation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginActivationStatus {
    /// Every enabled sessionStart hook of the plugin completed.
    #[serde(rename = "activated")]
    Activated,
    /// This server has no built-in plugin with the requested name.
    #[serde(rename = "notFound")]
    NotFound,
    /// Settings or managed policy turn off the plugin or its hooks, or managed policy turns on the sandbox. No hook ran.
    #[serde(rename = "disabled")]
    Disabled,
    /// The plugin has no enabled sessionStart hook.
    #[serde(rename = "noHooks")]
    NoHooks,
    /// A hook failed. Later hooks did not run.
    #[serde(rename = "failed")]
    Failed,
    /// A hook did not complete in its timeout. Later hooks did not run.
    #[serde(rename = "timedOut")]
    TimedOut,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PluginActivationStatus {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginActivationStatus",
            &[
                "activated",
                "notFound",
                "disabled",
                "noHooks",
                "failed",
                "timedOut",
                "Unknown",
            ],
            Some(6),
        )?;
        Ok(match index {
            0 => Self::Activated,
            1 => Self::NotFound,
            2 => Self::Disabled,
            3 => Self::NoHooks,
            4 => Self::Failed,
            5 => Self::TimedOut,
            _ => Self::Unknown,
        })
    }
}

/// Bounded refusal categories for verified Agent Plugin installation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationFailureReason {
    /// The selected session does not have the Agent Finder plugin installation feature flag enabled.
    #[serde(rename = "feature-disabled")]
    FeatureDisabled,
    /// The request is unsupported or malformed.
    #[serde(rename = "invalid-request")]
    InvalidRequest,
    /// The original host cannot receive human confirmation.
    #[serde(rename = "confirmation-unavailable")]
    ConfirmationUnavailable,
    /// The confirmation response is malformed or mismatched.
    #[serde(rename = "confirmation-invalid")]
    ConfirmationInvalid,
    /// Existing authenticated session and host authority is unavailable.
    #[serde(rename = "policy-context-unavailable")]
    PolicyContextUnavailable,
    /// The original authority or policy changed.
    #[serde(rename = "policy-changed")]
    PolicyChanged,
    /// Managed policy only allows plugins from known marketplaces, which excludes direct installs.
    #[serde(rename = "policy-blocked")]
    PolicyBlocked,
    /// The original plan deadline elapsed.
    #[serde(rename = "plan-expired")]
    PlanExpired,
    /// The one-use plan was already consumed.
    #[serde(rename = "plan-replayed")]
    PlanReplayed,
    /// The handle belongs to a different runtime, session or connection.
    #[serde(rename = "foreign-runtime")]
    ForeignRuntime,
    /// The handle is not the expected handle kind.
    #[serde(rename = "wrong-kind")]
    WrongKind,
    /// The handle was minted for a different search result or authority.
    #[serde(rename = "search-mismatch")]
    SearchMismatch,
    /// The handle or operation expired.
    #[serde(rename = "expired")]
    Expired,
    /// The candidate handle does not belong to an Agent Plugin with a complete materialisation reference.
    #[serde(rename = "invalid-candidate")]
    InvalidCandidate,
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
    /// The candidate's materialisation is not an Agent Plugin.
    #[serde(rename = "unsupported-resource")]
    UnsupportedResource,
    /// The plugin manifest could not be retrieved or verified.
    #[serde(rename = "entrypoint-unavailable")]
    EntrypointUnavailable,
    /// The plugin manifest is not a valid plugin manifest, or the bundle has another manifest that would take precedence over the reviewed one.
    #[serde(rename = "invalid-plugin")]
    InvalidPlugin,
    /// The plugin manifest exceeds the bounded complete review size.
    #[serde(rename = "review-too-large")]
    ReviewTooLarge,
    /// The reviewed plugin files could not be acquired.
    #[serde(rename = "payload-unavailable")]
    PayloadUnavailable,
    /// The acquired files no longer match the reviewed descriptor.
    #[serde(rename = "payload-mismatch")]
    PayloadMismatch,
    /// GitHub rate limited retrieval from the pinned public source; nothing was installed. Retry after retryAfterSeconds when present.
    #[serde(rename = "source-rate-limited")]
    SourceRateLimited,
    /// No GitHub sign-in or credential is available, so the caller must sign in.
    #[serde(rename = "no-credential")]
    NoCredential,
    /// Agent Finder refused the GitHub credential (HTTP 401, or a 403 without rate-limit evidence); the caller must sign in again.
    #[serde(rename = "credential-rejected")]
    CredentialRejected,
    /// Plugin installation storage or admission is busy.
    #[serde(rename = "busy")]
    Busy,
    /// A plugin with the same name or install location already exists.
    #[serde(rename = "already-installed")]
    AlreadyInstalled,
    /// A storage operation failed; nothing was registered.
    #[serde(rename = "write-failed")]
    WriteFailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for PluginInstallationFailureReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationFailureReason",
            &[
                "feature-disabled",
                "invalid-request",
                "confirmation-unavailable",
                "confirmation-invalid",
                "policy-context-unavailable",
                "policy-changed",
                "policy-blocked",
                "plan-expired",
                "plan-replayed",
                "foreign-runtime",
                "wrong-kind",
                "search-mismatch",
                "expired",
                "invalid-candidate",
                "descriptor-unavailable",
                "descriptor-not-found",
                "descriptor-withdrawn",
                "descriptor-invalid",
                "digest-mismatch",
                "unsupported-resource",
                "entrypoint-unavailable",
                "invalid-plugin",
                "review-too-large",
                "payload-unavailable",
                "payload-mismatch",
                "source-rate-limited",
                "no-credential",
                "credential-rejected",
                "busy",
                "already-installed",
                "write-failed",
                "Unknown",
            ],
            Some(31),
        )?;
        Ok(match index {
            0 => Self::FeatureDisabled,
            1 => Self::InvalidRequest,
            2 => Self::ConfirmationUnavailable,
            3 => Self::ConfirmationInvalid,
            4 => Self::PolicyContextUnavailable,
            5 => Self::PolicyChanged,
            6 => Self::PolicyBlocked,
            7 => Self::PlanExpired,
            8 => Self::PlanReplayed,
            9 => Self::ForeignRuntime,
            10 => Self::WrongKind,
            11 => Self::SearchMismatch,
            12 => Self::Expired,
            13 => Self::InvalidCandidate,
            14 => Self::DescriptorUnavailable,
            15 => Self::DescriptorNotFound,
            16 => Self::DescriptorWithdrawn,
            17 => Self::DescriptorInvalid,
            18 => Self::DigestMismatch,
            19 => Self::UnsupportedResource,
            20 => Self::EntrypointUnavailable,
            21 => Self::InvalidPlugin,
            22 => Self::ReviewTooLarge,
            23 => Self::PayloadUnavailable,
            24 => Self::PayloadMismatch,
            25 => Self::SourceRateLimited,
            26 => Self::NoCredential,
            27 => Self::CredentialRejected,
            28 => Self::Busy,
            29 => Self::AlreadyInstalled,
            30 => Self::WriteFailed,
            _ => Self::Unknown,
        })
    }
}

/// Agent Plugin installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationManagementOutcomeInstallPlannedKind {
    #[serde(rename = "install-planned")]
    #[default]
    InstallPlanned,
}
impl<'de> Deserialize<'de> for PluginInstallationManagementOutcomeInstallPlannedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationManagementOutcomeInstallPlannedKind",
            &["install-planned"],
            None,
        )?;
        Ok(Self::InstallPlanned)
    }
}

/// Agent Plugin installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationManagementOutcomeRefusedKind {
    #[serde(rename = "refused")]
    #[default]
    Refused,
}
impl<'de> Deserialize<'de> for PluginInstallationManagementOutcomeRefusedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationManagementOutcomeRefusedKind",
            &["refused"],
            None,
        )?;
        Ok(Self::Refused)
    }
}

/// Management outcome for verified Agent Plugin installation planning.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PluginInstallationManagementOutcome {
    InstallPlanned(PluginInstallationManagementOutcomeInstallPlanned),
    Refused(PluginInstallationManagementOutcomeRefused),
}

/// Discriminator: handled management outcome.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationManagementResultOutcomeKind {
    #[serde(rename = "outcome")]
    #[default]
    Outcome,
}
impl<'de> Deserialize<'de> for PluginInstallationManagementResultOutcomeKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationManagementResultOutcomeKind",
            &["outcome"],
            None,
        )?;
        Ok(Self::Outcome)
    }
}

/// Agent Plugin installation planning result with the honoured contract, or a typed refusal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PluginInstallationManagementResult {
    Outcome(PluginInstallationManagementResultOutcome),
    NegotiationRefused(CatalogNegotiationRefusedError),
    InvalidRequest(CatalogInvalidRequestError),
    NetworkFailure(CatalogNetworkFailureError),
}

/// Terminal Agent Plugin installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationOutcomeInstalledKind {
    #[serde(rename = "installed")]
    #[default]
    Installed,
}
impl<'de> Deserialize<'de> for PluginInstallationOutcomeInstalledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationOutcomeInstalledKind",
            &["installed"],
            None,
        )?;
        Ok(Self::Installed)
    }
}

/// Terminal Agent Plugin installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationOutcomeDeclinedKind {
    #[serde(rename = "declined")]
    #[default]
    Declined,
}
impl<'de> Deserialize<'de> for PluginInstallationOutcomeDeclinedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationOutcomeDeclinedKind",
            &["declined"],
            None,
        )?;
        Ok(Self::Declined)
    }
}

/// Terminal Agent Plugin installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationOutcomeCancelledKind {
    #[serde(rename = "cancelled")]
    #[default]
    Cancelled,
}
impl<'de> Deserialize<'de> for PluginInstallationOutcomeCancelledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationOutcomeCancelledKind",
            &["cancelled"],
            None,
        )?;
        Ok(Self::Cancelled)
    }
}

/// Terminal Agent Plugin installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationOutcomeRefusedKind {
    #[serde(rename = "refused")]
    #[default]
    Refused,
}
impl<'de> Deserialize<'de> for PluginInstallationOutcomeRefusedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationOutcomeRefusedKind",
            &["refused"],
            None,
        )?;
        Ok(Self::Refused)
    }
}

/// Terminal verified Agent Plugin installation result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PluginInstallationOutcome {
    Installed(PluginInstallationOutcomeInstalled),
    Declined(PluginInstallationOutcomeDeclined),
    Cancelled(PluginInstallationOutcomeCancelled),
    Refused(PluginInstallationOutcomeRefused),
}

/// Discriminator: handled operation outcome.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationResultOutcomeKind {
    #[serde(rename = "outcome")]
    #[default]
    Outcome,
}
impl<'de> Deserialize<'de> for PluginInstallationResultOutcomeKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationResultOutcomeKind",
            &["outcome"],
            None,
        )?;
        Ok(Self::Outcome)
    }
}

/// Agent Plugin installation result with the honoured contract, or a typed request/negotiation refusal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum PluginInstallationResult {
    Outcome(PluginInstallationResultOutcome),
    NegotiationRefused(CatalogNegotiationRefusedError),
    InvalidRequest(CatalogInvalidRequestError),
    NetworkFailure(CatalogNetworkFailureError),
}

/// Exact reviewed Agent Plugin installation action.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum PluginInstallationReviewAction {
    #[serde(rename = "install")]
    #[default]
    Install,
}
impl<'de> Deserialize<'de> for PluginInstallationReviewAction {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "PluginInstallationReviewAction",
            &["install"],
            None,
        )?;
        Ok(Self::Install)
    }
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
pub enum SkillDiscoveryScope {
    /// A project's repository skill directory.
    #[serde(rename = "project")]
    Project,
    /// The user's personal Copilot skill directory.
    #[serde(rename = "personal-copilot")]
    PersonalCopilot,
    /// The user's personal agents skill directory.
    #[serde(rename = "personal-agents")]
    PersonalAgents,
    /// A configured custom skill directory.
    #[serde(rename = "custom")]
    Custom,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SkillDiscoveryScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillDiscoveryScope",
            &[
                "project",
                "personal-copilot",
                "personal-agents",
                "custom",
                "Unknown",
            ],
            Some(4),
        )?;
        Ok(match index {
            0 => Self::Project,
            1 => Self::PersonalCopilot,
            2 => Self::PersonalAgents,
            3 => Self::Custom,
            _ => Self::Unknown,
        })
    }
}

/// Bounded refusal categories for verified Skill installation management.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationFailureReason {
    /// The selected session does not have the Agent Finder Skill installation feature flag enabled for acquisition.
    #[serde(rename = "feature-disabled")]
    FeatureDisabled,
    /// The request is unsupported or malformed.
    #[serde(rename = "invalid-request")]
    InvalidRequest,
    /// The bounded operation limit was reached.
    #[serde(rename = "operation-limit")]
    OperationLimit,
    /// The original operation was cancelled.
    #[serde(rename = "cancelled")]
    Cancelled,
    /// The original host cannot receive human confirmation.
    #[serde(rename = "confirmation-unavailable")]
    ConfirmationUnavailable,
    /// The confirmation response is malformed or mismatched.
    #[serde(rename = "confirmation-invalid")]
    ConfirmationInvalid,
    /// Existing authenticated session and host authority is unavailable.
    #[serde(rename = "policy-context-unavailable")]
    PolicyContextUnavailable,
    /// The original authority or policy changed.
    #[serde(rename = "policy-changed")]
    PolicyChanged,
    /// No matching owned resource or original operation exists.
    #[serde(rename = "resource-not-found")]
    ResourceNotFound,
    /// The original plan deadline elapsed.
    #[serde(rename = "plan-expired")]
    PlanExpired,
    /// The one-use plan was already consumed.
    #[serde(rename = "plan-replayed")]
    PlanReplayed,
    /// The handle belongs to a different runtime, session or connection.
    #[serde(rename = "foreign-runtime")]
    ForeignRuntime,
    /// The handle is not the expected Skill handle kind.
    #[serde(rename = "wrong-kind")]
    WrongKind,
    /// The handle was minted for a different search result or authority.
    #[serde(rename = "search-mismatch")]
    SearchMismatch,
    /// The handle or operation expired.
    #[serde(rename = "expired")]
    Expired,
    /// The candidate handle is not a verified installable Skill candidate.
    #[serde(rename = "invalid-candidate")]
    InvalidCandidate,
    /// The verified Skill descriptor could not be retrieved safely.
    #[serde(rename = "descriptor-unavailable")]
    DescriptorUnavailable,
    /// The verified Skill descriptor failed validation.
    #[serde(rename = "descriptor-invalid")]
    DescriptorInvalid,
    /// The Skill entrypoint could not be retrieved or verified.
    #[serde(rename = "entrypoint-unavailable")]
    EntrypointUnavailable,
    /// The Skill entrypoint is not a valid Skill.
    #[serde(rename = "invalid-skill")]
    InvalidSkill,
    /// The reviewed Skill payload could not be acquired.
    #[serde(rename = "payload-unavailable")]
    PayloadUnavailable,
    /// The acquired payload no longer matches the reviewed descriptor.
    #[serde(rename = "payload-mismatch")]
    PayloadMismatch,
    /// GitHub rate limited retrieval from the pinned public source; nothing was installed. Retry after retryAfterSeconds when present. Reported only to callers that require catalog-reference-installation; others receive payload-unavailable.
    #[serde(rename = "source-rate-limited")]
    SourceRateLimited,
    /// No GitHub sign-in or credential is available, so the caller must sign in. Reported only to callers that require catalog-reference-installation; others receive policy-context-unavailable.
    #[serde(rename = "no-credential")]
    NoCredential,
    /// Agent Finder refused the GitHub credential (HTTP 401, or a 403 without rate-limit evidence); the caller must sign in again. Reported only to callers that require catalog-reference-installation; others receive descriptor-unavailable.
    #[serde(rename = "credential-rejected")]
    CredentialRejected,
    /// The retained Skill source changed after planning.
    #[serde(rename = "source-changed")]
    SourceChanged,
    /// The selected runtime cannot inspect or control the requested lifecycle operation.
    #[serde(rename = "lifecycle-unavailable")]
    LifecycleUnavailable,
    /// Durable Skill installation evidence requires recovery before mutation.
    #[serde(rename = "recovery-required")]
    RecoveryRequired,
    /// The Skill entrypoint exceeds the bounded complete review size.
    #[serde(rename = "review-too-large")]
    ReviewTooLarge,
    /// Skill installation storage or admission is busy.
    #[serde(rename = "busy")]
    Busy,
    /// An owned Skill with the same identity or target already exists.
    #[serde(rename = "already-installed")]
    AlreadyInstalled,
    /// Installed Skill files no longer match ownership evidence.
    #[serde(rename = "configuration-modified")]
    ConfigurationModified,
    /// A storage operation failed; inspect durable state before retrying.
    #[serde(rename = "write-failed")]
    WriteFailed,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SkillInstallationFailureReason {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationFailureReason",
            &[
                "feature-disabled",
                "invalid-request",
                "operation-limit",
                "cancelled",
                "confirmation-unavailable",
                "confirmation-invalid",
                "policy-context-unavailable",
                "policy-changed",
                "resource-not-found",
                "plan-expired",
                "plan-replayed",
                "foreign-runtime",
                "wrong-kind",
                "search-mismatch",
                "expired",
                "invalid-candidate",
                "descriptor-unavailable",
                "descriptor-invalid",
                "entrypoint-unavailable",
                "invalid-skill",
                "payload-unavailable",
                "payload-mismatch",
                "source-rate-limited",
                "no-credential",
                "credential-rejected",
                "source-changed",
                "lifecycle-unavailable",
                "recovery-required",
                "review-too-large",
                "busy",
                "already-installed",
                "configuration-modified",
                "write-failed",
                "Unknown",
            ],
            Some(33),
        )?;
        Ok(match index {
            0 => Self::FeatureDisabled,
            1 => Self::InvalidRequest,
            2 => Self::OperationLimit,
            3 => Self::Cancelled,
            4 => Self::ConfirmationUnavailable,
            5 => Self::ConfirmationInvalid,
            6 => Self::PolicyContextUnavailable,
            7 => Self::PolicyChanged,
            8 => Self::ResourceNotFound,
            9 => Self::PlanExpired,
            10 => Self::PlanReplayed,
            11 => Self::ForeignRuntime,
            12 => Self::WrongKind,
            13 => Self::SearchMismatch,
            14 => Self::Expired,
            15 => Self::InvalidCandidate,
            16 => Self::DescriptorUnavailable,
            17 => Self::DescriptorInvalid,
            18 => Self::EntrypointUnavailable,
            19 => Self::InvalidSkill,
            20 => Self::PayloadUnavailable,
            21 => Self::PayloadMismatch,
            22 => Self::SourceRateLimited,
            23 => Self::NoCredential,
            24 => Self::CredentialRejected,
            25 => Self::SourceChanged,
            26 => Self::LifecycleUnavailable,
            27 => Self::RecoveryRequired,
            28 => Self::ReviewTooLarge,
            29 => Self::Busy,
            30 => Self::AlreadyInstalled,
            31 => Self::ConfigurationModified,
            32 => Self::WriteFailed,
            _ => Self::Unknown,
        })
    }
}

/// Skill installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationManagementOutcomeRecoveryRequiredKind {
    #[serde(rename = "recovery-required")]
    #[default]
    RecoveryRequired,
}
impl<'de> Deserialize<'de> for SkillInstallationManagementOutcomeRecoveryRequiredKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationManagementOutcomeRecoveryRequiredKind",
            &["recovery-required"],
            None,
        )?;
        Ok(Self::RecoveryRequired)
    }
}

/// Skill installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationManagementOutcomeInstallPlannedKind {
    #[serde(rename = "install-planned")]
    #[default]
    InstallPlanned,
}
impl<'de> Deserialize<'de> for SkillInstallationManagementOutcomeInstallPlannedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationManagementOutcomeInstallPlannedKind",
            &["install-planned"],
            None,
        )?;
        Ok(Self::InstallPlanned)
    }
}

/// Skill installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationManagementOutcomeListedKind {
    #[serde(rename = "listed")]
    #[default]
    Listed,
}
impl<'de> Deserialize<'de> for SkillInstallationManagementOutcomeListedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationManagementOutcomeListedKind",
            &["listed"],
            None,
        )?;
        Ok(Self::Listed)
    }
}

/// Skill installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationManagementOutcomeRecoveredKind {
    #[serde(rename = "recovered")]
    #[default]
    Recovered,
}
impl<'de> Deserialize<'de> for SkillInstallationManagementOutcomeRecoveredKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationManagementOutcomeRecoveredKind",
            &["recovered"],
            None,
        )?;
        Ok(Self::Recovered)
    }
}

/// Skill installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationManagementOutcomeRolledBackKind {
    #[serde(rename = "rolled-back")]
    #[default]
    RolledBack,
}
impl<'de> Deserialize<'de> for SkillInstallationManagementOutcomeRolledBackKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationManagementOutcomeRolledBackKind",
            &["rolled-back"],
            None,
        )?;
        Ok(Self::RolledBack)
    }
}

/// Skill installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationManagementOutcomeUninstallPlannedKind {
    #[serde(rename = "uninstall-planned")]
    #[default]
    UninstallPlanned,
}
impl<'de> Deserialize<'de> for SkillInstallationManagementOutcomeUninstallPlannedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationManagementOutcomeUninstallPlannedKind",
            &["uninstall-planned"],
            None,
        )?;
        Ok(Self::UninstallPlanned)
    }
}

/// Skill installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationManagementOutcomeOperationKind {
    #[serde(rename = "operation")]
    #[default]
    Operation,
}
impl<'de> Deserialize<'de> for SkillInstallationManagementOutcomeOperationKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationManagementOutcomeOperationKind",
            &["operation"],
            None,
        )?;
        Ok(Self::Operation)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOperationStatusPreparingPhase {
    #[serde(rename = "preparing")]
    #[default]
    Preparing,
}
impl<'de> Deserialize<'de> for SkillInstallationOperationStatusPreparingPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOperationStatusPreparingPhase",
            &["preparing"],
            None,
        )?;
        Ok(Self::Preparing)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOperationStatusPreparedPhase {
    #[serde(rename = "prepared")]
    #[default]
    Prepared,
}
impl<'de> Deserialize<'de> for SkillInstallationOperationStatusPreparedPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOperationStatusPreparedPhase",
            &["prepared"],
            None,
        )?;
        Ok(Self::Prepared)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOperationStatusAwaitingConfirmationPhase {
    #[serde(rename = "awaiting-confirmation")]
    #[default]
    AwaitingConfirmation,
}
impl<'de> Deserialize<'de> for SkillInstallationOperationStatusAwaitingConfirmationPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOperationStatusAwaitingConfirmationPhase",
            &["awaiting-confirmation"],
            None,
        )?;
        Ok(Self::AwaitingConfirmation)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOperationStatusRevalidatingPhase {
    #[serde(rename = "revalidating")]
    #[default]
    Revalidating,
}
impl<'de> Deserialize<'de> for SkillInstallationOperationStatusRevalidatingPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOperationStatusRevalidatingPhase",
            &["revalidating"],
            None,
        )?;
        Ok(Self::Revalidating)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOperationStatusApplyingPhase {
    #[serde(rename = "applying")]
    #[default]
    Applying,
}
impl<'de> Deserialize<'de> for SkillInstallationOperationStatusApplyingPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOperationStatusApplyingPhase",
            &["applying"],
            None,
        )?;
        Ok(Self::Applying)
    }
}

/// Terminal Skill installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOutcomeInstalledKind {
    #[serde(rename = "installed")]
    #[default]
    Installed,
}
impl<'de> Deserialize<'de> for SkillInstallationOutcomeInstalledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOutcomeInstalledKind",
            &["installed"],
            None,
        )?;
        Ok(Self::Installed)
    }
}

/// Terminal Skill installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOutcomeUninstalledKind {
    #[serde(rename = "uninstalled")]
    #[default]
    Uninstalled,
}
impl<'de> Deserialize<'de> for SkillInstallationOutcomeUninstalledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOutcomeUninstalledKind",
            &["uninstalled"],
            None,
        )?;
        Ok(Self::Uninstalled)
    }
}

/// Terminal Skill installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOutcomeRolledBackKind {
    #[serde(rename = "rolled-back")]
    #[default]
    RolledBack,
}
impl<'de> Deserialize<'de> for SkillInstallationOutcomeRolledBackKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOutcomeRolledBackKind",
            &["rolled-back"],
            None,
        )?;
        Ok(Self::RolledBack)
    }
}

/// Terminal Skill installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOutcomeRecoveryRequiredKind {
    #[serde(rename = "recovery-required")]
    #[default]
    RecoveryRequired,
}
impl<'de> Deserialize<'de> for SkillInstallationOutcomeRecoveryRequiredKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOutcomeRecoveryRequiredKind",
            &["recovery-required"],
            None,
        )?;
        Ok(Self::RecoveryRequired)
    }
}

/// Terminal Skill installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOutcomeDeclinedKind {
    #[serde(rename = "declined")]
    #[default]
    Declined,
}
impl<'de> Deserialize<'de> for SkillInstallationOutcomeDeclinedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOutcomeDeclinedKind",
            &["declined"],
            None,
        )?;
        Ok(Self::Declined)
    }
}

/// Terminal Skill installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOutcomeCancelledKind {
    #[serde(rename = "cancelled")]
    #[default]
    Cancelled,
}
impl<'de> Deserialize<'de> for SkillInstallationOutcomeCancelledKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOutcomeCancelledKind",
            &["cancelled"],
            None,
        )?;
        Ok(Self::Cancelled)
    }
}

/// Terminal Skill installation outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOutcomeRefusedKind {
    #[serde(rename = "refused")]
    #[default]
    Refused,
}
impl<'de> Deserialize<'de> for SkillInstallationOutcomeRefusedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOutcomeRefusedKind",
            &["refused"],
            None,
        )?;
        Ok(Self::Refused)
    }
}

/// Terminal verified Skill mutation result.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SkillInstallationOutcome {
    Installed(SkillInstallationOutcomeInstalled),
    Uninstalled(SkillInstallationOutcomeUninstalled),
    RolledBack(SkillInstallationOutcomeRolledBack),
    RecoveryRequired(SkillInstallationOutcomeRecoveryRequired),
    Declined(SkillInstallationOutcomeDeclined),
    Cancelled(SkillInstallationOutcomeCancelled),
    Refused(SkillInstallationOutcomeRefused),
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationOperationStatusCompletedPhase {
    #[serde(rename = "completed")]
    #[default]
    Completed,
}
impl<'de> Deserialize<'de> for SkillInstallationOperationStatusCompletedPhase {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationOperationStatusCompletedPhase",
            &["completed"],
            None,
        )?;
        Ok(Self::Completed)
    }
}

/// Status snapshot from the original connection, independent of new-work account availability.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SkillInstallationOperationStatus {
    Preparing(SkillInstallationOperationStatusPreparing),
    Prepared(SkillInstallationOperationStatusPrepared),
    AwaitingConfirmation(SkillInstallationOperationStatusAwaitingConfirmation),
    Revalidating(SkillInstallationOperationStatusRevalidating),
    Applying(SkillInstallationOperationStatusApplying),
    Completed(SkillInstallationOperationStatusCompleted),
}

/// Skill installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationManagementOutcomeEnabledChangedKind {
    #[serde(rename = "enabled-changed")]
    #[default]
    EnabledChanged,
}
impl<'de> Deserialize<'de> for SkillInstallationManagementOutcomeEnabledChangedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationManagementOutcomeEnabledChangedKind",
            &["enabled-changed"],
            None,
        )?;
        Ok(Self::EnabledChanged)
    }
}

/// Skill installation management outcome discriminator.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationManagementOutcomeRefusedKind {
    #[serde(rename = "refused")]
    #[default]
    Refused,
}
impl<'de> Deserialize<'de> for SkillInstallationManagementOutcomeRefusedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationManagementOutcomeRefusedKind",
            &["refused"],
            None,
        )?;
        Ok(Self::Refused)
    }
}

/// Management outcome for verified Skill inventory, planning and removal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SkillInstallationManagementOutcome {
    RecoveryRequired(SkillInstallationManagementOutcomeRecoveryRequired),
    InstallPlanned(SkillInstallationManagementOutcomeInstallPlanned),
    Listed(SkillInstallationManagementOutcomeListed),
    Recovered(SkillInstallationManagementOutcomeRecovered),
    RolledBack(SkillInstallationManagementOutcomeRolledBack),
    UninstallPlanned(SkillInstallationManagementOutcomeUninstallPlanned),
    Operation(SkillInstallationManagementOutcomeOperation),
    EnabledChanged(SkillInstallationManagementOutcomeEnabledChanged),
    Refused(SkillInstallationManagementOutcomeRefused),
}

/// Discriminator: handled management outcome.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationManagementResultOutcomeKind {
    #[serde(rename = "outcome")]
    #[default]
    Outcome,
}
impl<'de> Deserialize<'de> for SkillInstallationManagementResultOutcomeKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationManagementResultOutcomeKind",
            &["outcome"],
            None,
        )?;
        Ok(Self::Outcome)
    }
}

/// Skill installation management result with the honoured contract, or a typed refusal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SkillInstallationManagementResult {
    Outcome(SkillInstallationManagementResultOutcome),
    NegotiationRefused(CatalogNegotiationRefusedError),
    InvalidRequest(CatalogInvalidRequestError),
    NetworkFailure(CatalogNetworkFailureError),
}

/// Discriminator: handled operation outcome.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallationResultOutcomeKind {
    #[serde(rename = "outcome")]
    #[default]
    Outcome,
}
impl<'de> Deserialize<'de> for SkillInstallationResultOutcomeKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallationResultOutcomeKind",
            &["outcome"],
            None,
        )?;
        Ok(Self::Outcome)
    }
}

/// Skill installation result with the honoured contract, or a typed request/negotiation refusal.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SkillInstallationResult {
    Outcome(SkillInstallationResultOutcome),
    NegotiationRefused(CatalogNegotiationRefusedError),
    InvalidRequest(CatalogInvalidRequestError),
    NetworkFailure(CatalogNetworkFailureError),
}

/// Source acquisition used by a direct skill installation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallSourceKind {
    /// A skill file retrieved through HTTPS.
    #[serde(rename = "url")]
    Url,
    /// A local skill file copied into skill storage.
    #[serde(rename = "file")]
    File,
    /// A discovery directory registered without copying its contents.
    #[serde(rename = "directory")]
    Directory,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SkillInstallSourceKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallSourceKind",
            &["url", "file", "directory", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Url,
            1 => Self::File,
            2 => Self::Directory,
            _ => Self::Unknown,
        })
    }
}

/// Destination scope for a directly installed skill file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillInstallSourceScope {
    /// Personal skill storage.
    #[serde(rename = "personal")]
    Personal,
    /// Project skill storage.
    #[serde(rename = "project")]
    Project,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SkillInstallSourceScope {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillInstallSourceScope",
            &["personal", "project", "Unknown"],
            Some(2),
        )?;
        Ok(match index {
            0 => Self::Personal,
            1 => Self::Project,
            _ => Self::Unknown,
        })
    }
}

/// Resource removed by a direct-source operation.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillRemoveSourceKind {
    /// A discovery registration removed without deleting its contents.
    #[serde(rename = "directory")]
    Directory,
    /// A personal skill file or directory.
    #[serde(rename = "personal")]
    Personal,
    /// A project skill file or directory.
    #[serde(rename = "project")]
    Project,
    /// Unknown variant for forward compatibility.
    #[default]
    #[serde(other)]
    Unknown,
}
impl<'de> Deserialize<'de> for SkillRemoveSourceKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let index = crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillRemoveSourceKind",
            &["directory", "personal", "project", "Unknown"],
            Some(3),
        )?;
        Ok(match index {
            0 => Self::Directory,
            1 => Self::Personal,
            2 => Self::Project,
            _ => Self::Unknown,
        })
    }
}

/// Direct source removal or an inventory request without mutation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillsRemoveSourceOutcomeNeedsInventoryKind {
    #[serde(rename = "needs-inventory")]
    #[default]
    NeedsInventory,
}
impl<'de> Deserialize<'de> for SkillsRemoveSourceOutcomeNeedsInventoryKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillsRemoveSourceOutcomeNeedsInventoryKind",
            &["needs-inventory"],
            None,
        )?;
        Ok(Self::NeedsInventory)
    }
}

/// Direct source removal or an inventory request without mutation.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub enum SkillsRemoveSourceOutcomeRemovedKind {
    #[serde(rename = "removed")]
    #[default]
    Removed,
}
impl<'de> Deserialize<'de> for SkillsRemoveSourceOutcomeRemovedKind {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        crate::enum_serde::deserialize_variant(
            deserializer,
            "SkillsRemoveSourceOutcomeRemovedKind",
            &["removed"],
            None,
        )?;
        Ok(Self::Removed)
    }
}

/// Direct removal or a request for the inventory needed to resolve a file.
///
/// <div class="warning">
///
/// **Experimental.** This type is part of an experimental wire-protocol surface
/// and may change or be removed in future SDK or CLI releases.
///
/// </div>
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SkillsRemoveSourceOutcome {
    NeedsInventory(SkillsRemoveSourceOutcomeNeedsInventory),
    Removed(SkillsRemoveSourceOutcomeRemoved),
}

/// Compatibility alias for the confirmation review wrapper.
pub type InstallationConfirmationRequestReview = InstallationReview;

/// Resource discriminator for installation confirmation reviews.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum InstallationReviewResource {
    #[serde(rename = "mcp")]
    #[default]
    Mcp,
    #[serde(rename = "skill")]
    Skill,
}

/// Compatibility alias for the confirmation review resource discriminator.
pub type InstallationConfirmationRequestReviewResource = InstallationReviewResource;
