// Copyright (c) Microsoft Corporation. All rights reserved.

//! CopilotClient - Main entry point for the Copilot SDK.
//!
//! This module provides the [`CopilotClient`] struct, which manages the connection
//! to the Copilot CLI server and provides session management capabilities.

use std::collections::HashMap;
use std::sync::Arc;

use serde_json::Value;
use tokio::net::TcpStream;
use tokio::process::{Child, Command};
use tokio::sync::Mutex;

use crate::jsonrpc::{JsonRpcClient, NotificationHandler, RequestHandler};
use crate::sdk_protocol_version::get_sdk_protocol_version;
use crate::session::{CopilotSession, GitHubTokenRegistration, GitHubTokenRegistry};
use crate::types::*;
use crate::CopilotError;

/// Moves the Auto routing tier, WebSocket transport, and CAPI policy options into the
/// `capi` wire object (grouped as `github.capi` by [`group_session_params`]).
fn insert_capi_options(
    params: &mut Value,
    auto_tier: Option<AutoTier>,
    enable_web_socket_responses: Option<bool>,
    capi_config: Option<&CapiConfig>,
) {
    let mut capi = serde_json::Map::new();
    if let Some(enabled) = capi_config.and_then(|c| c.enabled) {
        capi.insert("enabled".to_string(), Value::Bool(enabled));
    }
    if let Some(tier) = auto_tier {
        if let Ok(value) = serde_json::to_value(tier) {
            capi.insert("autoTier".to_string(), value);
        }
    }
    if let Some(enabled) = enable_web_socket_responses {
        capi.insert("enableWebSocketResponses".to_string(), Value::Bool(enabled));
    }
    if !capi.is_empty() {
        if let Some(obj) = params.as_object_mut() {
            obj.insert("capi".to_string(), Value::Object(capi));
        }
    }
}

/// Sets a flat capability flag (for example `requestShellAuthentication`) on the params.
fn insert_flag(params: &mut Value, key: &str) {
    if let Some(obj) = params.as_object_mut() {
        obj.insert(key.to_string(), Value::Bool(true));
    }
}

/// Application callbacks declared on a session config and installed on the opened session.
struct SessionCallbacks {
    shell_authentication: Option<ShellAuthenticationHandlerFn>,
    memory_store: Option<MemoryStoreHandlerFn>,
}

impl SessionCallbacks {
    /// Adds the capability flags that tell the runtime which callbacks are registered
    /// (`interaction.requestShellAuthentication`, `state.hasMemoryStore`).
    fn declare(&self, params: &mut Value) {
        if self.shell_authentication.is_some() {
            insert_flag(params, "requestShellAuthentication");
        }
        if self.memory_store.is_some() {
            insert_flag(params, "hasMemoryStore");
        }
    }
}

// ============================================================================
// SDK protocol 4: grouped session payloads
// ============================================================================

/// Option groups of the SDK protocol 4 `session.create` / `session.resume` payloads.
const SESSION_OPTION_GROUPS: &[&str] = &[
    "inference",
    "tools",
    "permissions",
    "interaction",
    "events",
    "agents",
    "skills",
    "mcp",
    "github",
    "filesystem",
    "execution",
    "instructions",
    "hooks",
    "git",
    "state",
    "plugins",
    "canvases",
    "extensions",
    "experiments",
    "policy",
];

/// Keys valid only on `session.create`; dropped from resume payloads.
const CREATE_ONLY_SESSION_KEYS: &[&str] = &[
    "name",
    "cloud",
    "trustWorkingDirectory",
    "refreshCustomInstructions",
];

/// Keys valid only on `session.resume`; dropped from create payloads.
const RESUME_ONLY_SESSION_KEYS: &[&str] = &[
    "suppressResumeEvent",
    "continuePendingWork",
    "allowTranscriptRecovery",
    "requestedEnvironmentVariables",
    "openCanvases",
];

/// Flat session option key -> SDK protocol 4 `(group, member)`. Keys not listed here
/// (such as `sessionId`, `model`, `workingDirectory`, `systemMessage`) stay at the root.
const SESSION_OPTION_PATHS: &[(&str, &str, &str)] = &[
    ("reasoningEffort", "inference", "reasoningEffort"),
    ("reasoningSummary", "inference", "reasoningSummary"),
    ("contextTier", "inference", "contextTier"),
    ("allowMultipleToolCalls", "inference", "allowMultipleToolCalls"),
    ("enableCitations", "inference", "enableCitations"),
    ("modelCapabilities", "inference", "capabilities"),
    ("verbosity", "inference", "verbosity"),
    ("providers", "inference", "providers"),
    ("models", "inference", "models"),
    ("defaultProvider", "inference", "defaultProvider"),
    ("tools", "tools", "definitions"),
    ("availableTools", "tools", "availableTools"),
    ("excludedTools", "tools", "excludedTools"),
    ("toolSearch", "tools", "search"),
    ("largeOutput", "tools", "largeOutput"),
    ("imageGeneration", "tools", "imageGeneration"),
    ("manageScheduleEnabled", "tools", "manageScheduleEnabled"),
    ("webSearch", "tools", "webSearch"),
    ("shell", "tools", "shell"),
    ("maxInlineBinaryBytes", "tools", "maxInlineBinaryBytes"),
    ("requestPermission", "permissions", "request"),
    ("sandboxConfig", "permissions", "sandbox"),
    ("sandbox", "permissions", "sandbox"),
    ("permissionMode", "permissions", "mode"),
    ("sandboxConfigSource", "permissions", "sandboxSource"),
    ("commands", "interaction", "commands"),
    ("askUserVariant", "interaction", "askUserVariant"),
    ("requestUserInput", "interaction", "requestUserInput"),
    ("requestElicitation", "interaction", "requestElicitation"),
    ("requestShellAuthentication", "interaction", "requestShellAuthentication"),
    ("requestExitPlanMode", "interaction", "requestExitPlanMode"),
    ("requestAutoModeSwitch", "interaction", "requestAutoModeSwitch"),
    ("requestAutoModeRecovery", "interaction", "requestAutoModeRecovery"),
    ("streaming", "events", "streaming"),
    ("includeSubAgentStreamingEvents", "events", "includeSubAgentStreamingEvents"),
    ("customAgents", "agents", "definitions"),
    ("defaultAgent", "agents", "default"),
    ("agent", "agents", "selected"),
    ("includedBuiltinAgents", "agents", "includedBuiltinAgents"),
    ("excludedBuiltinAgents", "agents", "excludedBuiltinAgents"),
    ("customAgentsLocalOnly", "agents", "localDiscoveryOnly"),
    ("agentDirectories", "agents", "directories"),
    ("skillDirectories", "skills", "directories"),
    ("enableSkills", "skills", "enabled"),
    ("includedBuiltinSkills", "skills", "includedBuiltinSkills"),
    ("disabledSkills", "skills", "disabledSkills"),
    ("hasSkillProvider", "skills", "hasProvider"),
    ("ignoredSkillsLocations", "skills", "ignoredLocations"),
    ("mcpServers", "mcp", "servers"),
    ("disabledMcpServers", "mcp", "disabledServers"),
    ("requestMcpApps", "mcp", "enableApps"),
    ("enableMcpApps", "mcp", "enableApps"),
    ("mcpOAuthTokenStorage", "mcp", "oauthTokenStorage"),
    ("authClientIdMetadataUrl", "mcp", "authClientIdMetadataUrl"),
    ("envValueMode", "mcp", "envValueMode"),
    ("managedMcpServers", "mcp", "managedServers"),
    ("allowAllMcpServerInstructions", "mcp", "allowAllServerInstructions"),
    ("gitHubToken", "github", "token"),
    // This SDK historically serialized `github_token` as `githubToken`.
    ("githubToken", "github", "token"),
    ("gitHubTokenProviderRegistrationId", "github", "tokenProviderRegistrationId"),
    ("githubMcpToolConfig", "github", "mcp"),
    ("capi", "github", "capi"),
    ("enableSessionTelemetry", "github", "telemetryEnabled"),
    ("enableManagedSettings", "github", "fetchManagedSettings"),
    ("interactiveUser", "github", "interactiveUser"),
    ("enableGitHubTelemetryForwarding", "github", "telemetryForwarding"),
    ("authInfo", "github", "authInfo"),
    ("copilotUrl", "github", "copilotUrl"),
    ("additionalDirectories", "filesystem", "additionalDirectories"),
    ("enableFileChangeTracking", "filesystem", "trackFileChanges"),
    ("trustWorkingDirectory", "filesystem", "trustWorkingDirectory"),
    ("sessionLimits", "execution", "limits"),
    ("autopilot", "execution", "autopilot"),
    ("autopilotContinuation", "execution", "autopilotContinuation"),
    ("instructionDirectories", "instructions", "directories"),
    ("organizationCustomInstructions", "instructions", "organizationText"),
    ("enableOnDemandInstructionDiscovery", "instructions", "onDemandDiscovery"),
    ("skipCustomInstructions", "instructions", "skipCustomInstructions"),
    ("refreshCustomInstructions", "instructions", "refreshCustomInstructions"),
    ("hooks", "hooks", "hasHandlers"),
    ("hookDirectories", "hooks", "directories"),
    ("enableFileHooks", "hooks", "enableRepositoryHooks"),
    ("coauthorEnabled", "git", "coauthorEnabled"),
    ("enableHostGitOperations", "git", "enableHostGitOperations"),
    ("infiniteSessions", "state", "infiniteSessions"),
    ("memory", "state", "memory"),
    ("hasMemoryStore", "state", "hasMemoryStore"),
    ("enableSessionStore", "state", "enableCrossSessionStore"),
    ("skipEmbeddingRetrieval", "state", "skipEmbeddingRetrieval"),
    ("embeddingCacheStorage", "state", "embeddingCacheStorage"),
    ("pluginDirectories", "plugins", "directories"),
    ("installedPlugins", "plugins", "installed"),
    ("canvases", "canvases", "definitions"),
    ("canvasProvider", "canvases", "provider"),
    ("requestCanvasRenderer", "canvases", "requestRenderer"),
    ("openCanvases", "canvases", "open"),
    ("requestExtensions", "extensions", "enabled"),
    ("extensionSdkPath", "extensions", "sdkPath"),
    ("extensionInfo", "extensions", "identity"),
    ("enableExperimentalMode", "experiments", "enableExperimentalMode"),
    ("isExperimentalMode", "experiments", "enableExperimentalMode"),
    // This SDK historically sent the experimental-mode flag as `experimentalMode`.
    ("experimentalMode", "experiments", "enableExperimentalMode"),
    ("featureFlags", "experiments", "flags"),
    ("expAssignments", "experiments", "assignments"),
    ("featureFlagConfiguration", "experiments", "configuration"),
    ("allowedModels", "policy", "allowedModels"),
    ("managedSettings", "policy", "managedSettings"),
    ("excludedManagedSettings", "policy", "excludedManagedSettings"),
    ("enforceManagedModelDefaults", "policy", "enforceManagedModelDefaults"),
];

/// Legacy `provider` fields that describe the model rather than the connection.
const LEGACY_PROVIDER_MODEL_FIELDS: &[&str] = &[
    "modelId",
    "wireModel",
    "maxPromptTokens",
    "maxContextWindowTokens",
    "maxOutputTokens",
    "capabilities",
];

/// Defaults applied under the caller's values when the client runs in `ClientMode::Empty`.
fn empty_mode_defaults() -> [(&'static str, &'static str, Value); 10] {
    [
        ("experiments", "enableExperimentalMode", Value::Bool(false)),
        ("github", "telemetryEnabled", Value::Bool(false)),
        ("mcp", "oauthTokenStorage", Value::String("in-memory".to_string())),
        ("state", "skipEmbeddingRetrieval", Value::Bool(true)),
        ("state", "embeddingCacheStorage", Value::String("in-memory".to_string())),
        ("state", "enableCrossSessionStore", Value::Bool(false)),
        ("state", "memory", serde_json::json!({ "enabled": false })),
        ("instructions", "onDemandDiscovery", Value::Bool(false)),
        ("hooks", "enableRepositoryHooks", Value::Bool(false)),
        ("git", "enableHostGitOperations", Value::Bool(false)),
    ]
}

/// Returns the member object of `group`, creating it when missing.
fn group_members<'a>(
    groups: &'a mut serde_json::Map<String, Value>,
    group: &str,
) -> &'a mut serde_json::Map<String, Value> {
    let entry = groups
        .entry(group.to_string())
        .or_insert_with(|| Value::Object(serde_json::Map::new()));
    if !entry.is_object() {
        *entry = Value::Object(serde_json::Map::new());
    }
    match entry {
        Value::Object(members) => members,
        _ => unreachable!("group entry was just made an object"),
    }
}

/// Converts the flat session payload this SDK builds into the SDK protocol 4 grouped
/// payload sent with `session.create` (`is_resume == false`) and `session.resume`.
///
/// Protocol 4 runtimes reject the flat keys and any unknown group member, while
/// unknown root keys are tolerated. So known flat keys move to their exact
/// `group.member` (for example `streaming` to `events.streaming` and `hooks: true` to
/// `hooks.hasHandlers`), root keys and unrecognized keys stay at the top level, `null`
/// values are skipped (except an explicit `defaultProvider: null` on resume, which
/// clears the default), members invalid for the method are dropped
/// (`filesystem.trustWorkingDirectory` and `instructions.refreshCustomInstructions` on
/// resume, `canvases.open` on create), and empty groups are not emitted.
/// Already-grouped objects are merged; their members win. The legacy single `provider`
/// becomes a named `"default"` connection unless `providers`, `defaultProvider`, or
/// `capi.enabled` are set explicitly.
pub(crate) fn group_session_params(flat: Value, is_resume: bool) -> Value {
    let flat = match flat {
        Value::Object(flat) => flat,
        other => return other,
    };

    let mut root = serde_json::Map::new();
    let mut groups = serde_json::Map::new();
    let mut explicit_groups = serde_json::Map::new();
    let mut legacy_provider = None;

    for (key, value) in flat {
        let clears_default_provider = is_resume && key == "defaultProvider";
        if value.is_null() && !clears_default_provider {
            continue;
        }
        let key_str = key.as_str();
        if (is_resume && CREATE_ONLY_SESSION_KEYS.contains(&key_str))
            || (!is_resume && RESUME_ONLY_SESSION_KEYS.contains(&key_str))
        {
            continue;
        }
        match key_str {
            "provider" => {
                legacy_provider = Some(value);
                continue;
            }
            "configDir" => {
                root.entry("configDirectory".to_string()).or_insert(value);
                continue;
            }
            "disableResume" => {
                if is_resume {
                    root.entry("suppressResumeEvent".to_string()).or_insert(value);
                }
                continue;
            }
            _ => {}
        }
        if SESSION_OPTION_GROUPS.contains(&key_str) && value.is_object() {
            explicit_groups.insert(key, value);
        } else if let Some((_, group, member)) =
            SESSION_OPTION_PATHS.iter().find(|(flat_key, _, _)| *flat_key == key_str)
        {
            group_members(&mut groups, group).insert(member.to_string(), value);
        } else {
            root.insert(key, value);
        }
    }

    // Explicitly grouped members win over members moved from flat keys.
    for (group, members) in explicit_groups {
        if let Value::Object(members) = members {
            let target = group_members(&mut groups, &group);
            for (member, value) in members {
                if !value.is_null() || (is_resume && group == "inference" && member == "defaultProvider") {
                    target.insert(member, value);
                }
            }
        }
    }

    // The runtime rejects members that are invalid for this lifecycle method.
    let excluded_members: &[(&str, &str)] = if is_resume {
        &[
            ("filesystem", "trustWorkingDirectory"),
            ("instructions", "refreshCustomInstructions"),
        ]
    } else {
        &[("canvases", "open")]
    };
    for (group, member) in excluded_members {
        if let Some(Value::Object(members)) = groups.get_mut(*group) {
            members.remove(*member);
        }
    }

    if let Some(provider) = legacy_provider {
        translate_legacy_provider(provider, root.get("model"), &mut groups);
    }

    if let Some(Value::Object(mcp)) = groups.get_mut("mcp") {
        if mcp.get("servers").is_some_and(|servers| !servers.is_null()) {
            mcp.entry("envValueMode".to_string())
                .or_insert_with(|| Value::String("direct".to_string()));
        }
    }

    for (group, members) in groups {
        if members.as_object().is_some_and(|m| !m.is_empty()) {
            root.insert(group, members);
        }
    }
    Value::Object(root)
}

/// Translates the legacy single BYOK `provider` into a named `"default"` connection
/// (`inference.providers`), selects it (`inference.defaultProvider`), disables CAPI
/// (`github.capi.enabled = false`, as v1 BYOK did), and moves model-specific fields
/// into `inference.models` when the session model is set. Skipped when the caller
/// already supplied `providers`, `defaultProvider`, or `capi.enabled`.
fn translate_legacy_provider(
    provider: Value,
    model: Option<&Value>,
    groups: &mut serde_json::Map<String, Value>,
) {
    let explicit_inference = groups.get("inference").is_some_and(|inference| {
        inference.get("providers").is_some() || inference.get("defaultProvider").is_some()
    });
    let explicit_capi = groups
        .get("github")
        .and_then(|github| github.get("capi"))
        .is_some_and(|capi| capi.get("enabled").is_some());
    let provider = match provider {
        Value::Object(provider) if !explicit_inference && !explicit_capi => provider,
        _ => return,
    };

    let mut connection = serde_json::Map::new();
    connection.insert("name".to_string(), Value::String("default".to_string()));
    let mut model_fields = serde_json::Map::new();
    for (key, value) in provider {
        if value.is_null() || key == "name" {
            continue;
        }
        if LEGACY_PROVIDER_MODEL_FIELDS.contains(&key.as_str()) {
            model_fields.insert(key, value);
        } else {
            connection.insert(key, value);
        }
    }

    let inference = group_members(groups, "inference");
    inference.insert(
        "providers".to_string(),
        Value::Array(vec![Value::Object(connection)]),
    );
    inference.insert(
        "defaultProvider".to_string(),
        Value::String("default".to_string()),
    );
    if let (false, Some(Value::String(model))) = (model_fields.is_empty(), model) {
        let mut definition = serde_json::Map::new();
        definition.insert("provider".to_string(), Value::String("default".to_string()));
        definition.insert("id".to_string(), Value::String(model.clone()));
        definition.extend(model_fields);
        inference.insert(
            "models".to_string(),
            Value::Array(vec![Value::Object(definition)]),
        );
    }

    let github = group_members(groups, "github");
    let capi = github
        .entry("capi".to_string())
        .or_insert_with(|| Value::Object(serde_json::Map::new()));
    if let Value::Object(capi) = capi {
        capi.insert("enabled".to_string(), Value::Bool(false));
    } else {
        *capi = serde_json::json!({ "enabled": false });
    }
}

/// Applies the `ClientMode::Empty` defaults under the caller's grouped values.
fn apply_empty_mode_defaults(params: &mut Value) {
    let Some(root) = params.as_object_mut() else {
        return;
    };
    for (group, member, value) in empty_mode_defaults() {
        let members = group_members(root, group);
        if members.get(member).map_or(true, Value::is_null) {
            members.insert(member.to_string(), value);
        }
    }
}

/// Builds the command that launches the CLI. JavaScript entry points (`npm-loader.js`,
/// legacy `index.js`) run under `node`; any other path (a native binary or the
/// `node_modules/.bin/copilot` shim) is executed directly.
fn cli_command(cli_path: &str) -> Command {
    let is_script = [".js", ".mjs", ".cjs"]
        .iter()
        .any(|suffix| cli_path.to_ascii_lowercase().ends_with(suffix));
    if is_script {
        let mut cmd = Command::new("node");
        cmd.arg(cli_path);
        cmd
    } else {
        Command::new(cli_path)
    }
}

// ============================================================================
// CopilotClient
// ============================================================================

/// Main client for interacting with the Copilot CLI.
///
/// The CopilotClient manages the connection to the Copilot CLI server and provides
/// methods to create and manage conversation sessions. It can either spawn a CLI
/// server process or connect to an existing server.
///
/// # Examples
///
/// ```rust,no_run
/// # use copilot_sdk::*;
/// # async fn example() -> Result<(), CopilotError> {
/// // Create a client with default options (spawns CLI server via stdio)
/// let client = CopilotClient::new(CopilotClientOptions::default());
///
/// // Start the connection
/// client.start().await?;
///
/// // Create a session
/// let session = client.create_session(SessionConfig::default()).await?;
///
/// // Send messages and handle responses
/// let sub = session.on(|event| {
///     if let Some(content) = event.assistant_message_content() {
///         println!("Assistant: {}", content);
///     }
/// }).await;
///
/// session.send(MessageOptions {
///     prompt: "Hello!".to_string(),
///     ..Default::default()
/// }).await?;
///
/// // Clean up
/// session.destroy().await?;
/// client.stop().await?;
/// # Ok(())
/// # }
/// ```
pub struct CopilotClient {
    options: CopilotClientOptions,
    state: Arc<Mutex<ConnectionState>>,
    rpc_client: Arc<Mutex<Option<Arc<JsonRpcClient>>>>,
    cli_process: Arc<Mutex<Option<Child>>>,
    sessions: Arc<Mutex<HashMap<String, Arc<CopilotSession>>>>,
    is_external_server: bool,
    models_cache: Arc<Mutex<Option<Vec<ModelInfo>>>>,
    lifecycle_handlers: Arc<Mutex<Vec<(u64, Arc<dyn Fn(SessionLifecycleEvent) + Send + Sync>)>>>,
    next_lifecycle_handler_id: Arc<Mutex<u64>>,
    github_token_providers: GitHubTokenRegistry,
}

impl CopilotClient {
    /// Creates a new CopilotClient with the given options.
    ///
    /// This does not start the connection. Call `start()` or use `auto_start`
    /// (which triggers on first `create_session()`).
    pub fn new(options: CopilotClientOptions) -> Self {
        let is_external = options.cli_url.is_some();

        Self {
            options,
            state: Arc::new(Mutex::new(ConnectionState::Disconnected)),
            rpc_client: Arc::new(Mutex::new(None)),
            cli_process: Arc::new(Mutex::new(None)),
            sessions: Arc::new(Mutex::new(HashMap::new())),
            is_external_server: is_external,
            models_cache: Arc::new(Mutex::new(None)),
            lifecycle_handlers: Arc::new(Mutex::new(Vec::new())),
            next_lifecycle_handler_id: Arc::new(Mutex::new(0)),
            github_token_providers: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Returns the current connection state.
    pub async fn get_state(&self) -> ConnectionState {
        *self.state.lock().await
    }

    // ========================================================================
    // Start / Stop
    // ========================================================================

    /// Starts the CLI server and establishes a connection.
    ///
    /// If connecting to an external server (via `cli_url`), only establishes the connection.
    /// Otherwise, spawns the CLI server process and then connects.
    ///
    /// This method is called automatically when creating a session if `auto_start` is true.
    pub async fn start(&self) -> Result<(), CopilotError> {
        {
            let state = self.state.lock().await;
            if *state == ConnectionState::Connected {
                return Ok(());
            }
        }

        {
            let mut state = self.state.lock().await;
            *state = ConnectionState::Connecting;
        }

        let result = self.do_start().await;
        match &result {
            Ok(()) => {
                let mut state = self.state.lock().await;
                *state = ConnectionState::Connected;
            }
            Err(_) => {
                let mut state = self.state.lock().await;
                *state = ConnectionState::Error;
            }
        }
        result
    }

    async fn do_start(&self) -> Result<(), CopilotError> {
        if self.is_external_server {
            // Connect to an existing server via TCP
            self.connect_to_external_server().await?;
        } else if self.options.use_stdio {
            // Spawn CLI and connect via stdio
            self.start_cli_stdio().await?;
        } else {
            // Spawn CLI and connect via TCP
            self.start_cli_tcp().await?;
        }

        // Declare the application identity (if any) before the protocol handshake
        self.send_client_info().await?;

        // Verify protocol version
        self.verify_protocol_version().await?;

        // Register network callbacks (proxy resolution / proxy authentication)
        self.register_network_provider().await?;

        Ok(())
    }

    /// Stops the CLI server and closes all active sessions.
    ///
    /// Returns a list of errors encountered during cleanup.
    pub async fn stop(&self) -> Result<Vec<CopilotError>, CopilotError> {
        let mut errors = Vec::new();

        // Destroy all active sessions
        let session_ids: Vec<String> = {
            let sessions = self.sessions.lock().await;
            sessions.keys().cloned().collect()
        };

        for session_id in session_ids {
            let session = {
                let sessions = self.sessions.lock().await;
                sessions.get(&session_id).cloned()
            };
            if let Some(session) = session {
                for attempt in 1..=3 {
                    match session.destroy().await {
                        Ok(()) => break,
                        Err(e) => {
                            if attempt == 3 {
                                errors.push(CopilotError::SessionError(format!(
                                    "Failed to destroy session {} after 3 attempts: {}",
                                    session_id, e
                                )));
                            } else {
                                let delay = 100 * (1u64 << (attempt - 1));
                                tokio::time::sleep(std::time::Duration::from_millis(delay)).await;
                            }
                        }
                    }
                }
            }
        }

        {
            let mut sessions = self.sessions.lock().await;
            sessions.clear();
        }

        // Drop all GitHub token provider registrations
        {
            let mut providers = self.github_token_providers.lock().await;
            providers.clear();
        }

        // Stop the JSON-RPC client
        {
            let mut rpc = self.rpc_client.lock().await;
            if let Some(client) = rpc.take() {
                // We need mutable access; Arc doesn't give us that directly.
                // The client will be cleaned up when all references are dropped.
                // For a cleaner shutdown, we'd need Arc<Mutex<JsonRpcClient>>.
                drop(client);
            }
        }

        // Clear models cache
        {
            let mut cache = self.models_cache.lock().await;
            *cache = None;
        }

        // Kill CLI process
        if !self.is_external_server {
            let mut proc = self.cli_process.lock().await;
            if let Some(ref mut child) = *proc {
                let _ = child.kill().await;
            }
            *proc = None;
        }

        {
            let mut state = self.state.lock().await;
            *state = ConnectionState::Disconnected;
        }

        Ok(errors)
    }

    /// Forcefully stops the client without graceful session cleanup.
    pub async fn force_stop(&self) {
        // Clear sessions immediately
        {
            let mut sessions = self.sessions.lock().await;
            sessions.clear();
        }

        // Drop all GitHub token provider registrations
        {
            let mut providers = self.github_token_providers.lock().await;
            providers.clear();
        }

        // Drop RPC client
        {
            let mut rpc = self.rpc_client.lock().await;
            *rpc = None;
        }

        // Clear cache
        {
            let mut cache = self.models_cache.lock().await;
            *cache = None;
        }

        // Kill process
        if !self.is_external_server {
            let mut proc = self.cli_process.lock().await;
            if let Some(ref mut child) = *proc {
                let _ = child.kill().await;
            }
            *proc = None;
        }

        {
            let mut state = self.state.lock().await;
            *state = ConnectionState::Disconnected;
        }
    }

    // ========================================================================
    // Session Management
    // ========================================================================

    /// Creates a new conversation session with the Copilot CLI.
    pub async fn create_session(
        &self,
        mut config: SessionConfig,
    ) -> Result<Arc<CopilotSession>, CopilotError> {
        if config.github_token.is_some() && config.github_token_provider.is_some() {
            return Err(CopilotError::Configuration(
                "github_token and github_token_provider are mutually exclusive".to_string(),
            ));
        }
        self.validate_mode_tools(config.available_tools.as_ref())?;

        self.ensure_connected().await?;

        let rpc = self.get_rpc_client().await?;

        // Signal to the runtime that an MCP OAuth host-token handler is registered.
        if config.on_mcp_auth_request.is_some() {
            config.mcp_auth_handler = Some(true);
        }

        let mut params = serde_json::to_value(&config)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        insert_capi_options(
            &mut params,
            config.auto_tier,
            config.enable_web_socket_responses,
            config.capi.as_ref(),
        );
        let callbacks = SessionCallbacks {
            shell_authentication: config.on_shell_authentication_request.clone(),
            memory_store: config.store_memory.clone(),
        };
        callbacks.declare(&mut params);

        let registration_id = self
            .register_github_token_provider(
                config.github_token_provider.as_ref(),
                config.session_id.clone(),
                &mut params,
            )
            .await;

        let params = self.finalize_session_params(params, false);
        self.open_session(rpc, "session.create", params, registration_id, callbacks)
            .await
    }

    /// Resumes an existing conversation session by its ID.
    pub async fn resume_session(
        &self,
        config: ResumeSessionConfig,
    ) -> Result<Arc<CopilotSession>, CopilotError> {
        if config.github_token.is_some() && config.github_token_provider.is_some() {
            return Err(CopilotError::Configuration(
                "github_token and github_token_provider are mutually exclusive".to_string(),
            ));
        }
        self.validate_mode_tools(config.available_tools.as_ref())?;

        self.ensure_connected().await?;

        let rpc = self.get_rpc_client().await?;

        let mut params = serde_json::to_value(&config)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        insert_capi_options(
            &mut params,
            config.auto_tier,
            config.enable_web_socket_responses,
            config.capi.as_ref(),
        );
        let callbacks = SessionCallbacks {
            shell_authentication: config.on_shell_authentication_request.clone(),
            memory_store: config.store_memory.clone(),
        };
        callbacks.declare(&mut params);

        let registration_id = self
            .register_github_token_provider(
                config.github_token_provider.as_ref(),
                Some(config.session_id.clone()),
                &mut params,
            )
            .await;

        let params = self.finalize_session_params(params, true);
        self.open_session(rpc, "session.resume", params, registration_id, callbacks)
            .await
    }

    /// Returns the client mode (`CodingAgent` or `Empty`).
    pub fn mode(&self) -> ClientMode {
        self.options.mode
    }

    /// In `ClientMode::Empty`, every session must explicitly opt into its tools.
    fn validate_mode_tools(&self, available_tools: Option<&Vec<String>>) -> Result<(), CopilotError> {
        if self.options.mode == ClientMode::Empty && available_tools.is_none() {
            return Err(CopilotError::Configuration(
                "CopilotClient is in mode \"empty\" but the session config did not specify \
                 available_tools. Empty mode requires every session to explicitly opt into the \
                 tools it wants."
                    .to_string(),
            ));
        }
        Ok(())
    }

    /// Converts the flat params into the SDK protocol 4 grouped payload and applies the
    /// client mode defaults. Called immediately before sending create / resume.
    fn finalize_session_params(&self, params: Value, is_resume: bool) -> Value {
        let mut params = group_session_params(params, is_resume);
        if self.options.mode == ClientMode::Empty {
            apply_empty_mode_defaults(&mut params);
        }
        params
    }

    /// Sends `session.create` / `session.resume` and tracks the resulting session.
    ///
    /// A GitHub token provider registration is dropped if the request fails, and
    /// otherwise bound to the returned session (which removes it on destroy).
    /// A failed request keeps the JSON-RPC error data, so a refusing managed-settings
    /// policy helper is available through [`CopilotError::policy_helper_failure`].
    async fn open_session(
        &self,
        rpc: Arc<JsonRpcClient>,
        method: &str,
        params: Value,
        github_token_registration: Option<String>,
        callbacks: SessionCallbacks,
    ) -> Result<Arc<CopilotSession>, CopilotError> {
        let opened = rpc.request(method, params, None).await.and_then(|response| {
            let session_id = response
                .get("sessionId")
                .and_then(|v| v.as_str())
                .ok_or_else(|| {
                    CopilotError::Protocol("Missing sessionId in response".to_string())
                })?
                .to_string();
            let workspace_path = response
                .get("workspacePath")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            Ok((session_id, workspace_path))
        });
        let (session_id, workspace_path) = match opened {
            Ok(opened) => opened,
            Err(e) => {
                if let Some(registration_id) = github_token_registration {
                    let mut providers = self.github_token_providers.lock().await;
                    providers.remove(&registration_id);
                }
                return Err(e);
            }
        };

        let session = Arc::new(CopilotSession::new(
            session_id.clone(),
            rpc.clone(),
            workspace_path,
        ));
        if let Some(handler) = callbacks.shell_authentication {
            session.set_shell_authentication_handler(handler).await;
        }
        if let Some(store) = callbacks.memory_store {
            session.set_memory_store(store).await;
        }

        if let Some(registration_id) = github_token_registration {
            {
                let mut providers = self.github_token_providers.lock().await;
                if let Some(registration) = providers.get_mut(&registration_id) {
                    registration.session_id = Some(session_id.clone());
                }
            }
            session
                .set_github_token_registration(
                    registration_id,
                    Arc::clone(&self.github_token_providers),
                )
                .await;
        }

        {
            let mut sessions = self.sessions.lock().await;
            sessions.insert(session_id, Arc::clone(&session));
        }

        Ok(session)
    }

    /// Registers a session's GitHub token provider and adds its registration ID to
    /// the request params (`gitHubTokenProviderRegistrationId`).
    async fn register_github_token_provider(
        &self,
        provider: Option<&GitHubTokenProviderFn>,
        session_id: Option<String>,
        params: &mut Value,
    ) -> Option<String> {
        let provider = provider?.clone();
        let registration_id = uuid::Uuid::new_v4().to_string();
        {
            let mut providers = self.github_token_providers.lock().await;
            providers.insert(
                registration_id.clone(),
                GitHubTokenRegistration {
                    provider,
                    session_id,
                },
            );
        }
        if let Some(obj) = params.as_object_mut() {
            obj.insert(
                "gitHubTokenProviderRegistrationId".to_string(),
                Value::String(registration_id.clone()),
            );
        }
        Some(registration_id)
    }

    /// Gets the last session ID.
    pub async fn get_last_session_id(&self) -> Result<Option<String>, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("session.getLastId", serde_json::json!({}), None)
            .await?;
        Ok(response
            .get("sessionId")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string()))
    }

    /// Gets metadata for a specific session.
    pub async fn get_session_metadata(
        &self,
        session_id: &str,
    ) -> Result<serde_json::Value, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request(
                "session.getMetadata",
                serde_json::json!({ "sessionId": session_id }),
                None,
            )
            .await?;
        Ok(response)
    }

    /// Deletes a session permanently.
    pub async fn delete_session(&self, session_id: &str) -> Result<(), CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request(
                "session.delete",
                serde_json::json!({ "sessionId": session_id }),
                None,
            )
            .await?;

        let success = response.get("success").and_then(|v| v.as_bool()).unwrap_or(false);
        if !success {
            let error = response
                .get("error")
                .and_then(|v| v.as_str())
                .unwrap_or("Unknown error");
            return Err(CopilotError::SessionError(format!(
                "Failed to delete session {}: {}",
                session_id, error
            )));
        }

        {
            let mut sessions = self.sessions.lock().await;
            sessions.remove(session_id);
        }

        Ok(())
    }

    /// Lists all available sessions.
    pub async fn list_sessions(&self) -> Result<Vec<SessionMetadata>, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("session.list", serde_json::json!({}), None)
            .await?;
        let sessions: Vec<SessionMetadata> = serde_json::from_value(
            response
                .get("sessions")
                .cloned()
                .unwrap_or(Value::Array(vec![])),
        )
        .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        Ok(sessions)
    }

    // ========================================================================
    // Ping / Status / Models
    // ========================================================================

    /// Sends a ping request to the server.
    pub async fn ping(&self, message: Option<&str>) -> Result<PingResponse, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let params = serde_json::json!({ "message": message });
        let response = rpc.request("ping", params, None).await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Gets CLI status including version and protocol information.
    pub async fn get_status(&self) -> Result<GetStatusResponse, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("status.get", serde_json::json!({}), None)
            .await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Gets current authentication status.
    pub async fn get_auth_status(&self) -> Result<GetAuthStatusResponse, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("auth.getStatus", serde_json::json!({}), None)
            .await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Clears the runtime's cached managed (enterprise) settings (`managedSettings.clearCache`).
    pub async fn clear_managed_settings_cache(&self) -> Result<(), CopilotError> {
        let rpc = self.get_rpc_client().await?;
        rpc.request("managedSettings.clearCache", serde_json::json!({}), None)
            .await?;
        Ok(())
    }

    // ========================================================================
    // MCP Secret Store / Plugin Extension Approvals (Copilot CLI 1.0.116+)
    // ========================================================================

    /// Saves a legacy MCP configuration credential in the runtime-owned secret store
    /// (`mcp.secrets.save`). Requires Copilot CLI 1.0.116 or newer.
    ///
    /// `value` is the credential and is never logged. `config_dir` defaults to the
    /// runtime's configuration directory. The result's `used_keychain` is true when the
    /// OS keychain stored the credential and false for the file backend.
    pub async fn mcp_secrets_save(
        &self,
        secret_id: &str,
        value: &str,
        config_dir: Option<&str>,
    ) -> Result<McpSecretsSaveResult, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let mut params = serde_json::json!({ "secretId": secret_id, "value": value });
        if let Some(config_dir) = config_dir {
            params["configDir"] = Value::from(config_dir);
        }
        let response = rpc.request("mcp.secrets.save", params, None).await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Deletes a legacy MCP configuration credential from the runtime-owned secret store
    /// (`mcp.secrets.delete`). Requires Copilot CLI 1.0.116 or newer.
    pub async fn mcp_secrets_delete(
        &self,
        secret_id: &str,
        config_dir: Option<&str>,
    ) -> Result<(), CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let mut params = serde_json::json!({ "secretId": secret_id });
        if let Some(config_dir) = config_dir {
            params["configDir"] = Value::from(config_dir);
        }
        rpc.request("mcp.secrets.delete", params, None).await?;
        Ok(())
    }

    /// Deletes every legacy credential indexed for an MCP server
    /// (`mcp.secrets.deleteServer`). Requires Copilot CLI 1.0.116 or newer.
    pub async fn mcp_secrets_delete_server(
        &self,
        server_name: &str,
        config_dir: Option<&str>,
    ) -> Result<(), CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let mut params = serde_json::json!({ "serverName": server_name });
        if let Some(config_dir) = config_dir {
            params["configDir"] = Value::from(config_dir);
        }
        rpc.request("mcp.secrets.deleteServer", params, None)
            .await?;
        Ok(())
    }

    /// Persists a pending plugin extension permission request as a user-scoped approval,
    /// before the host answers that request normally (`permissions.extensionApprovals.grant`).
    /// Requires Copilot CLI 1.0.116 or newer.
    pub async fn extension_approvals_grant(
        &self,
        session_id: &str,
        request_id: &str,
    ) -> Result<PermissionsExtensionApprovalsGrantResult, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let params = serde_json::json!({ "sessionId": session_id, "requestId": request_id });
        let response = rpc
            .request("permissions.extensionApprovals.grant", params, None)
            .await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Lists the user-scoped plugin extension approvals
    /// (`permissions.extensionApprovals.list`). `session_id` selects an attached session's
    /// configuration profile. Requires Copilot CLI 1.0.116 or newer.
    pub async fn extension_approvals_list(
        &self,
        session_id: Option<&str>,
    ) -> Result<PermissionsExtensionApprovalsListResult, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let mut params = serde_json::json!({});
        if let Some(session_id) = session_id {
            params["sessionId"] = Value::from(session_id);
        }
        let response = rpc
            .request("permissions.extensionApprovals.list", params, None)
            .await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Revokes one user-scoped plugin extension approval
    /// (`permissions.extensionApprovals.revoke`). Pass the approval's `installation_key`
    /// back unchanged; `session_id` selects an attached session's configuration profile.
    /// Requires Copilot CLI 1.0.116 or newer.
    pub async fn extension_approvals_revoke(
        &self,
        extension_name: &str,
        installation_key: &str,
        session_id: Option<&str>,
    ) -> Result<PermissionsExtensionApprovalsRevokeResult, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let mut params = serde_json::json!({
            "extensionName": extension_name,
            "installationKey": installation_key,
        });
        if let Some(session_id) = session_id {
            params["sessionId"] = Value::from(session_id);
        }
        let response = rpc
            .request("permissions.extensionApprovals.revoke", params, None)
            .await?;
        serde_json::from_value(response).map_err(|e| CopilotError::Serialization(e.to_string()))
    }

    /// Lists available models with their metadata.
    ///
    /// Results are cached after the first successful call.
    pub async fn list_models(&self) -> Result<Vec<ModelInfo>, CopilotError> {
        // Check cache first
        {
            let cache = self.models_cache.lock().await;
            if let Some(ref models) = *cache {
                return Ok(models.clone());
            }
        }

        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("models.list", serde_json::json!({}), None)
            .await?;
        let models_response: HashMap<String, Vec<ModelInfo>> =
            serde_json::from_value(response)
                .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        let models = models_response.get("models").cloned().unwrap_or_default();

        // Update cache
        {
            let mut cache = self.models_cache.lock().await;
            *cache = Some(models.clone());
        }

        Ok(models)
    }

    // ========================================================================
    // Foreground Session (TUI+server mode)
    // ========================================================================

    /// Gets the foreground session ID in TUI+server mode.
    pub async fn get_foreground_session_id(&self) -> Result<Option<String>, CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request("session.getForeground", serde_json::json!({}), None)
            .await?;
        Ok(response
            .get("sessionId")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string()))
    }

    /// Sets the foreground session in TUI+server mode.
    pub async fn set_foreground_session_id(
        &self,
        session_id: &str,
    ) -> Result<(), CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let response = rpc
            .request(
                "session.setForeground",
                serde_json::json!({ "sessionId": session_id }),
                None,
            )
            .await?;
        let success = response.get("success").and_then(|v| v.as_bool()).unwrap_or(false);
        if !success {
            let error = response
                .get("error")
                .and_then(|v| v.as_str())
                .unwrap_or("Unknown error");
            return Err(CopilotError::SessionError(error.to_string()));
        }
        Ok(())
    }

    // ========================================================================
    // Session Filesystem Provider
    // ========================================================================

    /// Sets the session filesystem provider configuration.
    ///
    /// This configures the session filesystem with optional initial working directory,
    /// session state path, and conventions.
    pub async fn set_session_fs_provider(
        &self,
        config: SessionFsConfig,
    ) -> Result<(), CopilotError> {
        let rpc = self.get_rpc_client().await?;
        let params = serde_json::to_value(&config)
            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
        rpc.request("sessionFs.setProvider", params, None).await?;
        Ok(())
    }

    // ========================================================================
    // Lifecycle Event Subscription
    // ========================================================================

    /// Subscribes to session lifecycle events.
    ///
    /// Returns a handler ID that can be used to unsubscribe.
    pub async fn on_lifecycle<F>(&self, handler: F) -> u64
    where
        F: Fn(SessionLifecycleEvent) + Send + Sync + 'static,
    {
        let handler_id = {
            let mut id = self.next_lifecycle_handler_id.lock().await;
            *id += 1;
            *id
        };

        let mut handlers = self.lifecycle_handlers.lock().await;
        handlers.push((handler_id, Arc::new(handler)));
        handler_id
    }

    /// Unsubscribes a lifecycle event handler by its ID.
    pub async fn off_lifecycle(&self, handler_id: u64) {
        let mut handlers = self.lifecycle_handlers.lock().await;
        handlers.retain(|(id, _)| *id != handler_id);
    }

    // ========================================================================
    // Internal: Connection Setup
    // ========================================================================

    async fn ensure_connected(&self) -> Result<(), CopilotError> {
        let state = self.state.lock().await;
        if *state == ConnectionState::Connected {
            return Ok(());
        }
        drop(state);

        if self.options.auto_start {
            self.start().await
        } else {
            Err(CopilotError::NotConnected)
        }
    }

    async fn get_rpc_client(&self) -> Result<Arc<JsonRpcClient>, CopilotError> {
        let rpc = self.rpc_client.lock().await;
        rpc.clone().ok_or(CopilotError::NotConnected)
    }

    async fn start_cli_stdio(&self) -> Result<(), CopilotError> {
        let cli_path = self
            .options
            .cli_path
            .as_deref()
            .ok_or_else(|| CopilotError::Configuration("cli_path is required".to_string()))?;

        let mut args = self.options.cli_args.clone();
        args.extend_from_slice(&[
            "--headless".to_string(),
            "--no-auto-update".to_string(),
            "--log-level".to_string(),
            self.options.log_level.clone(),
            "--stdio".to_string(),
        ]);

        // Add auth flags
        if self.options.github_token.is_some() {
            args.push("--auth-token-env".to_string());
            args.push("COPILOT_SDK_AUTH_TOKEN".to_string());
        }
        let use_logged_in = self
            .options
            .use_logged_in_user
            .unwrap_or(self.options.github_token.is_none());
        if !use_logged_in {
            args.push("--no-auto-login".to_string());
        }

        let mut cmd = cli_command(cli_path);
        cmd.args(&args)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        if let Some(ref cwd) = self.options.cwd {
            cmd.current_dir(cwd);
        }

        // Set environment
        if let Some(ref env) = self.options.env {
            cmd.envs(env.iter());
        }
        if let Some(ref token) = self.options.github_token {
            cmd.env("COPILOT_SDK_AUTH_TOKEN", token);
        }

        let mut child = cmd.spawn().map_err(|e| {
            CopilotError::ProcessSpawn(format!("Failed to spawn CLI process: {}", e))
        })?;

        let stdin = child.stdin.take().ok_or_else(|| {
            CopilotError::ProcessSpawn("Failed to capture stdin".to_string())
        })?;
        let stdout = child.stdout.take().ok_or_else(|| {
            CopilotError::ProcessSpawn("Failed to capture stdout".to_string())
        })?;

        // Forward stderr in background
        let stderr = child.stderr.take();
        if let Some(stderr) = stderr {
            tokio::spawn(async move {
                use tokio::io::AsyncBufReadExt;
                let reader = tokio::io::BufReader::new(stderr);
                let mut lines = reader.lines();
                while let Ok(Some(line)) = lines.next_line().await {
                    if !line.trim().is_empty() {
                        eprintln!("[CLI subprocess] {}", line);
                    }
                }
            });
        }

        let rpc_client = Arc::new(JsonRpcClient::new(stdout, stdin));

        // Set up handlers
        self.attach_connection_handlers(&rpc_client).await;

        {
            let mut rpc = self.rpc_client.lock().await;
            *rpc = Some(rpc_client);
        }
        {
            let mut proc = self.cli_process.lock().await;
            *proc = Some(child);
        }

        Ok(())
    }

    async fn start_cli_tcp(&self) -> Result<(), CopilotError> {
        let cli_path = self
            .options
            .cli_path
            .as_deref()
            .ok_or_else(|| CopilotError::Configuration("cli_path is required".to_string()))?;

        let mut args = self.options.cli_args.clone();
        args.extend_from_slice(&[
            "--headless".to_string(),
            "--no-auto-update".to_string(),
            "--log-level".to_string(),
            self.options.log_level.clone(),
        ]);

        if self.options.port > 0 {
            args.push("--port".to_string());
            args.push(self.options.port.to_string());
        }

        let mut cmd = cli_command(cli_path);
        cmd.args(&args)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        if let Some(ref cwd) = self.options.cwd {
            cmd.current_dir(cwd);
        }
        if let Some(ref env) = self.options.env {
            cmd.envs(env.iter());
        }

        let mut child = cmd.spawn().map_err(|e| {
            CopilotError::ProcessSpawn(format!("Failed to spawn CLI process: {}", e))
        })?;

        // Read stdout to find the port
        let stdout = child.stdout.take().ok_or_else(|| {
            CopilotError::ProcessSpawn("Failed to capture stdout".to_string())
        })?;

        let port = {
            use tokio::io::AsyncBufReadExt;
            let reader = tokio::io::BufReader::new(stdout);
            let mut lines = reader.lines();
            let mut found_port = None;

            let timeout = tokio::time::timeout(std::time::Duration::from_secs(10), async {
                while let Ok(Some(line)) = lines.next_line().await {
                    if let Some(idx) = line.to_lowercase().find("listening on port ") {
                        let port_str = &line[idx + "listening on port ".len()..];
                        if let Ok(p) = port_str.trim().parse::<u16>() {
                            found_port = Some(p);
                            break;
                        }
                    }
                }
                found_port
            })
            .await;

            match timeout {
                Ok(Some(p)) => p,
                _ => {
                    let _ = child.kill().await;
                    return Err(CopilotError::Timeout(10000));
                }
            }
        };

        // Connect TCP
        let stream = TcpStream::connect(format!("localhost:{}", port))
            .await
            .map_err(|e| CopilotError::Connection(format!("Failed to connect via TCP: {}", e)))?;

        let (reader, writer) = stream.into_split();
        let rpc_client = Arc::new(JsonRpcClient::new(reader, writer));

        self.attach_connection_handlers(&rpc_client).await;

        {
            let mut rpc = self.rpc_client.lock().await;
            *rpc = Some(rpc_client);
        }
        {
            let mut proc = self.cli_process.lock().await;
            *proc = Some(child);
        }

        Ok(())
    }

    async fn connect_to_external_server(&self) -> Result<(), CopilotError> {
        let url = self
            .options
            .cli_url
            .as_deref()
            .ok_or_else(|| CopilotError::Configuration("cli_url is required".to_string()))?;

        let (host, port) = Self::parse_cli_url(url)?;

        let stream = TcpStream::connect(format!("{}:{}", host, port))
            .await
            .map_err(|e| {
                CopilotError::Connection(format!("Failed to connect to {}: {}", url, e))
            })?;

        let (reader, writer) = stream.into_split();
        let rpc_client = Arc::new(JsonRpcClient::new(reader, writer));

        self.attach_connection_handlers(&rpc_client).await;

        {
            let mut rpc = self.rpc_client.lock().await;
            *rpc = Some(rpc_client);
        }

        Ok(())
    }

    fn parse_cli_url(url: &str) -> Result<(String, u16), CopilotError> {
        let clean = url
            .trim_start_matches("http://")
            .trim_start_matches("https://");

        // Just a port number
        if let Ok(port) = clean.parse::<u16>() {
            return Ok(("localhost".to_string(), port));
        }

        // host:port format
        let parts: Vec<&str> = clean.split(':').collect();
        if parts.len() != 2 {
            return Err(CopilotError::Configuration(format!(
                "Invalid cli_url format: {}. Expected host:port, http://host:port, or port",
                url
            )));
        }

        let host = if parts[0].is_empty() {
            "localhost".to_string()
        } else {
            parts[0].to_string()
        };
        let port: u16 = parts[1].parse().map_err(|_| {
            CopilotError::Configuration(format!("Invalid port in cli_url: {}", url))
        })?;

        Ok((host, port))
    }

    async fn verify_protocol_version(&self) -> Result<(), CopilotError> {
        let expected_version = get_sdk_protocol_version();
        let ping_response = self.ping(None).await?;

        match ping_response.protocol_version {
            None => Err(CopilotError::ProtocolMismatch {
                expected: expected_version,
                actual: None,
            }),
            Some(server_version) if server_version != expected_version => {
                Err(CopilotError::ProtocolMismatch {
                    expected: expected_version,
                    actual: Some(server_version),
                })
            }
            _ => Ok(()),
        }
    }

    /// Sends the `connect` handshake carrying `client_info` (skipped when unset or all-empty).
    ///
    /// Older CLIs without `connect` answer -32601 / "Unhandled method connect"; that is ignored.
    async fn send_client_info(&self) -> Result<(), CopilotError> {
        let client_info = match self
            .options
            .client_info
            .as_ref()
            .and_then(|info| info.to_wire())
        {
            Some(client_info) => client_info,
            None => return Ok(()),
        };
        let rpc = self.get_rpc_client().await?;
        let params = serde_json::json!({ "clientInfo": client_info });
        match rpc.request("connect", params, None).await {
            Ok(_) => Ok(()),
            Err(CopilotError::JsonRpc { code, message, .. })
                if code == -32601 || message.contains("Unhandled method connect") =>
            {
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    /// Registers the client network callbacks with `network.setProvider` (skipped when
    /// no callback is configured). The request handlers are attached with the connection.
    async fn register_network_provider(&self) -> Result<(), CopilotError> {
        let network = match self.options.network.as_ref() {
            Some(network) if network.has_callbacks() => network,
            _ => return Ok(()),
        };
        let rpc = self.get_rpc_client().await?;
        rpc.request("network.setProvider", network.set_provider_params(), None)
            .await?;
        Ok(())
    }

    // ========================================================================
    // Internal: Connection Handlers
    // ========================================================================

    async fn attach_connection_handlers(&self, rpc_client: &Arc<JsonRpcClient>) {
        // Set up notification handler for session.event and session.lifecycle
        let sessions = Arc::clone(&self.sessions);
        let lifecycle_handlers = Arc::clone(&self.lifecycle_handlers);
        let notification_handler: NotificationHandler =
            Arc::new(move |method: String, params: Value| {
                let sessions = Arc::clone(&sessions);
                let lifecycle_handlers = Arc::clone(&lifecycle_handlers);

                match method.as_str() {
                    "session.event" => {
                        let session_id = params
                            .get("sessionId")
                            .and_then(|v| v.as_str())
                            .map(|s| s.to_string());
                        let event = params.get("event").cloned();

                        if let (Some(session_id), Some(event_value)) = (session_id, event) {
                            if let Ok(event) =
                                serde_json::from_value::<SessionEvent>(event_value)
                            {
                                tokio::spawn(async move {
                                    let sessions = sessions.lock().await;
                                    if let Some(session) = sessions.get(&session_id) {
                                        session.dispatch_event(event).await;
                                    }
                                });
                            }
                        }
                    }
                    "session.lifecycle" => {
                        if let Ok(event) =
                            serde_json::from_value::<SessionLifecycleEvent>(params)
                        {
                            tokio::spawn(async move {
                                let handlers = lifecycle_handlers.lock().await;
                                for (_, handler) in handlers.iter() {
                                    handler(event.clone());
                                }
                            });
                        }
                    }
                    _ => {}
                }
            });

        rpc_client
            .set_notification_handler(notification_handler)
            .await;

        // Set up tool.call request handler
        let sessions_for_tools = Arc::clone(&self.sessions);
        let tool_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_tools);
            Box::pin(async move {
                let session_id = params
                    .get("sessionId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing sessionId".to_string()))?;
                let tool_call_id = params
                    .get("toolCallId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing toolCallId".to_string()))?;
                let tool_name = params
                    .get("toolName")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing toolName".to_string()))?;
                let arguments = params.get("arguments").cloned().unwrap_or(Value::Null);

                let session = {
                    let sessions = sessions.lock().await;
                    sessions.get(session_id).cloned()
                };

                let session = session.ok_or_else(|| {
                    CopilotError::SessionError(format!("Unknown session {}", session_id))
                })?;

                let handler = session.get_tool_handler(tool_name).await;

                let result = if let Some(handler) = handler {
                    let invocation = ToolInvocation {
                        session_id: session_id.to_string(),
                        tool_call_id: tool_call_id.to_string(),
                        tool_name: tool_name.to_string(),
                        arguments: arguments.clone(),
                    };

                    match handler(arguments, invocation).await {
                        Ok(value) => normalize_tool_result(value),
                        Err(e) => ToolResultObject {
                            text_result_for_llm:
                                "Invoking this tool produced an error. Detailed information is not available."
                                    .to_string(),
                            binary_results_for_llm: None,
                            result_type: ToolResultType::Failure,
                            error: Some(e.to_string()),
                            session_log: None,
                            tool_telemetry: Some(HashMap::new()),
                        },
                    }
                } else {
                    ToolResultObject {
                        text_result_for_llm: format!(
                            "Tool '{}' is not supported by this client instance.",
                            tool_name
                        ),
                        binary_results_for_llm: None,
                        result_type: ToolResultType::Failure,
                        error: Some(format!("tool '{}' not supported", tool_name)),
                        session_log: None,
                        tool_telemetry: Some(HashMap::new()),
                    }
                };

                let response =
                    serde_json::to_value(ToolCallResponsePayload { result })
                        .map_err(|e| CopilotError::Serialization(e.to_string()))?;
                Ok(response)
            })
        });
        rpc_client.set_request_handler("tool.call", tool_handler).await;

        // Set up permission.request handler
        let sessions_for_perm = Arc::clone(&self.sessions);
        let permission_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_perm);
            Box::pin(async move {
                let session_id = params
                    .get("sessionId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing sessionId".to_string()))?;
                let perm_request = params
                    .get("permissionRequest")
                    .cloned()
                    .unwrap_or(Value::Null);

                let session = {
                    let sessions = sessions.lock().await;
                    sessions.get(session_id).cloned()
                };

                let session = session.ok_or_else(|| {
                    CopilotError::SessionError(format!("Session not found: {}", session_id))
                })?;

                let result = match session.handle_permission_request(perm_request).await {
                    Ok(result) => result,
                    Err(_) => PermissionRequestResult {
                        kind: PermissionResultKind::DeniedNoApprovalRuleAndCouldNotRequestFromUser,
                        rules: None,
                        decision_context: None,
                    },
                };

                let response = serde_json::json!({ "result": result });
                Ok(response)
            })
        });
        rpc_client
            .set_request_handler("permission.request", permission_handler)
            .await;

        // Set up userInput.request handler
        let sessions_for_input = Arc::clone(&self.sessions);
        let user_input_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_input);
            Box::pin(async move {
                let session_id = params
                    .get("sessionId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing sessionId".to_string()))?;

                let session = {
                    let sessions = sessions.lock().await;
                    sessions.get(session_id).cloned()
                };

                let session = session.ok_or_else(|| {
                    CopilotError::SessionError(format!("Session not found: {}", session_id))
                })?;

                let result = session.handle_user_input_request(params).await?;
                let response = serde_json::to_value(result)
                    .map_err(|e| CopilotError::Serialization(e.to_string()))?;
                Ok(response)
            })
        });
        rpc_client
            .set_request_handler("userInput.request", user_input_handler)
            .await;

        // Set up hooks.invoke handler
        let sessions_for_hooks = Arc::clone(&self.sessions);
        let hooks_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_hooks);
            Box::pin(async move {
                let session_id = params
                    .get("sessionId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing sessionId".to_string()))?;
                let hook_type = params
                    .get("hookType")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing hookType".to_string()))?;
                let input = params.get("input").cloned().unwrap_or(Value::Null);

                let session = {
                    let sessions = sessions.lock().await;
                    sessions.get(session_id).cloned()
                };

                let session = session.ok_or_else(|| {
                    CopilotError::SessionError(format!("Session not found: {}", session_id))
                })?;

                let output = session.handle_hooks_invoke(hook_type, input).await?;
                let response = serde_json::json!({ "output": output });
                Ok(response)
            })
        });
        rpc_client
            .set_request_handler("hooks.invoke", hooks_handler)
            .await;

        // Set up gitHubToken.getToken handler (session GitHub token providers)
        let github_token_providers = Arc::clone(&self.github_token_providers);
        let github_token_handler: RequestHandler = Arc::new(move |params: Value| {
            let providers = Arc::clone(&github_token_providers);
            Box::pin(async move {
                let registration_id = params
                    .get("registrationId")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing registrationId".to_string()))?;
                let host = params
                    .get("host")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing host".to_string()))?;
                let reason = params
                    .get("reason")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| CopilotError::Protocol("Missing reason".to_string()))?;

                let registration = {
                    let providers = providers.lock().await;
                    providers.get(registration_id).cloned()
                };
                let registration = registration.ok_or_else(|| {
                    CopilotError::NoHandler(format!(
                        "No GitHub token provider registered for registration ID \"{}\"",
                        registration_id
                    ))
                })?;

                let args = GitHubTokenProviderArgs {
                    host: host.to_string(),
                    session_id: params
                        .get("sessionId")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string())
                        .or(registration.session_id),
                    reason: reason.to_string(),
                };
                let result = (registration.provider.0)(args).await.map_err(|e| {
                    CopilotError::SessionError(format!("GitHub token provider failed: {}", e))
                })?;
                serde_json::to_value(result)
                    .map_err(|e| CopilotError::Serialization(e.to_string()))
            })
        });
        rpc_client
            .set_request_handler("gitHubToken.getToken", github_token_handler)
            .await;

        // Set up shellAuthentication.request / shellAuthentication.cancel handlers
        let sessions_for_shell_auth = Arc::clone(&self.sessions);
        let shell_auth_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_shell_auth);
            Box::pin(async move {
                let session = find_session(&sessions, &params).await?;
                let response = session.handle_shell_authentication_request(params).await?;
                serde_json::to_value(response)
                    .map_err(|e| CopilotError::Serialization(e.to_string()))
            })
        });
        rpc_client
            .set_request_handler("shellAuthentication.request", shell_auth_handler)
            .await;

        let sessions_for_shell_cancel = Arc::clone(&self.sessions);
        let shell_cancel_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_shell_cancel);
            Box::pin(async move {
                if let Ok(session) = find_session(&sessions, &params).await {
                    if let Some(request_id) = params.get("requestId").and_then(|v| v.as_str()) {
                        session.cancel_shell_authentication(request_id).await;
                    }
                }
                Ok(serde_json::json!({}))
            })
        });
        rpc_client
            .set_request_handler("shellAuthentication.cancel", shell_cancel_handler)
            .await;

        // Set up memoryStore.store handler (application memory store)
        let sessions_for_memory = Arc::clone(&self.sessions);
        let memory_store_handler: RequestHandler = Arc::new(move |params: Value| {
            let sessions = Arc::clone(&sessions_for_memory);
            Box::pin(async move {
                let session = find_session(&sessions, &params).await?;
                session.handle_memory_store(params).await?;
                Ok(Value::Null)
            })
        });
        rpc_client
            .set_request_handler("memoryStore.store", memory_store_handler)
            .await;

        // Set up network.* handlers for the configured network callbacks
        if let Some(network) = self.options.network.clone() {
            if let Some(resolve_proxy) = network.resolve_proxy {
                let handler: RequestHandler = Arc::new(move |params: Value| {
                    let resolve_proxy = resolve_proxy.clone();
                    Box::pin(async move {
                        let request: NetworkResolveProxyRequest = serde_json::from_value(params)
                            .map_err(|e| CopilotError::Serialization(e.to_string()))?;
                        let proxy = (resolve_proxy.0)(request).await.map_err(|e| {
                            CopilotError::SessionError(format!("Proxy resolution failed: {}", e))
                        })?;
                        Ok(match proxy {
                            Some(proxy) => serde_json::json!({ "proxy": proxy }),
                            None => serde_json::json!({}),
                        })
                    })
                });
                rpc_client
                    .set_request_handler("network.resolveProxy", handler)
                    .await;
            }
            if let Some(negotiate) = network.authenticate_negotiate {
                let handler: RequestHandler = Arc::new(move |params: Value| {
                    let negotiate = negotiate.clone();
                    Box::pin(async move {
                        let request: NetworkAuthenticateNegotiateRequest =
                            serde_json::from_value(params)
                                .map_err(|e| CopilotError::Serialization(e.to_string()))?;
                        // Never include the token in errors or logs.
                        let token = (negotiate.0)(request).await.map_err(|_| {
                            CopilotError::SessionError(
                                "Negotiate authentication callback failed".to_string(),
                            )
                        })?;
                        Ok(match token {
                            Some(token) => serde_json::json!({ "token": token }),
                            None => serde_json::json!({}),
                        })
                    })
                });
                rpc_client
                    .set_request_handler("network.authenticateNegotiate", handler)
                    .await;
            }
            if let Some(basic) = network.authenticate_basic {
                let handler: RequestHandler = Arc::new(move |params: Value| {
                    let basic = basic.clone();
                    Box::pin(async move {
                        let request: NetworkAuthenticateBasicRequest =
                            serde_json::from_value(params)
                                .map_err(|e| CopilotError::Serialization(e.to_string()))?;
                        // Never include credentials in errors or logs.
                        let credentials = (basic.0)(request).await.map_err(|_| {
                            CopilotError::SessionError(
                                "Basic authentication callback failed".to_string(),
                            )
                        })?;
                        let mut result = serde_json::Map::new();
                        if let Some(credentials) = credentials {
                            result.insert(
                                "username".to_string(),
                                Value::String(credentials.username),
                            );
                            if let Some(password) = credentials.password {
                                result.insert("password".to_string(), Value::String(password));
                            }
                        }
                        Ok(Value::Object(result))
                    })
                });
                rpc_client
                    .set_request_handler("network.authenticateBasic", handler)
                    .await;
            }
        }
    }
}

/// Looks up the session addressed by a server request's `sessionId`.
async fn find_session(
    sessions: &Arc<Mutex<HashMap<String, Arc<CopilotSession>>>>,
    params: &Value,
) -> Result<Arc<CopilotSession>, CopilotError> {
    let session_id = params
        .get("sessionId")
        .and_then(|v| v.as_str())
        .ok_or_else(|| CopilotError::Protocol("Missing sessionId".to_string()))?;
    let sessions = sessions.lock().await;
    sessions
        .get(session_id)
        .cloned()
        .ok_or_else(|| CopilotError::SessionError(format!("Session not found: {}", session_id)))
}

// ============================================================================
// Tool Result Normalization
// ============================================================================

/// Normalizes an arbitrary tool handler return value into a ToolResultObject.
fn normalize_tool_result(value: Value) -> ToolResultObject {
    if value.is_null() {
        return ToolResultObject {
            text_result_for_llm: "Tool returned no result".to_string(),
            binary_results_for_llm: None,
            result_type: ToolResultType::Failure,
            error: Some("tool returned no result".to_string()),
            session_log: None,
            tool_telemetry: Some(HashMap::new()),
        };
    }

    // Check if it's already a ToolResultObject (duck-type check)
    if value.get("textResultForLlm").is_some() && value.get("resultType").is_some() {
        if let Ok(result) = serde_json::from_value::<ToolResultObject>(value.clone()) {
            return result;
        }
    }

    // Wrap as success
    let text_result = if let Some(s) = value.as_str() {
        s.to_string()
    } else {
        serde_json::to_string(&value).unwrap_or_else(|_| "".to_string())
    };

    ToolResultObject {
        text_result_for_llm: text_result,
        binary_results_for_llm: None,
        result_type: ToolResultType::Success,
        error: None,
        session_log: None,
        tool_telemetry: Some(HashMap::new()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn groups_flat_resume_payload_for_protocol_4() {
        let flat = json!({
            "sessionId": "s1",
            "model": "m",
            "name": "create only",
            "streaming": true,
            "requestPermission": true,
            "tools": [{ "name": "t", "idempotent": true }],
            "hooks": true,
            "disableResume": true,
            "configDir": "/cfg",
            "trustWorkingDirectory": "session",
            "mcpServers": { "s": { "command": "x", "args": [], "tools": ["*"] } },
            "capi": { "autoTier": "fast" },
            "futureOption": 1,
            "reasoningEffort": null,
        });

        assert_eq!(
            group_session_params(flat, true),
            json!({
                "sessionId": "s1",
                "model": "m",
                "configDirectory": "/cfg",
                "suppressResumeEvent": true,
                "futureOption": 1,
                "events": { "streaming": true },
                "permissions": { "request": true },
                "tools": { "definitions": [{ "name": "t", "idempotent": true }] },
                "hooks": { "hasHandlers": true },
                "mcp": {
                    "servers": { "s": { "command": "x", "args": [], "tools": ["*"] } },
                    "envValueMode": "direct",
                },
                "github": { "capi": { "autoTier": "fast" } },
            })
        );
    }

    #[test]
    fn translates_legacy_provider_unless_named_providers_are_explicit() {
        let flat = json!({
            "model": "gpt-4o",
            "provider": {
                "type": "openai",
                "baseUrl": "https://models.example.com/v1",
                "apiKeyCommand": "print-key",
                "maxOutputTokens": 2048,
            },
        });
        assert_eq!(
            group_session_params(flat, false),
            json!({
                "model": "gpt-4o",
                "inference": {
                    "providers": [{
                        "name": "default",
                        "type": "openai",
                        "baseUrl": "https://models.example.com/v1",
                        "apiKeyCommand": "print-key",
                    }],
                    "defaultProvider": "default",
                    "models": [{ "provider": "default", "id": "gpt-4o", "maxOutputTokens": 2048 }],
                },
                "github": { "capi": { "enabled": false } },
            })
        );

        let explicit = json!({
            "sessionId": "s1",
            "provider": { "baseUrl": "https://legacy.example.com" },
            "defaultProvider": null,
        });
        assert_eq!(
            group_session_params(explicit, true),
            json!({ "sessionId": "s1", "inference": { "defaultProvider": null } })
        );
    }

    #[test]
    fn empty_mode_defaults_never_override_session_values() {
        let mut params = group_session_params(
            json!({ "availableTools": ["view"], "memory": { "enabled": true } }),
            false,
        );
        apply_empty_mode_defaults(&mut params);

        assert_eq!(params["tools"], json!({ "availableTools": ["view"] }));
        assert_eq!(params["state"]["memory"], json!({ "enabled": true }));
        assert_eq!(params["state"]["skipEmbeddingRetrieval"], json!(true));
        assert_eq!(params["mcp"]["oauthTokenStorage"], json!("in-memory"));
        assert_eq!(params["experiments"]["enableExperimentalMode"], json!(false));
        assert_eq!(params["git"]["enableHostGitOperations"], json!(false));
    }

    #[test]
    fn ping_timestamp_accepts_rfc3339_strings_and_epoch_millis() {
        let iso: PingResponse = serde_json::from_value(json!({
            "message": "pong",
            "timestamp": "1970-01-02T00:00:01.5Z",
            "protocolVersion": 4
        }))
        .unwrap();
        assert_eq!(iso.timestamp, 86_401_500.0);
        assert_eq!(iso.protocol_version, Some(4));

        let offset: PingResponse = serde_json::from_value(json!({
            "message": "pong",
            "timestamp": "2026-01-01T01:00:00+01:00"
        }))
        .unwrap();
        let utc: PingResponse = serde_json::from_value(json!({
            "message": "pong",
            "timestamp": "2026-01-01T00:00:00Z"
        }))
        .unwrap();
        assert_eq!(offset.timestamp, utc.timestamp);

        let millis: PingResponse =
            serde_json::from_value(json!({ "message": "pong", "timestamp": 1234.0 })).unwrap();
        assert_eq!(millis.timestamp, 1234.0);
    }

    async fn write_framed(writer: &mut tokio::io::DuplexStream, message: Value) {
        use tokio::io::AsyncWriteExt;
        let body = serde_json::to_vec(&message).unwrap();
        let header = format!("Content-Length: {}\r\n\r\n", body.len());
        writer.write_all(header.as_bytes()).await.unwrap();
        writer.write_all(&body).await.unwrap();
        writer.flush().await.unwrap();
    }

    async fn read_framed(reader: &mut tokio::io::DuplexStream) -> Value {
        use tokio::io::AsyncReadExt;
        let mut header = Vec::new();
        while !header.ends_with(b"\r\n\r\n") {
            header.push(reader.read_u8().await.unwrap());
        }
        let length: usize = String::from_utf8(header)
            .unwrap()
            .trim()
            .strip_prefix("Content-Length: ")
            .unwrap()
            .parse()
            .unwrap();
        let mut body = vec![0u8; length];
        reader.read_exact(&mut body).await.unwrap();
        serde_json::from_slice(&body).unwrap()
    }

    /// A client connected to an in-memory fake server; returns the server's read and
    /// write halves alongside the client.
    async fn connected_client() -> (
        CopilotClient,
        tokio::io::DuplexStream,
        tokio::io::DuplexStream,
    ) {
        let (client_write, server_read) = tokio::io::duplex(8192);
        let (server_write, client_read) = tokio::io::duplex(8192);
        let client = CopilotClient::new(CopilotClientOptions::default());
        let rpc = Arc::new(JsonRpcClient::new(client_read, client_write));
        client.attach_connection_handlers(&rpc).await;
        *client.rpc_client.lock().await = Some(rpc);
        (client, server_read, server_write)
    }

    #[tokio::test]
    async fn server_scoped_rpcs_send_wire_names_and_camel_case_params() {
        let (client, mut server_read, mut server_write) = connected_client().await;

        let server = async {
            let save = read_framed(&mut server_read).await;
            write_framed(
                &mut server_write,
                json!({ "jsonrpc": "2.0", "id": save["id"], "result": { "usedKeychain": true } }),
            )
            .await;
            let revoke = read_framed(&mut server_read).await;
            write_framed(
                &mut server_write,
                json!({ "jsonrpc": "2.0", "id": revoke["id"], "result": { "revoked": true } }),
            )
            .await;
            (save, revoke)
        };
        let calls = async {
            let saved = client
                .mcp_secrets_save("server-token", "credential", Some("/cfg"))
                .await
                .unwrap();
            let revoked = client
                .extension_approvals_revoke("plugin:p:ext", "install-1", None)
                .await
                .unwrap();
            (saved, revoked)
        };
        let ((save, revoke), (saved, revoked)) = tokio::join!(server, calls);

        assert_eq!(save["method"], "mcp.secrets.save");
        assert_eq!(
            save["params"],
            json!({ "secretId": "server-token", "value": "credential", "configDir": "/cfg" })
        );
        assert!(saved.used_keychain);
        assert_eq!(revoke["method"], "permissions.extensionApprovals.revoke");
        assert_eq!(
            revoke["params"],
            json!({ "extensionName": "plugin:p:ext", "installationKey": "install-1" })
        );
        assert!(revoked.revoked);
    }

    #[tokio::test]
    async fn user_input_request_passes_image_constraints_and_returns_attachments() {
        let (client, mut server_read, mut server_write) = connected_client().await;
        let rpc = client.get_rpc_client().await.unwrap();
        let session = Arc::new(CopilotSession::new("s1".to_string(), rpc, None));
        let (seen_tx, mut seen_rx) = tokio::sync::mpsc::unbounded_channel();
        session
            .register_user_input_handler(Arc::new(move |request: UserInputRequest, _session_id| {
                let _ = seen_tx.send(request);
                Box::pin(async move {
                    Ok(UserInputResponse {
                        answer: String::new(),
                        was_freeform: true,
                        attachments: Some(vec![UserInputImageAttachment {
                            data: "aGVsbG8=".to_string(),
                            mime_type: "image/png".to_string(),
                            ..Default::default()
                        }]),
                    })
                })
            }))
            .await;
        client
            .sessions
            .lock()
            .await
            .insert("s1".to_string(), session);

        write_framed(
            &mut server_write,
            json!({
                "jsonrpc": "2.0",
                "id": 7,
                "method": "userInput.request",
                "params": {
                    "sessionId": "s1",
                    "question": "Attach a screenshot?",
                    "requestId": "req-1",
                    "imageAttachmentConstraints": {
                        "allowedMimeTypes": ["image/png"],
                        "maxBytesPerImage": 1048576,
                        "maxCount": 2
                    },
                    "_meta": { "ext": { "k": 1 } }
                }
            }),
        )
        .await;
        let response = read_framed(&mut server_read).await;

        assert_eq!(response["id"], 7);
        assert_eq!(
            response["result"],
            json!({
                "answer": "",
                "wasFreeform": true,
                "attachments": [{ "type": "image", "data": "aGVsbG8=", "mimeType": "image/png" }]
            })
        );
        let request = seen_rx.recv().await.unwrap();
        assert_eq!(request.request_id.as_deref(), Some("req-1"));
        assert_eq!(
            request.image_attachment_constraints,
            Some(UserInputImageAttachmentConstraints {
                allowed_mime_types: vec!["image/png".to_string()],
                max_bytes_per_image: 1_048_576,
                max_count: 2,
            })
        );
        assert_eq!(request.meta, Some(json!({ "ext": { "k": 1 } })));
    }

    #[test]
    fn user_input_response_omits_attachments_when_unset() {
        let response = UserInputResponse {
            answer: "yes".to_string(),
            was_freeform: false,
            ..Default::default()
        };
        assert_eq!(
            serde_json::to_value(response).unwrap(),
            json!({ "answer": "yes", "wasFreeform": false })
        );
    }
}
