package copilot

import (
	"context"
	"encoding/json"
	"fmt"
	"os"

	"github.com/microsoft/copilot-sdk/go/v2/rpc"
)

// ClientPluginOptions configures client plugins.
type ClientPluginOptions struct {
	// BuiltinDirectories contains absolute paths to trusted plugin
	// directories bundled by the host. When non-empty, Start replaces the
	// runtime's complete trusted built-in plugin directory set before sessions
	// can be created.
	BuiltinDirectories []string
}

// ClientExtensionOptions configures client extensions.
type ClientExtensionOptions struct {
	// LaunchProvider resolves launch profiles for extension entrypoints
	// discovered by the runtime. When non-nil, Start registers the provider
	// before any sessions can be created.
	LaunchProvider ExtensionLaunchProvider
}

// ClientInstallationOptions configures client installations.
type ClientInstallationOptions struct {
	// ConfirmationHandler receives connection-global human reviews
	// for experimental `installations.confirm` callbacks. It does not enable
	// installation capabilities or perform any runtime registration RPC.
	ConfirmationHandler InstallationConfirmationHandler
}

// ClientObservabilityOptions configures client observability.
type ClientObservabilityOptions struct {
	// LogLevel for the runtime. When empty (the default), the runtime
	// uses its own default level; the SDK does not pass --log-level.
	// Recognized values: "none", "error", "warning", "info", "debug", "all".
	LogLevel string
	// Telemetry configures OpenTelemetry integration for the runtime.
	// When non-nil, COPILOT_OTEL_ENABLED=true is set and any populated
	// fields are mapped to the corresponding environment variables.
	Telemetry *TelemetryConfig
}

// ClientGitHubOptions configures client github.
type ClientGitHubOptions struct {
	// Token is the GitHub token to use for authentication.
	// When provided, the token is passed to the runtime via environment
	// variable. This takes priority over other authentication methods.
	Token string
	// UseLoggedInUser controls whether to use the logged-in user for
	// authentication. When true, the runtime attempts to use stored OAuth
	// tokens or gh CLI auth. When false, only explicit tokens such as
	// GitHubToken are used; environment variable tokens (COPILOT_GITHUB_TOKEN,
	// GH_TOKEN, GITHUB_TOKEN) are ignored.
	// Default: true (but defaults to false when GitHubToken is provided).
	UseLoggedInUser *bool
	// OnTelemetry registers a connection-level callback (experimental)
	// that receives GitHub telemetry events the runtime forwards for sessions
	// opened by this client. When non-nil, every session created or resumed by
	// this client opts into telemetry forwarding (enableGitHubTelemetryForwarding).
	OnTelemetry func(notification *rpc.GitHubTelemetryNotification)
}

// ClientModelOptions configures client models.
type ClientModelOptions struct {
	// OnListModels is a custom handler for listing available models.
	// When provided, [Client.ListModels] calls this handler instead of
	// querying the runtime. Useful in BYOK mode to return models available
	// from your custom provider.
	OnListModels func(ctx context.Context) ([]Model, error)
	// RequestHandler registers a connection-level LLM inference callback. When
	// non-nil, the client registers as the inference provider on connect, and
	// the runtime routes its model-layer HTTP and WebSocket traffic through
	// this handler instead of issuing the calls itself. Works for both CAPI
	// and BYOK sessions.
	RequestHandler *CopilotRequestHandler
}

// SessionInferenceOptions configures session inference.
type SessionInferenceOptions struct {
	// AllowMultipleToolCalls permits multiple model tool calls per response (default true).
	// False is a provider hint, not an execution guarantee: all returned calls are
	// handled and handlers may run concurrently. Re-supply on cold resume.
	AllowMultipleToolCalls *bool
	// ReasoningEffort level for models that support it.
	// Valid values: "low", "medium", "high", "xhigh", "max"
	// Only applies to models where capabilities.supports.reasoningEffort is true.
	ReasoningEffort string
	// ReasoningSummary mode for models that support configurable reasoning summaries.
	// Use ReasoningSummaryNone to suppress summary output regardless of whether reasoning is enabled.
	ReasoningSummary ReasoningSummary
	// ContextTier pins the session to a context window tier for models that support it.
	// Use ContextTierDefault or ContextTierLongContext for the currently known tiers.
	ContextTier ContextTier
	// DefaultProvider resolves unqualified BYOK selections using a named connection.
	// Requires GitHub.Capi.Enabled=false. Nil omits the option (preserving it on
	// resume); a pointer to a nil *string clears it. Does not select the model.
	DefaultProvider **string
	// Providers configures named BYOK connections independently of GitHub identity.
	//
	// Experimental: Providers is part of an experimental multi-provider BYOK
	// surface and may change or be removed in future SDK or CLI releases.
	Providers []NamedProviderConfig
	// Models adds BYOK model definitions to the session's selectable model list,
	// each referencing a Providers entry by name.
	//
	// Experimental: Models is part of an experimental multi-provider BYOK
	// surface and may change or be removed in future SDK or CLI releases.
	Models []ProviderModelConfig
	// EnableCitations enables native model citations for supported providers.
	//
	// Experimental: EnableCitations is part of an experimental model capability
	// surface and may change or be removed in future SDK or CLI releases.
	EnableCitations *bool
	// Capabilities overrides individual model capabilities resolved by the runtime.
	// Only non-nil fields are applied over the runtime-resolved capabilities.
	Capabilities *rpc.ModelCapabilitiesOverride
}

// UnmarshalJSON preserves an explicitly cleared default connection.
func (options *SessionInferenceOptions) UnmarshalJSON(data []byte) error {
	type fields SessionInferenceOptions
	value := fields(*options)
	raw := struct {
		*fields
		DefaultProvider json.RawMessage
	}{fields: &value}
	if err := json.Unmarshal(data, &raw); err != nil {
		return err
	}
	if raw.DefaultProvider != nil {
		var provider *string
		if err := json.Unmarshal(raw.DefaultProvider, &provider); err != nil {
			return err
		}
		value.DefaultProvider = &provider
	}
	*options = SessionInferenceOptions(value)
	return nil
}

// SessionFilesystemOptions configures session filesystem.
type SessionFilesystemOptions struct {
	// AdditionalDirectories are directories the agent may access beyond WorkingDirectory.
	// Relative paths are resolved against WorkingDirectory. Re-supply them when resuming.
	AdditionalDirectories []string
	// TrackFileChanges opts in to capturing file changes from the first
	// turn for session rewind and cumulative session diff.
	TrackFileChanges *bool
}

// CreateSessionFilesystemOptions adds operation-specific settings.
type CreateSessionFilesystemOptions struct {
	// TrustWorkingDirectory trusts the working directory for this session
	// ("session") or persistently for the user ("user"). Empty leaves trust unchanged.
	TrustWorkingDirectory WorkingDirectoryTrust
	// AdditionalDirectories are directories the agent may access beyond WorkingDirectory.
	// Relative paths are resolved against WorkingDirectory. Re-supply them when resuming.
	AdditionalDirectories []string
	// TrackFileChanges opts in to capturing file changes from the first
	// turn for session rewind and cumulative session diff.
	TrackFileChanges *bool
}

// SessionStateOptions configures session state.
type SessionStateOptions struct {
	// SkipEmbeddingRetrieval, when non-nil, controls embedding-based retrieval
	// for this session. Use in multitenant deployments to prevent cross-session
	// information leakage through the shared embedding cache.
	SkipEmbeddingRetrieval *bool
	// EmbeddingCacheStorage controls how the embedding cache is stored for this session.
	// "persistent" caches on disk and shares across sessions/restarts.
	// "in-memory" caches in memory only and discards when the session ends.
	EmbeddingCacheStorage *string
	// EnableCrossSessionStore, when non-nil, controls the cross-session store for search
	// and retrieval. When false, session content is not written to or read from the
	// shared session store.
	EnableCrossSessionStore *bool
	// InfiniteSessions configures infinite sessions for persistent workspaces and automatic compaction.
	// When enabled (default), sessions automatically manage context limits and persist state.
	InfiniteSessions *InfiniteSessionConfig
	// Memory configures the memory feature for the session. When omitted, the
	// runtime default applies.
	Memory *MemoryConfiguration
	// StoreMemory stores memories in the app instead of the built-in memory backends.
	//
	// The runtime validates the memory, filters secrets and requests approval,
	// then calls this callback. store_memory succeeds only after the callback
	// returns nil; an error fails the tool call with a generic message and
	// nothing is stored elsewhere. While set, the runtime retrieves no memories
	// and vote_memory is unavailable, so supply recall yourself, for example
	// through SystemMessage. Requires Memory.Enabled to be true; creating or resuming
	// a session fails otherwise.
	// Experimental: this option may change or be removed in a future release.
	StoreMemory func(ctx context.Context, memory StoredMemory) error `json:"-"`
	// CreateSessionFSProvider supplies a handler for session filesystem operations.
	// This takes effect only when ClientOptions.SessionFS is configured.
	CreateSessionFSProvider func(session *Session) SessionFSProvider
}

// SessionInstructionOptions configures session instructions.
type SessionInstructionOptions struct {
	// OrganizationText provides organization-level custom instructions
	// to include in the system prompt. Allows hosts to inject organization-specific
	// guidance without relying on filesystem-based instruction discovery.
	OrganizationText *string
	// OnDemandDiscovery, when non-nil, controls on-demand discovery
	// of instruction files (AGENTS.md, .github/copilot-instructions.md, etc.) after
	// successful file views.
	OnDemandDiscovery *bool
	// SkipCustomInstructions, when non-nil, controls whether the runtime loads
	// custom instruction files. See also [ClientOptions.Mode] = [ModeEmpty].
	SkipCustomInstructions *bool
	// Directories contains explicit discovery-only instruction roots using existing SDK formats.
	// No filesystem grant. Available without EnableConfigDiscovery, subject to instruction/managed
	// disables. Relative paths resolve once when supplied against the request's effective
	// WorkingDirectory; retained roots do not move. Nil and omission are equivalent;
	// an empty non-nil list clears only the explicit SDK contribution.
	Directories []string
}

// CreateSessionInstructionOptions adds operation-specific settings.
type CreateSessionInstructionOptions struct {
	// OrganizationText provides organization-level custom instructions
	// to include in the system prompt. Allows hosts to inject organization-specific
	// guidance without relying on filesystem-based instruction discovery.
	OrganizationText *string
	// OnDemandDiscovery, when non-nil, controls on-demand discovery
	// of instruction files (AGENTS.md, .github/copilot-instructions.md, etc.) after
	// successful file views.
	OnDemandDiscovery *bool
	// SkipCustomInstructions, when non-nil, controls whether the runtime loads
	// custom instruction files. See also [ClientOptions.Mode] = [ModeEmpty].
	SkipCustomInstructions *bool
	// RefreshCustomInstructions, when true, invalidates the process-wide custom-instruction
	// discovery cache before constructing this new session. Nil or false retains the cache.
	// Other sessions in this runtime may observe updated instructions on later turns
	// or discovery. It does not watch files or enable disabled instruction loading.
	RefreshCustomInstructions *bool
	// Directories contains explicit discovery-only instruction roots using existing SDK formats.
	// No filesystem grant. Available without EnableConfigDiscovery, subject to instruction/managed
	// disables. Relative paths resolve once when supplied against the request's effective
	// WorkingDirectory; retained roots do not move. Nil and omission are equivalent;
	// an empty non-nil list clears only the explicit SDK contribution.
	Directories []string
}

// SessionHookOptions configures session hooks.
type SessionHookOptions struct {
	// EnableRepositoryHooks, when non-nil, controls repository-origin file hooks from
	// .github/hooks/ and trusted repository-configured roots. It does not control
	// explicit HookDirectories or Hooks callbacks.
	EnableRepositoryHooks *bool
	// Handlers configures hook handlers for session lifecycle events
	Handlers *SessionHooks
	// Directories contains explicit discovery-only file-hook roots, separate from Hooks callbacks.
	// No filesystem grant. Available without EnableConfigDiscovery or EnableFileHooks
	// (repository-origin hooks only), subject to hook/managed disables. Relative paths resolve
	// once when supplied against the request's effective WorkingDirectory; retained roots do not
	// move. Nil and omission are equivalent; an empty non-nil list clears only the explicit SDK contribution.
	Directories []string
}

// SessionGitOptions configures session git.
type SessionGitOptions struct {
	// EnableHostGitOperations, when non-nil, controls git operations on the host
	// filesystem (branch detection, file status, commit history). When false, no
	// git context is surfaced in the system prompt.
	EnableHostGitOperations *bool
	// CoauthorEnabled, when non-nil, controls whether the agent is instructed to
	// include a Co-authored-by trailer in commit messages. Subagents inherit this
	// setting. See also [ClientOptions.Mode] = [ModeEmpty].
	CoauthorEnabled *bool
}

// SessionSkillOptions configures session skills.
type SessionSkillOptions struct {
	// Enabled, when non-nil, controls skill loading (including builtin skills
	// and discovered skill directories). When false, no skills are loaded regardless
	// of SkillDirectories or EnableConfigDiscovery settings.
	Enabled *bool
	// IncludedBuiltinSkills is the allowlist of runtime-bundled skill names.
	// In ModeEmpty, nil excludes all built-in skills; a non-nil list opts the
	// named built-ins back in. Skills from other sources remain eligible.
	IncludedBuiltinSkills []string
	// Directories contains explicit discovery-only skill roots, not filesystem grants.
	// Available without EnableConfigDiscovery, subject to skill/managed disables.
	// Relative paths resolve once when supplied against the request's effective WorkingDirectory;
	// retained roots do not move on cwd changes. Nil and omission are equivalent;
	// an empty non-nil list clears only the explicit SDK contribution.
	Directories []string
	// Provider supplies ephemeral SDK-hosted skills for this session. The
	// provider is never serialized or persisted; re-supply it when resuming.
	//
	// Experimental: this API may change or be removed in future SDK or CLI
	// releases. Implementations must be safe for concurrent calls.
	Provider SkillProvider `json:"-"`
	// DisabledSkills is a list of skill names to disable
	DisabledSkills []string
}

// SessionToolOptions configures session tools.
type SessionToolOptions struct {
	// Definitions exposes caller-implemented tools to the CLI. A Tool with a nil Handler
	// is declaration-only; the consumer must resolve its calls via pending tool RPCs.
	Definitions []Tool
	// AvailableTools is a list of tool names to allow. When specified, only these tools will be available.
	// When ExcludedTools is also set, a tool must match AvailableTools and not match ExcludedTools.
	AvailableTools []string
	// ExcludedTools is a list of tool names to disable. All other tools remain available.
	// Takes precedence over AvailableTools: a tool listed here is disabled even if it
	// also matches AvailableTools.
	ExcludedTools []string
	// ManageScheduleEnabled, when non-nil, controls whether the
	// `manage_schedule` tool is exposed. See also [ClientOptions.Mode] =
	// [ModeEmpty].
	ManageScheduleEnabled *bool
	// LargeOutput configures handling of large tool outputs. When a tool produces
	// output exceeding the configured size, the output is written to a temp file
	// and a reference is returned to the model instead of the full payload.
	LargeOutput *LargeToolOutputConfig
	// Search overrides the runtime's built-in tool-search behavior, which
	// defers rarely used tools behind a searchable index. When nil, the runtime
	// default applies.
	Search *ToolSearchConfig
	// ImageGeneration is the non-persisted image generation opt-in.
	// Experimental: this API may change or be removed in a future release.
	ImageGeneration *ImageGenerationConfig
}

// SessionAgentOptions configures session agents.
type SessionAgentOptions struct {
	// ExcludedBuiltinAgents is a list of built-in agent names to exclude from
	// the session. Excluded built-in agents are hidden from discovery and cannot
	// be selected or invoked unless a custom agent with the same name is
	// configured.
	ExcludedBuiltinAgents []string
	// IncludedBuiltinAgents allows only the named built-in subagents, subject to
	// availability and ExcludedBuiltInAgents. A non-nil []string{} disables all
	// built-in subagents; custom agents remain available, including same-name agents.
	// Nil uses defaults on create or cold resume and preserves the current allowlist
	// on warm resume. Not persisted; re-supply on resume after restart.
	IncludedBuiltinAgents []string
	// LocalDiscoveryOnly, when non-nil, restricts custom agents to those
	// defined locally. See also [ClientOptions.Mode] = [ModeEmpty].
	LocalDiscoveryOnly *bool
	// Definitions configures custom agents for the session
	Definitions []CustomAgentConfig
	// Default configures the default agent (the built-in agent that handles turns when no custom agent is selected).
	// Use ExcludedTools to hide tools from the default agent while keeping them available to sub-agents.
	Default *DefaultAgentConfig
	// Selected is the name of the custom agent to activate when the session starts.
	// Must match the Name of one of the agents in CustomAgents.
	Selected string
	// Directories contains explicit discovery-only agent roots, separate from CustomAgents.
	// No filesystem grant. Available without EnableConfigDiscovery, subject to source/managed
	// disables. Relative paths resolve once when supplied against the request's effective
	// WorkingDirectory; retained roots do not move. Nil and omission are equivalent;
	// an empty non-nil list clears only the explicit SDK contribution.
	Directories []string
}

// SessionPermissionOptions configures session permissions.
type SessionPermissionOptions struct {
	// OnRequestWithEnvelope overrides the other permission decision
	// handlers when non-nil, preserving the complete permission envelope.
	OnRequestWithEnvelope PermissionHandlerWithEnvelopeFunc
	// OnRequestWithPrompt receives the optional derived user-facing
	// prompt supplied by newer runtimes. When both permission handlers are set,
	// this handler takes precedence.
	OnRequestWithPrompt PermissionHandlerWithPromptFunc
	// OnRequest is an optional handler for permission requests from the server.
	// When nil, permission requests are surfaced as events and left pending for the
	// consumer to resolve via pending permission RPCs.
	OnRequest PermissionHandlerFunc
	// Sandbox is applied when the session is constructed, before MCP servers start.
	// Same shape as the sandboxConfig option of session options update;
	// an identical later update is a no-op.
	Sandbox *rpc.SandboxConfig
	// Mode is applied before the session's first turn: before
	// CreateSession returns. It uses the same checks as
	// session.RPC.Permissions.SetMode; a mode that managed policy or session
	// capabilities refuse leaves the current mode in place.
	// Session.PermissionModeResult reports whether it was applied and the
	// authoritative mode. Empty keeps the default mode. Not supported for cloud
	// sessions.
	Mode rpc.PermissionMode
}

// SessionMCPOptions configures session mcp.
type SessionMCPOptions struct {
	// OnAuthRuntimeRefresh overrides OnAuthRequest when non-nil.
	OnAuthRuntimeRefresh MCPAuthRuntimeRefreshHandler
	// OnAuthRequest is an optional handler for MCP OAuth requests from MCP servers.
	// When provided, the SDK can satisfy MCP server OAuth requests with host-provided
	// token data or cancellation.
	OnAuthRequest MCPAuthHandler
	// Servers configures MCP servers for the session
	Servers map[string]MCPServerConfig
	// OAuthTokenStorage controls how MCP OAuth tokens are stored for this session.
	// When empty, the runtime default ("in-memory") is used.
	OAuthTokenStorage string
	// AuthClientIDMetadataURL identifies the host for MCP OAuth authorization.
	// When empty, no host identity is supplied.
	AuthClientIDMetadataURL string
	// DisabledServers is a list of exact MCP server names to disable for this session.
	// Disabled servers are not started or authenticated on create or cold resume.
	// A resident resume cannot stop servers that are already running.
	DisabledServers []string
	// EnableApps enables MCP Apps (SEP-1865) UI passthrough on this session.
	//
	// Experimental: EnableMCPApps is part of an experimental wire-protocol
	// surface (SEP-1865) and may change or be removed in a future release.
	//
	// When true AND the runtime has MCP Apps enabled (via the MCP_APPS feature
	// flag or COPILOT_MCP_APPS=true environment override), the runtime adds the
	// mcp-apps capability to the session, which causes it to advertise the
	// extensions.io.modelcontextprotocol/ui extension to MCP servers (so they
	// expose _meta.ui.resourceUri on tools) and to expose the
	// session.rpc.mcp.apps.{listTools,callTool,readResource,setHostContext,
	// getHostContext,diagnose} JSON-RPC methods.
	//
	// If the runtime gate is off, the opt-in is silently dropped server-side
	// (the runtime logs a warning); the session is created normally but the
	// MCP Apps surface is unavailable. Inspect the runtime's
	// capabilities.ui.mcpApps on the create/resume response to detect this.
	//
	// SDK consumers MUST set this to true only when they have an iframe renderer
	// that can display ui:// MCP App bundles. Setting it without a renderer will
	// cause MCP servers to register UI-enabled tool variants the consumer cannot
	// display.
	EnableApps bool
}

// SessionGitHubOptions configures session github.
type SessionGitHubOptions struct {
	// TokenProvider acquires session-scoped GitHub tokens on demand. It
	// cannot be combined with GitHubToken.
	TokenProvider GitHubTokenProvider
	// Capi configures provider-scoped CAPI (Copilot API) session options.
	Capi *CapiSessionOptions
	// TelemetryEnabled enables or disables internal session telemetry for this session.
	// When false, disables session telemetry. When nil (the default) or true,
	// telemetry is enabled for eligible GitHub-authenticated sessions, including
	// custom Provider (BYOK) sessions. Host and account policies still control
	// uploads. This is independent of the OpenTelemetry
	// configuration in ClientOptions.Observability.Telemetry.
	TelemetryEnabled *bool
	// MCP configures the built-in GitHub MCP server.
	// DisableFormDeferral only applies to that server and only has an effect
	// when MCP Apps and form-backed GitHub tools are enabled.
	MCP *GitHubMCPToolConfig
	// Token is an optional per-session GitHub token used for authentication.
	// When provided, the session authenticates as the token's owner instead of
	// using the global client-level auth.
	Token string `json:"-"`
	// FetchManagedSettings, when set to true, opts the runtime into
	// self-fetching enterprise managed settings (bypass-permissions policy) at
	// session bootstrap using the session's GitHubToken. Requires GitHubToken to
	// be set; if omitted, the runtime is expected to reject session creation
	// (fail-closed). Unset behaves exactly as before.
	FetchManagedSettings *bool
	// InteractiveUser declares that a human drives this session's conversation,
	// enabling user-message analysis telemetry for user-authored messages.
	// Automated messages remain excluded. Defaults to false for SDK sessions.
	// Ignored for cloud sessions.
	InteractiveUser *bool
}

// SessionInteractionOptions configures session interaction.
type SessionInteractionOptions struct {
	// AutoModeRecovery enrolls in runtime-owned active-request recovery.
	// Requires OnAutoModeSwitchRequest; re-supply on resume. The handler must
	// not resend or rewind the request.
	AutoModeRecovery bool
	// OnUserInputRequest handles legacy question-and-answer requests from the agent
	// and enables the legacy ask_user tool.
	OnUserInputRequest UserInputHandler
	// OnAttributedUserInputRequest reports who answered; see [AttributedUserInputResponse].
	// When both handlers are set, this handler takes precedence.
	// Experimental: this API may change or be removed.
	OnAttributedUserInputRequest AttributedUserInputHandler
	// OnShellAuthenticationRequest handles passwords and other answers that shell
	// commands run by the agent ask for. When provided, `sudo -A`, `ssh`, and
	// `git` prompts reach this handler instead of failing for lack of a
	// terminal. Cancel requests when nobody can answer.
	OnShellAuthenticationRequest ShellAuthenticationHandler
	// AskUserVariant selects the model-facing shape of the ask_user tool.
	// The zero value preserves legacy behavior. AskUserVariantElicitation also
	// requires OnElicitationRequest or OnAttributedElicitationRequest so the host
	// can answer structured forms.
	AskUserVariant AskUserVariant
	// Commands registers slash-commands for this session. Each command appears as
	// /name in the CLI TUI for the user to invoke. The Handler is called when the
	// command is executed.
	Commands []CommandDefinition
	// OnElicitationRequest is a handler for elicitation requests from the server.
	// When provided, the server may call back to this client for form-based UI dialogs
	// (e.g. from MCP tools). Also enables the elicitation capability on the session.
	OnElicitationRequest ElicitationHandler
	// OnAttributedElicitationRequest reports who answered; see [AttributedElicitationResult].
	// When both handlers are set, this handler takes precedence.
	// Experimental: this API may change or be removed.
	OnAttributedElicitationRequest AttributedElicitationHandler
	// OnExitPlanModeRequest is a handler for exit-plan-mode requests from the server.
	// When provided, enables exitPlanMode.request callbacks for the session.
	OnExitPlanModeRequest ExitPlanModeRequestHandler
	// OnAttributedExitPlanModeRequest reports who reviewed the plan; see [AttributedExitPlanModeResult].
	// When both handlers are set, this handler takes precedence.
	// Experimental: this API may change or be removed.
	OnAttributedExitPlanModeRequest AttributedExitPlanModeRequestHandler
	// OnAutoModeSwitchRequest is a handler for auto-mode-switch requests from the server.
	// When provided, enables autoModeSwitch.request callbacks for the session.
	OnAutoModeSwitchRequest AutoModeSwitchRequestHandler
}

// SessionEventOptions configures session events.
type SessionEventOptions struct {
	// OnEventWithPermissionEnvelope registers a lossless permission event handler
	// before session creation. Equivalent to session.OnWithPermissionEnvelope,
	// but also receives events emitted before CreateSession returns.
	OnEventWithPermissionEnvelope SessionEventHandler
	// Streaming enables streaming of assistant message and reasoning chunks.
	// When non-nil and true, assistant.message_delta and assistant.reasoning_delta
	// events with deltaContent are sent as the response is generated.
	// When nil, the runtime decides (currently defaults to non-streaming).
	Streaming *bool
	// IncludeSubAgentStreamingEvents includes sub-agent streaming events in the
	// event stream. When true, streaming delta events from sub-agents (e.g.,
	// assistant.message_delta, assistant.reasoning_delta, assistant.streaming_delta
	// with agentId set) are forwarded to this connection. When false, only
	// non-streaming sub-agent events and subagent.* lifecycle events are forwarded;
	// streaming deltas from sub-agents are suppressed. When nil, defaults to true.
	IncludeSubAgentStreamingEvents *bool
	// OnEvent is an optional event handler that is registered on the session before
	// the session.create RPC is issued. This guarantees that early events emitted
	// by the CLI during session creation (e.g. session.start) are delivered to the
	// handler. Equivalent to calling session.On(handler) immediately after creation,
	// but executes earlier in the lifecycle so no events are missed.
	OnEvent SessionEventHandler
}

// SessionExecutionOptions configures session execution.
type SessionExecutionOptions struct {
	// Limits applies limits to this session's current accounting window.
	//
	// Experimental: SessionLimits is part of an experimental runtime accounting
	// surface and may change or be removed in future SDK or CLI releases.
	Limits *rpc.SessionLimitsConfig
	// Autopilot configures continuation without changing the session mode.
	// Omitted fields use defaults on create and preserve resident values on warm resume.
	// Experimental: this policy may change in a future release.
	Autopilot *rpc.AutopilotOptions
}

// SessionExperimentOptions configures session experiments.
type SessionExperimentOptions struct {
	// EnableExperimentalMode controls whether the session enables experimental
	// features. When nil, it defaults to false in [ModeEmpty]; otherwise the
	// runtime decides.
	EnableExperimentalMode *bool
	// Assignments injects ExP assignment ("flight") data for this session,
	// in the same JSON shape the Copilot CLI fetches from the experimentation
	// service (CopilotExpAssignmentResponse). When supplied, the runtime feeds
	// it into the same feature-flag path as CLI-fetched assignments and stamps
	// it onto telemetry and the CAPI request header. When absent, the session
	// does not block on ExP. Malformed payloads are dropped by the runtime
	// (fail-open).
	//
	// Internal: ExpAssignments is part of the SDK's internal API surface,
	// intended for trusted out-of-process integrators, and is not intended for
	// general external use.
	Assignments *CopilotExpAssignmentResponse
	// Flags contains feature-flag values resolved by the host for this session.
	// Re-supply them when resuming after a runtime restart.
	Flags map[string]bool
}

// SessionPluginOptions configures session plugins.
type SessionPluginOptions struct {
	// Directories is a list of local filesystem paths to Open Plugins-format
	// directories (https://open-plugins.com/) to load for this session.
	// Relative paths resolve against WorkingDirectory (or the runtime cwd if unset).
	// Treated as an explicit opt-in: plugin agents and rules load even when
	// EnableConfigDiscovery is false.
	Directories []string
}

// SessionCanvasOptions configures session canvases.
type SessionCanvasOptions struct {
	// Definitions declares canvases this session provides. Sent over the wire on
	// `session.create`. CanvasHandler must be set when this is non-empty (the
	// SDK does not enforce this — declarations without a handler will surface
	// canvas RPCs that return a canvas_handler_unset error envelope).
	Definitions []CanvasDeclaration
	// RequestRenderer asks the host to enable canvas rendering for this session.
	RequestRenderer *bool
	// Handler receives inbound canvas.open / canvas.close / canvas.action.invoke
	// requests for this session. The SDK does not maintain a per-canvas registry;
	// the handler must dispatch on CanvasProviderOpenRequest.CanvasID itself.
	Handler CanvasHandler `json:"-"`
	// Provider is the stable identity for a host/SDK connection that
	// supplies built-in canvases, so they survive reconnect and CLI restart.
	Provider *CanvasProviderIdentity
}

// ResumeSessionCanvasOptions adds operation-specific settings.
type ResumeSessionCanvasOptions struct {
	// Definitions declares canvases this session provides. Sent over the wire on
	// `session.create`. CanvasHandler must be set when this is non-empty (the
	// SDK does not enforce this — declarations without a handler will surface
	// canvas RPCs that return a canvas_handler_unset error envelope).
	Definitions []CanvasDeclaration
	// RequestRenderer asks the host to enable canvas rendering for this session.
	RequestRenderer *bool
	// Handler receives inbound canvas.open / canvas.close / canvas.action.invoke
	// requests for this session. The SDK does not maintain a per-canvas registry;
	// the handler must dispatch on CanvasProviderOpenRequest.CanvasID itself.
	Handler CanvasHandler `json:"-"`
	// Provider is the stable identity for a host/SDK connection that
	// supplies built-in canvases, so they survive reconnect and CLI restart.
	Provider *CanvasProviderIdentity
	// Open declares canvas instances the caller knows were open before
	// this resume so the runtime can re-attach them. Sent over the wire on
	// `session.resume` as `openCanvases`.
	Open []rpc.OpenCanvasInstance
}

// SessionExtensionOptions configures session extensions.
type SessionExtensionOptions struct {
	// Enabled opts this connection into the extension surface: when true,
	// the host wires extension management tools and per-extension tool dispatch onto
	// the session.
	Enabled *bool
	// SDKPath optionally overrides the bundled `@github/copilot-sdk` drop
	// injected into extension subprocesses. When set to an absolute path containing
	// a valid `copilot-sdk/` folder (with `index.js` and `extension.js` at the
	// root), the host injects the override into every forked extension; invalid or
	// missing paths fall back to the bundled SDK silently.
	SDKPath *string
	// Identity identifies the stable extension providing this session's canvases.
	Identity *ExtensionInfo
}

// SessionPolicyOptions configures session policy.
type SessionPolicyOptions struct {
	// ExcludedManagedSettings contains exact dotted setting paths excluded from
	// fetched, device, and policy-helper policy. Host-injected policy and availability
	// checks are unaffected. Not persisted; re-supply on resume.
	ExcludedManagedSettings []string
	// EnforceManagedModelDefaults, when true, enforces locked managed model
	// controls for this session. Conflicting changes are rejected; overridable
	// managed defaults remain mutable.
	EnforceManagedModelDefaults *bool
	// ManagedSettings supplies host-injected enterprise managed settings for
	// the session. Unlike EnableManagedSettings (which asks the runtime to
	// self-fetch account/org and device policy), this provides the managed
	// policy directly. The runtime validates it with the same
	// managed-permission parser it uses for fetched policy and composes it
	// restrictively with any self-fetched (server) and device-managed (MDM)
	// layers. It is startup-only and not persisted: re-supply it on resume,
	// where it replaces the prior injected layer (omitting it clears the
	// layer). It may be combined with EnableManagedSettings. Requires a runtime
	// whose RPC schema includes managedSettings.
	ManagedSettings *ManagedSettings
}

// ClientOptions groups options by the feature they configure.
type ClientOptions struct {
	// InternalTestOptions is unsupported SDK test infrastructure configuration.
	// Only applies to runtimes owned by this client.
	InternalTestOptions *InternalTestOptions
	Runtime             RuntimeConnection
	Plugins             ClientPluginOptions
	Extensions          ClientExtensionOptions
	// Network lets the SDK host resolve the runtime's proxies and answer
	// Negotiate (Kerberos) proxy challenges. When it configures a
	// callback, Start installs the `network.resolveProxy` and
	// `network.authenticateNegotiate` handlers
	// for the configured callbacks and registers them before any sessions can be
	// created. See [NetworkConfig].
	//
	// Experimental: Network is part of an experimental API and may change or be
	// removed.
	Network       *NetworkConfig
	Installations ClientInstallationOptions
	Observability ClientObservabilityOptions
	GitHub        ClientGitHubOptions
	Models        ClientModelOptions
	// SessionFS configures a custom session filesystem provider.
	// When provided, the client registers as the session filesystem provider
	// on connection, routing session-scoped file I/O through per-session
	// handlers.
	SessionFS *SessionFSConfig
	// ClientInfo declares the integrating application's identity, forwarded to the
	// runtime on the `server.connect` handshake. Declaring it lets the
	// telemetry the runtime emits on this connection be attributed to a
	// consistent surface (the application and its Copilot integration) instead of
	// the runtime's own build. All fields are optional; leave it nil to keep the
	// runtime's default attribution.
	ClientInfo *ClientInfo
	// Entra configures the host's Entra application for its sessions.
	// Required for built-in Entra sign-in and token renewal; other authentication methods remain available.
	//
	// Experimental: this API may change or be removed.
	Entra *EntraAuthOptions
	// Mode controls the default tool surface and feature flags presented to
	// sessions created by this client. Specify [ModeCodingAgent] for coding-agent
	// defaults or [ModeEmpty] to opt in to multi-tenant safe defaults — see [ClientMode] for details.
	//
	// When Mode is [ModeEmpty], NewClient requires an owned runtime's BaseDirectory,
	// SessionFS, or a [TCPConnection] so the runtime has persistent storage
	// for session state.
	Mode ClientMode
}

func (options *ClientOptions) normalize() *clientOptions {
	if options == nil {
		return &clientOptions{}
	}
	result := &clientOptions{
		InternalTestOptions:             options.InternalTestOptions,
		Connection:                      options.Runtime,
		BuiltinPluginDirectories:        options.Plugins.BuiltinDirectories,
		ExtensionLaunchProvider:         options.Extensions.LaunchProvider,
		Network:                         options.Network,
		InstallationConfirmationHandler: options.Installations.ConfirmationHandler,
		LogLevel:                        options.Observability.LogLevel,
		GitHubToken:                     options.GitHub.Token,
		UseLoggedInUser:                 options.GitHub.UseLoggedInUser,
		OnListModels:                    options.Models.OnListModels,
		SessionFS:                       options.SessionFS,
		RequestHandler:                  options.Models.RequestHandler,
		OnGitHubTelemetry:               options.GitHub.OnTelemetry,
		Telemetry:                       options.Observability.Telemetry,
		ClientInfo:                      options.ClientInfo,
		Entra:                           options.Entra,
		Mode:                            options.Mode,
	}

	connection := options.Runtime
	if connection == nil {
		connection = ForDefault(nil)
	}
	result.Connection = connection
	switch conn := connection.(type) {
	case SpawnedStdioConnection:
		result.BaseDirectory = conn.BaseDirectory
		result.SessionIdleTimeoutSeconds = conn.SessionIdleTimeoutSeconds
	case SpawnedTCPConnection:
		result.BaseDirectory = conn.BaseDirectory
		result.SessionIdleTimeoutSeconds = conn.SessionIdleTimeoutSeconds
	case InProcessConnection:
		result.BaseDirectory = conn.BaseDirectory
		result.SessionIdleTimeoutSeconds = conn.SessionIdleTimeoutSeconds
	}

	return result
}

// CreateSessionOptions groups options by the feature they configure.
type CreateSessionOptions struct {
	Inference SessionInferenceOptions
	// SessionID is an optional custom session ID
	SessionID string
	// Name names the session at creation, as session.name.set would, and is
	// validated the same way: non-empty, no leading or trailing whitespace, at
	// most 100 UTF-16 units, no control characters, and no double quotes. A name
	// also makes the session persist from the start, so it can be listed and
	// resumed without ever running a turn. Local sessions only: cloud creation
	// rejects it.
	Name string
	// ClientName identifies the application using the SDK.
	// Included in the User-Agent header for API requests.
	ClientName string
	// Model to use for this session
	Model string
	// ConfigDirectory overrides the default configuration directory location.
	// When specified, the session will use this directory for storing config and state.
	ConfigDirectory string
	// EnableConfigDiscovery enables runtime discovery of supported configuration.
	// Explicitly supplied configuration takes precedence over discovered values.
	// Nil leaves the runtime default unchanged; use Bool(false) to explicitly disable discovery.
	EnableConfigDiscovery *bool
	Filesystem            CreateSessionFilesystemOptions
	State                 SessionStateOptions
	Instructions          CreateSessionInstructionOptions
	Hooks                 SessionHookOptions
	Git                   SessionGitOptions
	Skills                SessionSkillOptions
	Tools                 SessionToolOptions
	// SystemMessage configures system message customization
	SystemMessage *SystemMessageConfig
	Agents        SessionAgentOptions
	Permissions   SessionPermissionOptions
	MCP           SessionMCPOptions
	GitHub        SessionGitHubOptions
	Interaction   SessionInteractionOptions
	// WorkingDirectory is the working directory for the session.
	// Tool operations will be relative to this directory.
	WorkingDirectory string
	Events           SessionEventOptions
	Execution        SessionExecutionOptions
	Experiments      SessionExperimentOptions
	// Diagnostics enables explicitly configured session diagnostic sources.
	// Diagnostics can contain MCP payloads, tool arguments, paths, and server
	// stderr, so hosts must not upload or export them automatically.
	Diagnostics *rpc.DiagnosticsConfiguration
	Plugins     SessionPluginOptions
	// Cloud creates a remote session in the cloud instead of a local session.
	// The optional repository is associated with the cloud session.
	Cloud      *CloudSessionOptions
	Canvases   SessionCanvasOptions
	Extensions SessionExtensionOptions
	Policy     SessionPolicyOptions
}

func (options *CreateSessionOptions) normalize() *sessionOptions {
	if options == nil {
		return &sessionOptions{}
	}
	result := &sessionOptions{
		OnPermissionRequestWithEnvelope:    options.Permissions.OnRequestWithEnvelope,
		OnMCPAuthRuntimeRefresh:            options.MCP.OnAuthRuntimeRefresh,
		AutoModeRecovery:                   options.Interaction.AutoModeRecovery,
		AllowMultipleToolCalls:             options.Inference.AllowMultipleToolCalls,
		SessionID:                          options.SessionID,
		Name:                               options.Name,
		ClientName:                         options.ClientName,
		Model:                              options.Model,
		ReasoningEffort:                    options.Inference.ReasoningEffort,
		ReasoningSummary:                   options.Inference.ReasoningSummary,
		ContextTier:                        options.Inference.ContextTier,
		ConfigDirectory:                    options.ConfigDirectory,
		EnableConfigDiscovery:              options.EnableConfigDiscovery,
		TrustWorkingDirectory:              options.Filesystem.TrustWorkingDirectory,
		SkipEmbeddingRetrieval:             options.State.SkipEmbeddingRetrieval,
		EmbeddingCacheStorage:              options.State.EmbeddingCacheStorage,
		OrganizationCustomInstructions:     options.Instructions.OrganizationText,
		EnableOnDemandInstructionDiscovery: options.Instructions.OnDemandDiscovery,
		EnableFileHooks:                    options.Hooks.EnableRepositoryHooks,
		EnableHostGitOperations:            options.Git.EnableHostGitOperations,
		EnableSessionStore:                 options.State.EnableCrossSessionStore,
		EnableSkills:                       options.Skills.Enabled,
		IncludedBuiltinSkills:              options.Skills.IncludedBuiltinSkills,
		Tools:                              options.Tools.Definitions,
		SystemMessage:                      options.SystemMessage,
		AvailableTools:                     options.Tools.AvailableTools,
		ExcludedTools:                      options.Tools.ExcludedTools,
		ExcludedBuiltInAgents:              options.Agents.ExcludedBuiltinAgents,
		IncludedBuiltInAgents:              options.Agents.IncludedBuiltinAgents,
		OnPermissionRequest:                options.Permissions.OnRequest,
		OnPermissionRequestWithPrompt:      options.Permissions.OnRequestWithPrompt,
		OnMCPAuthRequest:                   options.MCP.OnAuthRequest,
		GitHubTokenProvider:                options.GitHub.TokenProvider,
		OnUserInputRequest:                 options.Interaction.OnUserInputRequest,
		OnAttributedUserInputRequest:       options.Interaction.OnAttributedUserInputRequest,
		OnShellAuthenticationRequest:       options.Interaction.OnShellAuthenticationRequest,
		AskUserVariant:                     options.Interaction.AskUserVariant,
		Hooks:                              options.Hooks.Handlers,
		WorkingDirectory:                   options.WorkingDirectory,
		AdditionalDirectories:              options.Filesystem.AdditionalDirectories,
		Streaming:                          options.Events.Streaming,
		IncludeSubAgentStreamingEvents:     options.Events.IncludeSubAgentStreamingEvents,
		DefaultProvider:                    options.Inference.DefaultProvider,
		Capi:                               options.GitHub.Capi,
		Providers:                          options.Inference.Providers,
		Models:                             options.Inference.Models,
		EnableSessionTelemetry:             options.GitHub.TelemetryEnabled,
		EnableCitations:                    options.Inference.EnableCitations,
		EnableFileChangeTracking:           options.Filesystem.TrackFileChanges,
		SessionLimits:                      options.Execution.Limits,
		Autopilot:                          options.Execution.Autopilot,
		EnableExperimentalMode:             options.Experiments.EnableExperimentalMode,
		SkipCustomInstructions:             options.Instructions.SkipCustomInstructions,
		RefreshCustomInstructions:          options.Instructions.RefreshCustomInstructions,
		CustomAgentsLocalOnly:              options.Agents.LocalDiscoveryOnly,
		CoauthorEnabled:                    options.Git.CoauthorEnabled,
		ManageScheduleEnabled:              options.Tools.ManageScheduleEnabled,
		ModelCapabilities:                  options.Inference.Capabilities,
		MCPServers:                         options.MCP.Servers,
		Diagnostics:                        options.Diagnostics,
		MCPOAuthTokenStorage:               options.MCP.OAuthTokenStorage,
		AuthClientIDMetadataURL:            options.MCP.AuthClientIDMetadataURL,
		CustomAgents:                       options.Agents.Definitions,
		DefaultAgent:                       options.Agents.Default,
		Agent:                              options.Agents.Selected,
		SkillDirectories:                   options.Skills.Directories,
		SkillProvider:                      options.Skills.Provider,
		AgentDirectories:                   options.Agents.Directories,
		HookDirectories:                    options.Hooks.Directories,
		PluginDirectories:                  options.Plugins.Directories,
		InstructionDirectories:             options.Instructions.Directories,
		DisabledSkills:                     options.Skills.DisabledSkills,
		DisabledMCPServers:                 options.MCP.DisabledServers,
		SandboxConfig:                      options.Permissions.Sandbox,
		InfiniteSessions:                   options.State.InfiniteSessions,
		LargeOutput:                        options.Tools.LargeOutput,
		ToolSearch:                         options.Tools.Search,
		ImageGeneration:                    options.Tools.ImageGeneration,
		Memory:                             options.State.Memory,
		StoreMemory:                        options.State.StoreMemory,
		OnEvent:                            options.Events.OnEvent,
		OnEventWithPermissionEnvelope:      options.Events.OnEventWithPermissionEnvelope,
		CreateSessionFSProvider:            options.State.CreateSessionFSProvider,
		Commands:                           options.Interaction.Commands,
		OnElicitationRequest:               options.Interaction.OnElicitationRequest,
		OnAttributedElicitationRequest:     options.Interaction.OnAttributedElicitationRequest,
		OnExitPlanModeRequest:              options.Interaction.OnExitPlanModeRequest,
		OnAttributedExitPlanModeRequest:    options.Interaction.OnAttributedExitPlanModeRequest,
		OnAutoModeSwitchRequest:            options.Interaction.OnAutoModeSwitchRequest,
		EnableMCPApps:                      options.MCP.EnableApps,
		GitHubMCPToolConfig:                options.GitHub.MCP,
		GitHubToken:                        options.GitHub.Token,
		Cloud:                              options.Cloud,
		Canvases:                           options.Canvases.Definitions,
		RequestCanvasRenderer:              options.Canvases.RequestRenderer,
		RequestExtensions:                  options.Extensions.Enabled,
		ExtensionSDKPath:                   options.Extensions.SDKPath,
		CanvasHandler:                      options.Canvases.Handler,
		ExtensionInfo:                      options.Extensions.Identity,
		CanvasProvider:                     options.Canvases.Provider,
		ExpAssignments:                     options.Experiments.Assignments,
		FeatureFlags:                       options.Experiments.Flags,
		EnableManagedSettings:              options.GitHub.FetchManagedSettings,
		ExcludedManagedSettings:            options.Policy.ExcludedManagedSettings,
		EnforceManagedModelDefaults:        options.Policy.EnforceManagedModelDefaults,
		InteractiveUser:                    options.GitHub.InteractiveUser,
		PermissionMode:                     options.Permissions.Mode,
		ManagedSettings:                    options.Policy.ManagedSettings,
	}
	return result
}

// ResumeSessionOptions groups options by the feature they configure.
type ResumeSessionOptions struct {
	Inference SessionInferenceOptions
	// ClientName identifies the application using the SDK.
	// Included in the User-Agent header for API requests.
	ClientName string
	GitHub     SessionGitHubOptions
	// Model to use for this session. Can change the model when resuming.
	Model string
	Tools SessionToolOptions
	// SystemMessage configures system message customization
	SystemMessage *SystemMessageConfig
	Agents        SessionAgentOptions
	Skills        SessionSkillOptions
	Filesystem    SessionFilesystemOptions
	Execution     SessionExecutionOptions
	Experiments   SessionExperimentOptions
	Instructions  SessionInstructionOptions
	Git           SessionGitOptions
	Permissions   SessionPermissionOptions
	MCP           SessionMCPOptions
	Interaction   SessionInteractionOptions
	Hooks         SessionHookOptions
	// WorkingDirectory is the working directory for the session.
	// Tool operations will be relative to this directory.
	WorkingDirectory string
	// ConfigDirectory overrides the default configuration directory location.
	// When specified, the session will use this directory for storing config and state.
	ConfigDirectory string
	// EnableConfigDiscovery enables runtime discovery of supported configuration.
	// Explicitly supplied configuration takes precedence over discovered values.
	// Nil leaves the runtime default unchanged; use Bool(false) to explicitly disable discovery.
	EnableConfigDiscovery *bool
	State                 SessionStateOptions
	Events                SessionEventOptions
	// Diagnostics updates explicitly configured sources. Leave it nil on a
	// resident resume to preserve the current settings.
	Diagnostics *rpc.DiagnosticsConfiguration
	Plugins     SessionPluginOptions
	// SuppressResumeEvent, when true, skips emitting the session.resume event.
	// Useful for reconnecting to a session without triggering resume-related side effects.
	SuppressResumeEvent bool
	// ContinuePendingWork, when non-nil, controls whether the runtime continues any
	// tool calls or permission prompts that were still pending when the session was
	// last suspended. Nil leaves the runtime default unchanged; use Bool(false) to
	// explicitly treat pending work as interrupted on resume. Completed tool results
	// already durably recorded by the runtime are preserved.
	//
	// When true, pending permission requests and external tool calls wait for
	// caller-supplied results via the corresponding low-level RPC methods. Resuming
	// does not re-emit restored permission requests or re-invoke permission handlers
	// or already-dispatched external tool callbacks.
	// If a pending call was recorded as SDK-handler-backed and resume registers a
	// handler for that tool, the abandoned callback is interrupted, never replayed.
	// Handlerless calls remain pending. To supply an abandoned callback's result
	// manually, resume without a handler for it. Older requests without recorded
	// callback ownership also remain pending.
	ContinuePendingWork *bool
	// AllowTranscriptRecovery controls whether resume repairs a damaged transcript.
	// Nil uses the runtime default (true) in all modes. Set false to reject recovery.
	// Recovery may discard a torn tail; inspect Session.TranscriptRecovery().
	AllowTranscriptRecovery *bool
	Canvases                ResumeSessionCanvasOptions
	Extensions              SessionExtensionOptions
	Policy                  SessionPolicyOptions
}

func (options *ResumeSessionOptions) normalize() *resumeOptions {
	if options == nil {
		return &resumeOptions{}
	}
	result := &resumeOptions{
		OnPermissionRequestWithEnvelope:    options.Permissions.OnRequestWithEnvelope,
		OnMCPAuthRuntimeRefresh:            options.MCP.OnAuthRuntimeRefresh,
		AutoModeRecovery:                   options.Interaction.AutoModeRecovery,
		AllowMultipleToolCalls:             options.Inference.AllowMultipleToolCalls,
		ClientName:                         options.ClientName,
		GitHubTokenProvider:                options.GitHub.TokenProvider,
		Model:                              options.Model,
		Tools:                              options.Tools.Definitions,
		SystemMessage:                      options.SystemMessage,
		AvailableTools:                     options.Tools.AvailableTools,
		ExcludedTools:                      options.Tools.ExcludedTools,
		ExcludedBuiltInAgents:              options.Agents.ExcludedBuiltinAgents,
		IncludedBuiltInAgents:              options.Agents.IncludedBuiltinAgents,
		IncludedBuiltinSkills:              options.Skills.IncludedBuiltinSkills,
		DefaultProvider:                    options.Inference.DefaultProvider,
		Capi:                               options.GitHub.Capi,
		Providers:                          options.Inference.Providers,
		Models:                             options.Inference.Models,
		EnableSessionTelemetry:             options.GitHub.TelemetryEnabled,
		EnableCitations:                    options.Inference.EnableCitations,
		EnableFileChangeTracking:           options.Filesystem.TrackFileChanges,
		SessionLimits:                      options.Execution.Limits,
		Autopilot:                          options.Execution.Autopilot,
		EnableExperimentalMode:             options.Experiments.EnableExperimentalMode,
		SkipCustomInstructions:             options.Instructions.SkipCustomInstructions,
		CustomAgentsLocalOnly:              options.Agents.LocalDiscoveryOnly,
		CoauthorEnabled:                    options.Git.CoauthorEnabled,
		ManageScheduleEnabled:              options.Tools.ManageScheduleEnabled,
		ModelCapabilities:                  options.Inference.Capabilities,
		ReasoningEffort:                    options.Inference.ReasoningEffort,
		ReasoningSummary:                   options.Inference.ReasoningSummary,
		ContextTier:                        options.Inference.ContextTier,
		OnPermissionRequest:                options.Permissions.OnRequest,
		OnPermissionRequestWithPrompt:      options.Permissions.OnRequestWithPrompt,
		OnMCPAuthRequest:                   options.MCP.OnAuthRequest,
		OnUserInputRequest:                 options.Interaction.OnUserInputRequest,
		OnAttributedUserInputRequest:       options.Interaction.OnAttributedUserInputRequest,
		OnShellAuthenticationRequest:       options.Interaction.OnShellAuthenticationRequest,
		AskUserVariant:                     options.Interaction.AskUserVariant,
		Hooks:                              options.Hooks.Handlers,
		WorkingDirectory:                   options.WorkingDirectory,
		AdditionalDirectories:              options.Filesystem.AdditionalDirectories,
		ConfigDirectory:                    options.ConfigDirectory,
		EnableConfigDiscovery:              options.EnableConfigDiscovery,
		SkipEmbeddingRetrieval:             options.State.SkipEmbeddingRetrieval,
		EmbeddingCacheStorage:              options.State.EmbeddingCacheStorage,
		OrganizationCustomInstructions:     options.Instructions.OrganizationText,
		EnableOnDemandInstructionDiscovery: options.Instructions.OnDemandDiscovery,
		EnableFileHooks:                    options.Hooks.EnableRepositoryHooks,
		EnableHostGitOperations:            options.Git.EnableHostGitOperations,
		EnableSessionStore:                 options.State.EnableCrossSessionStore,
		EnableSkills:                       options.Skills.Enabled,
		Streaming:                          options.Events.Streaming,
		IncludeSubAgentStreamingEvents:     options.Events.IncludeSubAgentStreamingEvents,
		MCPServers:                         options.MCP.Servers,
		Diagnostics:                        options.Diagnostics,
		MCPOAuthTokenStorage:               options.MCP.OAuthTokenStorage,
		AuthClientIDMetadataURL:            options.MCP.AuthClientIDMetadataURL,
		CustomAgents:                       options.Agents.Definitions,
		DefaultAgent:                       options.Agents.Default,
		Agent:                              options.Agents.Selected,
		SkillDirectories:                   options.Skills.Directories,
		SkillProvider:                      options.Skills.Provider,
		AgentDirectories:                   options.Agents.Directories,
		HookDirectories:                    options.Hooks.Directories,
		PluginDirectories:                  options.Plugins.Directories,
		InstructionDirectories:             options.Instructions.Directories,
		DisabledSkills:                     options.Skills.DisabledSkills,
		DisabledMCPServers:                 options.MCP.DisabledServers,
		SandboxConfig:                      options.Permissions.Sandbox,
		InfiniteSessions:                   options.State.InfiniteSessions,
		LargeOutput:                        options.Tools.LargeOutput,
		ToolSearch:                         options.Tools.Search,
		ImageGeneration:                    options.Tools.ImageGeneration,
		Memory:                             options.State.Memory,
		StoreMemory:                        options.State.StoreMemory,
		GitHubToken:                        options.GitHub.Token,
		SuppressResumeEvent:                options.SuppressResumeEvent,
		ContinuePendingWork:                options.ContinuePendingWork,
		AllowTranscriptRecovery:            options.AllowTranscriptRecovery,
		OnEvent:                            options.Events.OnEvent,
		OnEventWithPermissionEnvelope:      options.Events.OnEventWithPermissionEnvelope,
		CreateSessionFSProvider:            options.State.CreateSessionFSProvider,
		Commands:                           options.Interaction.Commands,
		OnElicitationRequest:               options.Interaction.OnElicitationRequest,
		OnAttributedElicitationRequest:     options.Interaction.OnAttributedElicitationRequest,
		OnExitPlanModeRequest:              options.Interaction.OnExitPlanModeRequest,
		OnAttributedExitPlanModeRequest:    options.Interaction.OnAttributedExitPlanModeRequest,
		OnAutoModeSwitchRequest:            options.Interaction.OnAutoModeSwitchRequest,
		EnableMCPApps:                      options.MCP.EnableApps,
		GitHubMCPToolConfig:                options.GitHub.MCP,
		Canvases:                           options.Canvases.Definitions,
		OpenCanvases:                       options.Canvases.Open,
		RequestCanvasRenderer:              options.Canvases.RequestRenderer,
		RequestExtensions:                  options.Extensions.Enabled,
		ExtensionSDKPath:                   options.Extensions.SDKPath,
		CanvasHandler:                      options.Canvases.Handler,
		ExtensionInfo:                      options.Extensions.Identity,
		CanvasProvider:                     options.Canvases.Provider,
		ExpAssignments:                     options.Experiments.Assignments,
		FeatureFlags:                       options.Experiments.Flags,
		EnableManagedSettings:              options.GitHub.FetchManagedSettings,
		ExcludedManagedSettings:            options.Policy.ExcludedManagedSettings,
		EnforceManagedModelDefaults:        options.Policy.EnforceManagedModelDefaults,
		InteractiveUser:                    options.GitHub.InteractiveUser,
		PermissionMode:                     options.Permissions.Mode,
		ManagedSettings:                    options.Policy.ManagedSettings,
	}
	return result
}

// OwnedRuntimeOptions configures a runtime owned by the client.
type OwnedRuntimeOptions struct {
	BaseDirectory             string
	SessionIdleTimeoutSeconds int
}

// ForDefault resolves the current deployment default synchronously, without
// starting or connecting to a runtime. It reads COPILOT_SDK_DEFAULT_CONNECTION
// at invocation time; an unset or empty value uses in-process hosting.
func ForDefault(options *OwnedRuntimeOptions) RuntimeConnection {
	connection := resolveDefaultConnection(os.Environ())
	if options == nil {
		return connection
	}
	switch conn := connection.(type) {
	case SpawnedStdioConnection:
		conn.BaseDirectory = options.BaseDirectory
		conn.SessionIdleTimeoutSeconds = options.SessionIdleTimeoutSeconds
		return conn
	case InProcessConnection:
		conn.BaseDirectory = options.BaseDirectory
		conn.SessionIdleTimeoutSeconds = options.SessionIdleTimeoutSeconds
		return conn
	default:
		panic(fmt.Sprintf("unsupported deployment default %T", connection))
	}
}

var sessionWireGroups = map[string][]string{
	"allowMultipleToolCalls":             {"inference", "allowMultipleToolCalls"},
	"allowedModels":                      {"policy", "allowedModels"},
	"reasoningEffort":                    {"inference", "reasoningEffort"},
	"reasoningSummary":                   {"inference", "reasoningSummary"},
	"enableExperimentalMode":             {"experiments", "enableExperimentalMode"},
	"contextTier":                        {"inference", "contextTier"},
	"modelCapabilities":                  {"inference", "capabilities"},
	"largeOutput":                        {"tools", "largeOutput"},
	"tools":                              {"tools", "definitions"},
	"canvases":                           {"canvases", "definitions"},
	"requestCanvasRenderer":              {"canvases", "requestRenderer"},
	"requestExtensions":                  {"extensions", "enabled"},
	"extensionSdkPath":                   {"extensions", "sdkPath"},
	"extensionInfo":                      {"extensions", "identity"},
	"canvasProvider":                     {"canvases", "provider"},
	"commands":                           {"interaction", "commands"},
	"toolSearch":                         {"tools", "search"},
	"imageGeneration":                    {"tools", "imageGeneration"},
	"availableTools":                     {"tools", "availableTools"},
	"excludedTools":                      {"tools", "excludedTools"},
	"excludedBuiltinAgents":              {"agents", "excludedBuiltinAgents"},
	"includedBuiltinAgents":              {"agents", "includedBuiltinAgents"},
	"includedBuiltinSkills":              {"skills", "includedBuiltinSkills"},
	"defaultProvider":                    {"inference", "defaultProvider"},
	"capi":                               {"github", "capi"},
	"providers":                          {"inference", "providers"},
	"models":                             {"inference", "models"},
	"enableSessionTelemetry":             {"github", "telemetryEnabled"},
	"enableCitations":                    {"inference", "enableCitations"},
	"enableFileChangeTracking":           {"filesystem", "trackFileChanges"},
	"sessionLimits":                      {"execution", "limits"},
	"autopilot":                          {"execution", "autopilot"},
	"skipCustomInstructions":             {"instructions", "skipCustomInstructions"},
	"customAgentsLocalOnly":              {"agents", "localDiscoveryOnly"},
	"coauthorEnabled":                    {"git", "coauthorEnabled"},
	"manageScheduleEnabled":              {"tools", "manageScheduleEnabled"},
	"onPermissionRequest":                {"permissions", "onRequest"},
	"onMcpAuthRequest":                   {"mcp", "onAuthRequest"},
	"onUserInputRequest":                 {"interaction", "onUserInputRequest"},
	"askUserVariant":                     {"interaction", "askUserVariant"},
	"onElicitationRequest":               {"interaction", "onElicitationRequest"},
	"enableMcpApps":                      {"mcp", "enableApps"},
	"githubMcpToolConfig":                {"github", "mcp"},
	"onExitPlanModeRequest":              {"interaction", "onExitPlanModeRequest"},
	"onAutoModeSwitchRequest":            {"interaction", "onAutoModeSwitchRequest"},
	"autoModeRecovery":                   {"interaction", "autoModeRecovery"},
	"hooks":                              {"hooks", "hasHandlers"},
	"additionalDirectories":              {"filesystem", "additionalDirectories"},
	"streaming":                          {"events", "streaming"},
	"includeSubAgentStreamingEvents":     {"events", "includeSubAgentStreamingEvents"},
	"mcpOAuthTokenStorage":               {"mcp", "oauthTokenStorage"},
	"authClientIdMetadataUrl":            {"mcp", "authClientIdMetadataUrl"},
	"mcpServers":                         {"mcp", "servers"},
	"customAgents":                       {"agents", "definitions"},
	"defaultAgent":                       {"agents", "default"},
	"agent":                              {"agents", "selected"},
	"skillDirectories":                   {"skills", "directories"},
	"skillProvider":                      {"skills", "provider"},
	"agentDirectories":                   {"agents", "directories"},
	"hookDirectories":                    {"hooks", "directories"},
	"pluginDirectories":                  {"plugins", "directories"},
	"instructionDirectories":             {"instructions", "directories"},
	"disabledSkills":                     {"skills", "disabledSkills"},
	"disabledMcpServers":                 {"mcp", "disabledServers"},
	"sandboxConfig":                      {"permissions", "sandbox"},
	"infiniteSessions":                   {"state", "infiniteSessions"},
	"memory":                             {"state", "memory"},
	"gitHubToken":                        {"github", "token"},
	"gitHubTokenProvider":                {"github", "tokenProvider"},
	"enableManagedSettings":              {"github", "fetchManagedSettings"},
	"excludedManagedSettings":            {"policy", "excludedManagedSettings"},
	"enforceManagedModelDefaults":        {"policy", "enforceManagedModelDefaults"},
	"interactiveUser":                    {"github", "interactiveUser"},
	"permissionMode":                     {"permissions", "mode"},
	"managedSettings":                    {"policy", "managedSettings"},
	"skipEmbeddingRetrieval":             {"state", "skipEmbeddingRetrieval"},
	"embeddingCacheStorage":              {"state", "embeddingCacheStorage"},
	"organizationCustomInstructions":     {"instructions", "organizationText"},
	"enableOnDemandInstructionDiscovery": {"instructions", "onDemandDiscovery"},
	"enableFileHooks":                    {"hooks", "enableRepositoryHooks"},
	"enableHostGitOperations":            {"git", "enableHostGitOperations"},
	"enableSessionStore":                 {"state", "enableCrossSessionStore"},
	"enableSkills":                       {"skills", "enabled"},
	"onEvent":                            {"events", "onEvent"},
	"createSessionFsProvider":            {"state", "createSessionFsProvider"},
	"featureFlags":                       {"experiments", "flags"},
	"expAssignments":                     {"experiments", "assignments"},
	"trustWorkingDirectory":              {"filesystem", "trustWorkingDirectory"},
	"refreshCustomInstructions":          {"instructions", "refreshCustomInstructions"},
	"openCanvases":                       {"canvases", "open"},
	"requestPermission":                  {"permissions", "request"},
	"requestUserInput":                   {"interaction", "requestUserInput"},
	"requestShellAuthentication":         {"interaction", "requestShellAuthentication"},
	"requestElicitation":                 {"interaction", "requestElicitation"},
	"requestExitPlanMode":                {"interaction", "requestExitPlanMode"},
	"requestAutoModeSwitch":              {"interaction", "requestAutoModeSwitch"},
	"requestAutoModeRecovery":            {"interaction", "requestAutoModeRecovery"},
	"hasSkillProvider":                   {"skills", "hasProvider"},
	"hasMemoryStore":                     {"state", "hasMemoryStore"},
	"gitHubTokenProviderRegistrationId":  {"github", "tokenProviderRegistrationId"},
	"configDir":                          {"configDirectory"},
	"envValueMode":                       {"mcp", "envValueMode"},
	"requestMcpApps":                     {"mcp", "enableApps"},
	"isExperimentalMode":                 {"experiments", "enableExperimentalMode"},
	"enableGitHubTelemetryForwarding":    {"github", "telemetryForwarding"},
	"disableResume":                      {"suppressResumeEvent"},
	"webSearch":                          {"tools", "webSearch"},
	"shell":                              {"tools", "shell"},
	"managedMcpServers":                  {"mcp", "managedServers"},
	"ignoredSkillsLocations":             {"skills", "ignoredLocations"},
	"allowAllMcpServerInstructions":      {"mcp", "allowAllServerInstructions"},
	"autopilotContinuation":              {"execution", "autopilotContinuation"},
}

func groupSessionJSON(value any) ([]byte, error) {
	data, err := json.Marshal(value)
	if err != nil {
		return nil, err
	}
	var fields map[string]json.RawMessage
	if err := json.Unmarshal(data, &fields); err != nil {
		return nil, err
	}
	result := make(map[string]any)
	for name, value := range fields {
		path, ok := sessionWireGroups[name]
		if !ok {
			result[name] = value
			continue
		}
		if len(path) == 1 {
			result[path[0]] = value
			continue
		}
		group, ok := result[path[0]].(map[string]json.RawMessage)
		if !ok {
			group = make(map[string]json.RawMessage)
			result[path[0]] = group
		}
		group[path[1]] = value
	}
	return json.Marshal(result)
}

func ungroupSessionJSON(data []byte, target any) error {
	var fields map[string]json.RawMessage
	if err := json.Unmarshal(data, &fields); err != nil {
		return err
	}
	result := make(map[string]json.RawMessage)
	for old, path := range sessionWireGroups {
		value, ok := fields[path[0]]
		if !ok {
			continue
		}
		if len(path) == 1 {
			result[old] = value
			continue
		}
		var group map[string]json.RawMessage
		if err := json.Unmarshal(value, &group); err != nil {
			return err
		}
		if value, ok := group[path[1]]; ok {
			result[old] = value
		}
	}
	for name, value := range fields {
		if _, moved := sessionWireGroups[name]; !moved {
			result[name] = value
		}
	}
	data, err := json.Marshal(result)
	if err != nil {
		return err
	}
	return json.Unmarshal(data, target)
}

func (request createSessionRequest) MarshalJSON() ([]byte, error) {
	type fields createSessionRequest
	return groupSessionJSON(fields(request))
}
func (request *createSessionRequest) UnmarshalJSON(data []byte) error {
	type fields createSessionRequest
	return ungroupSessionJSON(data, (*fields)(request))
}
func (request resumeSessionRequest) MarshalJSON() ([]byte, error) {
	type fields resumeSessionRequest
	return groupSessionJSON(fields(request))
}
func (request *resumeSessionRequest) UnmarshalJSON(data []byte) error {
	type fields resumeSessionRequest
	return ungroupSessionJSON(data, (*fields)(request))
}
