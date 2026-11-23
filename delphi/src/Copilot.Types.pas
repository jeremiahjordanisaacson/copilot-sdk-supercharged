{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

unit Copilot.Types;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections;

type
  // Forward declarations
  TCopilotSession = class;

  // Permission handler callback
  TPermissionDecision = (pdApproved, pdDeniedByUser, pdDeniedByPolicy);
  TPermissionRequest = record
    Kind: string;
    Resource: string;
    Description: string;
    SessionId: string;
  end;
  TPermissionResult = record
    Decision: TPermissionDecision;
  end;
  TPermissionHandler = reference to function(const Request: TPermissionRequest): TPermissionResult;

  // User input handler callback
  TUserInputRequest = record
    Question: string;
    SessionId: string;
  end;
  TUserInputResponse = record
    Answer: string;
    Accepted: Boolean;
  end;
  TUserInputHandler = reference to function(const Request: TUserInputRequest): TUserInputResponse;

  // Tool definitions
  TToolInvocation = record
    ToolCallId: string;
    SessionId: string;
  end;
  TToolResultObject = record
    Content: string;
    IsError: Boolean;
  end;
  TToolHandler = reference to function(const Args: TJSONObject;
    const Invocation: TToolInvocation): TToolResultObject;

  TTool = record
    Name: string;
    Description: string;
    Parameters: TJSONObject;
    Handler: TToolHandler;
    // Defer loading policy: '' (default), 'auto' (lazy) or 'never' (eager).
    // See the ToolDefer* constants below.
    Defer: string;
    // Safe to retry: repeated calls have no extra effect (wire: idempotent).
    Idempotent: Boolean;
  end;

  // Hook types
  TPreToolUseHookInput = record
    ToolName: string;
    ToolArgs: TJSONObject;
  end;
  TPreToolUseHookOutput = record
    PermissionDecision: string;
  end;
  TPreToolUseHandler = reference to function(const Input: TPreToolUseHookInput;
    const SessionId: string): TPreToolUseHookOutput;

  TPostToolUseHookInput = record
    ToolName: string;
    ToolResult: string;
  end;
  TPostToolUseHookOutput = record
    OverrideResult: string;
  end;
  TPostToolUseHandler = reference to function(const Input: TPostToolUseHookInput;
    const SessionId: string): TPostToolUseHookOutput;

  // Pre-MCP-tool-call hook (runs before an MCP server tool is invoked)
  TPreMcpToolCallHookInput = record
    ServerName: string;
    ToolName: string;
    ToolArgs: TJSONObject;
  end;
  TPreMcpToolCallHookOutput = record
    PermissionDecision: string;
  end;
  TPreMcpToolCallHandler = reference to function(const Input: TPreMcpToolCallHookInput;
    const SessionId: string): TPreMcpToolCallHookOutput;

  // User-prompt-transformed hook (runs after a user prompt is transformed)
  TUserPromptTransformedHookInput = record
    Prompt: string;
  end;
  TUserPromptTransformedHandler = reference to procedure(const Input: TUserPromptTransformedHookInput;
    const SessionId: string);

  // Sub-agent lifecycle hooks (hookType "subagentStart" / "subagentStop").
  TSubagentHookInput = record
    AgentName: string;
    AgentDisplayName: string;
    AgentDescription: string;
    TranscriptPath: string;
    // subagentStop only
    AgentId: string;
    AgentType: string;
    StopReason: string;
    Response: string;
  end;
  // Returns additional context prepended to the child prompt ('' = none).
  TSubagentStartHandler = reference to function(const Input: TSubagentHookInput;
    const SessionId: string): string;
  TSubagentStopDecision = record
    Block: Boolean;
    Reason: string;
    ModifiedResponse: string;
  end;
  TSubagentStopHandler = reference to function(const Input: TSubagentHookInput;
    const SessionId: string): TSubagentStopDecision;

  TSessionHooks = record
    OnPreToolUse: TPreToolUseHandler;
    OnPostToolUse: TPostToolUseHandler;
    OnPreMcpToolCall: TPreMcpToolCallHandler;
    OnUserPromptTransformed: TUserPromptTransformedHandler;
    OnSubagentStart: TSubagentStartHandler;
    OnSubagentStop: TSubagentStopHandler;
  end;

  // Session event
  TSessionEvent = record
    EventType: string;
    Id: string;
    Timestamp: string;
    ParentId: string;
    AgentId: string;
    Ephemeral: Boolean;
    SessionId: string;
    Data: TJSONObject;
  end;
  TSessionEventHandler = reference to procedure(const Event: TSessionEvent);

  // Session lifecycle event
  TSessionLifecycleEvent = record
    EventType: string;
    SessionId: string;
    Data: TJSONObject;
  end;
  TSessionLifecycleHandler = reference to procedure(const Event: TSessionLifecycleEvent);

  // Provider config for BYOK (legacy single provider, sent as the named
  // provider "default").
  TProviderConfig = record
    ProviderType: string;
    BaseUrl: string;
    ApiKey: string;
    // Command whose output is the API key (wire: apiKeyCommand).
    ApiKeyCommand: string;
    BearerToken: string;
    WireApi: string;
    MaxOutputTokens: Integer;
  end;

  // MCP server config
  TMCPServerConfig = record
    Command: string;
    Args: TArray<string>;
    Tools: TArray<string>;
  end;

  // System message section override
  TSectionOverrideAction = (soaAppend, soaPrepend, soaReplace, soaRemove, soaTransform);
  TSectionOverride = record
    Action: TSectionOverrideAction;
    Content: string;
  end;
  TSystemMessageConfig = record
    Mode: string; // 'default' or 'customize'
    Sections: TDictionary<string, TSectionOverride>;
  end;

  // Response format
  TResponseFormat = (rfText, rfImage);

  // Image options
  TImageOptions = record
    Size: string;
    Quality: string;
    Style: string;
  end;

  // SessionFs configuration
  TSessionFsConfig = record
    InitialCwd: string;
    SessionStatePath: string;
    Conventions: string;
  end;

  // Elicitation handler callback
  TElicitationRequest = record
    Id: string;
    Title: string;
    Message: string;
    Options: TArray<string>;
    SessionId: string;
  end;
  TElicitationResponse = record
    Result: string;
  end;
  TElicitationHandler = reference to function(const Request: TElicitationRequest): TElicitationResponse;

  // Command definition
  TCommandDefinition = record
    Name: string;
    Description: string;
  end;

  // Image options
  TImageOptions = record
    Size: string;
    Quality: string;
    Style: string;
  end;

  // Message options
  TMessageOptions = record
    Prompt: string;
    Model: string;
    ResponseFormat: TResponseFormat;
    ImageOptions: TImageOptions;
    // Per-send agent mode override (wire key: agentMode)
    AgentMode: string;
    // Prompt text shown to the user instead of the real prompt (wire key: displayPrompt)
    DisplayPrompt: string;
    // Custom HTTP headers for this send's outbound model requests (wire key: requestHeaders)
    RequestHeaders: TDictionary<string, string>;
    // Who sent the message (upstream MessageSource, wire key: source):
    // 'user', 'system' or 'agent-<id>'.
    Source: string;
    // Opaque caller-chosen id echoed on this turn's events (wire key: clientTurnId).
    ClientTurnId: string;
    // JSON schema for structured output (upstream responseSchema). Sent as
    // responseFormat {type: "json_schema", jsonSchema: {...}}.
    ResponseSchema: TJSONObject;
  end;

  // --- SDK protocol 4 / v3.0.0 types (Microsoft Copilot SDK 2.0) ---

  // Optional boolean: obUnset leaves the option out of the wire payload.
  TOptionalBool = (obUnset, obFalse, obTrue);

  // Integrating application identity sent as clientInfo in the connect
  // handshake. Empty fields are dropped; nothing is sent when all are empty.
  TClientInfo = record
    EditorName: string;
    EditorVersion: string;
    ExtensionName: string;
    ExtensionVersion: string;
  end;

  // GitHub token provider (answers the gitHubToken.getToken server request).
  TGitHubTokenRequest = record
    RegistrationId: string;
    Host: string;
    SessionId: string;
    Reason: string;
  end;
  TGitHubTokenResult = record
    Cancelled: Boolean;
    AccessToken: string;
    TokenType: string;
    ExpiresIn: Integer; // seconds; 0 = not reported
  end;
  TGitHubTokenProvider = reference to function(
    const Request: TGitHubTokenRequest): TGitHubTokenResult;

  // Shell authentication prompts (shellAuthentication.request).
  TShellAuthenticationRequest = record
    SessionId: string;
    RequestId: string;
    ProgramName: string;
    Prompt: string;
    InputKind: string; // 'secret', 'text' or 'confirm'
    Command: string;
    Cwd: string;
  end;
  TShellAuthenticationResponse = record
    Accepted: Boolean; // False replies {"action":"cancel"}
    Value: string;
  end;
  TShellAuthenticationHandler = reference to function(
    const Request: TShellAuthenticationRequest): TShellAuthenticationResponse;

  // Memory store callback (memoryStore.store).
  TStoredMemory = record
    SessionId: string;
    Subject: string;
    Fact: string;
    CitationsJson: string;
    Reason: string;
  end;
  TMemoryStoreHandler = reference to procedure(const Memory: TStoredMemory);

  // Network callbacks (network.setProvider). Return '' to decline.
  TProxyResolver = reference to function(const Url: string): string;
  TNegotiateAuthenticator = reference to function(const Spn: string): string;
  TBasicCredentials = record
    Username: string;
    Password: string;
  end;
  TBasicAuthenticator = reference to function(const Proxy, Realm: string;
    PreviousRejected: Boolean): TBasicCredentials;
  TNetworkOptions = record
    Scope: string; // 'connection' or 'runtime'
    ResolveProxy: TProxyResolver;
    AuthenticateNegotiate: TNegotiateAuthenticator;
    AuthenticateBasic: TBasicAuthenticator;
  end;

  // Command sandbox (permissions.sandbox). AllowBypass lets the agent ask to
  // run a single command outside the sandbox through the permission flow.
  TSandboxConfig = record
    Enabled: Boolean;
    AllowBypass: Boolean;
    AddCurrentWorkingDirectory: TOptionalBool;
    UserPolicyJson: string;
  end;

  // Image generation (tools.imageGeneration).
  TImageGenerationConfig = record
    Enabled: TOptionalBool;
    Watermark: TOptionalBool;
    SaveToDisk: TOptionalBool;
  end;

  // Autopilot continuation (execution.autopilot). MaxContinues = 0 keeps
  // the runtime default.
  TAutopilotConfig = record
    ContinuationEnabled: TOptionalBool;
    MaxContinues: Integer;
  end;

  // Named BYOK provider (inference.providers[]).
  TNamedProviderConfig = record
    Name: string;
    ProviderType: string; // 'openai', 'azure', 'anthropic'
    BaseUrl: string;
    ApiKey: string;
    ApiKeyCommand: string; // command whose output is the API key
    BearerToken: string;
    WireApi: string;       // 'completions' or 'responses'
    Transport: string;     // 'http' or 'websockets'
    AzureJson: string;
    HeadersJson: string;
    ModelProvider: string;
  end;

  // Model served by a named provider (inference.models[]). Select it as the
  // session model with "provider/id".
  TProviderModelConfig = record
    Provider: string;
    Id: string;
    ModelId: string;
    WireModel: string;
    Name: string;
    MaxPromptTokens: Integer;
    MaxContextWindowTokens: Integer;
    MaxOutputTokens: Integer;
    CapabilitiesJson: string;
  end;

  // Structured Policy Helper failure (PolicyHelperFailure), read from
  // error.data.policyHelperFailure when session.create/resume fails.
  TPolicyHelperFailure = record
    Present: Boolean;
    Category: string;
    FailureMode: string;
    Message: string;
    PreviousPolicyRetained: Boolean;
    Reason: string;
    Severity: string;
    Authority: string;
    Code: string;
    DetailsJson: string;
    Diagnostic: string;
    HelpLinkUrl: string;
    HelpLinkLabel: string;
    Remediation: string;
  end;

  // Permission-mode change result (PermissionsSetModeResult).
  TPermissionModeResult = record
    Success: Boolean;
    Mode: string;
    Reason: string; // e.g. 'managed_policy' when policy refused the change
  end;

  // Data of a tool.shell_output event.
  TShellOutputEventData = record
    ToolCallId: string;
    Text: string;
    Sequence: Integer;
    Stream: string; // 'stdout', 'stderr' or '' when omitted
  end;

  // BYOK bearer token provider: mint a per-session bearer token on demand.
  TProviderTokenArgs = record
    SessionId: string;
  end;
  TBearerTokenProvider = reference to function(const Args: TProviderTokenArgs): string;

  // MCP OAuth host token handler: supply an OAuth token for an MCP server.
  TMcpAuthRequest = record
    SessionId: string;
    ServerUrl: string;
  end;
  TMcpAuthHandler = reference to function(const Request: TMcpAuthRequest): string;

  // HTTP request handler: intercept/mutate outbound LLM inference requests.
  TCopilotRequestHandler = reference to function(const Request: TJSONObject): TJSONObject;

  // Per-session AI-credit spending limits (SessionLimitsConfig).
  TSessionLimitsConfig = record
    MaxAiCredits: Integer;
  end;

  // Persistent session memory configuration (MemoryConfiguration).
  TMemoryConfiguration = record
    Enabled: Boolean;
  end;

  // Session configuration
  TSessionConfig = record
    Model: string;
    Streaming: Boolean;
    Tools: TArray<TTool>;
    OnPermissionRequest: TPermissionHandler;
    OnUserInputRequest: TUserInputHandler;
    OnElicitationRequest: TElicitationHandler;
    Hooks: TSessionHooks;
    SkillDirectories: TArray<string>;
    DisabledSkills: TArray<string>;
    ExcludedTools: TArray<string>;
    IncludeSubAgentStreamingEvents: Boolean;
    Provider: TProviderConfig;
    MCPServers: TDictionary<string, TMCPServerConfig>;
    SystemMessage: TSystemMessageConfig;
    Instructions: TArray<string>;
    Commands: TArray<TCommandDefinition>;
    RequestHeaders: TDictionary<string, string>;
    ModelCapabilities: TDictionary<string, string>;
    EnableConfigDiscovery: Boolean;
    GithubToken: string;
    ResponseFormat: TResponseFormat;
    ImageOptions: TImageOptions;
    AuthToken: string;
    InstructionDirectories: TArray<string>;
    // --- Upstream-sync session options (parity with @github/copilot-sdk) ---
    EnableCitations: Boolean;
    ExcludedBuiltinAgents: TArray<string>;
    SessionLimits: TSessionLimitsConfig;
    Memory: TMemoryConfiguration;
    OtlpProtocol: string;
    EnableWebSocketResponses: Boolean;
    ExpAssignments: TDictionary<string, string>;
    OnMcpAuthRequest: TMcpAuthHandler;
    // --- Newly-synced session options (parity with @github/copilot-sdk) ---
    RewindEnabled: Boolean;
    AdditionalDirectories: TArray<string>;
    DisabledMcpServers: TArray<string>;
    GithubMcpToolConfig: string;
    CanvasProvider: string;
    CustomAgentsLocalOnly: Boolean;
    DecisionContext: string;
    BuiltinPluginDirectories: TArray<string>;
    ArgsSchema: string;
    ReasoningEffort: string;
    ToolSearch: string;
    InProcess: Boolean;
    ExperimentalMode: Boolean;
    ContentExclusion: Boolean;
    // --- SDK protocol 4 / v3.0.0 session options ---
    // Working directory of the session (wire: workingDirectory).
    WorkingDirectory: string;
    // Tool allowlist (wire: availableTools). Required in client mode 'empty'.
    AvailableTools: TArray<string>;
    // Ask-user tool variant: 'legacy' or 'elicitation'.
    AskUserVariant: string;
    // Callback minting GitHub tokens (gitHubToken.getToken). Mutually
    // exclusive with GithubToken.
    GitHubTokenProvider: TGitHubTokenProvider;
    Sandbox: TSandboxConfig;
    // Feature flag overrides (wire: featureFlags).
    FeatureFlags: TDictionary<string, Boolean>;
    // Model allowlist (wire: allowedModels).
    AllowedModels: TArray<string>;
    // OAuth client id metadata document URL for MCP servers.
    AuthClientIdMetadataUrl: string;
    // Model capabilities override limits.max_output_tokens (0 = unset).
    MaxOutputTokens: Integer;
    AgentDirectories: TArray<string>;
    HookDirectories: TArray<string>;
    // Grant directory trust at creation: 'session' or 'user' (create only).
    TrustWorkingDirectory: string;
    ImageGeneration: TImageGenerationConfig;
    Autopilot: TAutopilotConfig;
    // Named BYOK providers and the models they serve.
    Providers: TArray<TNamedProviderConfig>;
    Models: TArray<TProviderModelConfig>;
    DefaultProvider: string;
    // Copilot API models: obFalse disables them (BYOK only).
    CapiEnabled: TOptionalBool;
    OnShellAuthenticationRequest: TShellAuthenticationHandler;
    StoreMemory: TMemoryStoreHandler;
  end;

  // Resume session config
  TResumeSessionConfig = record
    SessionId: string;
    Model: string;
    Streaming: Boolean;
    Tools: TArray<TTool>;
    OnPermissionRequest: TPermissionHandler;
    OnUserInputRequest: TUserInputHandler;
    OnElicitationRequest: TElicitationHandler;
    Hooks: TSessionHooks;
    Commands: TArray<TCommandDefinition>;
    ExcludedTools: TArray<string>;
    IncludeSubAgentStreamingEvents: Boolean;
    InstructionDirectories: TArray<string>;
    // Options shared with CreateSession (providers, sandbox, callbacks, ...).
    // The fields above take precedence over the same fields in Options.
    Options: TSessionConfig;
    // Do not emit a session.resume event (wire: suppressResumeEvent).
    DisableResume: Boolean;
    // Continue work that was pending when the session was saved.
    ContinuePendingWork: Boolean;
    // Rebuild a damaged session from its transcript.
    AllowTranscriptRecovery: Boolean;
    // Send an explicit null defaultProvider to clear it.
    ClearDefaultProvider: Boolean;
  end;

  // Session metadata
  TSessionMetadata = record
    SessionId: string;
    Model: string;
    CreatedAt: TDateTime;
    Status: string;
    MessageCount: Integer;
  end;

  // Session list filter
  TSessionListFilter = record
    Status: string;
  end;

  // Ping response
  TPingResponse = record
    ProtocolVersion: string;
    ServerVersion: string;
    Status: string;
  end;

  // Auth status
  TAuthStatus = record
    Authenticated: Boolean;
    Username: string;
  end;

  // Model info
  TModelInfo = record
    Id: string;
    Name: string;
    Provider: string;
  end;

  // Client options
  TCopilotClientOptions = record
    CliPath: string;
    CliUrl: string;
    LogLevel: string;
    SessionIdleTimeoutSeconds: Integer;
    SessionFs: TSessionFsConfig;
    CopilotHome: string;
    TcpConnectionToken: string;
    // HTTP request handler for outbound LLM inference requests (CopilotRequestHandler).
    RequestHandler: TCopilotRequestHandler;
    // BYOK bearer token provider (per-session token minting).
    BearerTokenProvider: TBearerTokenProvider;
    // Integrating application identity sent as clientInfo on connect.
    ClientInfo: TClientInfo;
    // Client mode: 'coding-agent' (default when empty), 'empty', or the
    // legacy alias 'copilot-cli'.
    Mode: string;
    // Proxy / authentication callbacks (network.setProvider).
    Network: TNetworkOptions;
  end;

  // Unsubscribe token
  TUnsubscribeProc = reference to procedure;

const
  // Tool "defer" loading policy (ToolDefer): eager pre-load or lazy search.
  ToolDeferAuto = 'auto';   // lazy: discovered on demand
  ToolDeferNever = 'never'; // eager: always pre-loaded

  // System-message section identifiers (used with system_message overrides).
  // 'preamble' targets the identity preamble; 'preserve' shields a section
  // from a group-level remove.
  SectionPreamble = 'preamble';
  SectionIdentity = 'identity';
  SectionToolInstructions = 'tool_instructions';
  SectionPreserve = 'preserve';

  // GitHub-anchored attachment variants.
  GitHubCommit = 'GitHubCommit';
  GitHubRelease = 'GitHubRelease';
  GitHubActionsJob = 'GitHubActionsJob';
  GitHubRepository = 'GitHubRepository';
  GitHubFileDiff = 'GitHubFileDiff';
  GitHubTreeComparison = 'GitHubTreeComparison';
  GitHubUrl = 'GitHubUrl';
  GitHubFile = 'GitHubFile';
  GitHubSnippet = 'GitHubSnippet';

  // --- SDK protocol 4 / v3.0.0 constants ---

  // Client modes (TCopilotClientOptions.Mode). 'copilot-cli' is a legacy
  // alias of 'coding-agent'.
  ClientModeCodingAgent = 'coding-agent';
  ClientModeCopilotCli = 'copilot-cli';
  ClientModeEmpty = 'empty';

  // Auto routing tiers for TCopilotSession.SwitchAutoTier.
  AutoTierFast = 'fast';
  AutoTierEfficiency = 'efficiency';
  AutoTierBalance = 'balance';
  AutoTierIntelligence = 'intelligence';

  // Ask-user tool variants (TSessionConfig.AskUserVariant).
  AskUserVariantLegacy = 'legacy';
  AskUserVariantElicitation = 'elicitation';

  // Message sources (MessageSource) for TMessageOptions.Source; agent
  // sources use 'agent-<id>'.
  MessageSourceUser = 'user';
  MessageSourceSystem = 'system';

  // Working-directory trust grants (TSessionConfig.TrustWorkingDirectory).
  WorkingDirectoryTrustSession = 'session';
  WorkingDirectoryTrustUser = 'user';

  // Streamed shell output event (replaces tool.execution_partial_result).
  EventToolShellOutput = 'tool.shell_output';

  // Permission-mode refusal reason (TPermissionModeResult.Reason).
  PermissionModeReasonManagedPolicy = 'managed_policy';

  // Helper functions for tool results
  function ToolSuccess(const Text: string): TToolResultObject;
  function ToolFailure(const ErrorMsg: string): TToolResultObject;

implementation

function ToolSuccess(const Text: string): TToolResultObject;
begin
  Result.Content := Text;
  Result.IsError := False;
end;

function ToolFailure(const ErrorMsg: string): TToolResultObject;
begin
  Result.Content := ErrorMsg;
  Result.IsError := True;
end;

end.
