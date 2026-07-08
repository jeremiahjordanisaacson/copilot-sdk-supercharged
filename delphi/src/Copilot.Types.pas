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

  TSessionHooks = record
    OnPreToolUse: TPreToolUseHandler;
    OnPostToolUse: TPostToolUseHandler;
    OnPreMcpToolCall: TPreMcpToolCallHandler;
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

  // Provider config for BYOK
  TProviderConfig = record
    ProviderType: string;
    BaseUrl: string;
    ApiKey: string;
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
