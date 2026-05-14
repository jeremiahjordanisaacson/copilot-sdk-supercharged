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

  TSessionHooks = record
    OnPreToolUse: TPreToolUseHandler;
    OnPostToolUse: TPostToolUseHandler;
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

  // Exit plan mode types
  TExitPlanModeRequest = record
    Summary: string;
    PlanContent: string;
    Actions: TArray<string>;
    RecommendedAction: string;
    SessionId: string;
  end;
  TExitPlanModeResult = record
    Approved: Boolean;
    SelectedAction: string;
    Feedback: string;
  end;
  TExitPlanModeHandler = reference to function(const Request: TExitPlanModeRequest): TExitPlanModeResult;

  // Trace context types
  TTraceContext = record
    Traceparent: string;
    Tracestate: string;
  end;
  TTraceContextProvider = reference to function: TTraceContext;

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

  // Slash command input completion
  TSlashCommandInputCompletion = (scicDirectory);

  // Slash command kind
  TSlashCommandKind = (sckBuiltin, sckClient, sckSkill);

  // Model picker price category
  TModelPickerPriceCategory = (mppcHigh, mppcLow, mppcMedium, mppcVeryHigh);

  // Slash command input
  TSlashCommandInput = record
    Hint: string;
    Completion: string;
  end;

  // Slash command info
  TSlashCommandInfo = record
    AllowDuringAgentExecution: Boolean;
    Description: string;
    Kind: string;
    Name: string;
    Aliases: TArray<string>;
    Experimental: Boolean;
    Input: TSlashCommandInput;
  end;

  // Commands invoke request
  TCommandsInvokeRequest = record
    Name: string;
    Input: string;
  end;

  // Commands list request
  TCommandsListRequest = record
    IncludeBuiltins: Boolean;
    IncludeClientCommands: Boolean;
    IncludeSkills: Boolean;
  end;

  // Model billing token prices
  TModelBillingTokenPrices = record
    BatchSize: Integer;
    CachePrice: Integer;
    InputPrice: Integer;
    OutputPrice: Integer;
  end;

  // Model billing
  TModelBilling = record
    Multiplier: Double;
    TokenPrices: TModelBillingTokenPrices;
    PickerPriceCategory: string;
  end;

  // Experimental
  // Diagnostics from loading skills
  TSkillsLoadDiagnostics = record
    Errors: TArray<string>;
    Warnings: TArray<string>;
  end;

  // Experimental
  // Mode for remote session control
  TRemoteSessionMode = (rsmExport, rsmOff, rsmOn);

  // Experimental
  // Request to enable or configure remote session mode
  TRemoteEnableRequest = record
    Mode: TRemoteSessionMode;
    HasMode: Boolean;
  end;

  // Experimental
  // Result of enabling remote session mode
  TRemoteEnableResult = record
    RemoteSteerable: Boolean;
    Url: string;
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
    EnableSessionTelemetry: Boolean;
    OnExitPlanMode: TExitPlanModeHandler;
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
    EnableSessionTelemetry: Boolean;
    OnExitPlanMode: TExitPlanModeHandler;
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
    Remote: Boolean;
    OnGetTraceContext: TTraceContextProvider;
  end;

  // Unsubscribe token
  TUnsubscribeProc = reference to procedure;

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
