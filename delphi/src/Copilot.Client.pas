{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

unit Copilot.Client;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.SyncObjs,
  System.Generics.Collections, System.Threading,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  Copilot.Types, Copilot.JsonRpc, Copilot.Session,
  Copilot.SdkProtocolVersion, Copilot.SessionParams;

type
  TConnectionState = (csDisconnected, csConnecting, csConnected, csError);

  TCopilotClient = class
  private
    FOptions: TCopilotClientOptions;
    FRpc: TJsonRpcClient;
    FState: TConnectionState;
    FSessions: TObjectDictionary<string, TCopilotSession>;
    FSessionsLock: TCriticalSection;
    FLifecycleHandlers: TList<TSessionLifecycleHandler>;
    FLifecycleLock: TCriticalSection;
    FTokenProviders: TDictionary<string, TGitHubTokenProvider>;
    FTokenLock: TCriticalSection;
    FLastPolicyHelperFailure: TPolicyHelperFailure;
    {$IFDEF MSWINDOWS}
    FProcessInfo: TProcessInformation;
    FChildStdinRead, FChildStdinWrite: THandle;
    FChildStdoutRead, FChildStdoutWrite: THandle;
    {$ENDIF}
    FStdinStream: THandleStream;
    FStdoutStream: THandleStream;
    FProcessStarted: Boolean;
    procedure SpawnProcess;
    procedure RegisterRpcHandlers;
    procedure HandleSessionEvent(const Method: string; const Params: TJSONValue);
    procedure HandleSessionLifecycle(const Method: string; const Params: TJSONValue);
    function HandleToolCall(const Method: string;
      const Params: TJSONValue): TJSONValue;
    function HandlePermissionRequest(const Method: string;
      const Params: TJSONValue): TJSONValue;
    function HandleUserInputRequest(const Method: string;
      const Params: TJSONValue): TJSONValue;
    function HandleHooksInvoke(const Method: string;
      const Params: TJSONValue): TJSONValue;
    function HandleGitHubToken(const Method: string;
      const Params: TJSONValue): TJSONValue;
    function HandleShellAuthentication(const Method: string;
      const Params: TJSONValue): TJSONValue;
    function HandleMemoryStore(const Method: string;
      const Params: TJSONValue): TJSONValue;
    function HandleNetwork(const Method: string;
      const Params: TJSONValue): TJSONValue;
    function FindSession(const ASessionId: string): TCopilotSession;
    function BuildToolsArray(const ATools: TArray<TTool>): TJSONArray;
    function GetCliArgs: TArray<string>;
    function PermissionDecisionToString(D: TPermissionDecision): string;
    // SDK protocol 4 helpers
    function NegotiateProtocolVersion: Integer;
    function BuildClientInfo: TJSONObject;
    function IsEmptyMode: Boolean;
    function BuildFlatParams(const Config: TSessionConfig;
      const RegistrationId: string): TJSONObject;
    function RegisterTokenProvider(const Provider: TGitHubTokenProvider): string;
    procedure UnregisterTokenProvider(const RegistrationId: string);
    function SendSessionRequest(const Method: string; Flat: TJSONObject;
      IsResume: Boolean; const RegistrationId: string): TJSONValue;
    procedure AttachSessionCallbacks(Session: TCopilotSession;
      const Config: TSessionConfig; const RegistrationId: string);
  public
    constructor Create; overload;
    constructor Create(const AOptions: TCopilotClientOptions); overload;
    destructor Destroy; override;

    // Start the client (spawn CLI process or connect to URL)
    procedure Start;

    // Stop the client and terminate the CLI process
    procedure Stop;

    // Verify connectivity and protocol version
    function Ping: TPingResponse;

    // Create a new session
    function CreateSession: TCopilotSession; overload;
    function CreateSession(const Config: TSessionConfig): TCopilotSession; overload;

    // Resume an existing session
    function ResumeSession(const Config: TResumeSessionConfig): TCopilotSession;

    // Get session metadata
    function GetSessionMetadata(const ASessionId: string): TSessionMetadata;

    // Get the foreground session id (TUI mode)
    function GetForegroundSessionId: string;

    // Set the foreground session id (TUI mode)
    procedure SetForegroundSessionId(const ASessionId: string);

    // List all sessions
    function ListSessions: TJSONArray; overload;
    function ListSessions(const Filter: TSessionListFilter): TJSONArray; overload;

    // Delete a session permanently
    procedure DeleteSession(const ASessionId: string);

    // Get the last-used session ID (may be empty)
    function GetLastSessionId: string;

    // List available models
    function ListModels: TJSONArray;

    // Get CLI status information
    function GetStatus: TJSONObject;

    // Get authentication status
    function GetAuthStatus: TJSONObject;

    // Subscribe to session lifecycle events
    function OnLifecycle(Handler: TSessionLifecycleHandler): TUnsubscribeProc;

    // Clear the runtime's cached enterprise managed settings
    // (managedSettings.clearCache).
    procedure ClearManagedSettingsCache;

    // --- MCP secret store and plugin extension approvals (v3.1.0) ---
    // Server-scoped requests that need Copilot CLI 1.0.116 or newer; older
    // CLIs answer method-not-found, raised as EJsonRpcError (Code -32601)
    // like any other RPC error.

    // Save a legacy MCP configuration credential in the runtime-owned secret
    // store (mcp.secrets.save). Value is a credential and is never logged.
    // ConfigDir = '' uses the runtime's default configuration directory.
    function McpSecretsSave(const SecretId: string; const Value: string;
      const ConfigDir: string = ''): TMcpSecretsSaveResult;

    // Delete a legacy MCP configuration credential (mcp.secrets.delete).
    procedure McpSecretsDelete(const SecretId: string;
      const ConfigDir: string = '');

    // Delete the legacy MCP credentials indexed for a server
    // (mcp.secrets.deleteServer).
    procedure McpSecretsDeleteServer(const ServerName: string;
      const ConfigDir: string = '');

    // Persist a pending plugin extension permission request as a user-scoped
    // approval, before answering that request normally
    // (permissions.extensionApprovals.grant).
    function ExtensionApprovalsGrant(const ASessionId: string;
      const RequestId: string): TPermissionsExtensionApprovalsGrantResult;

    // List the user-scoped plugin extension approvals
    // (permissions.extensionApprovals.list). ASessionId = '' lists the
    // server's configuration profile, otherwise the attached session's.
    function ExtensionApprovalsList(
      const ASessionId: string = ''): TArray<TPermissionsExtensionApproval>;

    // Revoke one user-scoped plugin extension approval
    // (permissions.extensionApprovals.revoke). Pass the approval's
    // ExtensionName and InstallationKey unchanged.
    function ExtensionApprovalsRevoke(const ExtensionName: string;
      const InstallationKey: string;
      const ASessionId: string = ''): TPermissionsExtensionApprovalsRevokeResult;

    // Get connection state
    property State: TConnectionState read FState;
    property Options: TCopilotClientOptions read FOptions;
    // Policy Helper failure attached to the last failed CreateSession or
    // ResumeSession (error.data.policyHelperFailure); Present = False if none.
    property LastPolicyHelperFailure: TPolicyHelperFailure
      read FLastPolicyHelperFailure;
  end;

implementation

uses
  System.IOUtils;

const
  // SDK protocol 4 only (Copilot CLI 1.0.113 or newer).
  MIN_PROTOCOL_VERSION = 4;
  MAX_PROTOCOL_VERSION = 4;

// Parse JSON text options (objects, arrays, literals); fall back to a string.
function JsonOrString(const Text: string): TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(Text);
  if not Assigned(Result) then
    Result := TJSONString.Create(Text);
end;

function StringArray(const Items: TArray<string>): TJSONArray;
begin
  Result := TJSONArray.Create;
  for var Item in Items do
    Result.Add(Item);
end;

procedure AddOptionalBool(Obj: TJSONObject; const Key: string;
  Value: TOptionalBool);
begin
  case Value of
    obFalse: Obj.AddPair(Key, TJSONBool.Create(False));
    obTrue: Obj.AddPair(Key, TJSONBool.Create(True));
  end;
end;

function HasAnyHook(const Hooks: TSessionHooks): Boolean;
begin
  Result := Assigned(Hooks.OnPreToolUse) or Assigned(Hooks.OnPostToolUse) or
    Assigned(Hooks.OnPreMcpToolCall) or Assigned(Hooks.OnUserPromptTransformed) or
    Assigned(Hooks.OnSubagentStart) or Assigned(Hooks.OnSubagentStop);
end;

function ReadSubagentInput(Input: TJSONObject): TSubagentHookInput;
begin
  Result := Default(TSubagentHookInput);
  Result.AgentName := Input.GetValue<string>('agentName', '');
  Result.AgentDisplayName := Input.GetValue<string>('agentDisplayName', '');
  Result.AgentDescription := Input.GetValue<string>('agentDescription', '');
  Result.TranscriptPath := Input.GetValue<string>('transcriptPath', '');
  Result.AgentId := Input.GetValue<string>('agentId', '');
  Result.AgentType := Input.GetValue<string>('agentType', '');
  Result.StopReason := Input.GetValue<string>('stopReason', '');
  Result.Response := Input.GetValue<string>('response', '');
end;

// String items of a JSON array (empty when Value is not an array).
function JsonStrings(const Value: TJSONValue): TArray<string>;
begin
  Result := nil;
  if Value is TJSONArray then
    for var Item in TJSONArray(Value) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Item.Value;
    end;
end;

// Decode one PermissionsExtensionApproval object.
function ParseExtensionApproval(Obj: TJSONObject): TPermissionsExtensionApproval;
begin
  Result := Default(TPermissionsExtensionApproval);
  Result.ExtensionName := Obj.GetValue<string>('extensionName', '');
  Result.PluginName := Obj.GetValue<string>('pluginName', '');
  Result.Capabilities := JsonStrings(Obj.GetValue('capabilities'));
  Result.GrantedAt := Obj.GetValue<string>('grantedAt', '');
  Result.InstallationKey := Obj.GetValue<string>('installationKey', '');
  Result.InstallationLabel := Obj.GetValue<string>('installationLabel', '');
end;

// Decode error.data.policyHelperFailure from a JSON-RPC error object.
function ParsePolicyHelperFailure(const ErrorJson: string): TPolicyHelperFailure;
var
  Parsed, DataVal, FailureVal, HelpVal: TJSONValue;
  F: TJSONObject;
begin
  Result := Default(TPolicyHelperFailure);
  Parsed := TJSONObject.ParseJSONValue(ErrorJson);
  try
    if not (Parsed is TJSONObject) then
      Exit;
    DataVal := TJSONObject(Parsed).GetValue('data');
    if not (DataVal is TJSONObject) then
      Exit;
    FailureVal := TJSONObject(DataVal).GetValue('policyHelperFailure');
    if not (FailureVal is TJSONObject) then
      Exit;
    F := TJSONObject(FailureVal);
    Result.Present := True;
    Result.Category := F.GetValue<string>('category', '');
    Result.FailureMode := F.GetValue<string>('failureMode', '');
    Result.Message := F.GetValue<string>('message', '');
    Result.PreviousPolicyRetained := F.GetValue<Boolean>('previousPolicyRetained', False);
    Result.Reason := F.GetValue<string>('reason', '');
    Result.Severity := F.GetValue<string>('severity', '');
    Result.Authority := F.GetValue<string>('authority', '');
    Result.Code := F.GetValue<string>('code', '');
    if Assigned(F.GetValue('details')) then
      Result.DetailsJson := F.GetValue('details').ToJSON;
    Result.Diagnostic := F.GetValue<string>('diagnostic', '');
    Result.Remediation := F.GetValue<string>('remediation', '');
    HelpVal := F.GetValue('helpLink');
    if HelpVal is TJSONObject then
    begin
      Result.HelpLinkUrl := TJSONObject(HelpVal).GetValue<string>('url', '');
      Result.HelpLinkLabel := TJSONObject(HelpVal).GetValue<string>('label', '');
    end;
  finally
    Parsed.Free;
  end;
end;

{ TCopilotClient }

constructor TCopilotClient.Create;
begin
  Create(Default(TCopilotClientOptions));
end;

constructor TCopilotClient.Create(const AOptions: TCopilotClientOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FState := csDisconnected;
  FProcessStarted := False;
  FSessions := TObjectDictionary<string, TCopilotSession>.Create([doOwnsValues]);
  FSessionsLock := TCriticalSection.Create;
  FLifecycleHandlers := TList<TSessionLifecycleHandler>.Create;
  FLifecycleLock := TCriticalSection.Create;
  FTokenProviders := TDictionary<string, TGitHubTokenProvider>.Create;
  FTokenLock := TCriticalSection.Create;
end;

destructor TCopilotClient.Destroy;
begin
  Stop;
  FTokenLock.Free;
  FTokenProviders.Free;
  FLifecycleLock.Free;
  FLifecycleHandlers.Free;
  FSessionsLock.Free;
  FSessions.Free;
  inherited;
end;

function TCopilotClient.GetCliArgs: TArray<string>;
begin
  Result := TArray<string>.Create(
    '--headless',
    '--no-auto-update',
    '--log-level', 'info',
    '--stdio'
  );
  if FOptions.LogLevel <> '' then
  begin
    Result[3] := FOptions.LogLevel;
  end;
end;

procedure TCopilotClient.SpawnProcess;
{$IFDEF MSWINDOWS}
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  CmdLine: string;
  CliPath: string;
  Args: TArray<string>;
begin
  CliPath := FOptions.CliPath;
  if CliPath = '' then
    CliPath := 'copilot';

  SA.nLength := SizeOf(TSecurityAttributes);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  // Create stdin pipe
  if not CreatePipe(FChildStdinRead, FChildStdinWrite, @SA, 0) then
    raise Exception.Create('Failed to create stdin pipe');
  SetHandleInformation(FChildStdinWrite, HANDLE_FLAG_INHERIT, 0);

  // Create stdout pipe
  if not CreatePipe(FChildStdoutRead, FChildStdoutWrite, @SA, 0) then
    raise Exception.Create('Failed to create stdout pipe');
  SetHandleInformation(FChildStdoutRead, HANDLE_FLAG_INHERIT, 0);

  ZeroMemory(@SI, SizeOf(SI));
  SI.cb := SizeOf(SI);
  SI.hStdInput := FChildStdinRead;
  SI.hStdOutput := FChildStdoutWrite;
  SI.hStdError := GetStdHandle(STD_ERROR_HANDLE);
  SI.dwFlags := STARTF_USESTDHANDLES;

  Args := GetCliArgs;
  // A ".js" entry point (npm-loader.js) runs through node; any other path
  // (native binary or launcher) runs directly. Quote paths with spaces.
  if SameText(ExtractFileExt(CliPath), '.js') then
    CmdLine := 'node "' + CliPath + '"'
  else
    CmdLine := '"' + CliPath + '"';
  for var Arg in Args do
    CmdLine := CmdLine + ' ' + Arg;

  ZeroMemory(@FProcessInfo, SizeOf(FProcessInfo));

  if not CreateProcess(nil, PChar(CmdLine), nil, nil, True,
    CREATE_NO_WINDOW, nil, nil, SI, FProcessInfo) then
    raise Exception.CreateFmt('Failed to start CLI process: %s (error %d)',
      [CliPath, GetLastError]);

  // Close child-side handles so our streams detect EOF properly
  CloseHandle(FChildStdinRead);
  CloseHandle(FChildStdoutWrite);

  FStdinStream := THandleStream.Create(FChildStdinWrite);
  FStdoutStream := THandleStream.Create(FChildStdoutRead);
  FProcessStarted := True;
end;
{$ELSE}
begin
  raise Exception.Create('Process spawning is only supported on Windows. ' +
    'Use CliUrl to connect to an external server on other platforms.');
end;
{$ENDIF}

procedure TCopilotClient.RegisterRpcHandlers;
begin
  // Server -> Client notifications
  FRpc.OnNotification('session.event',
    procedure(const Method: string; const Params: TJSONValue)
    begin
      HandleSessionEvent(Method, Params);
    end);

  FRpc.OnNotification('session.lifecycle',
    procedure(const Method: string; const Params: TJSONValue)
    begin
      HandleSessionLifecycle(Method, Params);
    end);

  // Server -> Client requests
  FRpc.OnRequest('tool.call',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandleToolCall(Method, Params);
    end);

  FRpc.OnRequest('permission.request',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandlePermissionRequest(Method, Params);
    end);

  FRpc.OnRequest('userInput.request',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandleUserInputRequest(Method, Params);
    end);

  FRpc.OnRequest('hooks.invoke',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandleHooksInvoke(Method, Params);
    end);

  // SDK protocol 4 host callbacks
  FRpc.OnRequest('gitHubToken.getToken',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandleGitHubToken(Method, Params);
    end);

  FRpc.OnRequest('shellAuthentication.request',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandleShellAuthentication(Method, Params);
    end);

  // Prompts are answered synchronously, so there is nothing to cancel.
  FRpc.OnRequest('shellAuthentication.cancel',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := TJSONObject.Create;
    end);

  FRpc.OnRequest('memoryStore.store',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandleMemoryStore(Method, Params);
    end);

  FRpc.OnRequest('network.resolveProxy',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandleNetwork(Method, Params);
    end);

  FRpc.OnRequest('network.authenticateNegotiate',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandleNetwork(Method, Params);
    end);

  FRpc.OnRequest('network.authenticateBasic',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandleNetwork(Method, Params);
    end);
end;

procedure TCopilotClient.Start;
var
  Mode: string;
  ServerVersion: Integer;
  NetParams: TJSONObject;
begin
  if FState = csConnected then
    Exit;

  Mode := FOptions.Mode;
  if (Mode <> '') and (Mode <> ClientModeCodingAgent) and
    (Mode <> ClientModeCopilotCli) and (Mode <> ClientModeEmpty) then
    raise Exception.CreateFmt('Unknown client mode "%s"', [Mode]);

  FState := csConnecting;
  try
    if FOptions.CliUrl <> '' then
      raise Exception.Create('TCP/URL connections are not yet supported. ' +
        'Use CliPath to spawn the CLI process.')
    else
      SpawnProcess;

    FRpc := TJsonRpcClient.Create(FStdoutStream, FStdinStream);
    RegisterRpcHandlers;
    FRpc.Start;

    // SDK protocol 4 handshake: connect (with clientInfo), falling back to
    // ping for runtimes without connect. Exactly version 4 is accepted.
    ServerVersion := NegotiateProtocolVersion;
    if ServerVersion < 0 then
      raise Exception.CreateFmt(
        'SDK protocol version mismatch: SDK supports version %d, but server ' +
        'does not report a protocol version. Please update your SDK or server ' +
        'to ensure compatibility.', [MAX_PROTOCOL_VERSION]);
    if (ServerVersion < MIN_PROTOCOL_VERSION) or
      (ServerVersion > MAX_PROTOCOL_VERSION) then
      raise Exception.CreateFmt(
        'SDK protocol version mismatch: SDK supports version %d, but server ' +
        'reports version %d. Please update your SDK or server to ensure ' +
        'compatibility.', [MAX_PROTOCOL_VERSION, ServerVersion]);

    // Register network callbacks (network.setProvider).
    if Assigned(FOptions.Network.ResolveProxy) or
      Assigned(FOptions.Network.AuthenticateNegotiate) or
      Assigned(FOptions.Network.AuthenticateBasic) then
    begin
      NetParams := TJSONObject.Create;
      try
        if FOptions.Network.Scope <> '' then
          NetParams.AddPair('scope', FOptions.Network.Scope);
        if Assigned(FOptions.Network.ResolveProxy) then
          NetParams.AddPair('resolveProxy', TJSONObject.Create);
        if Assigned(FOptions.Network.AuthenticateNegotiate) then
          NetParams.AddPair('authenticateNegotiate', TJSONObject.Create);
        if Assigned(FOptions.Network.AuthenticateBasic) then
          NetParams.AddPair('authenticateBasic', TJSONObject.Create);
        FRpc.SendRequest('network.setProvider', NetParams).Free;
      finally
        NetParams.Free;
      end;
    end;

    // Set up session filesystem provider if configured
    if FOptions.SessionFs.InitialCwd <> '' then
    begin
      var FsParams := TJSONObject.Create;
      try
        FsParams.AddPair('initialCwd', FOptions.SessionFs.InitialCwd);
        FsParams.AddPair('sessionStatePath', FOptions.SessionFs.SessionStatePath);
        FsParams.AddPair('conventions', FOptions.SessionFs.Conventions);
        FRpc.SendRequest('sessionFs.setProvider', FsParams);
      finally
        FsParams.Free;
      end;
    end;

    FState := csConnected;
  except
    on E: Exception do
    begin
      FState := csError;
      raise;
    end;
  end;
end;

procedure TCopilotClient.Stop;
begin
  if FState = csDisconnected then
    Exit;

  FSessionsLock.Enter;
  try
    FSessions.Clear;
  finally
    FSessionsLock.Leave;
  end;

  if Assigned(FRpc) then
  begin
    FRpc.Stop;
    FreeAndNil(FRpc);
  end;

  FreeAndNil(FStdinStream);
  FreeAndNil(FStdoutStream);

  {$IFDEF MSWINDOWS}
  if FProcessStarted then
  begin
    TerminateProcess(FProcessInfo.hProcess, 0);
    CloseHandle(FProcessInfo.hProcess);
    CloseHandle(FProcessInfo.hThread);
    CloseHandle(FChildStdinWrite);
    CloseHandle(FChildStdoutRead);
    FProcessStarted := False;
  end;
  {$ENDIF}

  FState := csDisconnected;
end;

function TCopilotClient.Ping: TPingResponse;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Params := TJSONObject.Create;
  Params.AddPair('sdkProtocolVersion', SDK_PROTOCOL_VERSION);

  ResultVal := FRpc.SendRequest('ping', Params);
  try
    if Assigned(ResultVal) and (ResultVal is TJSONObject) then
    begin
      var Obj := TJSONObject(ResultVal);
      Result.ProtocolVersion := Obj.GetValue<string>('protocolVersion', '');
      Result.ServerVersion := Obj.GetValue<string>('serverVersion', '');
      Result.Status := Obj.GetValue<string>('status', 'ok');
    end;
  finally
    ResultVal.Free;
  end;
end;

function TCopilotClient.BuildToolsArray(const ATools: TArray<TTool>): TJSONArray;
begin
  Result := TJSONArray.Create;
  for var Tool in ATools do
  begin
    var ToolObj := TJSONObject.Create;
    ToolObj.AddPair('name', Tool.Name);
    ToolObj.AddPair('description', Tool.Description);
    if Tool.Defer <> '' then
      ToolObj.AddPair('defer', Tool.Defer);
    if Tool.Idempotent then
      ToolObj.AddPair('idempotent', TJSONBool.Create(True));
    if Assigned(Tool.Parameters) then
      ToolObj.AddPair('parameters', Tool.Parameters.Clone as TJSONObject);
    Result.AddElement(ToolObj);
  end;
end;

function TCopilotClient.IsEmptyMode: Boolean;
begin
  Result := FOptions.Mode = ClientModeEmpty;
end;

function TCopilotClient.BuildClientInfo: TJSONObject;
begin
  Result := TJSONObject.Create;
  if FOptions.ClientInfo.EditorName <> '' then
    Result.AddPair('editorName', FOptions.ClientInfo.EditorName);
  if FOptions.ClientInfo.EditorVersion <> '' then
    Result.AddPair('editorVersion', FOptions.ClientInfo.EditorVersion);
  if FOptions.ClientInfo.ExtensionName <> '' then
    Result.AddPair('extensionName', FOptions.ClientInfo.ExtensionName);
  if FOptions.ClientInfo.ExtensionVersion <> '' then
    Result.AddPair('extensionVersion', FOptions.ClientInfo.ExtensionVersion);
  if Result.Count = 0 then
    FreeAndNil(Result);
end;

function TCopilotClient.NegotiateProtocolVersion: Integer;
var
  Params, PingParams, Info: TJSONObject;
  ResultVal: TJSONValue;
begin
  ResultVal := nil;
  Params := TJSONObject.Create;
  try
    Info := BuildClientInfo;
    if Assigned(Info) then
      Params.AddPair('clientInfo', Info);
    try
      ResultVal := FRpc.SendRequest('connect', Params);
    except
      on E: EJsonRpcError do
      begin
        // Runtime without connect: fall back to ping.
        if (E.Code <> -32601) and
          (Pos('Unhandled method connect', E.ErrorJson) = 0) then
          raise;
        PingParams := TJSONObject.Create;
        try
          PingParams.AddPair('message', 'ping');
          ResultVal := FRpc.SendRequest('ping', PingParams);
        finally
          PingParams.Free;
        end;
      end;
    end;
  finally
    Params.Free;
  end;

  try
    Result := -1;
    if ResultVal is TJSONObject then
      Result := TJSONObject(ResultVal).GetValue<Integer>('protocolVersion', -1);
  finally
    ResultVal.Free;
  end;
end;

function TCopilotClient.RegisterTokenProvider(
  const Provider: TGitHubTokenProvider): string;
var
  Guid: TGUID;
begin
  Result := '';
  if not Assigned(Provider) then
    Exit;
  CreateGUID(Guid);
  // RFC 4122 form without braces.
  Result := LowerCase(Copy(GUIDToString(Guid), 2, 36));
  FTokenLock.Enter;
  try
    FTokenProviders.AddOrSetValue(Result, Provider);
  finally
    FTokenLock.Leave;
  end;
end;

procedure TCopilotClient.UnregisterTokenProvider(const RegistrationId: string);
begin
  if RegistrationId = '' then
    Exit;
  FTokenLock.Enter;
  try
    FTokenProviders.Remove(RegistrationId);
  finally
    FTokenLock.Leave;
  end;
end;

// Build the flat (v1-style) session payload. SendSessionRequest converts it
// to the SDK protocol 4 grouped payload (GroupSessionParams) before sending.
function TCopilotClient.BuildFlatParams(const Config: TSessionConfig;
  const RegistrationId: string): TJSONObject;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  try
    if Config.Model <> '' then
      Params.AddPair('model', Config.Model);
    Params.AddPair('streaming', TJSONBool.Create(Config.Streaming));
    Params.AddPair('sdkProtocolVersion', SDK_PROTOCOL_VERSION);

    if Length(Config.Tools) > 0 then
      Params.AddPair('tools', BuildToolsArray(Config.Tools));

    if Length(Config.SkillDirectories) > 0 then
      Params.AddPair('skillDirectories', StringArray(Config.SkillDirectories));

    if Length(Config.DisabledSkills) > 0 then
      Params.AddPair('disabledSkills', StringArray(Config.DisabledSkills));

    if Length(Config.ExcludedTools) > 0 then
      Params.AddPair('excludedTools', StringArray(Config.ExcludedTools));

    if Config.IncludeSubAgentStreamingEvents then
      Params.AddPair('includeSubAgentStreamingEvents', TJSONBool.Create(True));

    // Free-form "instructions" has no protocol 4 equivalent; the translator
    // drops it. Use InstructionDirectories instead.
    if Length(Config.Instructions) > 0 then
      Params.AddPair('instructions', StringArray(Config.Instructions));

    if Length(Config.InstructionDirectories) > 0 then
      Params.AddPair('instructionDirectories',
        StringArray(Config.InstructionDirectories));

    if Config.EnableConfigDiscovery then
      Params.AddPair('enableConfigDiscovery', TJSONBool.Create(True));

    if Config.GithubToken <> '' then
      Params.AddPair('gitHubToken', Config.GithubToken);

    if FOptions.SessionIdleTimeoutSeconds > 0 then
      Params.AddPair('sessionIdleTimeoutSeconds',
        TJSONNumber.Create(FOptions.SessionIdleTimeoutSeconds));

    if FOptions.SessionFs.WorkspaceRoot <> '' then
    begin
      var FsObj := TJSONObject.Create;
      FsObj.AddPair('workspaceRoot', FOptions.SessionFs.WorkspaceRoot);
      FsObj.AddPair('stateRoot', FOptions.SessionFs.StateRoot);
      if FOptions.SessionFs.PathStyle <> '' then
        FsObj.AddPair('pathStyle', FOptions.SessionFs.PathStyle);
      Params.AddPair('sessionFs', FsObj);
    end;

    // --- Upstream-sync session config passthroughs (camelCase wire keys) ---
    if Config.EnableCitations then
      Params.AddPair('enableCitations', TJSONBool.Create(True));

    if Length(Config.ExcludedBuiltinAgents) > 0 then
      Params.AddPair('excludedBuiltinAgents',
        StringArray(Config.ExcludedBuiltinAgents));

    if Config.SessionLimits.MaxAiCredits > 0 then
    begin
      var LimitsObj := TJSONObject.Create;
      LimitsObj.AddPair('maxAiCredits',
        TJSONNumber.Create(Config.SessionLimits.MaxAiCredits));
      Params.AddPair('sessionLimits', LimitsObj);
    end;

    if Config.Memory.Enabled then
    begin
      var MemObj := TJSONObject.Create;
      MemObj.AddPair('enabled', TJSONBool.Create(True));
      Params.AddPair('memory', MemObj);
    end;

    if Config.OtlpProtocol <> '' then
      Params.AddPair('otlpProtocol', Config.OtlpProtocol);

    if Config.EnableWebSocketResponses then
      Params.AddPair('enableWebSocketResponses', TJSONBool.Create(True));

    if Assigned(Config.ExpAssignments) and (Config.ExpAssignments.Count > 0) then
    begin
      var ExpObj := TJSONObject.Create;
      for var ExpPair in Config.ExpAssignments do
        ExpObj.AddPair(ExpPair.Key, ExpPair.Value);
      Params.AddPair('expAssignments', ExpObj);
    end;

    // --- Newly-synced session config passthroughs (camelCase wire keys) ---
    if Config.RewindEnabled then
      Params.AddPair('rewindEnabled', TJSONBool.Create(True));

    if Length(Config.AdditionalDirectories) > 0 then
      Params.AddPair('additionalDirectories',
        StringArray(Config.AdditionalDirectories));

    if Length(Config.DisabledMcpServers) > 0 then
      Params.AddPair('disabledMcpServers', StringArray(Config.DisabledMcpServers));

    // JSON text options are sent as JSON values (objects stay objects).
    if Config.GithubMcpToolConfig <> '' then
      Params.AddPair('githubMcpToolConfig', JsonOrString(Config.GithubMcpToolConfig));

    if Config.CanvasProvider <> '' then
      Params.AddPair('canvasProvider', JsonOrString(Config.CanvasProvider));

    if Config.CustomAgentsLocalOnly then
      Params.AddPair('customAgentsLocalOnly', TJSONBool.Create(True));

    if Config.DecisionContext <> '' then
      Params.AddPair('decisionContext', JsonOrString(Config.DecisionContext));

    // User-prompt-transformed hook: signal the runtime that a handler is registered.
    if Assigned(Config.Hooks.OnUserPromptTransformed) then
      Params.AddPair('userPromptTransformed', TJSONBool.Create(True));

    if Length(Config.BuiltinPluginDirectories) > 0 then
      Params.AddPair('builtinPluginDirectories',
        StringArray(Config.BuiltinPluginDirectories));

    if Config.ArgsSchema <> '' then
      Params.AddPair('argsSchema', JsonOrString(Config.ArgsSchema));

    if Config.ReasoningEffort <> '' then
      Params.AddPair('reasoningEffort', Config.ReasoningEffort);

    if Config.ToolSearch <> '' then
      Params.AddPair('toolSearch', JsonOrString(Config.ToolSearch));

    if Config.InProcess then
      Params.AddPair('inProcess', TJSONBool.Create(True));

    if Config.ExperimentalMode then
      Params.AddPair('experimentalMode', TJSONBool.Create(True));

    if Config.ContentExclusion then
      Params.AddPair('contentExclusion', TJSONBool.Create(True));

    // MCP OAuth host token handler: signal the runtime that a handler is registered.
    if Assigned(Config.OnMcpAuthRequest) then
      Params.AddPair('mcpAuthHandler', TJSONBool.Create(True));

    // --- SDK protocol 4 / v3.0.0 options ---
    if Config.WorkingDirectory <> '' then
      Params.AddPair('workingDirectory', Config.WorkingDirectory);

    if Length(Config.AvailableTools) > 0 then
      Params.AddPair('availableTools', StringArray(Config.AvailableTools));

    if Config.AskUserVariant <> '' then
      Params.AddPair('askUserVariant', Config.AskUserVariant);

    if RegistrationId <> '' then
      Params.AddPair('gitHubTokenProviderRegistrationId', RegistrationId);

    if Config.Sandbox.Enabled then
    begin
      var SandboxObj := TJSONObject.Create;
      SandboxObj.AddPair('enabled', TJSONBool.Create(True));
      if Config.Sandbox.AllowBypass then
        SandboxObj.AddPair('allowBypass', TJSONBool.Create(True));
      AddOptionalBool(SandboxObj, 'addCurrentWorkingDirectory',
        Config.Sandbox.AddCurrentWorkingDirectory);
      if Config.Sandbox.UserPolicyJson <> '' then
        SandboxObj.AddPair('userPolicy', JsonOrString(Config.Sandbox.UserPolicyJson));
      Params.AddPair('sandbox', SandboxObj);
    end;

    if Assigned(Config.FeatureFlags) and (Config.FeatureFlags.Count > 0) then
    begin
      var FlagsObj := TJSONObject.Create;
      for var Flag in Config.FeatureFlags do
        FlagsObj.AddPair(Flag.Key, TJSONBool.Create(Flag.Value));
      Params.AddPair('featureFlags', FlagsObj);
    end;

    if Length(Config.AllowedModels) > 0 then
      Params.AddPair('allowedModels', StringArray(Config.AllowedModels));

    if Config.AuthClientIdMetadataUrl <> '' then
      Params.AddPair('authClientIdMetadataUrl', Config.AuthClientIdMetadataUrl);

    if Config.MaxOutputTokens > 0 then
    begin
      // The capabilities limits object uses snake_case wire keys.
      var LimitsCaps := TJSONObject.Create;
      LimitsCaps.AddPair('max_output_tokens',
        TJSONNumber.Create(Config.MaxOutputTokens));
      var CapsObj := TJSONObject.Create;
      CapsObj.AddPair('limits', LimitsCaps);
      Params.AddPair('modelCapabilities', CapsObj);
    end;

    if Length(Config.AgentDirectories) > 0 then
      Params.AddPair('agentDirectories', StringArray(Config.AgentDirectories));

    if Length(Config.HookDirectories) > 0 then
      Params.AddPair('hookDirectories', StringArray(Config.HookDirectories));

    if Config.TrustWorkingDirectory <> '' then
      Params.AddPair('trustWorkingDirectory', Config.TrustWorkingDirectory);

    var ImageObj := TJSONObject.Create;
    AddOptionalBool(ImageObj, 'enabled', Config.ImageGeneration.Enabled);
    AddOptionalBool(ImageObj, 'watermark', Config.ImageGeneration.Watermark);
    AddOptionalBool(ImageObj, 'saveToDisk', Config.ImageGeneration.SaveToDisk);
    if ImageObj.Count > 0 then
      Params.AddPair('imageGeneration', ImageObj)
    else
      ImageObj.Free;

    var AutopilotObj := TJSONObject.Create;
    AddOptionalBool(AutopilotObj, 'continuationEnabled',
      Config.Autopilot.ContinuationEnabled);
    if Config.Autopilot.MaxContinues > 0 then
      AutopilotObj.AddPair('maxContinues',
        TJSONNumber.Create(Config.Autopilot.MaxContinues));
    if AutopilotObj.Count > 0 then
      Params.AddPair('autopilot', AutopilotObj)
    else
      AutopilotObj.Free;

    // Hook handlers run through hooks.invoke (hooks.hasHandlers).
    if HasAnyHook(Config.Hooks) then
      Params.AddPair('hooks', TJSONBool.Create(True));

    // Legacy single BYOK provider, sent as the named provider "default".
    if Config.Provider.BaseUrl <> '' then
    begin
      var ProviderObj := TJSONObject.Create;
      if Config.Provider.ProviderType <> '' then
        ProviderObj.AddPair('type', Config.Provider.ProviderType);
      ProviderObj.AddPair('baseUrl', Config.Provider.BaseUrl);
      if Config.Provider.ApiKey <> '' then
        ProviderObj.AddPair('apiKey', Config.Provider.ApiKey);
      if Config.Provider.ApiKeyCommand <> '' then
        ProviderObj.AddPair('apiKeyCommand', Config.Provider.ApiKeyCommand);
      if Config.Provider.BearerToken <> '' then
        ProviderObj.AddPair('bearerToken', Config.Provider.BearerToken);
      if Config.Provider.WireApi <> '' then
        ProviderObj.AddPair('wireApi', Config.Provider.WireApi);
      if Config.Provider.MaxOutputTokens > 0 then
        ProviderObj.AddPair('maxOutputTokens',
          TJSONNumber.Create(Config.Provider.MaxOutputTokens));
      Params.AddPair('provider', ProviderObj);
    end;

    if Length(Config.Providers) > 0 then
    begin
      var ProvidersArr := TJSONArray.Create;
      for var P in Config.Providers do
      begin
        var PObj := TJSONObject.Create;
        PObj.AddPair('name', P.Name);
        if P.ProviderType <> '' then
          PObj.AddPair('type', P.ProviderType);
        PObj.AddPair('baseUrl', P.BaseUrl);
        if P.ApiKey <> '' then
          PObj.AddPair('apiKey', P.ApiKey);
        if P.ApiKeyCommand <> '' then
          PObj.AddPair('apiKeyCommand', P.ApiKeyCommand);
        if P.BearerToken <> '' then
          PObj.AddPair('bearerToken', P.BearerToken);
        if P.WireApi <> '' then
          PObj.AddPair('wireApi', P.WireApi);
        if P.Transport <> '' then
          PObj.AddPair('transport', P.Transport);
        if P.AzureJson <> '' then
          PObj.AddPair('azure', JsonOrString(P.AzureJson));
        if P.HeadersJson <> '' then
          PObj.AddPair('headers', JsonOrString(P.HeadersJson));
        if P.ModelProvider <> '' then
          PObj.AddPair('modelProvider', P.ModelProvider);
        ProvidersArr.AddElement(PObj);
      end;
      Params.AddPair('providers', ProvidersArr);
    end;

    if Length(Config.Models) > 0 then
    begin
      var ModelsArr := TJSONArray.Create;
      for var M in Config.Models do
      begin
        var MObj := TJSONObject.Create;
        MObj.AddPair('provider', M.Provider);
        MObj.AddPair('id', M.Id);
        if M.ModelId <> '' then
          MObj.AddPair('modelId', M.ModelId);
        if M.WireModel <> '' then
          MObj.AddPair('wireModel', M.WireModel);
        if M.Name <> '' then
          MObj.AddPair('name', M.Name);
        if M.MaxPromptTokens > 0 then
          MObj.AddPair('maxPromptTokens', TJSONNumber.Create(M.MaxPromptTokens));
        if M.MaxContextWindowTokens > 0 then
          MObj.AddPair('maxContextWindowTokens',
            TJSONNumber.Create(M.MaxContextWindowTokens));
        if M.MaxOutputTokens > 0 then
          MObj.AddPair('maxOutputTokens', TJSONNumber.Create(M.MaxOutputTokens));
        if M.CapabilitiesJson <> '' then
          MObj.AddPair('capabilities', JsonOrString(M.CapabilitiesJson));
        ModelsArr.AddElement(MObj);
      end;
      Params.AddPair('models', ModelsArr);
    end;

    if Config.DefaultProvider <> '' then
      Params.AddPair('defaultProvider', Config.DefaultProvider);

    if Config.CapiEnabled <> obUnset then
    begin
      var CapiObj := TJSONObject.Create;
      CapiObj.AddPair('enabled', TJSONBool.Create(Config.CapiEnabled = obTrue));
      Params.AddPair('capi', CapiObj);
    end;

    if Assigned(Config.OnShellAuthenticationRequest) then
      Params.AddPair('requestShellAuthentication', TJSONBool.Create(True));

    if Assigned(Config.StoreMemory) then
      Params.AddPair('hasMemoryStore', TJSONBool.Create(True));

    // The ask_user tool is answered through userInput.request.
    if Assigned(Config.OnUserInputRequest) then
      Params.AddPair('requestUserInput', TJSONBool.Create(True));
  except
    Params.Free;
    raise;
  end;
  Result := Params;
end;

function TCopilotClient.SendSessionRequest(const Method: string;
  Flat: TJSONObject; IsResume: Boolean; const RegistrationId: string): TJSONValue;
var
  Wire: TJSONObject;
begin
  FLastPolicyHelperFailure := Default(TPolicyHelperFailure);
  // Translate the flat payload to the SDK protocol 4 grouped shape.
  Wire := GroupSessionParams(Flat, IsResume, IsEmptyMode);
  try
    try
      Result := FRpc.SendRequest(Method, Wire);
    except
      on E: Exception do
      begin
        if E is EJsonRpcError then
          FLastPolicyHelperFailure :=
            ParsePolicyHelperFailure(EJsonRpcError(E).ErrorJson);
        UnregisterTokenProvider(RegistrationId);
        raise;
      end;
    end;
  finally
    Wire.Free;
  end;
end;

procedure TCopilotClient.AttachSessionCallbacks(Session: TCopilotSession;
  const Config: TSessionConfig; const RegistrationId: string);
var
  RegId: string;
begin
  RegId := RegistrationId;
  Session.OnShellAuthenticationRequest := Config.OnShellAuthenticationRequest;
  Session.StoreMemory := Config.StoreMemory;
  if RegId <> '' then
    Session.OnDetach := procedure
      begin
        Self.UnregisterTokenProvider(RegId);
      end;
end;

function TCopilotClient.CreateSession: TCopilotSession;
begin
  Result := CreateSession(Default(TSessionConfig));
end;

function TCopilotClient.CreateSession(const Config: TSessionConfig): TCopilotSession;
var
  Flat: TJSONObject;
  ResultVal: TJSONValue;
  SessionId, RegistrationId: string;
  Session: TCopilotSession;
begin
  if IsEmptyMode and (Length(Config.AvailableTools) = 0) then
    raise Exception.Create('Client mode "empty" requires AvailableTools on ' +
      'every session create and resume');
  if Assigned(Config.GitHubTokenProvider) and (Config.GithubToken <> '') then
    raise Exception.Create('GithubToken and GitHubTokenProvider are mutually exclusive');

  RegistrationId := RegisterTokenProvider(Config.GitHubTokenProvider);
  Flat := BuildFlatParams(Config, RegistrationId);
  try
    ResultVal := SendSessionRequest('session.create', Flat, False, RegistrationId);
  finally
    Flat.Free;
  end;

  try
    if not (ResultVal is TJSONObject) then
      raise Exception.Create('Invalid response from session.create');

    SessionId := TJSONObject(ResultVal).GetValue<string>('sessionId', '');
    if SessionId = '' then
      raise Exception.Create('No sessionId in session.create response');
  finally
    ResultVal.Free;
  end;

  Session := TCopilotSession.Create(SessionId, FRpc, Config.Streaming);
  Session.Tools := Config.Tools;
  Session.OnPermissionRequest := Config.OnPermissionRequest;
  Session.OnUserInputRequest := Config.OnUserInputRequest;
  Session.Hooks := Config.Hooks;
  AttachSessionCallbacks(Session, Config, RegistrationId);

  FSessionsLock.Enter;
  try
    FSessions.Add(SessionId, Session);
  finally
    FSessionsLock.Leave;
  end;

  Result := Session;
end;

function TCopilotClient.ResumeSession(
  const Config: TResumeSessionConfig): TCopilotSession;
var
  Shared: TSessionConfig;
  Flat: TJSONObject;
  RegistrationId: string;
  Session: TCopilotSession;
begin
  // Resume fields take precedence over the shared create options.
  Shared := Config.Options;
  if Config.Model <> '' then
    Shared.Model := Config.Model;
  Shared.Streaming := Config.Streaming;
  if Length(Config.Tools) > 0 then
    Shared.Tools := Config.Tools;
  if Length(Config.ExcludedTools) > 0 then
    Shared.ExcludedTools := Config.ExcludedTools;
  if Config.IncludeSubAgentStreamingEvents then
    Shared.IncludeSubAgentStreamingEvents := True;
  if Length(Config.InstructionDirectories) > 0 then
    Shared.InstructionDirectories := Config.InstructionDirectories;
  if Assigned(Config.OnPermissionRequest) then
    Shared.OnPermissionRequest := Config.OnPermissionRequest;
  if Assigned(Config.OnUserInputRequest) then
    Shared.OnUserInputRequest := Config.OnUserInputRequest;
  if HasAnyHook(Config.Hooks) then
    Shared.Hooks := Config.Hooks;

  if IsEmptyMode and (Length(Shared.AvailableTools) = 0) then
    raise Exception.Create('Client mode "empty" requires AvailableTools on ' +
      'every session create and resume');
  if Assigned(Shared.GitHubTokenProvider) and (Shared.GithubToken <> '') then
    raise Exception.Create('GithubToken and GitHubTokenProvider are mutually exclusive');

  RegistrationId := RegisterTokenProvider(Shared.GitHubTokenProvider);
  Flat := BuildFlatParams(Shared, RegistrationId);
  try
    Flat.AddPair('sessionId', Config.SessionId);
    if Config.DisableResume then
      Flat.AddPair('disableResume', TJSONBool.Create(True));
    if Config.ContinuePendingWork then
      Flat.AddPair('continuePendingWork', TJSONBool.Create(True));
    if Config.AllowTranscriptRecovery then
      Flat.AddPair('allowTranscriptRecovery', TJSONBool.Create(True));
    // Explicit null clears the default provider on resume.
    if Config.ClearDefaultProvider and (Shared.DefaultProvider = '') then
      Flat.AddPair('defaultProvider', TJSONNull.Create);
    SendSessionRequest('session.resume', Flat, True, RegistrationId).Free;
  finally
    Flat.Free;
  end;

  Session := TCopilotSession.Create(Config.SessionId, FRpc, Shared.Streaming);
  Session.Tools := Shared.Tools;
  Session.OnPermissionRequest := Shared.OnPermissionRequest;
  Session.OnUserInputRequest := Shared.OnUserInputRequest;
  Session.Hooks := Shared.Hooks;
  AttachSessionCallbacks(Session, Shared, RegistrationId);

  FSessionsLock.Enter;
  try
    FSessions.AddOrSetValue(Config.SessionId, Session);
  finally
    FSessionsLock.Leave;
  end;

  Result := Session;
end;

procedure TCopilotClient.ClearManagedSettingsCache;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  try
    FRpc.SendRequest('managedSettings.clearCache', Params).Free;
  finally
    Params.Free;
  end;
end;

function TCopilotClient.McpSecretsSave(const SecretId: string;
  const Value: string; const ConfigDir: string): TMcpSecretsSaveResult;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Result := Default(TMcpSecretsSaveResult);
  Params := TJSONObject.Create;
  try
    Params.AddPair('secretId', SecretId);
    Params.AddPair('value', Value);
    if ConfigDir <> '' then
      Params.AddPair('configDir', ConfigDir);
    ResultVal := FRpc.SendRequest('mcp.secrets.save', Params);
  finally
    Params.Free;
  end;
  try
    if ResultVal is TJSONObject then
      Result.UsedKeychain :=
        TJSONObject(ResultVal).GetValue<Boolean>('usedKeychain', False);
  finally
    ResultVal.Free;
  end;
end;

procedure TCopilotClient.McpSecretsDelete(const SecretId: string;
  const ConfigDir: string);
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  try
    Params.AddPair('secretId', SecretId);
    if ConfigDir <> '' then
      Params.AddPair('configDir', ConfigDir);
    FRpc.SendRequest('mcp.secrets.delete', Params).Free;
  finally
    Params.Free;
  end;
end;

procedure TCopilotClient.McpSecretsDeleteServer(const ServerName: string;
  const ConfigDir: string);
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  try
    Params.AddPair('serverName', ServerName);
    if ConfigDir <> '' then
      Params.AddPair('configDir', ConfigDir);
    FRpc.SendRequest('mcp.secrets.deleteServer', Params).Free;
  finally
    Params.Free;
  end;
end;

function TCopilotClient.ExtensionApprovalsGrant(const ASessionId: string;
  const RequestId: string): TPermissionsExtensionApprovalsGrantResult;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Result := Default(TPermissionsExtensionApprovalsGrantResult);
  Params := TJSONObject.Create;
  try
    Params.AddPair('sessionId', ASessionId);
    Params.AddPair('requestId', RequestId);
    ResultVal := FRpc.SendRequest('permissions.extensionApprovals.grant', Params);
  finally
    Params.Free;
  end;
  try
    if ResultVal is TJSONObject then
    begin
      Result.Granted := TJSONObject(ResultVal).GetValue<Boolean>('granted', False);
      var ApprovalVal := TJSONObject(ResultVal).GetValue('approval');
      if ApprovalVal is TJSONObject then
      begin
        Result.HasApproval := True;
        Result.Approval := ParseExtensionApproval(TJSONObject(ApprovalVal));
      end;
    end;
  finally
    ResultVal.Free;
  end;
end;

function TCopilotClient.ExtensionApprovalsList(
  const ASessionId: string): TArray<TPermissionsExtensionApproval>;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Result := nil;
  Params := TJSONObject.Create;
  try
    if ASessionId <> '' then
      Params.AddPair('sessionId', ASessionId);
    ResultVal := FRpc.SendRequest('permissions.extensionApprovals.list', Params);
  finally
    Params.Free;
  end;
  try
    if ResultVal is TJSONObject then
    begin
      var ApprovalsVal := TJSONObject(ResultVal).GetValue('approvals');
      if ApprovalsVal is TJSONArray then
        for var Item in TJSONArray(ApprovalsVal) do
          if Item is TJSONObject then
          begin
            SetLength(Result, Length(Result) + 1);
            Result[High(Result)] := ParseExtensionApproval(TJSONObject(Item));
          end;
    end;
  finally
    ResultVal.Free;
  end;
end;

function TCopilotClient.ExtensionApprovalsRevoke(const ExtensionName: string;
  const InstallationKey: string;
  const ASessionId: string): TPermissionsExtensionApprovalsRevokeResult;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Result := Default(TPermissionsExtensionApprovalsRevokeResult);
  Params := TJSONObject.Create;
  try
    Params.AddPair('extensionName', ExtensionName);
    Params.AddPair('installationKey', InstallationKey);
    if ASessionId <> '' then
      Params.AddPair('sessionId', ASessionId);
    ResultVal := FRpc.SendRequest('permissions.extensionApprovals.revoke', Params);
  finally
    Params.Free;
  end;
  try
    if ResultVal is TJSONObject then
      Result.Revoked := TJSONObject(ResultVal).GetValue<Boolean>('revoked', False);
  finally
    ResultVal.Free;
  end;
end;

function TCopilotClient.GetSessionMetadata(
  const ASessionId: string): TSessionMetadata;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Params := TJSONObject.Create;
  Params.AddPair('sessionId', ASessionId);

  ResultVal := FRpc.SendRequest('session.getMetadata', Params);
  try
    if ResultVal is TJSONObject then
    begin
      var Obj := TJSONObject(ResultVal);
      Result.SessionId := Obj.GetValue<string>('sessionId', '');
      Result.Model := Obj.GetValue<string>('model', '');
      Result.Status := Obj.GetValue<string>('status', '');
      Result.MessageCount := Obj.GetValue<Integer>('messageCount', 0);
    end;
  finally
    ResultVal.Free;
  end;
end;

function TCopilotClient.GetForegroundSessionId: string;
var
  ResultVal: TJSONValue;
begin
  ResultVal := FRpc.SendRequest('session.getForeground', TJSONObject.Create);
  try
    if ResultVal is TJSONObject then
      Result := TJSONObject(ResultVal).GetValue<string>('sessionId', '')
    else
      Result := '';
  finally
    ResultVal.Free;
  end;
end;

procedure TCopilotClient.SetForegroundSessionId(const ASessionId: string);
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('sessionId', ASessionId);
  var R := FRpc.SendRequest('session.setForeground', Params);
  R.Free;
end;

function TCopilotClient.ListSessions: TJSONArray;
begin
  Result := ListSessions(Default(TSessionListFilter));
end;

function TCopilotClient.ListSessions(const Filter: TSessionListFilter): TJSONArray;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Params := TJSONObject.Create;
  if Filter.Status <> '' then
    Params.AddPair('status', Filter.Status);

  ResultVal := FRpc.SendRequest('session.list', Params);
  if ResultVal is TJSONArray then
    Result := TJSONArray(ResultVal)
  else
  begin
    ResultVal.Free;
    Result := TJSONArray.Create;
  end;
end;

procedure TCopilotClient.DeleteSession(const ASessionId: string);
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('sessionId', ASessionId);
  var R := FRpc.SendRequest('session.delete', Params);
  R.Free;

  FSessionsLock.Enter;
  try
    FSessions.Remove(ASessionId);
  finally
    FSessionsLock.Leave;
  end;
end;

function TCopilotClient.GetLastSessionId: string;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Params := TJSONObject.Create;
  ResultVal := FRpc.SendRequest('session.getLastId', Params);
  try
    if (ResultVal is TJSONObject) and TJSONObject(ResultVal).TryGetValue<string>('sessionId', Result) then
      Exit;
    Result := '';
  finally
    ResultVal.Free;
  end;
end;

function TCopilotClient.ListModels: TJSONArray;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Params := TJSONObject.Create;
  ResultVal := FRpc.SendRequest('models.list', Params);
  if ResultVal is TJSONArray then
    Result := TJSONArray(ResultVal)
  else
  begin
    ResultVal.Free;
    Result := TJSONArray.Create;
  end;
end;

function TCopilotClient.GetStatus: TJSONObject;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Params := TJSONObject.Create;
  ResultVal := FRpc.SendRequest('status.get', Params);
  if ResultVal is TJSONObject then
    Result := TJSONObject(ResultVal)
  else
  begin
    ResultVal.Free;
    Result := TJSONObject.Create;
  end;
end;

function TCopilotClient.GetAuthStatus: TJSONObject;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Params := TJSONObject.Create;
  ResultVal := FRpc.SendRequest('auth.getStatus', Params);
  if ResultVal is TJSONObject then
    Result := TJSONObject(ResultVal)
  else
  begin
    ResultVal.Free;
    Result := TJSONObject.Create;
  end;
end;

function TCopilotClient.OnLifecycle(
  Handler: TSessionLifecycleHandler): TUnsubscribeProc;
var
  CapturedHandler: TSessionLifecycleHandler;
begin
  CapturedHandler := Handler;
  FLifecycleLock.Enter;
  try
    FLifecycleHandlers.Add(Handler);
  finally
    FLifecycleLock.Leave;
  end;

  Result := procedure
    begin
      Self.FLifecycleLock.Enter;
      try
        Self.FLifecycleHandlers.Remove(CapturedHandler);
      finally
        Self.FLifecycleLock.Leave;
      end;
    end;
end;

function TCopilotClient.FindSession(const ASessionId: string): TCopilotSession;
begin
  FSessionsLock.Enter;
  try
    if not FSessions.TryGetValue(ASessionId, Result) then
      Result := nil;
  finally
    FSessionsLock.Leave;
  end;
end;

procedure TCopilotClient.HandleSessionEvent(const Method: string;
  const Params: TJSONValue);
var
  Obj: TJSONObject;
  SessionId, EventType: string;
  Session: TCopilotSession;
begin
  if not (Params is TJSONObject) then
    Exit;
  Obj := TJSONObject(Params);

  SessionId := Obj.GetValue<string>('sessionId', '');

  // SDK protocol 4 sends {sessionId, event: {type, data, ...}}; older
  // runtimes put type and data at the top level.
  var EventObj := Obj;
  var EventVal := Obj.GetValue('event');
  if EventVal is TJSONObject then
    EventObj := TJSONObject(EventVal);
  EventType := EventObj.GetValue<string>('type', '');

  Session := FindSession(SessionId);
  if Assigned(Session) then
  begin
    var DataVal := EventObj.GetValue('data');
    var Data: TJSONObject := nil;
    if Assigned(DataVal) and (DataVal is TJSONObject) then
      Data := TJSONObject(DataVal);
    Session.HandleSessionEvent(EventType, SessionId, Data);
  end;
end;

procedure TCopilotClient.HandleSessionLifecycle(const Method: string;
  const Params: TJSONValue);
var
  Obj: TJSONObject;
  Event: TSessionLifecycleEvent;
  Handlers: TArray<TSessionLifecycleHandler>;
begin
  if not (Params is TJSONObject) then
    Exit;
  Obj := TJSONObject(Params);

  Event.EventType := Obj.GetValue<string>('type', '');
  Event.SessionId := Obj.GetValue<string>('sessionId', '');
  Event.Data := Obj;

  FLifecycleLock.Enter;
  try
    Handlers := FLifecycleHandlers.ToArray;
  finally
    FLifecycleLock.Leave;
  end;

  for var Handler in Handlers do
  begin
    try
      Handler(Event);
    except
      // Swallow handler exceptions
    end;
  end;
end;

function TCopilotClient.PermissionDecisionToString(
  D: TPermissionDecision): string;
begin
  case D of
    pdApproved: Result := 'approved';
    pdDeniedByUser: Result := 'denied-interactively-by-user';
    pdDeniedByPolicy: Result := 'denied-by-policy';
  else
    Result := 'denied-interactively-by-user';
  end;
end;

function TCopilotClient.HandleToolCall(const Method: string;
  const Params: TJSONValue): TJSONValue;
var
  Obj: TJSONObject;
  SessionId, ToolName: string;
  Session: TCopilotSession;
  Args: TJSONObject;
  Invocation: TToolInvocation;
  ToolResult: TToolResultObject;
  ResultObj: TJSONObject;
begin
  Obj := Params as TJSONObject;
  SessionId := Obj.GetValue<string>('sessionId', '');
  ToolName := Obj.GetValue<string>('toolName', '');

  var ArgsVal := Obj.GetValue('arguments');
  if Assigned(ArgsVal) and (ArgsVal is TJSONObject) then
    Args := TJSONObject(ArgsVal)
  else
    Args := TJSONObject.Create;

  Invocation.ToolCallId := Obj.GetValue<string>('toolCallId', '');
  Invocation.SessionId := SessionId;

  Session := FindSession(SessionId);
  if Assigned(Session) then
    ToolResult := Session.HandleToolCall(ToolName, Args, Invocation)
  else
    ToolResult := ToolFailure('Session not found: ' + SessionId);

  ResultObj := TJSONObject.Create;
  ResultObj.AddPair('content', ToolResult.Content);
  if ToolResult.IsError then
    ResultObj.AddPair('isError', TJSONBool.Create(True));
  Result := ResultObj;
end;

function TCopilotClient.HandlePermissionRequest(const Method: string;
  const Params: TJSONValue): TJSONValue;
var
  Obj: TJSONObject;
  Request: TPermissionRequest;
  Session: TCopilotSession;
  PermResult: TPermissionResult;
  ResultObj: TJSONObject;
begin
  Obj := Params as TJSONObject;
  Request.Kind := Obj.GetValue<string>('kind', '');
  Request.Resource := Obj.GetValue<string>('resource', '');
  Request.Description := Obj.GetValue<string>('description', '');
  Request.SessionId := Obj.GetValue<string>('sessionId', '');

  Session := FindSession(Request.SessionId);
  if Assigned(Session) then
    PermResult := Session.HandlePermission(Request)
  else
  begin
    PermResult.Decision := pdDeniedByUser;
  end;

  ResultObj := TJSONObject.Create;
  ResultObj.AddPair('decision', PermissionDecisionToString(PermResult.Decision));
  Result := ResultObj;
end;

function TCopilotClient.HandleUserInputRequest(const Method: string;
  const Params: TJSONValue): TJSONValue;
var
  Obj: TJSONObject;
  Request: TUserInputRequest;
  Session: TCopilotSession;
  InputResult: TUserInputResponse;
  ResultObj: TJSONObject;
  WasFreeform: Boolean;
begin
  Obj := Params as TJSONObject;
  Request := Default(TUserInputRequest);
  Request.Question := Obj.GetValue<string>('question', '');
  Request.SessionId := Obj.GetValue<string>('sessionId', '');
  Request.Choices := JsonStrings(Obj.GetValue('choices'));
  Request.AllowFreeform := Obj.GetValue<Boolean>('allowFreeform', True);
  Request.RequestId := Obj.GetValue<string>('requestId', '');
  var MetaVal := Obj.GetValue('_meta');
  if MetaVal is TJSONObject then
    Request.MetaJson := MetaVal.ToJSON;
  // Image support advertised by the runtime (absent when not accepted).
  var ConstraintsVal := Obj.GetValue('imageAttachmentConstraints');
  if ConstraintsVal is TJSONObject then
  begin
    Request.ImageAttachmentConstraints.Present := True;
    Request.ImageAttachmentConstraints.AllowedMimeTypes :=
      JsonStrings(TJSONObject(ConstraintsVal).GetValue('allowedMimeTypes'));
    Request.ImageAttachmentConstraints.MaxBytesPerImage :=
      TJSONObject(ConstraintsVal).GetValue<Int64>('maxBytesPerImage', 0);
    Request.ImageAttachmentConstraints.MaxCount :=
      TJSONObject(ConstraintsVal).GetValue<Integer>('maxCount', 0);
  end;

  Session := FindSession(Request.SessionId);
  if Assigned(Session) then
    InputResult := Session.HandleUserInput(Request)
  else
    InputResult := Default(TUserInputResponse);

  // wasFreeform: the answer is not one of the offered choices (so an
  // image-only answer with an empty Answer is freeform).
  WasFreeform := True;
  for var Choice in Request.Choices do
    if Choice = InputResult.Answer then
      WasFreeform := False;

  ResultObj := TJSONObject.Create;
  ResultObj.AddPair('answer', InputResult.Answer);
  ResultObj.AddPair('wasFreeform', TJSONBool.Create(WasFreeform));
  ResultObj.AddPair('accepted', TJSONBool.Create(InputResult.Accepted));
  // Image attachments are sent only when the handler attached any.
  if Length(InputResult.Attachments) > 0 then
  begin
    var AttachmentsArr := TJSONArray.Create;
    for var Attachment in InputResult.Attachments do
    begin
      var AttachmentObj := TJSONObject.Create;
      AttachmentObj.AddPair('type', 'image');
      AttachmentObj.AddPair('data', Attachment.Data);
      AttachmentObj.AddPair('mimeType', Attachment.MimeType);
      if Attachment.Description <> '' then
        AttachmentObj.AddPair('description', Attachment.Description);
      if Attachment.DisplayName <> '' then
        AttachmentObj.AddPair('displayName', Attachment.DisplayName);
      AttachmentsArr.AddElement(AttachmentObj);
    end;
    ResultObj.AddPair('attachments', AttachmentsArr);
  end;
  Result := ResultObj;
end;

function TCopilotClient.HandleHooksInvoke(const Method: string;
  const Params: TJSONValue): TJSONValue;
var
  Obj, InputObj, Output: TJSONObject;
  HookName, SessionId: string;
  Session: TCopilotSession;
  InputVal: TJSONValue;
begin
  Obj := Params as TJSONObject;
  // SDK protocol 4 sends {sessionId, hookType, input}; older runtimes sent
  // hookName with the input fields at the top level.
  HookName := Obj.GetValue<string>('hookType', Obj.GetValue<string>('hookName', ''));
  SessionId := Obj.GetValue<string>('sessionId', '');
  InputVal := Obj.GetValue('input');
  if InputVal is TJSONObject then
    InputObj := TJSONObject(InputVal)
  else
    InputObj := Obj;

  Result := TJSONObject.Create;
  Session := FindSession(SessionId);
  if not Assigned(Session) then
    Exit;

  Output := nil;
  if (HookName = 'preToolUse') and Assigned(Session.Hooks.OnPreToolUse) then
  begin
    var Input: TPreToolUseHookInput;
    Input.ToolName := InputObj.GetValue<string>('toolName', '');
    var ArgsVal := InputObj.GetValue('toolArgs');
    if Assigned(ArgsVal) and (ArgsVal is TJSONObject) then
      Input.ToolArgs := TJSONObject(ArgsVal)
    else
      Input.ToolArgs := nil;

    var PreOutput := Session.Hooks.OnPreToolUse(Input, SessionId);
    Output := TJSONObject.Create;
    if PreOutput.PermissionDecision <> '' then
      Output.AddPair('permissionDecision', PreOutput.PermissionDecision);
  end
  else if (HookName = 'postToolUse') and Assigned(Session.Hooks.OnPostToolUse) then
  begin
    var Input: TPostToolUseHookInput;
    Input.ToolName := InputObj.GetValue<string>('toolName', '');
    Input.ToolResult := InputObj.GetValue<string>('toolResult', '');

    var PostOutput := Session.Hooks.OnPostToolUse(Input, SessionId);
    Output := TJSONObject.Create;
    if PostOutput.OverrideResult <> '' then
      Output.AddPair('overrideResult', PostOutput.OverrideResult);
  end
  else if (HookName = 'subagentStart') and Assigned(Session.Hooks.OnSubagentStart) then
  begin
    var Context := Session.Hooks.OnSubagentStart(ReadSubagentInput(InputObj), SessionId);
    Output := TJSONObject.Create;
    if Context <> '' then
      Output.AddPair('additionalContext', Context);
  end
  else if (HookName = 'subagentStop') and Assigned(Session.Hooks.OnSubagentStop) then
  begin
    var Decision := Session.Hooks.OnSubagentStop(ReadSubagentInput(InputObj), SessionId);
    Output := TJSONObject.Create;
    if Decision.Block and (Decision.Reason <> '') then
    begin
      Output.AddPair('decision', 'block');
      Output.AddPair('reason', Decision.Reason);
    end;
    if Decision.ModifiedResponse <> '' then
      Output.AddPair('modifiedResponse', Decision.ModifiedResponse);
  end;

  // Hook results travel as {"output": {...}}; no output means no change.
  if Assigned(Output) then
  begin
    if Output.Count > 0 then
      TJSONObject(Result).AddPair('output', Output)
    else
      Output.Free;
  end;
end;

function TCopilotClient.HandleGitHubToken(const Method: string;
  const Params: TJSONValue): TJSONValue;
var
  Obj, ResObj: TJSONObject;
  Request: TGitHubTokenRequest;
  Provider: TGitHubTokenProvider;
  Token: TGitHubTokenResult;
begin
  Obj := Params as TJSONObject;
  Request.RegistrationId := Obj.GetValue<string>('registrationId', '');
  Request.Host := Obj.GetValue<string>('host', '');
  Request.SessionId := Obj.GetValue<string>('sessionId', '');
  Request.Reason := Obj.GetValue<string>('reason', '');

  FTokenLock.Enter;
  try
    if not FTokenProviders.TryGetValue(Request.RegistrationId, Provider) then
      Provider := nil;
  finally
    FTokenLock.Leave;
  end;
  if not Assigned(Provider) then
    raise Exception.Create('No GitHub token provider is registered for ' +
      Request.RegistrationId);

  Token := Provider(Request);
  ResObj := TJSONObject.Create;
  if Token.Cancelled or (Token.AccessToken = '') then
    ResObj.AddPair('kind', 'cancelled')
  else
  begin
    ResObj.AddPair('kind', 'token');
    ResObj.AddPair('accessToken', Token.AccessToken);
    if Token.TokenType <> '' then
      ResObj.AddPair('tokenType', Token.TokenType);
    if Token.ExpiresIn > 0 then
      ResObj.AddPair('expiresIn', TJSONNumber.Create(Token.ExpiresIn));
  end;
  Result := ResObj;
end;

function TCopilotClient.HandleShellAuthentication(const Method: string;
  const Params: TJSONValue): TJSONValue;
var
  Obj, ResObj: TJSONObject;
  Request: TShellAuthenticationRequest;
  Reply: TShellAuthenticationResponse;
  Session: TCopilotSession;
begin
  Obj := Params as TJSONObject;
  Request.SessionId := Obj.GetValue<string>('sessionId', '');
  Request.RequestId := Obj.GetValue<string>('requestId', '');
  Request.ProgramName := Obj.GetValue<string>('program', '');
  Request.Prompt := Obj.GetValue<string>('prompt', '');
  Request.InputKind := Obj.GetValue<string>('inputKind', '');
  Request.Command := Obj.GetValue<string>('command', '');
  Request.Cwd := Obj.GetValue<string>('cwd', '');

  ResObj := TJSONObject.Create;
  Session := FindSession(Request.SessionId);
  if Assigned(Session) and Session.HandleShellAuthentication(Request, Reply) and
    Reply.Accepted then
  begin
    ResObj.AddPair('action', 'accept');
    ResObj.AddPair('value', Reply.Value);
  end
  else
    ResObj.AddPair('action', 'cancel');
  Result := ResObj;
end;

function TCopilotClient.HandleMemoryStore(const Method: string;
  const Params: TJSONValue): TJSONValue;
var
  Obj: TJSONObject;
  Memory: TStoredMemory;
  Session: TCopilotSession;
  CitationsVal: TJSONValue;
begin
  Obj := Params as TJSONObject;
  Memory.SessionId := Obj.GetValue<string>('sessionId', '');
  Memory.Subject := Obj.GetValue<string>('subject', '');
  Memory.Fact := Obj.GetValue<string>('fact', '');
  Memory.Reason := Obj.GetValue<string>('reason', '');
  CitationsVal := Obj.GetValue('citations');
  if Assigned(CitationsVal) then
    Memory.CitationsJson := CitationsVal.ToJSON
  else
    Memory.CitationsJson := '';

  Session := FindSession(Memory.SessionId);
  if not (Assigned(Session) and Session.HandleStoreMemory(Memory)) then
    raise Exception.Create('Memory store owner is unavailable');
  Result := TJSONNull.Create;
end;

function TCopilotClient.HandleNetwork(const Method: string;
  const Params: TJSONValue): TJSONValue;
var
  Obj, ResObj: TJSONObject;
  Answer: string;
  Resolver: TProxyResolver;
  Negotiator: TNegotiateAuthenticator;
  BasicAuth: TBasicAuthenticator;
  Credentials: TBasicCredentials;
begin
  Obj := Params as TJSONObject;
  ResObj := TJSONObject.Create;
  Resolver := FOptions.Network.ResolveProxy;
  Negotiator := FOptions.Network.AuthenticateNegotiate;
  BasicAuth := FOptions.Network.AuthenticateBasic;

  // An empty reply declines; the runtime falls back to its defaults.
  // Tokens and passwords are never logged.
  if (Method = 'network.resolveProxy') and Assigned(Resolver) then
  begin
    Answer := Resolver(Obj.GetValue<string>('url', ''));
    if Answer <> '' then
      ResObj.AddPair('proxy', Answer);
  end
  else if (Method = 'network.authenticateNegotiate') and Assigned(Negotiator) then
  begin
    Answer := Negotiator(Obj.GetValue<string>('spn', ''));
    if Answer <> '' then
      ResObj.AddPair('token', Answer);
  end
  else if (Method = 'network.authenticateBasic') and Assigned(BasicAuth) then
  begin
    Credentials := BasicAuth(Obj.GetValue<string>('proxy', ''),
      Obj.GetValue<string>('realm', ''),
      Obj.GetValue<Boolean>('previousRejected', False));
    if Credentials.Username <> '' then
    begin
      ResObj.AddPair('username', Credentials.Username);
      ResObj.AddPair('password', Credentials.Password);
    end;
  end;
  Result := ResObj;
end;

end.
