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
  Copilot.SdkProtocolVersion;

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
    function HandleExitPlanModeRequest(const Method: string;
      const Params: TJSONValue): TJSONValue;
    function HandleHooksInvoke(const Method: string;
      const Params: TJSONValue): TJSONValue;
    function FindSession(const ASessionId: string): TCopilotSession;
    function BuildToolsArray(const ATools: TArray<TTool>): TJSONArray;
    function GetCliArgs: TArray<string>;
    function GetTraceContextParams: TJSONObject;
    function PermissionDecisionToString(D: TPermissionDecision): string;
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

    // Get connection state
    property State: TConnectionState read FState;
    property Options: TCopilotClientOptions read FOptions;
  end;

implementation

uses
  System.IOUtils;

const
  MIN_PROTOCOL_VERSION = 3;

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
end;

destructor TCopilotClient.Destroy;
begin
  Stop;
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
  if FOptions.Remote then
    Result := Result + TArray<string>.Create('--remote');
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
  CmdLine := CliPath;
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

  FRpc.OnRequest('exitPlanMode.request',
    function(const Method: string; const Params: TJSONValue): TJSONValue
    begin
      Result := HandleExitPlanModeRequest(Method, Params);
    end);
end;

procedure TCopilotClient.Start;
begin
  if FState = csConnected then
    Exit;

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

    // Verify protocol version
    var PingResult := Ping;
    var ServerVersion := StrToIntDef(PingResult.ProtocolVersion, 0);
    if ServerVersion < MIN_PROTOCOL_VERSION then
      raise Exception.CreateFmt(
        'Protocol version mismatch: server=%s, minimum=%d',
        [PingResult.ProtocolVersion, MIN_PROTOCOL_VERSION]);

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
    if Assigned(Tool.Parameters) then
      ToolObj.AddPair('parameters', Tool.Parameters.Clone as TJSONObject);
    Result.AddElement(ToolObj);
  end;
end;

function TCopilotClient.CreateSession: TCopilotSession;
begin
  Result := CreateSession(Default(TSessionConfig));
end;

function TCopilotClient.CreateSession(const Config: TSessionConfig): TCopilotSession;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
  SessionId: string;
  Session: TCopilotSession;
begin
  Params := TJSONObject.Create;

  if Config.Model <> '' then
    Params.AddPair('model', Config.Model);
  Params.AddPair('streaming', TJSONBool.Create(Config.Streaming));
  Params.AddPair('sdkProtocolVersion', SDK_PROTOCOL_VERSION);

  if Length(Config.Tools) > 0 then
    Params.AddPair('tools', BuildToolsArray(Config.Tools));

  if Length(Config.SkillDirectories) > 0 then
  begin
    var SkillArr := TJSONArray.Create;
    for var Dir in Config.SkillDirectories do
      SkillArr.Add(Dir);
    Params.AddPair('skillDirectories', SkillArr);
  end;

  if Length(Config.DisabledSkills) > 0 then
  begin
    var DisArr := TJSONArray.Create;
    for var S in Config.DisabledSkills do
      DisArr.Add(S);
    Params.AddPair('disabledSkills', DisArr);
  end;

  if Config.IncludeSubAgentStreamingEvents then
    Params.AddPair('includeSubAgentStreamingEvents', TJSONBool.Create(True));

  if Length(Config.Instructions) > 0 then
  begin
    var InstArr := TJSONArray.Create;
    for var Inst in Config.Instructions do
      InstArr.Add(Inst);
    Params.AddPair('instructions', InstArr);
  end;

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

  if Assigned(Config.OnExitPlanMode) then
    Params.AddPair('requestExitPlanMode', TJSONBool.Create(True));

  if Config.EnableSessionTelemetry then
    Params.AddPair('enableSessionTelemetry', TJSONBool.Create(True));

  // Merge trace context
  var TraceCtx := GetTraceContextParams;
  if TraceCtx.Count > 0 then
  begin
    for var Pair in TraceCtx do
      Params.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone);
  end;
  TraceCtx.Free;

  ResultVal := FRpc.SendRequest('session.create', Params);
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
  Session.OnExitPlanMode := Config.OnExitPlanMode;
  Session.Hooks := Config.Hooks;

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
  Params: TJSONObject;
  ResultVal: TJSONValue;
  Session: TCopilotSession;
begin
  Params := TJSONObject.Create;
  Params.AddPair('sessionId', Config.SessionId);
  Params.AddPair('sdkProtocolVersion', SDK_PROTOCOL_VERSION);

  if Config.Model <> '' then
    Params.AddPair('model', Config.Model);
  Params.AddPair('streaming', TJSONBool.Create(Config.Streaming));

  if Length(Config.Tools) > 0 then
    Params.AddPair('tools', BuildToolsArray(Config.Tools));

  if Assigned(Config.OnExitPlanMode) then
    Params.AddPair('requestExitPlanMode', TJSONBool.Create(True));

  if Config.EnableSessionTelemetry then
    Params.AddPair('enableSessionTelemetry', TJSONBool.Create(True));

  // Merge trace context
  var ResumeTraceCtx := GetTraceContextParams;
  if ResumeTraceCtx.Count > 0 then
  begin
    for var Pair in ResumeTraceCtx do
      Params.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone);
  end;
  ResumeTraceCtx.Free;

  ResultVal := FRpc.SendRequest('session.resume', Params);
  ResultVal.Free;

  Session := TCopilotSession.Create(Config.SessionId, FRpc, Config.Streaming);
  Session.Tools := Config.Tools;
  Session.OnPermissionRequest := Config.OnPermissionRequest;
  Session.OnUserInputRequest := Config.OnUserInputRequest;
  Session.OnExitPlanMode := Config.OnExitPlanMode;
  Session.Hooks := Config.Hooks;

  FSessionsLock.Enter;
  try
    FSessions.AddOrSetValue(Config.SessionId, Session);
  finally
    FSessionsLock.Leave;
  end;

  Result := Session;
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
  EventType := Obj.GetValue<string>('type', '');

  Session := FindSession(SessionId);
  if Assigned(Session) then
  begin
    var DataVal := Obj.GetValue('data');
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
begin
  Obj := Params as TJSONObject;
  Request.Question := Obj.GetValue<string>('question', '');
  Request.SessionId := Obj.GetValue<string>('sessionId', '');

  Session := FindSession(Request.SessionId);
  if Assigned(Session) then
    InputResult := Session.HandleUserInput(Request)
  else
  begin
    InputResult.Answer := '';
    InputResult.Accepted := False;
  end;

  ResultObj := TJSONObject.Create;
  ResultObj.AddPair('answer', InputResult.Answer);
  ResultObj.AddPair('accepted', TJSONBool.Create(InputResult.Accepted));
  Result := ResultObj;
end;

function TCopilotClient.HandleHooksInvoke(const Method: string;
  const Params: TJSONValue): TJSONValue;
var
  Obj: TJSONObject;
  HookName, SessionId: string;
  Session: TCopilotSession;
begin
  Obj := Params as TJSONObject;
  HookName := Obj.GetValue<string>('hookName', '');
  SessionId := Obj.GetValue<string>('sessionId', '');

  Session := FindSession(SessionId);
  if not Assigned(Session) then
  begin
    Result := TJSONObject.Create;
    Exit;
  end;

  if (HookName = 'preToolUse') and Assigned(Session.Hooks.OnPreToolUse) then
  begin
    var Input: TPreToolUseHookInput;
    Input.ToolName := Obj.GetValue<string>('toolName', '');
    var ArgsVal := Obj.GetValue('toolArgs');
    if Assigned(ArgsVal) and (ArgsVal is TJSONObject) then
      Input.ToolArgs := TJSONObject(ArgsVal)
    else
      Input.ToolArgs := nil;

    var Output := Session.Hooks.OnPreToolUse(Input, SessionId);
    var ResObj := TJSONObject.Create;
    ResObj.AddPair('permissionDecision', Output.PermissionDecision);
    Result := ResObj;
  end
  else if (HookName = 'postToolUse') and Assigned(Session.Hooks.OnPostToolUse) then
  begin
    var Input: TPostToolUseHookInput;
    Input.ToolName := Obj.GetValue<string>('toolName', '');
    Input.ToolResult := Obj.GetValue<string>('toolResult', '');

    var Output := Session.Hooks.OnPostToolUse(Input, SessionId);
    var ResObj := TJSONObject.Create;
    if Output.OverrideResult <> '' then
      ResObj.AddPair('overrideResult', Output.OverrideResult);
    Result := ResObj;
  end
  else
    Result := TJSONObject.Create;
end;

function TCopilotClient.GetTraceContextParams: TJSONObject;
var
  Ctx: TTraceContext;
begin
  Result := TJSONObject.Create;
  if Assigned(FOptions.OnGetTraceContext) then
  begin
    try
      Ctx := FOptions.OnGetTraceContext();
      if Ctx.Traceparent <> '' then
        Result.AddPair('traceparent', Ctx.Traceparent);
      if Ctx.Tracestate <> '' then
        Result.AddPair('tracestate', Ctx.Tracestate);
    except
      // Swallow trace context errors
    end;
  end;
end;

function TCopilotClient.HandleExitPlanModeRequest(const Method: string;
  const Params: TJSONValue): TJSONValue;
var
  Obj: TJSONObject;
  Request: TExitPlanModeRequest;
  Session: TCopilotSession;
  PlanResult: TExitPlanModeResult;
  ResultObj: TJSONObject;
  ActionsArr: TJSONArray;
  I: Integer;
begin
  Obj := Params as TJSONObject;
  Request.Summary := Obj.GetValue<string>('summary', '');
  Request.PlanContent := Obj.GetValue<string>('planContent', '');
  Request.RecommendedAction := Obj.GetValue<string>('recommendedAction', '');
  Request.SessionId := Obj.GetValue<string>('sessionId', '');

  var ActionsVal := Obj.GetValue('actions');
  if Assigned(ActionsVal) and (ActionsVal is TJSONArray) then
  begin
    ActionsArr := TJSONArray(ActionsVal);
    SetLength(Request.Actions, ActionsArr.Count);
    for I := 0 to ActionsArr.Count - 1 do
      Request.Actions[I] := ActionsArr.Items[I].Value;
  end;

  Session := FindSession(Request.SessionId);
  if Assigned(Session) then
    PlanResult := Session.HandleExitPlanMode(Request)
  else
  begin
    PlanResult.Approved := True;
    PlanResult.SelectedAction := '';
    PlanResult.Feedback := '';
  end;

  ResultObj := TJSONObject.Create;
  ResultObj.AddPair('approved', TJSONBool.Create(PlanResult.Approved));
  if PlanResult.SelectedAction <> '' then
    ResultObj.AddPair('selectedAction', PlanResult.SelectedAction);
  if PlanResult.Feedback <> '' then
    ResultObj.AddPair('feedback', PlanResult.Feedback);
  Result := ResultObj;
end;

end.
