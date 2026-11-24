{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

unit Copilot.Session;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.SyncObjs,
  System.Generics.Collections, System.Threading,
  Copilot.Types, Copilot.JsonRpc;

type
  TCopilotSession = class
  private
    FSessionId: string;
    FRpc: TJsonRpcClient;
    FEventHandlers: TDictionary<Integer, TSessionEventHandler>;
    FTypedHandlers: TDictionary<string, TList<TSessionEventHandler>>;
    FHandlerLock: TCriticalSection;
    FNextHandlerId: Integer;
    FStreaming: Boolean;
    FModel: string;
    FTools: TArray<TTool>;
    FOnPermissionRequest: TPermissionHandler;
    FOnUserInputRequest: TUserInputHandler;
    FOnExitPlanMode: TExitPlanModeHandler;
    FHooks: TSessionHooks;
    FOnShellAuthenticationRequest: TShellAuthenticationHandler;
    FStoreMemory: TMemoryStoreHandler;
    FOnDetach: TProc;
    procedure DispatchEvent(const Event: TSessionEvent);
    function GetNextHandlerId: Integer;
    function BuildSendParams(const Options: TMessageOptions): TJSONObject;
  public
    constructor Create(const ASessionId: string; ARpc: TJsonRpcClient;
      AStreaming: Boolean);
    destructor Destroy; override;

    // Send a message to the session (session.send; returns once admitted)
    procedure Send(const Options: TMessageOptions);

    // Send a message and wait for the final response
    function SendAndWait(const Options: TMessageOptions;
      TimeoutMs: Cardinal = 60000): TJSONValue;

    // Abort current processing (session.abort). Failures are raised.
    procedure Abort;

    // Get message history
    function GetMessages: TJSONArray;

    // Disconnect (detach from the session on the server; errors are raised)
    procedure Disconnect;

    // Detach this connection from the session (session.detach). The session
    // stays on disk and can be resumed later.
    procedure Detach;

    // Request an Auto routing tier for future turns that use the "auto"
    // model (session.model.switchAutoTier): 'fast', 'efficiency', 'balance'
    // or 'intelligence' (see the AutoTier* constants).
    procedure SwitchAutoTier(const Tier: string);

    // Re-run the enterprise Policy Helper
    // (session.managedSettings.recheckPolicyHelper). Caller frees the result.
    function RecheckPolicyHelper: TJSONValue;

    // Change the permission mode (session.permissions.setMode). Reason is
    // 'managed_policy' when enterprise policy refused the change.
    function SetPermissionMode(const Mode: string): TPermissionModeResult;

    // Decode the data object of a tool.shell_output event.
    class function ParseShellOutput(Data: TJSONObject): TShellOutputEventData; static;

    // Subscribe to all events; returns an unsubscribe procedure
    function On_(Handler: TSessionEventHandler): TUnsubscribeProc;

    // Subscribe to a specific event type; returns an unsubscribe procedure
    function OnEvent(const EventType: string;
      Handler: TSessionEventHandler): TUnsubscribeProc;

    // Remove a handler by its id
    procedure Off(HandlerId: Integer);

    // Handle an incoming session event from the RPC layer
    procedure HandleSessionEvent(const EventType, ASessionId: string;
      Data: TJSONObject);

    // Handle a tool call request from the server
    function HandleToolCall(const ToolName: string; Args: TJSONObject;
      const Invocation: TToolInvocation): TToolResultObject;

    // Handle a permission request from the server
    function HandlePermission(const Request: TPermissionRequest): TPermissionResult;

    // Handle a user input request from the server
    function HandleUserInput(const Request: TUserInputRequest): TUserInputResponse;

    // Handle an exit plan mode request from the server
    function HandleExitPlanMode(const Request: TExitPlanModeRequest): TExitPlanModeResult;

    // Handle a shell authentication prompt; False when no handler is set
    function HandleShellAuthentication(const Request: TShellAuthenticationRequest;
      out Response: TShellAuthenticationResponse): Boolean;

    // Handle a memory store request; False when no handler is set
    function HandleStoreMemory(const Memory: TStoredMemory): Boolean;

    property SessionId: string read FSessionId;
    property Streaming: Boolean read FStreaming write FStreaming;
    property Model: string read FModel write FModel;
    property Tools: TArray<TTool> read FTools write FTools;
    property OnPermissionRequest: TPermissionHandler
      read FOnPermissionRequest write FOnPermissionRequest;
    property OnUserInputRequest: TUserInputHandler
      read FOnUserInputRequest write FOnUserInputRequest;
    property OnExitPlanMode: TExitPlanModeHandler
      read FOnExitPlanMode write FOnExitPlanMode;
    property Hooks: TSessionHooks read FHooks write FHooks;
    property OnShellAuthenticationRequest: TShellAuthenticationHandler
      read FOnShellAuthenticationRequest write FOnShellAuthenticationRequest;
    property StoreMemory: TMemoryStoreHandler
      read FStoreMemory write FStoreMemory;
    // Called once when the session is detached (releases client registrations)
    property OnDetach: TProc read FOnDetach write FOnDetach;
  end;

implementation

{ TCopilotSession }

constructor TCopilotSession.Create(const ASessionId: string;
  ARpc: TJsonRpcClient; AStreaming: Boolean);
begin
  inherited Create;
  FSessionId := ASessionId;
  FRpc := ARpc;
  FStreaming := AStreaming;
  FNextHandlerId := 1;
  FEventHandlers := TDictionary<Integer, TSessionEventHandler>.Create;
  FTypedHandlers := TDictionary<string, TList<TSessionEventHandler>>.Create;
  FHandlerLock := TCriticalSection.Create;
end;

destructor TCopilotSession.Destroy;
begin
  FHandlerLock.Free;
  for var List in FTypedHandlers.Values do
    List.Free;
  FTypedHandlers.Free;
  FEventHandlers.Free;
  inherited;
end;

function TCopilotSession.GetNextHandlerId: Integer;
begin
  FHandlerLock.Enter;
  try
    Result := FNextHandlerId;
    Inc(FNextHandlerId);
  finally
    FHandlerLock.Leave;
  end;
end;

function TCopilotSession.BuildSendParams(const Options: TMessageOptions): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('sessionId', FSessionId);
  // SDK protocol 4 requires the message text as "prompt".
  Result.AddPair('prompt', Options.Prompt);
  if Options.Model <> '' then
    Result.AddPair('model', Options.Model);
  if Options.AgentMode <> '' then
    Result.AddPair('agentMode', Options.AgentMode);
  if Options.DisplayPrompt <> '' then
    Result.AddPair('displayPrompt', Options.DisplayPrompt);
  if Assigned(Options.RequestHeaders) and (Options.RequestHeaders.Count > 0) then
  begin
    var HeadersObj := TJSONObject.Create;
    for var H in Options.RequestHeaders do
      HeadersObj.AddPair(H.Key, H.Value);
    Result.AddPair('requestHeaders', HeadersObj);
  end;
  // Message source (MessageSource): 'user', 'system' or 'agent-<id>'.
  if Options.Source <> '' then
    Result.AddPair('source', Options.Source);
  if Options.ClientTurnId <> '' then
    Result.AddPair('clientTurnId', Options.ClientTurnId);
  // Structured output (responseSchema) => responseFormat json_schema.
  if Assigned(Options.ResponseSchema) then
  begin
    var JsonSchema := TJSONObject.Create;
    JsonSchema.AddPair('name', 'response');
    JsonSchema.AddPair('strict', TJSONBool.Create(True));
    JsonSchema.AddPair('schema', Options.ResponseSchema.Clone as TJSONObject);
    var ResponseFormatObj := TJSONObject.Create;
    ResponseFormatObj.AddPair('type', 'json_schema');
    ResponseFormatObj.AddPair('jsonSchema', JsonSchema);
    Result.AddPair('responseFormat', ResponseFormatObj);
  end;
end;

procedure TCopilotSession.Send(const Options: TMessageOptions);
var
  Params: TJSONObject;
begin
  Params := BuildSendParams(Options);
  try
    FRpc.SendRequest('session.send', Params).Free;
  finally
    Params.Free;
  end;
end;

function TCopilotSession.SendAndWait(const Options: TMessageOptions;
  TimeoutMs: Cardinal): TJSONValue;
var
  Params: TJSONObject;
begin
  Params := BuildSendParams(Options);
  try
    Result := FRpc.SendRequest('session.send', Params, TimeoutMs);
  finally
    Params.Free;
  end;
end;

procedure TCopilotSession.Abort;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  try
    Params.AddPair('sessionId', FSessionId);
    // SDK protocol 4 reports failures as JSON-RPC errors (raised here).
    FRpc.SendRequest('session.abort', Params).Free;
  finally
    Params.Free;
  end;
end;

function TCopilotSession.GetMessages: TJSONArray;
var
  Params, Response: TJSONObject;
  ResultVal: TJSONValue;
begin
  Params := TJSONObject.Create;
  Params.AddPair('sessionId', FSessionId);

  ResultVal := FRpc.SendRequest('session.getMessages', Params);
  if Assigned(ResultVal) and (ResultVal is TJSONArray) then
    Result := TJSONArray(ResultVal)
  else
    Result := TJSONArray.Create;
end;

procedure TCopilotSession.Disconnect;
begin
  Detach;
end;

procedure TCopilotSession.Detach;
var
  Params: TJSONObject;
  Callback: TProc;
begin
  Params := TJSONObject.Create;
  try
    Params.AddPair('sessionId', FSessionId);
    try
      FRpc.SendRequest('session.detach', Params).Free;
    finally
      // Release the client registrations owned by this session once.
      Callback := FOnDetach;
      FOnDetach := nil;
      if Assigned(Callback) then
        Callback();
    end;
  finally
    Params.Free;
  end;
end;

procedure TCopilotSession.SwitchAutoTier(const Tier: string);
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  try
    Params.AddPair('sessionId', FSessionId);
    Params.AddPair('autoTier', Tier);
    FRpc.SendRequest('session.model.switchAutoTier', Params).Free;
  finally
    Params.Free;
  end;
end;

function TCopilotSession.RecheckPolicyHelper: TJSONValue;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  try
    Params.AddPair('sessionId', FSessionId);
    Result := FRpc.SendRequest('session.managedSettings.recheckPolicyHelper', Params);
  finally
    Params.Free;
  end;
end;

function TCopilotSession.SetPermissionMode(const Mode: string): TPermissionModeResult;
var
  Params: TJSONObject;
  ResultVal: TJSONValue;
begin
  Result := Default(TPermissionModeResult);
  Params := TJSONObject.Create;
  try
    Params.AddPair('sessionId', FSessionId);
    Params.AddPair('mode', Mode);
    ResultVal := FRpc.SendRequest('session.permissions.setMode', Params);
  finally
    Params.Free;
  end;
  try
    if ResultVal is TJSONObject then
    begin
      Result.Success := TJSONObject(ResultVal).GetValue<Boolean>('success', False);
      Result.Mode := TJSONObject(ResultVal).GetValue<string>('mode', '');
      Result.Reason := TJSONObject(ResultVal).GetValue<string>('reason', '');
    end;
  finally
    ResultVal.Free;
  end;
end;

class function TCopilotSession.ParseShellOutput(Data: TJSONObject): TShellOutputEventData;
begin
  Result := Default(TShellOutputEventData);
  if not Assigned(Data) then
    Exit;
  Result.ToolCallId := Data.GetValue<string>('toolCallId', '');
  Result.Text := Data.GetValue<string>('text', '');
  Result.Sequence := Data.GetValue<Integer>('sequence', 0);
  Result.Stream := Data.GetValue<string>('stream', '');
end;

function TCopilotSession.HandleShellAuthentication(
  const Request: TShellAuthenticationRequest;
  out Response: TShellAuthenticationResponse): Boolean;
begin
  Response := Default(TShellAuthenticationResponse);
  Result := Assigned(FOnShellAuthenticationRequest);
  if Result then
    Response := FOnShellAuthenticationRequest(Request);
end;

function TCopilotSession.HandleStoreMemory(const Memory: TStoredMemory): Boolean;
begin
  Result := Assigned(FStoreMemory);
  if Result then
    FStoreMemory(Memory);
end;

function TCopilotSession.On_(Handler: TSessionEventHandler): TUnsubscribeProc;
var
  Id: Integer;
begin
  Id := GetNextHandlerId;
  FHandlerLock.Enter;
  try
    FEventHandlers.Add(Id, Handler);
  finally
    FHandlerLock.Leave;
  end;

  Result := procedure
    begin
      Self.Off(Id);
    end;
end;

function TCopilotSession.OnEvent(const EventType: string;
  Handler: TSessionEventHandler): TUnsubscribeProc;
var
  List: TList<TSessionEventHandler>;
  CapturedHandler: TSessionEventHandler;
begin
  CapturedHandler := Handler;
  FHandlerLock.Enter;
  try
    if not FTypedHandlers.TryGetValue(EventType, List) then
    begin
      List := TList<TSessionEventHandler>.Create;
      FTypedHandlers.Add(EventType, List);
    end;
    List.Add(Handler);
  finally
    FHandlerLock.Leave;
  end;

  Result := procedure
    begin
      Self.FHandlerLock.Enter;
      try
        var L: TList<TSessionEventHandler>;
        if Self.FTypedHandlers.TryGetValue(EventType, L) then
          L.Remove(CapturedHandler);
      finally
        Self.FHandlerLock.Leave;
      end;
    end;
end;

procedure TCopilotSession.Off(HandlerId: Integer);
begin
  FHandlerLock.Enter;
  try
    FEventHandlers.Remove(HandlerId);
  finally
    FHandlerLock.Leave;
  end;
end;

procedure TCopilotSession.HandleSessionEvent(const EventType, ASessionId: string;
  Data: TJSONObject);
var
  Event: TSessionEvent;
begin
  if ASessionId <> FSessionId then
    Exit;

  Event.EventType := EventType;
  Event.SessionId := ASessionId;
  Event.Data := Data;
  DispatchEvent(Event);
end;

procedure TCopilotSession.DispatchEvent(const Event: TSessionEvent);
var
  Handlers: TArray<TSessionEventHandler>;
  List: TList<TSessionEventHandler>;
begin
  FHandlerLock.Enter;
  try
    // Collect wildcard handlers
    Handlers := FEventHandlers.Values.ToArray;

    // Collect typed handlers
    if FTypedHandlers.TryGetValue(Event.EventType, List) then
      for var H in List do
      begin
        SetLength(Handlers, Length(Handlers) + 1);
        Handlers[High(Handlers)] := H;
      end;
  finally
    FHandlerLock.Leave;
  end;

  for var Handler in Handlers do
  begin
    try
      Handler(Event);
    except
      // Swallow handler exceptions to avoid crashing the reader
    end;
  end;
end;

function TCopilotSession.HandleToolCall(const ToolName: string;
  Args: TJSONObject; const Invocation: TToolInvocation): TToolResultObject;
begin
  for var Tool in FTools do
  begin
    if Tool.Name = ToolName then
    begin
      if Assigned(Tool.Handler) then
        Exit(Tool.Handler(Args, Invocation));
    end;
  end;
  Result := ToolFailure('Tool not found: ' + ToolName);
end;

function TCopilotSession.HandlePermission(
  const Request: TPermissionRequest): TPermissionResult;
begin
  if Assigned(FOnPermissionRequest) then
    Result := FOnPermissionRequest(Request)
  else
  begin
    Result.Decision := pdDeniedByUser;
  end;
end;

function TCopilotSession.HandleUserInput(
  const Request: TUserInputRequest): TUserInputResponse;
begin
  if Assigned(FOnUserInputRequest) then
    Result := FOnUserInputRequest(Request)
  else
    Result := Default(TUserInputResponse);
end;

function TCopilotSession.HandleExitPlanMode(
  const Request: TExitPlanModeRequest): TExitPlanModeResult;
begin
  if Assigned(FOnExitPlanMode) then
    Result := FOnExitPlanMode(Request)
  else
  begin
    Result.Approved := True;
    Result.SelectedAction := '';
    Result.Feedback := '';
  end;
end;

end.
