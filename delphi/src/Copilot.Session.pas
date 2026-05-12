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
    procedure DispatchEvent(const Event: TSessionEvent);
    function GetNextHandlerId: Integer;
  public
    constructor Create(const ASessionId: string; ARpc: TJsonRpcClient;
      AStreaming: Boolean);
    destructor Destroy; override;

    // Send a message to the session (non-blocking)
    procedure Send(const Options: TMessageOptions);

    // Send a message and wait for the final response
    function SendAndWait(const Options: TMessageOptions;
      TimeoutMs: Cardinal = 60000): TJSONValue;

    // Abort current processing
    procedure Abort;

    // Get message history
    function GetMessages: TJSONArray;

    // Disconnect (destroy session on server)
    procedure Disconnect;

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

procedure TCopilotSession.Send(const Options: TMessageOptions);
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('sessionId', FSessionId);
  Params.AddPair('message', Options.Prompt);
  if Options.Model <> '' then
    Params.AddPair('model', Options.Model);

  FRpc.SendNotification('session.send', Params);
end;

function TCopilotSession.SendAndWait(const Options: TMessageOptions;
  TimeoutMs: Cardinal): TJSONValue;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('sessionId', FSessionId);
  Params.AddPair('message', Options.Prompt);
  if Options.Model <> '' then
    Params.AddPair('model', Options.Model);

  Result := FRpc.SendRequest('session.send', Params, TimeoutMs);
end;

procedure TCopilotSession.Abort;
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('sessionId', FSessionId);
  FRpc.SendNotification('session.abort', Params);
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
var
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('sessionId', FSessionId);
  FRpc.SendRequest('session.destroy', Params);
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
  begin
    Result.Answer := '';
    Result.Accepted := False;
  end;
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
