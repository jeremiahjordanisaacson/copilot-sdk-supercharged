{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

unit Copilot.JsonRpc;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.SyncObjs,
  System.Generics.Collections, System.Threading;

type
  TNotificationHandler = reference to procedure(const Method: string; const Params: TJSONValue);
  TRequestHandler = reference to function(const Method: string;
    const Params: TJSONValue): TJSONValue;

  TPendingRequest = class
  private
    FEvent: TEvent;
    FResult: TJSONValue;
    FError: TJSONValue;
    FOwnsResult: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Resolve(AResult: TJSONValue);
    procedure Reject(AError: TJSONValue);
    function WaitFor(TimeoutMs: Cardinal): Boolean;
    property Result: TJSONValue read FResult;
    property Error: TJSONValue read FError;
  end;

  TJsonRpcClient = class
  private
    FInputStream: TStream;
    FOutputStream: TStream;
    FNextId: Integer;
    FIdLock: TCriticalSection;
    FPendingLock: TCriticalSection;
    FWriteLock: TCriticalSection;
    FPending: TObjectDictionary<Integer, TPendingRequest>;
    FNotificationHandlers: TDictionary<string, TNotificationHandler>;
    FRequestHandlers: TDictionary<string, TRequestHandler>;
    FReaderTask: ITask;
    FRunning: Boolean;
    procedure ReaderLoop;
    procedure HandleMessage(const Msg: TJSONObject);
    procedure HandleResponse(const Msg: TJSONObject);
    procedure HandleNotification(const Method: string; const Params: TJSONValue);
    procedure HandleRequest(const Id: TJSONValue; const Method: string;
      const Params: TJSONValue);
    function ReadMessage: TJSONObject;
    procedure WriteMessage(const Msg: TJSONObject);
    function GetNextId: Integer;
  public
    constructor Create(AInputStream, AOutputStream: TStream);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function SendRequest(const Method: string; const Params: TJSONValue;
      TimeoutMs: Cardinal = 30000): TJSONValue;
    procedure SendNotification(const Method: string; const Params: TJSONValue);
    procedure OnNotification(const Method: string; Handler: TNotificationHandler);
    procedure OnRequest(const Method: string; Handler: TRequestHandler);
  end;

implementation

uses
  System.Math;

{ TPendingRequest }

constructor TPendingRequest.Create;
begin
  inherited Create;
  FEvent := TEvent.Create(nil, True, False, '');
  FResult := nil;
  FError := nil;
  FOwnsResult := False;
end;

destructor TPendingRequest.Destroy;
begin
  FEvent.Free;
  if FOwnsResult and Assigned(FResult) then
    FResult.Free;
  if Assigned(FError) then
    FError.Free;
  inherited;
end;

procedure TPendingRequest.Resolve(AResult: TJSONValue);
begin
  FResult := AResult;
  FOwnsResult := True;
  FEvent.SetEvent;
end;

procedure TPendingRequest.Reject(AError: TJSONValue);
begin
  FError := AError;
  FEvent.SetEvent;
end;

function TPendingRequest.WaitFor(TimeoutMs: Cardinal): Boolean;
begin
  Result := FEvent.WaitFor(TimeoutMs) = wrSignaled;
end;

{ TJsonRpcClient }

constructor TJsonRpcClient.Create(AInputStream, AOutputStream: TStream);
begin
  inherited Create;
  FInputStream := AInputStream;
  FOutputStream := AOutputStream;
  FNextId := 1;
  FRunning := False;
  FIdLock := TCriticalSection.Create;
  FPendingLock := TCriticalSection.Create;
  FWriteLock := TCriticalSection.Create;
  FPending := TObjectDictionary<Integer, TPendingRequest>.Create([doOwnsValues]);
  FNotificationHandlers := TDictionary<string, TNotificationHandler>.Create;
  FRequestHandlers := TDictionary<string, TRequestHandler>.Create;
end;

destructor TJsonRpcClient.Destroy;
begin
  Stop;
  FRequestHandlers.Free;
  FNotificationHandlers.Free;
  FPending.Free;
  FWriteLock.Free;
  FPendingLock.Free;
  FIdLock.Free;
  inherited;
end;

function TJsonRpcClient.GetNextId: Integer;
begin
  FIdLock.Enter;
  try
    Result := FNextId;
    Inc(FNextId);
  finally
    FIdLock.Leave;
  end;
end;

procedure TJsonRpcClient.Start;
begin
  FRunning := True;
  FReaderTask := TTask.Run(ReaderLoop);
end;

procedure TJsonRpcClient.Stop;
begin
  FRunning := False;
  // Signal all pending requests so they don't block indefinitely
  FPendingLock.Enter;
  try
    for var Pair in FPending do
      Pair.Value.Reject(TJSONString.Create('Client stopped'));
  finally
    FPendingLock.Leave;
  end;
end;

function TJsonRpcClient.ReadMessage: TJSONObject;
var
  Line: string;
  ContentLength: Integer;
  Buffer: TBytes;
  BytesRead, TotalRead: Integer;
  Ch: Byte;
  LineBuilder: TStringBuilder;
begin
  Result := nil;
  ContentLength := -1;
  LineBuilder := TStringBuilder.Create;
  try
    // Read headers until we hit an empty line
    while FRunning do
    begin
      LineBuilder.Clear;
      while FRunning do
      begin
        BytesRead := FInputStream.Read(Ch, 1);
        if BytesRead = 0 then
          Exit(nil);
        if Ch = 10 then // LF
          Break;
        if Ch <> 13 then // skip CR
          LineBuilder.Append(Chr(Ch));
      end;

      Line := LineBuilder.ToString;
      if Line = '' then
        Break;

      if Line.StartsWith('Content-Length:', True) then
        ContentLength := StrToIntDef(Trim(Copy(Line, 16, MaxInt)), -1);
    end;
  finally
    LineBuilder.Free;
  end;

  if ContentLength <= 0 then
    Exit(nil);

  // Read the body
  SetLength(Buffer, ContentLength);
  TotalRead := 0;
  while (TotalRead < ContentLength) and FRunning do
  begin
    BytesRead := FInputStream.Read(Buffer[TotalRead], ContentLength - TotalRead);
    if BytesRead = 0 then
      Exit(nil);
    Inc(TotalRead, BytesRead);
  end;

  var JsonStr := TEncoding.UTF8.GetString(Buffer);
  var Parsed := TJSONObject.ParseJSONValue(JsonStr);
  if Assigned(Parsed) and (Parsed is TJSONObject) then
    Result := TJSONObject(Parsed)
  else
  begin
    Parsed.Free;
    Result := nil;
  end;
end;

procedure TJsonRpcClient.WriteMessage(const Msg: TJSONObject);
var
  Body: TBytes;
  Header: string;
  HeaderBytes: TBytes;
begin
  Body := TEncoding.UTF8.GetBytes(Msg.ToJSON);
  Header := Format('Content-Length: %d'#13#10#13#10, [Length(Body)]);
  HeaderBytes := TEncoding.UTF8.GetBytes(Header);

  FWriteLock.Enter;
  try
    FOutputStream.Write(HeaderBytes, Length(HeaderBytes));
    FOutputStream.Write(Body, Length(Body));
  finally
    FWriteLock.Leave;
  end;
end;

procedure TJsonRpcClient.ReaderLoop;
var
  Msg: TJSONObject;
begin
  while FRunning do
  begin
    try
      Msg := ReadMessage;
      if Msg = nil then
      begin
        if FRunning then
          Sleep(10);
        Continue;
      end;
      try
        HandleMessage(Msg);
      finally
        Msg.Free;
      end;
    except
      on E: Exception do
      begin
        if FRunning then
          Sleep(50);
      end;
    end;
  end;
end;

procedure TJsonRpcClient.HandleMessage(const Msg: TJSONObject);
var
  MethodVal: TJSONValue;
  IdVal: TJSONValue;
begin
  MethodVal := Msg.GetValue('method');
  IdVal := Msg.GetValue('id');

  if Assigned(MethodVal) and Assigned(IdVal) then
    HandleRequest(IdVal, MethodVal.Value, Msg.GetValue('params'))
  else if Assigned(MethodVal) and not Assigned(IdVal) then
    HandleNotification(MethodVal.Value, Msg.GetValue('params'))
  else if Assigned(IdVal) then
    HandleResponse(Msg);
end;

procedure TJsonRpcClient.HandleResponse(const Msg: TJSONObject);
var
  IdVal: TJSONValue;
  ReqId: Integer;
  Pending: TPendingRequest;
begin
  IdVal := Msg.GetValue('id');
  if not Assigned(IdVal) then
    Exit;

  ReqId := IdVal.GetValue<Integer>;

  FPendingLock.Enter;
  try
    if not FPending.TryGetValue(ReqId, Pending) then
      Exit;
    FPending.ExtractPair(ReqId);
  finally
    FPendingLock.Leave;
  end;

  var ErrorVal := Msg.GetValue('error');
  if Assigned(ErrorVal) then
    Pending.Reject(ErrorVal.Clone as TJSONValue)
  else
  begin
    var ResultVal := Msg.GetValue('result');
    if Assigned(ResultVal) then
      Pending.Resolve(ResultVal.Clone as TJSONValue)
    else
      Pending.Resolve(TJSONNull.Create);
  end;
end;

procedure TJsonRpcClient.HandleNotification(const Method: string;
  const Params: TJSONValue);
var
  Handler: TNotificationHandler;
begin
  if FNotificationHandlers.TryGetValue(Method, Handler) then
    Handler(Method, Params);
end;

procedure TJsonRpcClient.HandleRequest(const Id: TJSONValue; const Method: string;
  const Params: TJSONValue);
var
  Handler: TRequestHandler;
  Response: TJSONObject;
  ResultVal: TJSONValue;
begin
  Response := TJSONObject.Create;
  try
    Response.AddPair('jsonrpc', '2.0');
    Response.AddPair('id', Id.Clone as TJSONValue);

    if FRequestHandlers.TryGetValue(Method, Handler) then
    begin
      try
        ResultVal := Handler(Method, Params);
        Response.AddPair('result', ResultVal);
      except
        on E: Exception do
        begin
          var ErrObj := TJSONObject.Create;
          ErrObj.AddPair('code', TJSONNumber.Create(-32603));
          ErrObj.AddPair('message', E.Message);
          Response.AddPair('error', ErrObj);
        end;
      end;
    end
    else
    begin
      var ErrObj := TJSONObject.Create;
      ErrObj.AddPair('code', TJSONNumber.Create(-32601));
      ErrObj.AddPair('message', 'Method not found: ' + Method);
      Response.AddPair('error', ErrObj);
    end;

    WriteMessage(Response);
  finally
    Response.Free;
  end;
end;

function TJsonRpcClient.SendRequest(const Method: string; const Params: TJSONValue;
  TimeoutMs: Cardinal): TJSONValue;
var
  ReqId: Integer;
  Msg: TJSONObject;
  Pending: TPendingRequest;
begin
  ReqId := GetNextId;
  Pending := TPendingRequest.Create;

  FPendingLock.Enter;
  try
    FPending.Add(ReqId, Pending);
  finally
    FPendingLock.Leave;
  end;

  Msg := TJSONObject.Create;
  try
    Msg.AddPair('jsonrpc', '2.0');
    Msg.AddPair('id', TJSONNumber.Create(ReqId));
    Msg.AddPair('method', Method);
    if Assigned(Params) then
      Msg.AddPair('params', Params.Clone as TJSONValue);
    WriteMessage(Msg);
  finally
    Msg.Free;
  end;

  if not Pending.WaitFor(TimeoutMs) then
  begin
    FPendingLock.Enter;
    try
      FPending.Remove(ReqId);
    finally
      FPendingLock.Leave;
    end;
    raise Exception.CreateFmt('Request timed out: %s (id=%d)', [Method, ReqId]);
  end;

  if Assigned(Pending.Error) then
    raise Exception.CreateFmt('JSON-RPC error for %s: %s',
      [Method, Pending.Error.ToJSON]);

  Result := Pending.Result;
end;

procedure TJsonRpcClient.SendNotification(const Method: string; const Params: TJSONValue);
var
  Msg: TJSONObject;
begin
  Msg := TJSONObject.Create;
  try
    Msg.AddPair('jsonrpc', '2.0');
    Msg.AddPair('method', Method);
    if Assigned(Params) then
      Msg.AddPair('params', Params.Clone as TJSONValue);
    WriteMessage(Msg);
  finally
    Msg.Free;
  end;
end;

procedure TJsonRpcClient.OnNotification(const Method: string;
  Handler: TNotificationHandler);
begin
  FNotificationHandlers.AddOrSetValue(Method, Handler);
end;

procedure TJsonRpcClient.OnRequest(const Method: string;
  Handler: TRequestHandler);
begin
  FRequestHandlers.AddOrSetValue(Method, Handler);
end;

end.
