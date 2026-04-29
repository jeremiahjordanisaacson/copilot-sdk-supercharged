{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

unit TestClient;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils, System.JSON,
  Copilot.Types, Copilot.JsonRpc, Copilot.Session,
  Copilot.Client, Copilot.DefineTool, Copilot.SdkProtocolVersion;

type
  [TestFixture]
  TClientTests = class
  public
    [Test]
    procedure TestConstructorDefaults;

    [Test]
    procedure TestConstructorWithOptions;

    [Test]
    procedure TestStateIsDisconnectedInitially;

    [Test]
    procedure TestProtocolVersionConstant;
  end;

  [TestFixture]
  TTypesTests = class
  public
    [Test]
    procedure TestToolSuccess;

    [Test]
    procedure TestToolFailure;

    [Test]
    procedure TestDefaultSessionConfig;

    [Test]
    procedure TestDefaultMessageOptions;

    [Test]
    procedure TestPermissionDecisionValues;
  end;

  [TestFixture]
  TDefineToolTests = class
  public
    [Test]
    procedure TestDefineToolWithParameters;

    [Test]
    procedure TestDefineSimpleTool;

    [Test]
    procedure TestToolHandlerExecution;
  end;

  [TestFixture]
  TJsonRpcTests = class
  public
    [Test]
    procedure TestPendingRequestResolve;

    [Test]
    procedure TestPendingRequestReject;

    [Test]
    procedure TestPendingRequestTimeout;
  end;

implementation

{ TClientTests }

procedure TClientTests.TestConstructorDefaults;
var
  Client: TCopilotClient;
begin
  Client := TCopilotClient.Create;
  try
    Assert.AreEqual(csDisconnected, Client.State);
    Assert.AreEqual('', Client.Options.CliPath);
    Assert.AreEqual('', Client.Options.CliUrl);
    Assert.AreEqual('', Client.Options.LogLevel);
    Assert.AreEqual(0, Client.Options.SessionIdleTimeoutSeconds);
  finally
    Client.Free;
  end;
end;

procedure TClientTests.TestConstructorWithOptions;
var
  Opts: TCopilotClientOptions;
  Client: TCopilotClient;
begin
  Opts := Default(TCopilotClientOptions);
  Opts.CliPath := '/usr/local/bin/copilot';
  Opts.LogLevel := 'debug';
  Opts.SessionIdleTimeoutSeconds := 300;

  Client := TCopilotClient.Create(Opts);
  try
    Assert.AreEqual('/usr/local/bin/copilot', Client.Options.CliPath);
    Assert.AreEqual('debug', Client.Options.LogLevel);
    Assert.AreEqual(300, Client.Options.SessionIdleTimeoutSeconds);
  finally
    Client.Free;
  end;
end;

procedure TClientTests.TestStateIsDisconnectedInitially;
var
  Client: TCopilotClient;
begin
  Client := TCopilotClient.Create;
  try
    Assert.AreEqual(csDisconnected, Client.State);
  finally
    Client.Free;
  end;
end;

procedure TClientTests.TestProtocolVersionConstant;
begin
  Assert.AreEqual('2', SDK_PROTOCOL_VERSION);
end;

{ TTypesTests }

procedure TTypesTests.TestToolSuccess;
var
  R: TToolResultObject;
begin
  R := ToolSuccess('Hello world');
  Assert.AreEqual('Hello world', R.Content);
  Assert.IsFalse(R.IsError);
end;

procedure TTypesTests.TestToolFailure;
var
  R: TToolResultObject;
begin
  R := ToolFailure('Something went wrong');
  Assert.AreEqual('Something went wrong', R.Content);
  Assert.IsTrue(R.IsError);
end;

procedure TTypesTests.TestDefaultSessionConfig;
var
  Config: TSessionConfig;
begin
  Config := Default(TSessionConfig);
  Assert.AreEqual('', Config.Model);
  Assert.IsFalse(Config.Streaming);
  Assert.AreEqual(0, Length(Config.Tools));
  Assert.IsFalse(Config.IncludeSubAgentStreamingEvents);
end;

procedure TTypesTests.TestDefaultMessageOptions;
var
  Opts: TMessageOptions;
begin
  Opts := Default(TMessageOptions);
  Assert.AreEqual('', Opts.Prompt);
  Assert.AreEqual('', Opts.Model);
end;

procedure TTypesTests.TestPermissionDecisionValues;
begin
  Assert.AreEqual(Ord(pdApproved), 0);
  Assert.AreEqual(Ord(pdDeniedByUser), 1);
  Assert.AreEqual(Ord(pdDeniedByPolicy), 2);
end;

{ TDefineToolTests }

procedure TDefineToolTests.TestDefineToolWithParameters;
var
  Tool: TTool;
  Params: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.AddPair('type', 'object');

  Tool := DefineTool('my_tool', 'A test tool', Params,
    function(const Args: TJSONObject;
      const Inv: TToolInvocation): TToolResultObject
    begin
      Result := ToolSuccess('ok');
    end
  );

  Assert.AreEqual('my_tool', Tool.Name);
  Assert.AreEqual('A test tool', Tool.Description);
  Assert.IsNotNull(Tool.Parameters);
  Assert.IsTrue(Assigned(Tool.Handler));
end;

procedure TDefineToolTests.TestDefineSimpleTool;
var
  Tool: TTool;
begin
  Tool := DefineSimpleTool('simple', 'No params',
    function(const Args: TJSONObject;
      const Inv: TToolInvocation): TToolResultObject
    begin
      Result := ToolSuccess('done');
    end
  );

  Assert.AreEqual('simple', Tool.Name);
  Assert.AreEqual('No params', Tool.Description);
  Assert.IsNull(Tool.Parameters);
end;

procedure TDefineToolTests.TestToolHandlerExecution;
var
  Tool: TTool;
  Args: TJSONObject;
  Inv: TToolInvocation;
  Res: TToolResultObject;
begin
  Tool := DefineSimpleTool('echo', 'Echoes input',
    function(const Args: TJSONObject;
      const Inv: TToolInvocation): TToolResultObject
    begin
      Result := ToolSuccess('echo:' + Args.GetValue<string>('text', ''));
    end
  );

  Args := TJSONObject.Create;
  try
    Args.AddPair('text', 'hello');
    Inv.ToolCallId := 'tc-1';
    Inv.SessionId := 'sess-1';

    Res := Tool.Handler(Args, Inv);
    Assert.AreEqual('echo:hello', Res.Content);
    Assert.IsFalse(Res.IsError);
  finally
    Args.Free;
  end;
end;

{ TJsonRpcTests }

procedure TJsonRpcTests.TestPendingRequestResolve;
var
  Pending: TPendingRequest;
begin
  Pending := TPendingRequest.Create;
  try
    Pending.Resolve(TJSONString.Create('success'));
    Assert.IsTrue(Pending.WaitFor(100));
    Assert.IsNotNull(Pending.Result);
    Assert.IsNull(Pending.Error);
  finally
    Pending.Free;
  end;
end;

procedure TJsonRpcTests.TestPendingRequestReject;
var
  Pending: TPendingRequest;
begin
  Pending := TPendingRequest.Create;
  try
    Pending.Reject(TJSONString.Create('failure'));
    Assert.IsTrue(Pending.WaitFor(100));
    Assert.IsNotNull(Pending.Error);
  finally
    Pending.Free;
  end;
end;

procedure TJsonRpcTests.TestPendingRequestTimeout;
var
  Pending: TPendingRequest;
begin
  Pending := TPendingRequest.Create;
  try
    Assert.IsFalse(Pending.WaitFor(50));
  finally
    Pending.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TClientTests);
  TDUnitX.RegisterTestFixture(TTypesTests);
  TDUnitX.RegisterTestFixture(TDefineToolTests);
  TDUnitX.RegisterTestFixture(TJsonRpcTests);

end.
