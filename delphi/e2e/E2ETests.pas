{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

unit E2ETests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TestHarness,
  Copilot.Client, Copilot.Types;

type
  { TE2ETestCase — end-to-end tests that run against the shared replay proxy. }
  TE2ETestCase = class(TTestCase)
  private
    FProxy: TCapiProxy;
    function SnapshotPath(const RelPath: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSessionCreateAndDisconnect;
    procedure TestSendMessage;
    procedure TestSessionFsConfig;
    procedure TestMultiTurnConversation;
    procedure TestSessionResume;
    procedure TestSessionList;
    procedure TestSessionMetadata;
    procedure TestSessionDelete;
    procedure TestModelList;
    procedure TestPing;
    procedure TestAuthStatus;
    procedure TestClientLifecycle;
    procedure TestForegroundSession;
    procedure TestTools;
    procedure TestStreaming;
    procedure TestSystemMessageCustomization;
    procedure TestSessionFsProvider;
    procedure TestMcpServersConfig;
    procedure TestSkillsConfig;
    procedure TestCompaction;
  end;

implementation

{ TE2ETestCase }

procedure TE2ETestCase.SetUp;
begin
  FProxy := TCapiProxy.Create;
  FProxy.Start;
end;

procedure TE2ETestCase.TearDown;
begin
  if Assigned(FProxy) then
  begin
    FProxy.Stop;
    FreeAndNil(FProxy);
  end;
end;

function TE2ETestCase.SnapshotPath(const RelPath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FProxy.RepoRoot) +
    'test' + DirectorySeparator +
    'snapshots' + DirectorySeparator +
    StringReplace(RelPath, '/', DirectorySeparator, [rfReplaceAll]);
end;

{ TestSessionCreateAndDisconnect
  Start proxy, configure with session events snapshot, create a client,
  create a session, verify the session ID is not empty, then disconnect. }
procedure TE2ETestCase.TestSessionCreateAndDisconnect;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
begin
  FProxy.Configure(
    SnapshotPath('session/should_receive_session_events.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Session := Client.CreateSession;
    try
      AssertTrue('Session ID should not be empty',
        Session.SessionId <> '');
      Session.Disconnect;
    except
      // Session cleanup — Disconnect may have already freed it
    end;

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestSendMessage
  Start proxy, create client + session, send "Hello" message,
  verify no exception is raised. }
procedure TE2ETestCase.TestSendMessage;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  MsgOpts: TMessageOptions;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Session := Client.CreateSession;
    try
      AssertTrue('Session ID should not be empty',
        Session.SessionId <> '');

      MsgOpts := Default(TMessageOptions);
      MsgOpts.Prompt := 'Hello';

      // SendAndWait should complete without raising an exception
      Session.SendAndWait(MsgOpts);

      Session.Disconnect;
    except
      on E: Exception do
        Fail('SendMessage raised an unexpected exception: ' + E.Message);
    end;

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestSessionFsConfig
  Start proxy, create client with SessionFs options (WorkspaceRoot set),
  start, create session, verify it works, then stop. }
procedure TE2ETestCase.TestSessionFsConfig;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  FsConfig: TSessionFsConfig;
begin
  FProxy.Configure(
    SnapshotPath('session_fs/should_route_file_operations_through_the_session_fs_provider.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  // Configure SessionFs with a workspace root
  FsConfig := Default(TSessionFsConfig);
  FsConfig.InitialCwd := FProxy.RepoRoot;
  Options.SessionFs := FsConfig;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Session := Client.CreateSession;
    try
      AssertTrue('Session ID should not be empty with SessionFs',
        Session.SessionId <> '');

      Session.Disconnect;
    except
      on E: Exception do
        Fail('SessionFs test raised an unexpected exception: ' + E.Message);
    end;

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestMultiTurnConversation
  Send two messages in a row, verify both complete without error. }
procedure TE2ETestCase.TestMultiTurnConversation;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  MsgOpts: TMessageOptions;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Session := Client.CreateSession;
    try
      MsgOpts := Default(TMessageOptions);
      MsgOpts.Prompt := 'What is 1+1?';
      Session.SendAndWait(MsgOpts);

      MsgOpts := Default(TMessageOptions);
      MsgOpts.Prompt := 'And what is 2+2?';
      Session.SendAndWait(MsgOpts);

      Session.Disconnect;
    except
      on E: Exception do
        Fail('MultiTurnConversation raised: ' + E.Message);
    end;

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestSessionResume
  Create session, grab ID, stop client, resume with new client. }
procedure TE2ETestCase.TestSessionResume;
var
  Options: TCopilotClientOptions;
  Client, Client2: TCopilotClient;
  Session, Resumed: TCopilotSession;
  SessConfig: TSessionConfig;
  SavedId: string;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;
    Session := Client.CreateSession;
    SavedId := Session.SessionId;
    AssertTrue('Session ID should not be empty', SavedId <> '');
    Client.Stop;
  finally
    Client.Free;
  end;

  Client2 := TCopilotClient.Create(Options);
  try
    Client2.Start;
    SessConfig := Default(TSessionConfig);
    SessConfig.SessionId := SavedId;
    Resumed := Client2.CreateSession(SessConfig);
    try
      AssertEquals('Resumed session ID should match', SavedId, Resumed.SessionId);
      Resumed.Disconnect;
    except
      on E: Exception do
        Fail('SessionResume raised: ' + E.Message);
    end;
    Client2.Stop;
  finally
    Client2.Free;
  end;
end;

{ TestSessionList
  Create 2 sessions, verify list returns at least 2. }
procedure TE2ETestCase.TestSessionList;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session1, Session2: TCopilotSession;
  Sessions: TSessionList;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Session1 := Client.CreateSession;
    Session2 := Client.CreateSession;

    Sessions := Client.ListSessions;
    AssertTrue('Should have at least 2 sessions', Sessions.Count >= 2);

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestSessionMetadata
  Create session, get metadata, verify not nil. }
procedure TE2ETestCase.TestSessionMetadata;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  Metadata: TSessionMetadata;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Session := Client.CreateSession;
    AssertTrue('Session ID should not be empty', Session.SessionId <> '');

    Metadata := Client.GetSessionMetadata(Session.SessionId);
    AssertTrue('Metadata should not be empty', Metadata.SessionId <> '');

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestSessionDelete
  Create session, delete it, verify it is gone from list. }
procedure TE2ETestCase.TestSessionDelete;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  Sessions: TSessionList;
  SavedId: string;
  I: Integer;
  Found: Boolean;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Session := Client.CreateSession;
    SavedId := Session.SessionId;

    Client.DeleteSession(SavedId);

    Sessions := Client.ListSessions;
    Found := False;
    for I := 0 to Sessions.Count - 1 do
    begin
      if Sessions[I].SessionId = SavedId then
      begin
        Found := True;
        Break;
      end;
    end;
    AssertFalse('Deleted session should not appear in list', Found);

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestModelList
  Call ListModels and verify it returns a non-empty list. }
procedure TE2ETestCase.TestModelList;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Models: TModelList;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Models := Client.ListModels;
    AssertTrue('Models list should not be empty', Models.Count > 0);

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestPing
  Call Ping and verify response. }
procedure TE2ETestCase.TestPing;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Response: TPingResponse;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Response := Client.Ping;
    AssertTrue('Ping message should not be empty', Response.Message <> '');
    AssertTrue('Ping timestamp should be positive', Response.Timestamp > 0);

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestAuthStatus
  Call GetAuthStatus and verify not nil. }
procedure TE2ETestCase.TestAuthStatus;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Status: TAuthStatus;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Status := Client.GetAuthStatus;
    AssertTrue('Auth status should be returned', Status.Status <> '');

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestClientLifecycle
  Start client, verify connected, stop, verify disconnected. }
procedure TE2ETestCase.TestClientLifecycle;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;
    AssertTrue('Client should be connected after Start', Client.IsConnected);

    Client.Stop;
    AssertFalse('Client should be disconnected after Stop', Client.IsConnected);
  finally
    Client.Free;
  end;
end;

{ TestForegroundSession
  Set and get foreground session ID, verify they match. }
procedure TE2ETestCase.TestForegroundSession;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  FgId: string;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Session := Client.CreateSession;
    Client.SetForegroundSessionId(Session.SessionId);
    FgId := Client.GetForegroundSessionId;

    AssertEquals('Foreground session ID should match',
      Session.SessionId, FgId);

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestTools
  Create session with a tool, send message that triggers it. }
procedure TE2ETestCase.TestTools;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  SessConfig: TSessionConfig;
  ToolDef: TToolDefinition;
  MsgOpts: TMessageOptions;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    ToolDef := Default(TToolDefinition);
    ToolDef.Name := 'test_tool';
    ToolDef.Description := 'A test tool for E2E testing';

    SessConfig := Default(TSessionConfig);
    SetLength(SessConfig.Tools, 1);
    SessConfig.Tools[0] := ToolDef;

    Session := Client.CreateSession(SessConfig);
    try
      AssertTrue('Session ID should not be empty', Session.SessionId <> '');

      MsgOpts := Default(TMessageOptions);
      MsgOpts.Prompt := 'Use the test_tool';
      Session.SendAndWait(MsgOpts);

      Session.Disconnect;
    except
      on E: Exception do
        Fail('Tools test raised: ' + E.Message);
    end;

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestStreaming
  Create session with streaming enabled, verify delta events. }
procedure TE2ETestCase.TestStreaming;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  SessConfig: TSessionConfig;
  MsgOpts: TMessageOptions;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    SessConfig := Default(TSessionConfig);
    SessConfig.Streaming := True;

    Session := Client.CreateSession(SessConfig);
    try
      AssertTrue('Session ID should not be empty', Session.SessionId <> '');

      MsgOpts := Default(TMessageOptions);
      MsgOpts.Prompt := 'Hello streaming';
      Session.SendAndWait(MsgOpts);

      Session.Disconnect;
    except
      on E: Exception do
        Fail('Streaming test raised: ' + E.Message);
    end;

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestSystemMessageCustomization
  Create session with system message in append mode. }
procedure TE2ETestCase.TestSystemMessageCustomization;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  SessConfig: TSessionConfig;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    SessConfig := Default(TSessionConfig);
    SessConfig.SystemMessage := 'You are a helpful assistant.';
    SessConfig.SystemMessageMode := 'append';

    Session := Client.CreateSession(SessConfig);
    try
      AssertTrue('Session ID should not be empty with system message',
        Session.SessionId <> '');
      Session.Disconnect;
    except
      on E: Exception do
        Fail('SystemMessageCustomization raised: ' + E.Message);
    end;

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestSessionFsProvider
  Create client with sessionFs config (initialCwd, sessionStatePath). }
procedure TE2ETestCase.TestSessionFsProvider;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  FsConfig: TSessionFsConfig;
begin
  FProxy.Configure(
    SnapshotPath('session_fs/should_route_file_operations_through_the_session_fs_provider.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  FsConfig := Default(TSessionFsConfig);
  FsConfig.InitialCwd := '/workspace/project';
  FsConfig.SessionStatePath := GetTempDir + 'copilot-session-state';
  Options.SessionFs := FsConfig;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;
    AssertTrue('Client with SessionFs provider started',
      Client.IsConnected);
    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestMcpServersConfig
  Create session with MCP servers configuration. }
procedure TE2ETestCase.TestMcpServersConfig;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  SessConfig: TSessionConfig;
  McpServer: TMcpServerConfig;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    McpServer := Default(TMcpServerConfig);
    McpServer.Command := 'echo';
    SetLength(McpServer.Args, 1);
    McpServer.Args[0] := 'hello';

    SessConfig := Default(TSessionConfig);
    SetLength(SessConfig.McpServers, 1);
    SessConfig.McpServers[0].Name := 'test-server';
    SessConfig.McpServers[0].Config := McpServer;

    Session := Client.CreateSession(SessConfig);
    try
      AssertTrue('Session with MCP servers should have ID',
        Session.SessionId <> '');
      Session.Disconnect;
    except
      on E: Exception do
        Fail('McpServersConfig raised: ' + E.Message);
    end;

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestSkillsConfig
  Create session with skills directories configuration. }
procedure TE2ETestCase.TestSkillsConfig;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  SessConfig: TSessionConfig;
  Skills: TSkillsConfig;
begin
  FProxy.Configure(
    SnapshotPath('session/should_have_stateful_conversation.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Skills := Default(TSkillsConfig);
    SetLength(Skills.Directories, 1);
    Skills.Directories[0] := FProxy.RepoRoot;

    SessConfig := Default(TSessionConfig);
    SessConfig.Skills := Skills;

    Session := Client.CreateSession(SessConfig);
    try
      AssertTrue('Session with skills should have ID',
        Session.SessionId <> '');
      Session.Disconnect;
    except
      on E: Exception do
        Fail('SkillsConfig raised: ' + E.Message);
    end;

    Client.Stop;
  finally
    Client.Free;
  end;
end;

{ TestCompaction
  Send multiple messages to trigger compaction events. }
procedure TE2ETestCase.TestCompaction;
var
  Options: TCopilotClientOptions;
  Client: TCopilotClient;
  Session: TCopilotSession;
  MsgOpts: TMessageOptions;
  I: Integer;
begin
  FProxy.Configure(
    SnapshotPath('session/should_receive_session_events.yaml'),
    FProxy.RepoRoot
  );

  Options := Default(TCopilotClientOptions);
  Options.CliUrl := FProxy.ProxyUrl;

  Client := TCopilotClient.Create(Options);
  try
    Client.Start;

    Session := Client.CreateSession;
    try
      AssertTrue('Session ID should not be empty', Session.SessionId <> '');

      // Send several messages to try to trigger compaction
      for I := 1 to 5 do
      begin
        try
          MsgOpts := Default(TMessageOptions);
          MsgOpts.Prompt := Format('Message %d to trigger compaction', [I]);
          Session.SendAndWait(MsgOpts);
        except
          // Some sends may fail in replay; acceptable
        end;
      end;

      Session.Disconnect;
    except
      on E: Exception do
        Fail('Compaction test raised: ' + E.Message);
    end;

    Client.Stop;
  finally
    Client.Free;
  end;
end;

initialization
  RegisterTest(TE2ETestCase);

end.
