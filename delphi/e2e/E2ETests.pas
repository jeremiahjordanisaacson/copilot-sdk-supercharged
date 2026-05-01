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

initialization
  RegisterTest(TE2ETestCase);

end.
