# Persisting Sessions in Delphi

Patterns for saving and resuming sessions across application restarts.

## Scenario

Your application needs to save a session ID so users can continue conversations later, even after the application has been closed and restarted.

## Saving a Session ID

```pascal
uses
  System.SysUtils, System.IOUtils;

const
  SESSION_FILE = 'last_session.txt';

procedure SaveSessionId(const SessionId: string);
begin
  TFile.WriteAllText(SESSION_FILE, SessionId);
end;

function LoadSessionId: string;
begin
  if TFile.Exists(SESSION_FILE) then
    Result := Trim(TFile.ReadAllText(SESSION_FILE))
  else
    Result := '';
end;
```

## Resuming a Session

```pascal
var
  Client: TCopilotClient;
  Session: TCopilotSession;
  SavedId: string;
  Config: TSessionConfig;
  ResumeConfig: TResumeSessionConfig;
begin
  Client := TCopilotClient.Create;
  Client.Start;

  SavedId := LoadSessionId;

  if SavedId <> '' then
  begin
    // Try to resume the saved session
    try
      ResumeConfig := Default(TResumeSessionConfig);
      ResumeConfig.SessionId := SavedId;
      ResumeConfig.Streaming := True;
      ResumeConfig.OnPermissionRequest :=
        function(const Req: TPermissionRequest): TPermissionResult
        begin
          Result.Decision := pdApproved;
        end;

      Session := Client.ResumeSession(ResumeConfig);
      WriteLn('Resumed session: ', Session.SessionId);
    except
      on E: Exception do
      begin
        WriteLn('Could not resume, creating new session: ', E.Message);
        Config := Default(TSessionConfig);
        Config.Streaming := True;
        Session := Client.CreateSession(Config);
      end;
    end;
  end
  else
  begin
    Config := Default(TSessionConfig);
    Config.Streaming := True;
    Session := Client.CreateSession(Config);
  end;

  // Save the session ID for next time
  SaveSessionId(Session.SessionId);

  // Use the session...
  Session.Disconnect;
  Client.Stop;
  Client.Free;
end;
```

## Getting Session History

```pascal
var
  Messages: TJSONArray;
begin
  Messages := Session.GetMessages;
  try
    WriteLn('Message history (', Messages.Count, ' messages):');
    for var I := 0 to Messages.Count - 1 do
    begin
      var Msg := Messages.Items[I] as TJSONObject;
      WriteLn('  [', Msg.GetValue<string>('role', ''), '] ',
        Msg.GetValue<string>('content', ''));
    end;
  finally
    Messages.Free;
  end;
end;
```

## Infinite Sessions and Compaction

Sessions persist workspace state automatically. Listen for compaction events:

```pascal
Session.OnEvent('session.compaction_start',
  procedure(const Event: TSessionEvent)
  begin
    WriteLn('Compaction started...');
  end
);

Session.OnEvent('session.compaction_complete',
  procedure(const Event: TSessionEvent)
  begin
    WriteLn('Compaction complete.');
  end
);
```

## Deleting Old Sessions

```pascal
procedure CleanupOldSessions(Client: TCopilotClient;
  const KeepSessionId: string);
var
  Sessions: TJSONArray;
begin
  Sessions := Client.ListSessions;
  try
    for var I := 0 to Sessions.Count - 1 do
    begin
      var Obj := Sessions.Items[I] as TJSONObject;
      var Id := Obj.GetValue<string>('sessionId', '');
      if (Id <> '') and (Id <> KeepSessionId) then
      begin
        try
          Client.DeleteSession(Id);
          WriteLn('Deleted session: ', Id);
        except
          on E: Exception do
            WriteLn('Failed to delete ', Id, ': ', E.Message);
        end;
      end;
    end;
  finally
    Sessions.Free;
  end;
end;
```
