# Multiple Sessions in Delphi

Patterns for managing multiple independent conversations simultaneously.

## Scenario

Your application needs to run several AI conversations in parallel, each with its own context, tools, and event handling.

## Creating Multiple Sessions

```pascal
var
  Client: TCopilotClient;
  CodeSession, DocSession: TCopilotSession;
  CodeConfig, DocConfig: TSessionConfig;
begin
  Client := TCopilotClient.Create;
  Client.Start;

  // Session for code tasks
  CodeConfig := Default(TSessionConfig);
  CodeConfig.Model := 'gpt-4';
  CodeConfig.Streaming := True;
  CodeConfig.Instructions := ['You are a code assistant.'];
  CodeSession := Client.CreateSession(CodeConfig);

  // Session for documentation tasks
  DocConfig := Default(TSessionConfig);
  DocConfig.Model := 'gpt-4';
  DocConfig.Streaming := True;
  DocConfig.Instructions := ['You are a documentation writer.'];
  DocSession := Client.CreateSession(DocConfig);

  // Use both sessions independently...

  CodeSession.Disconnect;
  DocSession.Disconnect;
  Client.Stop;
  Client.Free;
end;
```

## Parallel Send with TTask

```pascal
uses
  System.Threading;

procedure SendToMultipleSessions(Sessions: TArray<TCopilotSession>;
  const Prompt: string);
var
  Tasks: TArray<ITask>;
  I: Integer;
begin
  SetLength(Tasks, Length(Sessions));
  for I := 0 to High(Sessions) do
  begin
    var Sess := Sessions[I];
    Tasks[I] := TTask.Run(
      procedure
      var
        Opts: TMessageOptions;
        Response: TJSONValue;
      begin
        Opts := Default(TMessageOptions);
        Opts.Prompt := Prompt;
        Response := Sess.SendAndWait(Opts);
        try
          WriteLn('[', Sess.SessionId, '] ', Response.ToJSON);
        finally
          Response.Free;
        end;
      end
    );
  end;

  TTask.WaitForAll(Tasks);
end;
```

## Per-Session Event Routing

```pascal
procedure SetupSession(Session: TCopilotSession; const Label_: string);
begin
  Session.On_(
    procedure(const Event: TSessionEvent)
    begin
      if Event.EventType = 'assistant.message' then
        WriteLn('[', Label_, '] ', Event.Data.GetValue<string>('content', ''));
    end
  );
end;

// Usage:
SetupSession(CodeSession, 'CODE');
SetupSession(DocSession, 'DOCS');
```

## Listing and Managing Sessions

```pascal
var
  SessionList: TJSONArray;
begin
  SessionList := Client.ListSessions;
  try
    WriteLn('Active sessions: ', SessionList.Count);
    for var I := 0 to SessionList.Count - 1 do
      WriteLn('  - ', SessionList.Items[I].ToJSON);
  finally
    SessionList.Free;
  end;
end;
```
