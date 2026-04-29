# Error Handling in Delphi

Patterns for handling errors when using the GitHub Copilot SDK in Delphi.

## Scenario

Your application needs to handle connection failures, request timeouts, and unexpected errors gracefully while ensuring resources are properly cleaned up via try/finally.

## Wrapping Client Lifecycle with try/finally

Delphi uses deterministic cleanup with try/finally blocks.

```pascal
program SafeClient;
{$APPTYPE CONSOLE}
uses
  System.SysUtils,
  Copilot.Client, Copilot.Types;

var
  Client: TCopilotClient;
begin
  Client := TCopilotClient.Create;
  try
    try
      Client.Start;
      // ... use sessions ...
    except
      on E: Exception do
        WriteLn('Error: ', E.Message);
    end;
  finally
    Client.Stop;
    Client.Free;
  end;
end.
```

## Catching Connection Errors with Retry

```pascal
function TryConnect(const CliPath: string; MaxRetries: Integer): TCopilotClient;
var
  Opts: TCopilotClientOptions;
  Attempt: Integer;
begin
  Opts := Default(TCopilotClientOptions);
  Opts.CliPath := CliPath;

  for Attempt := 1 to MaxRetries do
  begin
    Result := TCopilotClient.Create(Opts);
    try
      Result.Start;
      WriteLn('Connected on attempt ', Attempt);
      Exit;
    except
      on E: Exception do
      begin
        WriteLn('Attempt ', Attempt, ' failed: ', E.Message);
        Result.Free;
        Result := nil;
        if Attempt < MaxRetries then
          Sleep(1000 * Attempt);  // Exponential backoff
      end;
    end;
  end;

  raise Exception.Create('Failed to connect after ' + IntToStr(MaxRetries) + ' attempts');
end;
```

## Handling Send Timeouts

```pascal
var
  Response: TJSONValue;
begin
  try
    Response := Session.SendAndWait(MsgOpts, 15000);  // 15-second timeout
    try
      WriteLn(Response.ToJSON);
    finally
      Response.Free;
    end;
  except
    on E: Exception do
    begin
      if Pos('timed out', E.Message) > 0 then
      begin
        WriteLn('Request timed out, aborting...');
        Session.Abort;
      end
      else
        raise;
    end;
  end;
end;
```

## Graceful Shutdown

```pascal
procedure GracefulShutdown(Client: TCopilotClient;
  Sessions: TArray<TCopilotSession>);
var
  Session: TCopilotSession;
begin
  for Session in Sessions do
  begin
    try
      Session.Disconnect;
    except
      on E: Exception do
        WriteLn('Warning: failed to disconnect session: ', E.Message);
    end;
  end;

  try
    Client.Stop;
  except
    on E: Exception do
      WriteLn('Warning: failed to stop client: ', E.Message);
  end;
end;
```

## Error Events in Streaming

```pascal
Session.OnEvent('error',
  procedure(const Event: TSessionEvent)
  begin
    WriteLn('Session error: ', Event.Data.GetValue<string>('message', ''));
  end
);
```
