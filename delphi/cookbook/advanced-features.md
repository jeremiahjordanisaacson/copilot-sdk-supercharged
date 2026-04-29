# Advanced v2.0 Features in Delphi

Recipes for advanced SDK features: SessionFs, idle timeout, system prompts, skills, BYOK, image generation, and more.

## SessionFs (Persistent Session Filesystem)

SessionFs provides a virtual filesystem scoped to each session for persistent state.

```pascal
var
  Opts: TCopilotClientOptions;
  Client: TCopilotClient;
begin
  Opts := Default(TCopilotClientOptions);
  Opts.SessionFs.WorkspaceRoot := 'C:\MyProject';
  Opts.SessionFs.StateRoot := 'C:\MyProject\.copilot-state';
  Opts.SessionFs.PathStyle := 'windows';

  Client := TCopilotClient.Create(Opts);
  Client.Start;
end;
```

## Session Idle Timeout

Automatically clean up sessions after inactivity:

```pascal
Opts.SessionIdleTimeoutSeconds := 600;  // 10 minutes
```

## Custom System Prompts

```pascal
Config.SystemMessage.Mode := 'customize';
// Use instructions for simpler customization:
Config.Instructions := [
  'You are a Delphi expert.',
  'Always suggest Object Pascal best practices.',
  'Use try/finally for resource management.'
];
```

## BYOK (Bring Your Own Key)

Use your own API key with a custom provider:

```pascal
Config.Provider.ProviderType := 'openai';
Config.Provider.BaseUrl := 'https://api.openai.com/v1';
Config.Provider.ApiKey := GetEnvironmentVariable('OPENAI_API_KEY');
Config.Model := 'gpt-4';
```

## Image Generation

Request image responses using `ResponseFormat` and `ImageOptions`:

```pascal
var
  MsgOpts: TMessageOptions;
  Response: TJSONValue;
begin
  MsgOpts := Default(TMessageOptions);
  MsgOpts.Prompt := 'Generate a logo for a Delphi SDK library';
  MsgOpts.ResponseFormat := rfImage;
  MsgOpts.ImageOptions.Size := '1024x1024';
  MsgOpts.ImageOptions.Quality := 'hd';
  MsgOpts.ImageOptions.Style := 'natural';

  Response := Session.SendAndWait(MsgOpts);
  try
    WriteLn('Image response: ', Response.ToJSON);
  finally
    Response.Free;
  end;
end;
```

## Streaming with Delta Events

```pascal
Session.On_(
  procedure(const Event: TSessionEvent)
  begin
    case IndexStr(Event.EventType, [
      'assistant.message_delta',
      'assistant.reasoning_delta',
      'assistant.message',
      'session.idle',
      'session.compaction_start',
      'session.compaction_complete'
    ]) of
      0: Write(Event.Data.GetValue<string>('content', ''));
      1: Write('[Think] ', Event.Data.GetValue<string>('content', ''));
      2: WriteLn;
      3: WriteLn('[Idle]');
      4: WriteLn('[Compacting...]');
      5: WriteLn('[Compaction done]');
    end;
  end
);
```

## MCP Server Configuration

Configure external MCP servers:

```pascal
// MCP servers are configured via the MCPServers dictionary on TSessionConfig.
// Each entry maps a server name to a TMCPServerConfig record.
var
  McpConfig: TMCPServerConfig;
begin
  McpConfig.Command := 'node';
  McpConfig.Args := ['./mcp-server.js'];
  McpConfig.Tools := ['*'];

  Config.MCPServers := TDictionary<string, TMCPServerConfig>.Create;
  Config.MCPServers.Add('my-server', McpConfig);
end;
```

## Session Metadata

```pascal
var
  Meta: TSessionMetadata;
begin
  Meta := Client.GetSessionMetadata(Session.SessionId);
  WriteLn('Session: ', Meta.SessionId);
  WriteLn('Model: ', Meta.Model);
  WriteLn('Status: ', Meta.Status);
  WriteLn('Messages: ', Meta.MessageCount);
end;
```

## Foreground Session (TUI Mode)

```pascal
Client.SetForegroundSessionId(Session.SessionId);
```

## Session Lifecycle Monitoring

```pascal
Client.OnLifecycle(
  procedure(const Event: TSessionLifecycleEvent)
  begin
    WriteLn('Lifecycle [', Event.EventType, ']: session=', Event.SessionId);
  end
);
```

## Ping and Health Check

```pascal
var
  PingResult: TPingResponse;
begin
  PingResult := Client.Ping;
  WriteLn('Protocol: ', PingResult.ProtocolVersion);
  WriteLn('Server: ', PingResult.ServerVersion);
  WriteLn('Status: ', PingResult.Status);
end;
```

## Thread-Safe Event Handling

All handler registrations and dispatches are protected by `TCriticalSection`:

```pascal
// Safe to subscribe from any thread
TTask.Run(
  procedure
  begin
    Session.OnEvent('assistant.message',
      procedure(const Event: TSessionEvent)
      begin
        // This handler is also thread-safe
        TThread.Queue(nil,
          procedure
          begin
            Memo1.Lines.Add(Event.Data.GetValue<string>('content', ''));
          end
        );
      end
    );
  end
);
```

## VCL/FMX Integration

When using the SDK in a GUI application, marshal events to the main thread:

```pascal
Session.On_(
  procedure(const Event: TSessionEvent)
  begin
    TThread.Queue(nil,
      procedure
      begin
        if Event.EventType = 'assistant.message_delta' then
          Memo1.Text := Memo1.Text + Event.Data.GetValue<string>('content', '')
        else if Event.EventType = 'session.idle' then
          StatusBar1.SimpleText := 'Ready';
      end
    );
  end
);
```
