# Tools and Skills in Delphi

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the GitHub Copilot SDK in Delphi.

## Scenario

Your application needs to give the AI assistant the ability to call custom functions (tools), compose capabilities into skills, and coordinate multiple agents for complex workflows.

## Defining a Simple Tool

```pascal
uses
  System.SysUtils, System.JSON,
  Copilot.Types, Copilot.DefineTool;

var
  TimeTool: TTool;
begin
  TimeTool := DefineTool(
    'get_current_time',
    'Returns the current date and time',
    nil,
    function(const Args: TJSONObject;
      const Inv: TToolInvocation): TToolResultObject
    begin
      Result := ToolSuccess(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    end
  );
end;
```

## Defining a Tool with Parameters

```pascal
var
  ReadFileTool: TTool;
  Params: TJSONObject;
begin
  Params := TJSONObject.ParseJSONValue(
    '{"type":"object","properties":{"path":{"type":"string","description":"File path to read"}},' +
    '"required":["path"]}') as TJSONObject;

  ReadFileTool := DefineTool(
    'read_file',
    'Reads the contents of a file given its path',
    Params,
    function(const Args: TJSONObject;
      const Inv: TToolInvocation): TToolResultObject
    begin
      var Path := Args.GetValue<string>('path', '');
      if Path = '' then
        Exit(ToolFailure('Missing required parameter: path'));

      try
        var Content := TFile.ReadAllText(Path);
        Result := ToolSuccess(Content);
      except
        on E: Exception do
          Result := ToolFailure('Could not read file: ' + E.Message);
      end;
    end
  );
end;
```

## Using Multiple Tools in a Session

```pascal
var
  Config: TSessionConfig;
begin
  Config := Default(TSessionConfig);
  Config.Tools := [TimeTool, ReadFileTool, CalcTool];
  Config.Streaming := True;
  Config.OnPermissionRequest :=
    function(const Req: TPermissionRequest): TPermissionResult
    begin
      Result.Decision := pdApproved;
    end;

  Session := Client.CreateSession(Config);
end;
```

## Registering Skill Directories

```pascal
Config.SkillDirectories := [
  '.\skills',
  'C:\MyApp\copilot-skills'
];
Config.DisabledSkills := ['experimental-skill'];
```

## Sub-Agent Streaming Events

```pascal
Config.IncludeSubAgentStreamingEvents := True;

Session.On_(
  procedure(const Event: TSessionEvent)
  begin
    if Event.EventType = 'assistant.message_delta' then
      Write(Event.Data.GetValue<string>('content', ''))
    else if Event.EventType = 'subagent.message_delta' then
      Write('[Sub] ', Event.Data.GetValue<string>('content', ''));
  end
);
```

## Tool with Pre/Post Hooks

```pascal
var
  Hooks: TSessionHooks;
begin
  Hooks.OnPreToolUse :=
    function(const Input: TPreToolUseHookInput;
      const SessionId: string): TPreToolUseHookOutput
    begin
      WriteLn('Pre-tool: ', Input.ToolName);
      if Input.ToolName = 'dangerous_tool' then
        Result.PermissionDecision := 'deny'
      else
        Result.PermissionDecision := 'allow';
    end;

  Hooks.OnPostToolUse :=
    function(const Input: TPostToolUseHookInput;
      const SessionId: string): TPostToolUseHookOutput
    begin
      WriteLn('Post-tool: ', Input.ToolName, ' result=', Input.ToolResult);
      Result.OverrideResult := '';  // Empty means no override
    end;

  Config.Hooks := Hooks;
end;
```

## Async Tool Execution with TTask

If your tool handler needs to perform I/O, consider wrapping heavy work in a task:

```pascal
var
  ApiTool: TTool;
begin
  ApiTool := DefineTool(
    'fetch_api',
    'Fetches data from an API endpoint',
    Params,
    function(const Args: TJSONObject;
      const Inv: TToolInvocation): TToolResultObject
    begin
      var Url := Args.GetValue<string>('url', '');
      // Note: Tool handlers are called synchronously by the SDK.
      // For truly async operations, the handler blocks until complete.
      try
        var Http := THTTPClient.Create;
        try
          var Response := Http.Get(Url);
          Result := ToolSuccess(Response.ContentAsString);
        finally
          Http.Free;
        end;
      except
        on E: Exception do
          Result := ToolFailure('API call failed: ' + E.Message);
      end;
    end
  );
end;
```
