{---------------------------------------------------------------------------------------------
 Copyright (c) Microsoft Corporation. All rights reserved.
 --------------------------------------------------------------------------------------------}

unit Copilot.SessionParams;

{ SDK protocol 4 grouped session payloads.

  TCopilotClient builds flat (v1-style) session.create / session.resume
  payloads from TSessionConfig. SDK protocol 4 groups most options into
  objects such as "inference", "tools" and "permissions", and rejects the old
  flat keys. GroupSessionParams converts the flat payload right before it is
  sent, so callers keep using the same TSessionConfig fields.

  Rules:
  - root keys (sessionId, model, systemMessage, ...) stay at the top level;
  - known flat keys move to group.member (streaming -> events.streaming,
    requestPermission -> permissions.request, tools -> tools.definitions, ...);
  - unknown keys stay at the root; null values are skipped;
  - members of an already grouped object win over flat keys;
  - configDir becomes configDirectory; on resume disableResume becomes
    suppressResumeEvent and create-only options are dropped;
  - a legacy single "provider" becomes the named provider "default";
  - mcp.envValueMode is "direct" whenever MCP servers are sent. }

interface

uses
  System.SysUtils, System.JSON;

// Convert a flat session payload into the SDK protocol 4 grouped payload.
// Returns a new object owned by the caller. IsResume applies the
// session.resume rules; EmptyMode applies the "empty" client-mode defaults
// underneath the caller's values.
function GroupSessionParams(const Flat: TJSONObject; IsResume: Boolean;
  EmptyMode: Boolean = False): TJSONObject;

implementation

const
  GROUP_NAMES = '|inference|tools|permissions|interaction|events|agents|skills' +
    '|mcp|github|filesystem|execution|instructions|hooks|git|state|plugins' +
    '|canvases|extensions|experiments|policy|';

  ROOT_KEYS = '|sessionId|model|name|clientName|workingDirectory' +
    '|configDirectory|enableConfigDiscovery|systemMessage|diagnostics' +
    '|lifecycleDiagnostics|cloud|traceparent|tracestate|suppressResumeEvent' +
    '|continuePendingWork|allowTranscriptRecovery' +
    '|requestedEnvironmentVariables|';

  // Flat wire key => group.member (docs/v2-breaking-changes.md).
  GROUP_TABLE =
    '|reasoningEffort=inference.reasoningEffort' +
    '|reasoningSummary=inference.reasoningSummary' +
    '|contextTier=inference.contextTier' +
    '|allowMultipleToolCalls=inference.allowMultipleToolCalls' +
    '|enableCitations=inference.enableCitations' +
    '|modelCapabilities=inference.capabilities' +
    '|verbosity=inference.verbosity' +
    '|providers=inference.providers' +
    '|models=inference.models' +
    '|defaultProvider=inference.defaultProvider' +
    '|tools=tools.definitions' +
    '|availableTools=tools.availableTools' +
    '|excludedTools=tools.excludedTools' +
    '|toolSearch=tools.search' +
    '|largeOutput=tools.largeOutput' +
    '|imageGeneration=tools.imageGeneration' +
    '|manageScheduleEnabled=tools.manageScheduleEnabled' +
    '|webSearch=tools.webSearch' +
    '|shell=tools.shell' +
    '|maxInlineBinaryBytes=tools.maxInlineBinaryBytes' +
    '|requestPermission=permissions.request' +
    '|sandboxConfig=permissions.sandbox' +
    '|sandbox=permissions.sandbox' +
    '|permissionMode=permissions.mode' +
    '|sandboxConfigSource=permissions.sandboxSource' +
    '|commands=interaction.commands' +
    '|askUserVariant=interaction.askUserVariant' +
    '|requestUserInput=interaction.requestUserInput' +
    '|requestElicitation=interaction.requestElicitation' +
    '|requestShellAuthentication=interaction.requestShellAuthentication' +
    '|requestExitPlanMode=interaction.requestExitPlanMode' +
    '|requestAutoModeSwitch=interaction.requestAutoModeSwitch' +
    '|requestAutoModeRecovery=interaction.requestAutoModeRecovery' +
    '|streaming=events.streaming' +
    '|includeSubAgentStreamingEvents=events.includeSubAgentStreamingEvents' +
    '|customAgents=agents.definitions' +
    '|defaultAgent=agents.default' +
    '|agent=agents.selected' +
    '|includedBuiltinAgents=agents.includedBuiltinAgents' +
    '|excludedBuiltinAgents=agents.excludedBuiltinAgents' +
    '|customAgentsLocalOnly=agents.localDiscoveryOnly' +
    '|agentDirectories=agents.directories' +
    '|skillDirectories=skills.directories' +
    '|enableSkills=skills.enabled' +
    '|includedBuiltinSkills=skills.includedBuiltinSkills' +
    '|disabledSkills=skills.disabledSkills' +
    '|hasSkillProvider=skills.hasProvider' +
    '|ignoredSkillsLocations=skills.ignoredLocations' +
    '|mcpServers=mcp.servers' +
    '|disabledMcpServers=mcp.disabledServers' +
    '|requestMcpApps=mcp.enableApps' +
    '|enableMcpApps=mcp.enableApps' +
    '|mcpOAuthTokenStorage=mcp.oauthTokenStorage' +
    '|authClientIdMetadataUrl=mcp.authClientIdMetadataUrl' +
    '|envValueMode=mcp.envValueMode' +
    '|managedMcpServers=mcp.managedServers' +
    '|allowAllMcpServerInstructions=mcp.allowAllServerInstructions' +
    '|gitHubToken=github.token' +
    '|gitHubTokenProviderRegistrationId=github.tokenProviderRegistrationId' +
    '|githubMcpToolConfig=github.mcp' +
    '|capi=github.capi' +
    '|enableSessionTelemetry=github.telemetryEnabled' +
    '|enableManagedSettings=github.fetchManagedSettings' +
    '|interactiveUser=github.interactiveUser' +
    '|enableGitHubTelemetryForwarding=github.telemetryForwarding' +
    '|authInfo=github.authInfo' +
    '|copilotUrl=github.copilotUrl' +
    '|additionalDirectories=filesystem.additionalDirectories' +
    '|enableFileChangeTracking=filesystem.trackFileChanges' +
    '|trustWorkingDirectory=filesystem.trustWorkingDirectory' +
    '|sessionLimits=execution.limits' +
    '|autopilot=execution.autopilot' +
    '|autopilotContinuation=execution.autopilotContinuation' +
    '|instructionDirectories=instructions.directories' +
    '|organizationCustomInstructions=instructions.organizationText' +
    '|enableOnDemandInstructionDiscovery=instructions.onDemandDiscovery' +
    '|skipCustomInstructions=instructions.skipCustomInstructions' +
    '|refreshCustomInstructions=instructions.refreshCustomInstructions' +
    '|hooks=hooks.hasHandlers' +
    '|hookDirectories=hooks.directories' +
    '|enableFileHooks=hooks.enableRepositoryHooks' +
    '|coauthorEnabled=git.coauthorEnabled' +
    '|enableHostGitOperations=git.enableHostGitOperations' +
    '|infiniteSessions=state.infiniteSessions' +
    '|memory=state.memory' +
    '|hasMemoryStore=state.hasMemoryStore' +
    '|enableSessionStore=state.enableCrossSessionStore' +
    '|skipEmbeddingRetrieval=state.skipEmbeddingRetrieval' +
    '|embeddingCacheStorage=state.embeddingCacheStorage' +
    '|pluginDirectories=plugins.directories' +
    '|installedPlugins=plugins.installed' +
    '|canvases=canvases.definitions' +
    '|canvasProvider=canvases.provider' +
    '|requestCanvasRenderer=canvases.requestRenderer' +
    '|openCanvases=canvases.open' +
    '|requestExtensions=extensions.enabled' +
    '|extensionSdkPath=extensions.sdkPath' +
    '|extensionInfo=extensions.identity' +
    '|enableExperimentalMode=experiments.enableExperimentalMode' +
    '|isExperimentalMode=experiments.enableExperimentalMode' +
    '|featureFlags=experiments.flags' +
    '|expAssignments=experiments.assignments' +
    '|featureFlagConfiguration=experiments.configuration' +
    '|allowedModels=policy.allowedModels' +
    '|managedSettings=policy.managedSettings' +
    '|excludedManagedSettings=policy.excludedManagedSettings' +
    '|enforceManagedModelDefaults=policy.enforceManagedModelDefaults' +
    '|';

  // Fields of the legacy single provider that describe the model.
  PROVIDER_MODEL_KEYS = '|modelId|wireModel|maxPromptTokens' +
    '|maxContextWindowTokens|maxOutputTokens|capabilities|';

function InList(const List, Key: string): Boolean;
begin
  Result := Pos('|' + Key + '|', List) > 0;
end;

// Look up the group.member destination of a flat key.
function LookupMember(const Key: string; out Group, Member: string): Boolean;
var
  Needle, Rest, Dest: string;
  P, Dot: Integer;
begin
  Result := False;
  Group := '';
  Member := '';
  Needle := '|' + Key + '=';
  P := Pos(Needle, GROUP_TABLE);
  if P = 0 then
    Exit;
  Rest := Copy(GROUP_TABLE, P + Length(Needle), MaxInt);
  Dest := Copy(Rest, 1, Pos('|', Rest) - 1);
  Dot := Pos('.', Dest);
  Group := Copy(Dest, 1, Dot - 1);
  Member := Copy(Dest, Dot + 1, MaxInt);
  Result := True;
end;

function GroupOf(Root: TJSONObject; const Name: string): TJSONObject;
var
  Existing: TJSONValue;
begin
  Existing := Root.GetValue(Name);
  if Existing is TJSONObject then
    Exit(TJSONObject(Existing));
  Result := TJSONObject.Create;
  Root.AddPair(Name, Result);
end;

function HasMember(Root: TJSONObject; const Group, Member: string): Boolean;
var
  Existing: TJSONValue;
begin
  Existing := Root.GetValue(Group);
  Result := (Existing is TJSONObject) and
    (TJSONObject(Existing).GetValue(Member) <> nil);
end;

// Add a copy of Value as group.member unless it is null or already present
// (earlier values win, so explicit grouped options win over flat keys).
procedure AddMember(Root: TJSONObject; const Group, Member: string;
  Value: TJSONValue; AllowNull: Boolean = False);
var
  Target: TJSONObject;
begin
  if (Value = nil) or ((Value is TJSONNull) and not AllowNull) then
    Exit;
  Target := GroupOf(Root, Group);
  if Target.GetValue(Member) = nil then
    Target.AddPair(Member, Value.Clone as TJSONValue);
end;

procedure AddStringMember(Root: TJSONObject; const Group, Member, Value: string);
var
  Temp: TJSONString;
begin
  Temp := TJSONString.Create(Value);
  try
    AddMember(Root, Group, Member, Temp);
  finally
    Temp.Free;
  end;
end;

procedure AddBoolMember(Root: TJSONObject; const Group, Member: string;
  Value: Boolean);
var
  Temp: TJSONBool;
begin
  Temp := TJSONBool.Create(Value);
  try
    AddMember(Root, Group, Member, Temp);
  finally
    Temp.Free;
  end;
end;

procedure AddRoot(Root: TJSONObject; const Key: string; Value: TJSONValue);
begin
  if Root.GetValue(Key) = nil then
    Root.AddPair(Key, Value.Clone as TJSONValue);
end;

// Legacy single BYOK provider => named provider "default".
procedure TranslateLegacyProvider(Root, Provider: TJSONObject;
  ModelValue: TJSONValue);
var
  Pair: TJSONPair;
  Connection, ModelEntry, Capi: TJSONObject;
  List: TJSONArray;
  HasModelFields: Boolean;
begin
  // Explicit named providers, default provider or capi settings win.
  if HasMember(Root, 'inference', 'providers') or
    HasMember(Root, 'inference', 'defaultProvider') or
    HasMember(Root, 'github', 'capi') then
    Exit;

  Connection := TJSONObject.Create;
  ModelEntry := TJSONObject.Create;
  try
    Connection.AddPair('name', 'default');
    ModelEntry.AddPair('provider', 'default');
    if Assigned(ModelValue) then
      ModelEntry.AddPair('id', ModelValue.Clone as TJSONValue);
    HasModelFields := False;
    for Pair in Provider do
    begin
      if (Pair.JsonString.Value = 'name') or (Pair.JsonValue is TJSONNull) then
        Continue;
      if InList(PROVIDER_MODEL_KEYS, Pair.JsonString.Value) then
      begin
        ModelEntry.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);
        HasModelFields := True;
      end
      else
        Connection.AddPair(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);
    end;

    List := TJSONArray.Create;
    try
      List.AddElement(Connection.Clone as TJSONValue);
      AddMember(Root, 'inference', 'providers', List);
    finally
      List.Free;
    end;
    AddStringMember(Root, 'inference', 'defaultProvider', 'default');
    Capi := TJSONObject.Create;
    try
      Capi.AddPair('enabled', TJSONBool.Create(False));
      AddMember(Root, 'github', 'capi', Capi);
    finally
      Capi.Free;
    end;

    if HasModelFields and Assigned(ModelValue) then
    begin
      List := TJSONArray.Create;
      try
        List.AddElement(ModelEntry.Clone as TJSONValue);
        AddMember(Root, 'inference', 'models', List);
      finally
        List.Free;
      end;
    end;
  finally
    Connection.Free;
    ModelEntry.Free;
  end;
end;

procedure ApplyEmptyModeDefaults(Root: TJSONObject);
var
  Memory: TJSONObject;
begin
  AddBoolMember(Root, 'experiments', 'enableExperimentalMode', False);
  AddBoolMember(Root, 'github', 'telemetryEnabled', False);
  AddStringMember(Root, 'mcp', 'oauthTokenStorage', 'in-memory');
  AddBoolMember(Root, 'state', 'skipEmbeddingRetrieval', True);
  AddStringMember(Root, 'state', 'embeddingCacheStorage', 'in-memory');
  AddBoolMember(Root, 'state', 'enableCrossSessionStore', False);
  Memory := TJSONObject.Create;
  try
    Memory.AddPair('enabled', TJSONBool.Create(False));
    AddMember(Root, 'state', 'memory', Memory);
  finally
    Memory.Free;
  end;
  AddBoolMember(Root, 'instructions', 'onDemandDiscovery', False);
  AddBoolMember(Root, 'hooks', 'enableRepositoryHooks', False);
  AddBoolMember(Root, 'git', 'enableHostGitOperations', False);
end;

function GroupSessionParams(const Flat: TJSONObject; IsResume: Boolean;
  EmptyMode: Boolean): TJSONObject;
var
  Pair, Inner: TJSONPair;
  Key, Group, Member: string;
  Value, ModelValue: TJSONValue;
  Provider: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Provider := nil;
    ModelValue := nil;

    // First pass: members of objects that are already grouped win.
    for Pair in Flat do
      if InList(GROUP_NAMES, Pair.JsonString.Value) and
        (Pair.JsonValue is TJSONObject) then
        for Inner in TJSONObject(Pair.JsonValue) do
          AddMember(Result, Pair.JsonString.Value, Inner.JsonString.Value,
            Inner.JsonValue);

    // Second pass: place every flat key.
    for Pair in Flat do
    begin
      Key := Pair.JsonString.Value;
      Value := Pair.JsonValue;
      if InList(GROUP_NAMES, Key) and (Value is TJSONObject) then
        Continue;
      if Value is TJSONNull then
      begin
        // An explicit null defaultProvider clears it on resume.
        if IsResume and (Key = 'defaultProvider') then
          AddMember(Result, 'inference', Key, Value, True);
        Continue;
      end;
      if Key = 'model' then
        ModelValue := Value;

      if Key = 'configDir' then
        AddRoot(Result, 'configDirectory', Value)
      else if (Key = 'disableResume') and IsResume then
        AddRoot(Result, 'suppressResumeEvent', Value)
      else if Key = 'provider' then
      begin
        if Value is TJSONObject then
          Provider := TJSONObject(Value);
      end
      else if IsResume and ((Key = 'trustWorkingDirectory') or
        (Key = 'refreshCustomInstructions')) then
        Continue // create-only options
      else if (not IsResume) and (Key = 'openCanvases') then
        Continue // resume-only option
      else if InList(ROOT_KEYS, Key) then
        AddRoot(Result, Key, Value)
      else if LookupMember(Key, Group, Member) then
        AddMember(Result, Group, Member, Value)
      else if InList(GROUP_NAMES, Key) then
        Continue // a group name with a non-object value is rejected by protocol 4
      else
        AddRoot(Result, Key, Value); // unknown keys stay at the root
    end;

    if Assigned(Provider) then
      TranslateLegacyProvider(Result, Provider, ModelValue);

    if HasMember(Result, 'mcp', 'servers') then
      AddStringMember(Result, 'mcp', 'envValueMode', 'direct');

    if EmptyMode then
      ApplyEmptyModeDefaults(Result);
  except
    Result.Free;
    raise;
  end;
end;

end.
