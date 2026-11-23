## SDK protocol 4 session payload translation for the GitHub Copilot SDK for Nim.
##
## The SDK builds a flat `session.create` / `session.resume` payload from
## `SessionConfig`. Protocol 4 servers expect most options inside named
## groups (`inference`, `tools`, `permissions`, ...). `groupSessionParams`
## converts the flat payload right before it is sent, so users keep passing
## the same options they always have.

import std/json

const
  SessionRootKeys = [
    "sessionId", "model", "name", "clientName", "workingDirectory",
    "configDirectory", "enableConfigDiscovery", "systemMessage", "diagnostics",
    "lifecycleDiagnostics", "cloud", "traceparent", "tracestate",
    "suppressResumeEvent", "continuePendingWork", "allowTranscriptRecovery",
    "requestedEnvironmentVariables",
  ]

  SessionGroupNames = [
    "inference", "tools", "permissions", "interaction", "events", "agents",
    "skills", "mcp", "github", "filesystem", "execution", "instructions",
    "hooks", "git", "state", "plugins", "canvases", "extensions",
    "experiments", "policy",
  ]

  CreateOnlyGroupedKeys = ["trustWorkingDirectory", "refreshCustomInstructions"]
  ResumeOnlyGroupedKeys = ["openCanvases"]

  # Flat wire key -> (group, member).
  SessionGroupedKeys = [
    ("reasoningEffort", "inference", "reasoningEffort"),
    ("reasoningSummary", "inference", "reasoningSummary"),
    ("contextTier", "inference", "contextTier"),
    ("allowMultipleToolCalls", "inference", "allowMultipleToolCalls"),
    ("enableCitations", "inference", "enableCitations"),
    ("modelCapabilities", "inference", "capabilities"),
    ("verbosity", "inference", "verbosity"),
    ("providers", "inference", "providers"),
    ("models", "inference", "models"),
    ("defaultProvider", "inference", "defaultProvider"),
    ("tools", "tools", "definitions"),
    ("availableTools", "tools", "availableTools"),
    ("excludedTools", "tools", "excludedTools"),
    ("toolSearch", "tools", "search"),
    ("largeOutput", "tools", "largeOutput"),
    ("imageGeneration", "tools", "imageGeneration"),
    ("manageScheduleEnabled", "tools", "manageScheduleEnabled"),
    ("webSearch", "tools", "webSearch"),
    ("shell", "tools", "shell"),
    ("maxInlineBinaryBytes", "tools", "maxInlineBinaryBytes"),
    ("requestPermission", "permissions", "request"),
    ("sandboxConfig", "permissions", "sandbox"),
    ("sandbox", "permissions", "sandbox"),
    ("permissionMode", "permissions", "mode"),
    ("sandboxConfigSource", "permissions", "sandboxSource"),
    ("commands", "interaction", "commands"),
    ("askUserVariant", "interaction", "askUserVariant"),
    ("requestUserInput", "interaction", "requestUserInput"),
    ("requestElicitation", "interaction", "requestElicitation"),
    ("requestShellAuthentication", "interaction", "requestShellAuthentication"),
    ("requestExitPlanMode", "interaction", "requestExitPlanMode"),
    ("requestAutoModeSwitch", "interaction", "requestAutoModeSwitch"),
    ("requestAutoModeRecovery", "interaction", "requestAutoModeRecovery"),
    ("streaming", "events", "streaming"),
    ("includeSubAgentStreamingEvents", "events", "includeSubAgentStreamingEvents"),
    ("customAgents", "agents", "definitions"),
    ("defaultAgent", "agents", "default"),
    ("agent", "agents", "selected"),
    ("includedBuiltinAgents", "agents", "includedBuiltinAgents"),
    ("excludedBuiltinAgents", "agents", "excludedBuiltinAgents"),
    ("customAgentsLocalOnly", "agents", "localDiscoveryOnly"),
    ("agentDirectories", "agents", "directories"),
    ("skillDirectories", "skills", "directories"),
    ("enableSkills", "skills", "enabled"),
    ("includedBuiltinSkills", "skills", "includedBuiltinSkills"),
    ("disabledSkills", "skills", "disabledSkills"),
    ("hasSkillProvider", "skills", "hasProvider"),
    ("ignoredSkillsLocations", "skills", "ignoredLocations"),
    ("mcpServers", "mcp", "servers"),
    ("disabledMcpServers", "mcp", "disabledServers"),
    ("requestMcpApps", "mcp", "enableApps"),
    ("enableMcpApps", "mcp", "enableApps"),
    ("mcpOAuthTokenStorage", "mcp", "oauthTokenStorage"),
    ("authClientIdMetadataUrl", "mcp", "authClientIdMetadataUrl"),
    ("envValueMode", "mcp", "envValueMode"),
    ("managedMcpServers", "mcp", "managedServers"),
    ("allowAllMcpServerInstructions", "mcp", "allowAllServerInstructions"),
    ("gitHubToken", "github", "token"),
    ("gitHubTokenProviderRegistrationId", "github", "tokenProviderRegistrationId"),
    ("githubMcpToolConfig", "github", "mcp"),
    ("capi", "github", "capi"),
    ("enableSessionTelemetry", "github", "telemetryEnabled"),
    ("enableManagedSettings", "github", "fetchManagedSettings"),
    ("interactiveUser", "github", "interactiveUser"),
    ("enableGitHubTelemetryForwarding", "github", "telemetryForwarding"),
    ("authInfo", "github", "authInfo"),
    ("copilotUrl", "github", "copilotUrl"),
    ("additionalDirectories", "filesystem", "additionalDirectories"),
    ("enableFileChangeTracking", "filesystem", "trackFileChanges"),
    ("trustWorkingDirectory", "filesystem", "trustWorkingDirectory"),
    ("sessionLimits", "execution", "limits"),
    ("autopilot", "execution", "autopilot"),
    ("autopilotContinuation", "execution", "autopilotContinuation"),
    ("instructionDirectories", "instructions", "directories"),
    ("organizationCustomInstructions", "instructions", "organizationText"),
    ("enableOnDemandInstructionDiscovery", "instructions", "onDemandDiscovery"),
    ("skipCustomInstructions", "instructions", "skipCustomInstructions"),
    ("refreshCustomInstructions", "instructions", "refreshCustomInstructions"),
    ("hooks", "hooks", "hasHandlers"),
    ("hookDirectories", "hooks", "directories"),
    ("enableFileHooks", "hooks", "enableRepositoryHooks"),
    ("coauthorEnabled", "git", "coauthorEnabled"),
    ("enableHostGitOperations", "git", "enableHostGitOperations"),
    ("infiniteSessions", "state", "infiniteSessions"),
    ("memory", "state", "memory"),
    ("hasMemoryStore", "state", "hasMemoryStore"),
    ("enableSessionStore", "state", "enableCrossSessionStore"),
    ("skipEmbeddingRetrieval", "state", "skipEmbeddingRetrieval"),
    ("embeddingCacheStorage", "state", "embeddingCacheStorage"),
    ("pluginDirectories", "plugins", "directories"),
    ("installedPlugins", "plugins", "installed"),
    ("canvases", "canvases", "definitions"),
    ("canvasProvider", "canvases", "provider"),
    ("requestCanvasRenderer", "canvases", "requestRenderer"),
    ("openCanvases", "canvases", "open"),
    ("requestExtensions", "extensions", "enabled"),
    ("extensionSdkPath", "extensions", "sdkPath"),
    ("extensionInfo", "extensions", "identity"),
    ("enableExperimentalMode", "experiments", "enableExperimentalMode"),
    ("isExperimentalMode", "experiments", "enableExperimentalMode"),
    ("featureFlags", "experiments", "flags"),
    ("expAssignments", "experiments", "assignments"),
    ("featureFlagConfiguration", "experiments", "configuration"),
    ("allowedModels", "policy", "allowedModels"),
    ("managedSettings", "policy", "managedSettings"),
    ("excludedManagedSettings", "policy", "excludedManagedSettings"),
    ("enforceManagedModelDefaults", "policy", "enforceManagedModelDefaults"),
  ]

  # Legacy `provider` members that describe the model rather than the connection.
  LegacyProviderModelFields = [
    "modelId", "wireModel", "maxPromptTokens", "maxContextWindowTokens",
    "maxOutputTokens", "capabilities",
  ]

proc findGroupedKey(key: string): int =
  for i, entry in SessionGroupedKeys:
    if entry[0] == key:
      return i
  -1

proc putMember(groups: JsonNode; group, member: string; value: JsonNode) =
  if not groups.hasKey(group):
    groups[group] = newJObject()
  groups[group][member] = value

proc hasMember(groups: JsonNode; group, member: string): bool =
  groups.hasKey(group) and groups[group].hasKey(member)

proc translateLegacyProvider(groups, provider, model: JsonNode) =
  ## v1 BYOK: a single provider becomes the named "default" connection with
  ## Copilot models disabled, plus a model definition for model-specific fields.
  let connection = %*{"name": "default"}
  let modelFields = newJObject()
  for key, value in provider.pairs:
    if value.isNil or value.kind == JNull or key == "name":
      continue
    if key in LegacyProviderModelFields:
      modelFields[key] = value
    else:
      connection[key] = value
  let providers = newJArray()
  providers.add(connection)
  groups.putMember("inference", "providers", providers)
  groups.putMember("inference", "defaultProvider", %"default")
  groups.putMember("github", "capi", %*{"enabled": false})
  if modelFields.len > 0 and not groups.hasMember("inference", "models") and
      not model.isNil and model.kind == JString and model.getStr().len > 0:
    let entry = %*{"provider": "default", "id": model.getStr()}
    for key, value in modelFields.pairs:
      entry[key] = value
    let models = newJArray()
    models.add(entry)
    groups.putMember("inference", "models", models)

proc groupSessionParams*(flat: JsonNode; isResume: bool): JsonNode =
  ## Converts a flat `session.create` / `session.resume` payload into the
  ## grouped SDK protocol 4 payload.
  ##
  ## Root keys stay at the top level, known flat keys move to their group
  ## member, and unknown keys stay at the root. Null values are skipped and
  ## empty groups are not emitted. Members of an already-grouped object win
  ## over the equivalent flat keys.
  result = newJObject()
  if flat.isNil or flat.kind != JObject:
    return
  let groups = newJObject()
  let explicitGroups = newJObject()
  var legacyProvider: JsonNode = nil
  var webSocketResponses: JsonNode = nil
  for key, value in flat.pairs:
    if value.isNil:
      continue
    if value.kind == JNull and not (isResume and key == "defaultProvider"):
      continue
    if key in SessionGroupNames and value.kind == JObject:
      explicitGroups[key] = value
      continue
    case key
    of "configDir":
      if not flat.hasKey("configDirectory"):
        result["configDirectory"] = value
    of "disableResume":
      if not flat.hasKey("suppressResumeEvent"):
        result["suppressResumeEvent"] = value
    of "provider":
      if value.kind == JObject:
        legacyProvider = value
    of "enableWebSocketResponses":
      webSocketResponses = value
    else:
      if key in SessionRootKeys:
        result[key] = value
      elif isResume and key in CreateOnlyGroupedKeys:
        discard
      elif not isResume and key in ResumeOnlyGroupedKeys:
        discard
      else:
        let idx = findGroupedKey(key)
        if idx >= 0:
          let (_, group, member) = SessionGroupedKeys[idx]
          groups.putMember(group, member, value)
        else:
          result[key] = value

  for group, members in explicitGroups.pairs:
    for member, value in members.pairs:
      if value.isNil:
        continue
      if value.kind == JNull and not (isResume and group == "inference" and
                                      member == "defaultProvider"):
        continue
      groups.putMember(group, member, value)

  if not legacyProvider.isNil and
      not groups.hasMember("inference", "providers") and
      not groups.hasMember("inference", "defaultProvider") and
      not groups.hasMember("github", "capi"):
    translateLegacyProvider(groups, legacyProvider, flat{"model"})

  # The legacy WebSocket flag is a Copilot API option in protocol 4.
  if not webSocketResponses.isNil:
    if not groups.hasMember("github", "capi") or groups["github"]["capi"].kind != JObject:
      groups.putMember("github", "capi", newJObject())
    let capi = groups["github"]["capi"]
    if not capi.hasKey("enableWebSocketResponses"):
      capi["enableWebSocketResponses"] = webSocketResponses

  if groups.hasMember("mcp", "servers") and not groups.hasMember("mcp", "envValueMode"):
    groups.putMember("mcp", "envValueMode", %"direct")

  for group, members in groups.pairs:
    if members.len > 0:
      result[group] = members

proc setDefaultMember(payload: JsonNode; group, member: string; value: JsonNode) =
  if not payload.hasKey(group) or payload[group].kind != JObject:
    payload[group] = newJObject()
  if not payload[group].hasKey(member):
    payload[group][member] = value

proc applyEmptyModeDefaults*(payload: JsonNode) =
  ## Fills the conservative "empty" client mode defaults into a grouped
  ## payload without overriding values the user supplied.
  payload.setDefaultMember("experiments", "enableExperimentalMode", %false)
  payload.setDefaultMember("github", "telemetryEnabled", %false)
  payload.setDefaultMember("mcp", "oauthTokenStorage", %"in-memory")
  payload.setDefaultMember("state", "skipEmbeddingRetrieval", %true)
  payload.setDefaultMember("state", "embeddingCacheStorage", %"in-memory")
  payload.setDefaultMember("state", "enableCrossSessionStore", %false)
  payload.setDefaultMember("state", "memory", %*{"enabled": false})
  payload.setDefaultMember("instructions", "onDemandDiscovery", %false)
  payload.setDefaultMember("hooks", "enableRepositoryHooks", %false)
  payload.setDefaultMember("git", "enableHostGitOperations", %false)
