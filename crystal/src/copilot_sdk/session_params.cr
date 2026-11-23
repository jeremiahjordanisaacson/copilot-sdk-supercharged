# Copyright (c) Microsoft Corporation. All rights reserved.

# SDK protocol 4 session payload grouping.
#
# The client keeps building the flat (protocol 3) `session.create` /
# `session.resume` payload from the session configuration.
# `CopilotSDK.group_session_params` converts it into the grouped protocol 4
# shape right before the request is sent.

require "json"

module CopilotSDK
  # Option groups of the protocol 4 session payload.
  SESSION_PARAM_GROUP_NAMES = %w(inference tools permissions interaction events agents skills mcp
    github filesystem execution instructions hooks git state plugins canvases extensions experiments
    policy)

  # Flat keys only valid on `session.create` (dropped on resume).
  CREATE_ONLY_SESSION_PARAMS = %w(name cloud trustWorkingDirectory refreshCustomInstructions)

  # Flat keys only valid on `session.resume` (dropped on create).
  RESUME_ONLY_SESSION_PARAMS = %w(disableResume suppressResumeEvent continuePendingWork
    allowTranscriptRecovery requestedEnvironmentVariables openCanvases)

  # Flat (protocol 3) key => {group, member} of the protocol 4 payload.
  #
  # Root keys (`sessionId`, `model`, `name`, `clientName`, `workingDirectory`,
  # `configDirectory`, `enableConfigDiscovery`, `systemMessage`, `diagnostics`,
  # `lifecycleDiagnostics`, `cloud`, `traceparent`, `tracestate` and the resume
  # controls) and unknown keys stay top level.
  SESSION_PARAM_GROUPS = {
    "reasoningEffort"                    => {"inference", "reasoningEffort"},
    "reasoningSummary"                   => {"inference", "reasoningSummary"},
    "contextTier"                        => {"inference", "contextTier"},
    "allowMultipleToolCalls"             => {"inference", "allowMultipleToolCalls"},
    "enableCitations"                    => {"inference", "enableCitations"},
    "modelCapabilities"                  => {"inference", "capabilities"},
    "verbosity"                          => {"inference", "verbosity"},
    "providers"                          => {"inference", "providers"},
    "models"                             => {"inference", "models"},
    "defaultProvider"                    => {"inference", "defaultProvider"},
    "tools"                              => {"tools", "definitions"},
    "availableTools"                     => {"tools", "availableTools"},
    "excludedTools"                      => {"tools", "excludedTools"},
    "toolSearch"                         => {"tools", "search"},
    "largeOutput"                        => {"tools", "largeOutput"},
    "imageGeneration"                    => {"tools", "imageGeneration"},
    "manageScheduleEnabled"              => {"tools", "manageScheduleEnabled"},
    "webSearch"                          => {"tools", "webSearch"},
    "shell"                              => {"tools", "shell"},
    "maxInlineBinaryBytes"               => {"tools", "maxInlineBinaryBytes"},
    "requestPermission"                  => {"permissions", "request"},
    "sandboxConfig"                      => {"permissions", "sandbox"},
    "sandbox"                            => {"permissions", "sandbox"},
    "permissionMode"                     => {"permissions", "mode"},
    "sandboxConfigSource"                => {"permissions", "sandboxSource"},
    "commands"                           => {"interaction", "commands"},
    "askUserVariant"                     => {"interaction", "askUserVariant"},
    "requestUserInput"                   => {"interaction", "requestUserInput"},
    "requestElicitation"                 => {"interaction", "requestElicitation"},
    "requestShellAuthentication"         => {"interaction", "requestShellAuthentication"},
    "requestExitPlanMode"                => {"interaction", "requestExitPlanMode"},
    "requestAutoModeSwitch"              => {"interaction", "requestAutoModeSwitch"},
    "requestAutoModeRecovery"            => {"interaction", "requestAutoModeRecovery"},
    "streaming"                          => {"events", "streaming"},
    "includeSubAgentStreamingEvents"     => {"events", "includeSubAgentStreamingEvents"},
    "customAgents"                       => {"agents", "definitions"},
    "defaultAgent"                       => {"agents", "default"},
    "agent"                              => {"agents", "selected"},
    "includedBuiltinAgents"              => {"agents", "includedBuiltinAgents"},
    "excludedBuiltinAgents"              => {"agents", "excludedBuiltinAgents"},
    "customAgentsLocalOnly"              => {"agents", "localDiscoveryOnly"},
    "agentDirectories"                   => {"agents", "directories"},
    "skillDirectories"                   => {"skills", "directories"},
    "enableSkills"                       => {"skills", "enabled"},
    "includedBuiltinSkills"              => {"skills", "includedBuiltinSkills"},
    "disabledSkills"                     => {"skills", "disabledSkills"},
    "hasSkillProvider"                   => {"skills", "hasProvider"},
    "ignoredSkillsLocations"             => {"skills", "ignoredLocations"},
    "mcpServers"                         => {"mcp", "servers"},
    "disabledMcpServers"                 => {"mcp", "disabledServers"},
    "requestMcpApps"                     => {"mcp", "enableApps"},
    "enableMcpApps"                      => {"mcp", "enableApps"},
    "mcpOAuthTokenStorage"               => {"mcp", "oauthTokenStorage"},
    "authClientIdMetadataUrl"            => {"mcp", "authClientIdMetadataUrl"},
    "envValueMode"                       => {"mcp", "envValueMode"},
    "managedMcpServers"                  => {"mcp", "managedServers"},
    "allowAllMcpServerInstructions"      => {"mcp", "allowAllServerInstructions"},
    "gitHubToken"                        => {"github", "token"},
    "gitHubTokenProviderRegistrationId"  => {"github", "tokenProviderRegistrationId"},
    "githubMcpToolConfig"                => {"github", "mcp"},
    "capi"                               => {"github", "capi"},
    "enableSessionTelemetry"             => {"github", "telemetryEnabled"},
    "enableManagedSettings"              => {"github", "fetchManagedSettings"},
    "interactiveUser"                    => {"github", "interactiveUser"},
    "enableGitHubTelemetryForwarding"    => {"github", "telemetryForwarding"},
    "authInfo"                           => {"github", "authInfo"},
    "copilotUrl"                         => {"github", "copilotUrl"},
    "additionalDirectories"              => {"filesystem", "additionalDirectories"},
    "enableFileChangeTracking"           => {"filesystem", "trackFileChanges"},
    "trustWorkingDirectory"              => {"filesystem", "trustWorkingDirectory"},
    "sessionLimits"                      => {"execution", "limits"},
    "autopilot"                          => {"execution", "autopilot"},
    "autopilotContinuation"              => {"execution", "autopilotContinuation"},
    "instructionDirectories"             => {"instructions", "directories"},
    "organizationCustomInstructions"     => {"instructions", "organizationText"},
    "enableOnDemandInstructionDiscovery" => {"instructions", "onDemandDiscovery"},
    "skipCustomInstructions"             => {"instructions", "skipCustomInstructions"},
    "refreshCustomInstructions"          => {"instructions", "refreshCustomInstructions"},
    "hooks"                              => {"hooks", "hasHandlers"},
    "hookDirectories"                    => {"hooks", "directories"},
    "enableFileHooks"                    => {"hooks", "enableRepositoryHooks"},
    "coauthorEnabled"                    => {"git", "coauthorEnabled"},
    "enableHostGitOperations"            => {"git", "enableHostGitOperations"},
    "infiniteSessions"                   => {"state", "infiniteSessions"},
    "memory"                             => {"state", "memory"},
    "hasMemoryStore"                     => {"state", "hasMemoryStore"},
    "enableSessionStore"                 => {"state", "enableCrossSessionStore"},
    "skipEmbeddingRetrieval"             => {"state", "skipEmbeddingRetrieval"},
    "embeddingCacheStorage"              => {"state", "embeddingCacheStorage"},
    "pluginDirectories"                  => {"plugins", "directories"},
    "installedPlugins"                   => {"plugins", "installed"},
    "canvases"                           => {"canvases", "definitions"},
    "canvasProvider"                     => {"canvases", "provider"},
    "requestCanvasRenderer"              => {"canvases", "requestRenderer"},
    "openCanvases"                       => {"canvases", "open"},
    "requestExtensions"                  => {"extensions", "enabled"},
    "extensionSdkPath"                   => {"extensions", "sdkPath"},
    "extensionInfo"                      => {"extensions", "identity"},
    "enableExperimentalMode"             => {"experiments", "enableExperimentalMode"},
    "isExperimentalMode"                 => {"experiments", "enableExperimentalMode"},
    "featureFlags"                       => {"experiments", "flags"},
    "expAssignments"                     => {"experiments", "assignments"},
    "featureFlagConfiguration"           => {"experiments", "configuration"},
    "allowedModels"                      => {"policy", "allowedModels"},
    "managedSettings"                    => {"policy", "managedSettings"},
    "excludedManagedSettings"            => {"policy", "excludedManagedSettings"},
    "enforceManagedModelDefaults"        => {"policy", "enforceManagedModelDefaults"},
  }

  # Legacy single-provider fields that describe the model, not the connection.
  LEGACY_PROVIDER_MODEL_FIELDS = %w(modelId wireModel maxPromptTokens maxContextWindowTokens
    maxOutputTokens capabilities)

  # Session defaults of the `"empty"` client mode: {group, member, value}.
  EMPTY_MODE_SESSION_DEFAULTS = [
    {"experiments", "enableExperimentalMode", JSON::Any.new(false)},
    {"github", "telemetryEnabled", JSON::Any.new(false)},
    {"mcp", "oauthTokenStorage", JSON::Any.new("in-memory")},
    {"state", "skipEmbeddingRetrieval", JSON::Any.new(true)},
    {"state", "embeddingCacheStorage", JSON::Any.new("in-memory")},
    {"state", "enableCrossSessionStore", JSON::Any.new(false)},
    {"state", "memory", JSON::Any.new({"enabled" => JSON::Any.new(false)})},
    {"instructions", "onDemandDiscovery", JSON::Any.new(false)},
    {"hooks", "enableRepositoryHooks", JSON::Any.new(false)},
    {"git", "enableHostGitOperations", JSON::Any.new(false)},
  ]

  # Converts the flat `session.create` / `session.resume` payload into the
  # grouped SDK protocol 4 shape.
  #
  # Known flat keys move into their option group (`streaming` becomes
  # `events.streaming`, a boolean `hooks` becomes `hooks.hasHandlers`), root
  # keys and unknown keys stay top level, null values and empty groups are
  # dropped (except an explicit null `defaultProvider` on resume, which clears
  # the default provider), and grouped objects already present in `flat` are
  # merged with their members winning. `configDir` becomes `configDirectory`
  # and `disableResume` becomes `suppressResumeEvent`. A legacy single
  # `provider` becomes a `"default"` named provider with Copilot API models
  # disabled, unless explicit `providers`, `defaultProvider` or `capi.enabled`
  # are present.
  def self.group_session_params(flat : Hash(String, JSON::Any), resume : Bool) : Hash(String, JSON::Any)
    payload = {} of String => JSON::Any
    groups = {} of String => Hash(String, JSON::Any)
    explicit_groups = {} of String => Hash(String, JSON::Any)
    legacy_provider : Hash(String, JSON::Any)? = nil

    flat.each do |key, value|
      next if value.raw.nil? && !(resume && key == "defaultProvider")
      next if resume && CREATE_ONLY_SESSION_PARAMS.includes?(key)
      next if !resume && RESUME_ONLY_SESSION_PARAMS.includes?(key)

      members = value.as_h?
      path = SESSION_PARAM_GROUPS[key]?
      if members && SESSION_PARAM_GROUP_NAMES.includes?(key)
        explicit_groups[key] = members
      elsif key == "configDir"
        payload["configDirectory"] = value
      elsif key == "disableResume"
        payload["suppressResumeEvent"] = value
      elsif members && key == "provider"
        legacy_provider = members
      elsif path
        (groups[path[0]] ||= {} of String => JSON::Any)[path[1]] = value
      else
        payload[key] = value
      end
    end

    explicit_groups.each do |name, members|
      target = (groups[name] ||= {} of String => JSON::Any)
      members.each do |member, value|
        clears_default_provider = resume && name == "inference" && member == "defaultProvider"
        next if value.raw.nil? && !clears_default_provider
        target[member] = value
      end
    end

    # Grouped members that only exist on the other operation are rejected by the runtime.
    if resume
      groups["filesystem"]?.try &.delete("trustWorkingDirectory")
      groups["instructions"]?.try &.delete("refreshCustomInstructions")
    else
      groups["canvases"]?.try &.delete("open")
    end

    if provider = legacy_provider
      group_legacy_provider(provider, payload["model"]?.try(&.as_s?), groups)
    end

    if (mcp = groups["mcp"]?) && mcp.has_key?("servers") && !mcp.has_key?("envValueMode")
      mcp["envValueMode"] = JSON::Any.new("direct")
    end

    groups.each do |name, members|
      payload[name] = JSON::Any.new(members) unless members.empty?
    end
    payload
  end

  # Translates a legacy single `provider` into `inference.providers`,
  # `inference.defaultProvider`, `github.capi.enabled = false` and, when the
  # session model is set and the provider carries model fields,
  # `inference.models`. Internal helper of `group_session_params`.
  def self.group_legacy_provider(provider : Hash(String, JSON::Any), model : String?,
                                 groups : Hash(String, Hash(String, JSON::Any))) : Nil
    inference = groups["inference"]? || {} of String => JSON::Any
    github = groups["github"]? || {} of String => JSON::Any
    capi = github["capi"]?.try(&.as_h?)
    explicit_capi = capi ? (capi.has_key?("enabled") && !capi["enabled"].raw.nil?) : false
    return if inference.has_key?("providers") || inference.has_key?("defaultProvider") || explicit_capi

    connection = {"name" => JSON::Any.new("default")}
    model_fields = {} of String => JSON::Any
    provider.each do |key, value|
      next if key == "name" || value.raw.nil?
      if LEGACY_PROVIDER_MODEL_FIELDS.includes?(key)
        model_fields[key] = value
      else
        connection[key] = value
      end
    end

    inference["providers"] = JSON::Any.new([JSON::Any.new(connection)])
    inference["defaultProvider"] = JSON::Any.new("default")
    wire_capi = capi ? capi.dup : {} of String => JSON::Any
    wire_capi["enabled"] = JSON::Any.new(false)
    github["capi"] = JSON::Any.new(wire_capi)
    if model && !model_fields.empty?
      entry = {"provider" => JSON::Any.new("default"), "id" => JSON::Any.new(model)}
      model_fields.each { |key, value| entry[key] = value unless entry.has_key?(key) }
      inference["models"] = JSON::Any.new([JSON::Any.new(entry)])
    end
    groups["inference"] = inference
    groups["github"] = github
  end

  # Applies the `"empty"` client mode session defaults to a grouped payload;
  # values the caller already set win.
  def self.apply_empty_mode_session_defaults(payload : Hash(String, JSON::Any)) : Hash(String, JSON::Any)
    EMPTY_MODE_SESSION_DEFAULTS.each do |entry|
      group, member, value = entry
      target = payload[group]?.try(&.as_h?) || {} of String => JSON::Any
      target[member] = value if !target.has_key?(member) || target[member].raw.nil?
      payload[group] = JSON::Any.new(target)
    end
    payload
  end
end
