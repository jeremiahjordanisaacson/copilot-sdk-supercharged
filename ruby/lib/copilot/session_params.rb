# frozen_string_literal: true

# Copyright (c) Microsoft Corporation. All rights reserved.

module Copilot
  # Option groups of the SDK protocol 4 session.create / session.resume payloads.
  SESSION_PARAM_GROUP_NAMES = %i[
    inference tools permissions interaction events agents skills mcp github filesystem
    execution instructions hooks git state plugins canvases extensions experiments policy
  ].freeze

  # Flat wire keys that are only valid on session.create (dropped on resume).
  SESSION_CREATE_ONLY_PARAMS = %i[name cloud trustWorkingDirectory refreshCustomInstructions].freeze

  # Flat wire keys that are only valid on session.resume (dropped on create).
  SESSION_RESUME_ONLY_PARAMS = %i[
    disableResume suppressResumeEvent continuePendingWork allowTranscriptRecovery
    requestedEnvironmentVariables openCanvases
  ].freeze

  # Flat (SDK protocol 3) wire keys and the SDK protocol 4 [group, member] they move to.
  # Root keys (sessionId, model, name, clientName, workingDirectory, configDirectory,
  # enableConfigDiscovery, systemMessage, diagnostics, lifecycleDiagnostics, cloud,
  # traceparent, tracestate and the resume controls) and unknown keys stay top level.
  SESSION_PARAM_GROUPS = {
    reasoningEffort: %i[inference reasoningEffort],
    reasoningSummary: %i[inference reasoningSummary],
    contextTier: %i[inference contextTier],
    allowMultipleToolCalls: %i[inference allowMultipleToolCalls],
    enableCitations: %i[inference enableCitations],
    modelCapabilities: %i[inference capabilities],
    verbosity: %i[inference verbosity],
    providers: %i[inference providers],
    models: %i[inference models],
    defaultProvider: %i[inference defaultProvider],
    tools: %i[tools definitions],
    availableTools: %i[tools availableTools],
    excludedTools: %i[tools excludedTools],
    toolSearch: %i[tools search],
    largeOutput: %i[tools largeOutput],
    imageGeneration: %i[tools imageGeneration],
    manageScheduleEnabled: %i[tools manageScheduleEnabled],
    webSearch: %i[tools webSearch],
    shell: %i[tools shell],
    maxInlineBinaryBytes: %i[tools maxInlineBinaryBytes],
    requestPermission: %i[permissions request],
    sandboxConfig: %i[permissions sandbox],
    sandbox: %i[permissions sandbox],
    permissionMode: %i[permissions mode],
    sandboxConfigSource: %i[permissions sandboxSource],
    commands: %i[interaction commands],
    askUserVariant: %i[interaction askUserVariant],
    requestUserInput: %i[interaction requestUserInput],
    requestElicitation: %i[interaction requestElicitation],
    requestShellAuthentication: %i[interaction requestShellAuthentication],
    requestExitPlanMode: %i[interaction requestExitPlanMode],
    requestAutoModeSwitch: %i[interaction requestAutoModeSwitch],
    requestAutoModeRecovery: %i[interaction requestAutoModeRecovery],
    streaming: %i[events streaming],
    includeSubAgentStreamingEvents: %i[events includeSubAgentStreamingEvents],
    customAgents: %i[agents definitions],
    defaultAgent: %i[agents default],
    agent: %i[agents selected],
    includedBuiltinAgents: %i[agents includedBuiltinAgents],
    excludedBuiltinAgents: %i[agents excludedBuiltinAgents],
    customAgentsLocalOnly: %i[agents localDiscoveryOnly],
    agentDirectories: %i[agents directories],
    skillDirectories: %i[skills directories],
    enableSkills: %i[skills enabled],
    includedBuiltinSkills: %i[skills includedBuiltinSkills],
    disabledSkills: %i[skills disabledSkills],
    hasSkillProvider: %i[skills hasProvider],
    ignoredSkillsLocations: %i[skills ignoredLocations],
    mcpServers: %i[mcp servers],
    disabledMcpServers: %i[mcp disabledServers],
    requestMcpApps: %i[mcp enableApps],
    enableMcpApps: %i[mcp enableApps],
    mcpOAuthTokenStorage: %i[mcp oauthTokenStorage],
    authClientIdMetadataUrl: %i[mcp authClientIdMetadataUrl],
    envValueMode: %i[mcp envValueMode],
    managedMcpServers: %i[mcp managedServers],
    allowAllMcpServerInstructions: %i[mcp allowAllServerInstructions],
    gitHubToken: %i[github token],
    gitHubTokenProviderRegistrationId: %i[github tokenProviderRegistrationId],
    githubMcpToolConfig: %i[github mcp],
    capi: %i[github capi],
    enableSessionTelemetry: %i[github telemetryEnabled],
    enableManagedSettings: %i[github fetchManagedSettings],
    interactiveUser: %i[github interactiveUser],
    enableGitHubTelemetryForwarding: %i[github telemetryForwarding],
    authInfo: %i[github authInfo],
    copilotUrl: %i[github copilotUrl],
    additionalDirectories: %i[filesystem additionalDirectories],
    enableFileChangeTracking: %i[filesystem trackFileChanges],
    trustWorkingDirectory: %i[filesystem trustWorkingDirectory],
    sessionLimits: %i[execution limits],
    autopilot: %i[execution autopilot],
    autopilotContinuation: %i[execution autopilotContinuation],
    instructionDirectories: %i[instructions directories],
    organizationCustomInstructions: %i[instructions organizationText],
    enableOnDemandInstructionDiscovery: %i[instructions onDemandDiscovery],
    skipCustomInstructions: %i[instructions skipCustomInstructions],
    refreshCustomInstructions: %i[instructions refreshCustomInstructions],
    hooks: %i[hooks hasHandlers],
    hookDirectories: %i[hooks directories],
    enableFileHooks: %i[hooks enableRepositoryHooks],
    coauthorEnabled: %i[git coauthorEnabled],
    enableHostGitOperations: %i[git enableHostGitOperations],
    infiniteSessions: %i[state infiniteSessions],
    memory: %i[state memory],
    hasMemoryStore: %i[state hasMemoryStore],
    enableSessionStore: %i[state enableCrossSessionStore],
    skipEmbeddingRetrieval: %i[state skipEmbeddingRetrieval],
    embeddingCacheStorage: %i[state embeddingCacheStorage],
    pluginDirectories: %i[plugins directories],
    installedPlugins: %i[plugins installed],
    canvases: %i[canvases definitions],
    canvasProvider: %i[canvases provider],
    requestCanvasRenderer: %i[canvases requestRenderer],
    openCanvases: %i[canvases open],
    requestExtensions: %i[extensions enabled],
    extensionSdkPath: %i[extensions sdkPath],
    extensionInfo: %i[extensions identity],
    enableExperimentalMode: %i[experiments enableExperimentalMode],
    isExperimentalMode: %i[experiments enableExperimentalMode],
    featureFlags: %i[experiments flags],
    expAssignments: %i[experiments assignments],
    featureFlagConfiguration: %i[experiments configuration],
    allowedModels: %i[policy allowedModels],
    managedSettings: %i[policy managedSettings],
    excludedManagedSettings: %i[policy excludedManagedSettings],
    enforceManagedModelDefaults: %i[policy enforceManagedModelDefaults],
  }.freeze

  # Legacy single-provider fields that describe the model rather than the connection.
  LEGACY_PROVIDER_MODEL_FIELDS = %i[
    modelId wireModel maxPromptTokens maxContextWindowTokens maxOutputTokens capabilities
  ].freeze

  # Session defaults applied under the caller's values when the client mode is "empty".
  EMPTY_MODE_SESSION_DEFAULTS = [
    [:experiments, :enableExperimentalMode, false],
    [:github, :telemetryEnabled, false],
    [:mcp, :oauthTokenStorage, "in-memory"],
    [:state, :skipEmbeddingRetrieval, true],
    [:state, :embeddingCacheStorage, "in-memory"],
    [:state, :enableCrossSessionStore, false],
    [:state, :memory, { enabled: false }.freeze],
    [:instructions, :onDemandDiscovery, false],
    [:hooks, :enableRepositoryHooks, false],
    [:git, :enableHostGitOperations, false],
  ].freeze

  # @api private
  #
  # Converts the flat session.create / session.resume payload built by the client into
  # the grouped SDK protocol 4 shape. Root keys stay top level, known flat keys move into
  # their option group (for example +streaming+ becomes +events.streaming+), and unknown
  # keys pass through unchanged. Nil values and empty groups are dropped, except an
  # explicit +defaultProvider: nil+ on resume, which clears the default provider. Group
  # objects already present in +flat+ are merged and their members win. A legacy single
  # +provider+ becomes a "default" named provider with Copilot API models disabled,
  # unless explicit +providers+, +defaultProvider+ or +capi.enabled+ are present.
  #
  # @param flat   [Hash]    flat payload (Symbol or String keys)
  # @param resume [Boolean] true for session.resume, false for session.create
  # @return [Hash] grouped payload
  def self.group_session_params(flat, resume: false)
    payload = {}
    groups = {}
    explicit_groups = {}
    legacy_provider = nil

    flat.each do |raw_key, value|
      key = raw_key.to_sym
      next if value.nil? && !(resume && key == :defaultProvider)
      next if resume && SESSION_CREATE_ONLY_PARAMS.include?(key)
      next if !resume && SESSION_RESUME_ONLY_PARAMS.include?(key)

      if SESSION_PARAM_GROUP_NAMES.include?(key) && value.is_a?(Hash)
        explicit_groups[key] = value
      elsif key == :configDir
        payload[:configDirectory] = value
      elsif key == :disableResume
        payload[:suppressResumeEvent] = value
      elsif key == :provider
        legacy_provider = value
      elsif (path = SESSION_PARAM_GROUPS[key])
        (groups[path[0]] ||= {})[path[1]] = value
      else
        payload[key] = value
      end
    end

    explicit_groups.each do |name, members|
      target = (groups[name] ||= {})
      members.each { |member, value| target[member.to_sym] = value unless value.nil? }
    end

    legacy_provider = legacy_provider.to_wire if legacy_provider.respond_to?(:to_wire)
    group_legacy_provider(legacy_provider, payload[:model], groups) if legacy_provider.is_a?(Hash)

    mcp = groups[:mcp]
    mcp[:envValueMode] ||= "direct" if mcp&.key?(:servers)

    groups.each { |name, members| payload[name] = members unless members.empty? }
    payload
  end

  # @api private
  #
  # Applies the "empty" client mode session defaults to a grouped payload. Values the
  # caller already set win.
  #
  # @param payload [Hash] grouped payload from {group_session_params}
  # @return [Hash] the same payload
  def self.apply_empty_mode_session_defaults(payload)
    EMPTY_MODE_SESSION_DEFAULTS.each do |group, member, value|
      target = payload[group]
      target = payload[group] = {} unless target.is_a?(Hash)
      next unless target[member].nil?

      target[member] = value.is_a?(Hash) ? value.dup : value
    end
    payload
  end

  # @api private
  # Translates a legacy single +provider+ into +inference.providers+ /
  # +inference.models+ / +inference.defaultProvider+ and +github.capi.enabled = false+.
  def self.group_legacy_provider(provider, model, groups)
    inference = (groups[:inference] ||= {})
    github = (groups[:github] ||= {})
    capi = github[:capi]
    explicit_capi = capi.is_a?(Hash) && (capi.key?(:enabled) || capi.key?("enabled"))
    return if inference.key?(:providers) || inference.key?(:defaultProvider) || explicit_capi

    connection = { name: "default" }
    model_fields = {}
    provider.each do |raw_key, value|
      next if value.nil?

      key = raw_key.to_sym
      next if key == :name

      if LEGACY_PROVIDER_MODEL_FIELDS.include?(key)
        model_fields[key] = value
      else
        connection[key] = value
      end
    end

    inference[:providers] = [connection]
    inference[:defaultProvider] = "default"
    github[:capi] = (capi.is_a?(Hash) ? capi : {}).merge(enabled: false)
    return if model_fields.empty? || model.nil?

    inference[:models] = [{ provider: "default", id: model }.merge(model_fields)]
  end
  private_class_method :group_legacy_provider
end
