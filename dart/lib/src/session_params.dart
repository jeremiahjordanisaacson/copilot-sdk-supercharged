// Copyright (c) Microsoft Corporation. All rights reserved.

/// SDK protocol 4 grouping of the `session.create` / `session.resume` payload.
///
/// The client keeps building the flat (protocol 3) payload from the session
/// configuration. [groupSessionParams] converts it into the grouped protocol 4
/// shape right before the request is sent.

/// Option groups of the protocol 4 session payload.
const List<String> sessionParamGroupNames = [
  'inference',
  'tools',
  'permissions',
  'interaction',
  'events',
  'agents',
  'skills',
  'mcp',
  'github',
  'filesystem',
  'execution',
  'instructions',
  'hooks',
  'git',
  'state',
  'plugins',
  'canvases',
  'extensions',
  'experiments',
  'policy',
];

/// Flat keys only valid on `session.create` (dropped on resume).
const Set<String> _createOnlyParams = {
  'name',
  'cloud',
  'trustWorkingDirectory',
  'refreshCustomInstructions',
};

/// Flat keys only valid on `session.resume` (dropped on create).
const Set<String> _resumeOnlyParams = {
  'disableResume',
  'suppressResumeEvent',
  'continuePendingWork',
  'allowTranscriptRecovery',
  'requestedEnvironmentVariables',
  'openCanvases',
};

/// Flat (protocol 3) key -> protocol 4 `[group, member]`.
///
/// Root keys (`sessionId`, `model`, `name`, `clientName`, `workingDirectory`,
/// `configDirectory`, `enableConfigDiscovery`, `systemMessage`, `diagnostics`,
/// `lifecycleDiagnostics`, `cloud`, `traceparent`, `tracestate` and the resume
/// controls) and unknown keys stay top level.
const Map<String, List<String>> _sessionParamGroups = {
  'reasoningEffort': ['inference', 'reasoningEffort'],
  'reasoningSummary': ['inference', 'reasoningSummary'],
  'contextTier': ['inference', 'contextTier'],
  'allowMultipleToolCalls': ['inference', 'allowMultipleToolCalls'],
  'enableCitations': ['inference', 'enableCitations'],
  'modelCapabilities': ['inference', 'capabilities'],
  'verbosity': ['inference', 'verbosity'],
  'providers': ['inference', 'providers'],
  'models': ['inference', 'models'],
  'defaultProvider': ['inference', 'defaultProvider'],
  'tools': ['tools', 'definitions'],
  'availableTools': ['tools', 'availableTools'],
  'excludedTools': ['tools', 'excludedTools'],
  'toolSearch': ['tools', 'search'],
  'largeOutput': ['tools', 'largeOutput'],
  'imageGeneration': ['tools', 'imageGeneration'],
  'manageScheduleEnabled': ['tools', 'manageScheduleEnabled'],
  'webSearch': ['tools', 'webSearch'],
  'shell': ['tools', 'shell'],
  'maxInlineBinaryBytes': ['tools', 'maxInlineBinaryBytes'],
  'requestPermission': ['permissions', 'request'],
  'sandboxConfig': ['permissions', 'sandbox'],
  'sandbox': ['permissions', 'sandbox'],
  'permissionMode': ['permissions', 'mode'],
  'sandboxConfigSource': ['permissions', 'sandboxSource'],
  'commands': ['interaction', 'commands'],
  'askUserVariant': ['interaction', 'askUserVariant'],
  'requestUserInput': ['interaction', 'requestUserInput'],
  'requestElicitation': ['interaction', 'requestElicitation'],
  'requestShellAuthentication': ['interaction', 'requestShellAuthentication'],
  'requestExitPlanMode': ['interaction', 'requestExitPlanMode'],
  'requestAutoModeSwitch': ['interaction', 'requestAutoModeSwitch'],
  'requestAutoModeRecovery': ['interaction', 'requestAutoModeRecovery'],
  'streaming': ['events', 'streaming'],
  'includeSubAgentStreamingEvents': [
    'events',
    'includeSubAgentStreamingEvents'
  ],
  'customAgents': ['agents', 'definitions'],
  'defaultAgent': ['agents', 'default'],
  'agent': ['agents', 'selected'],
  'includedBuiltinAgents': ['agents', 'includedBuiltinAgents'],
  'excludedBuiltinAgents': ['agents', 'excludedBuiltinAgents'],
  'customAgentsLocalOnly': ['agents', 'localDiscoveryOnly'],
  'agentDirectories': ['agents', 'directories'],
  'skillDirectories': ['skills', 'directories'],
  'enableSkills': ['skills', 'enabled'],
  'includedBuiltinSkills': ['skills', 'includedBuiltinSkills'],
  'disabledSkills': ['skills', 'disabledSkills'],
  'hasSkillProvider': ['skills', 'hasProvider'],
  'ignoredSkillsLocations': ['skills', 'ignoredLocations'],
  'mcpServers': ['mcp', 'servers'],
  'disabledMcpServers': ['mcp', 'disabledServers'],
  'requestMcpApps': ['mcp', 'enableApps'],
  'enableMcpApps': ['mcp', 'enableApps'],
  'mcpOAuthTokenStorage': ['mcp', 'oauthTokenStorage'],
  'authClientIdMetadataUrl': ['mcp', 'authClientIdMetadataUrl'],
  'envValueMode': ['mcp', 'envValueMode'],
  'managedMcpServers': ['mcp', 'managedServers'],
  'allowAllMcpServerInstructions': ['mcp', 'allowAllServerInstructions'],
  'gitHubToken': ['github', 'token'],
  'gitHubTokenProviderRegistrationId': ['github', 'tokenProviderRegistrationId'],
  'githubMcpToolConfig': ['github', 'mcp'],
  'capi': ['github', 'capi'],
  'enableSessionTelemetry': ['github', 'telemetryEnabled'],
  'enableManagedSettings': ['github', 'fetchManagedSettings'],
  'interactiveUser': ['github', 'interactiveUser'],
  'enableGitHubTelemetryForwarding': ['github', 'telemetryForwarding'],
  'authInfo': ['github', 'authInfo'],
  'copilotUrl': ['github', 'copilotUrl'],
  'additionalDirectories': ['filesystem', 'additionalDirectories'],
  'enableFileChangeTracking': ['filesystem', 'trackFileChanges'],
  'trustWorkingDirectory': ['filesystem', 'trustWorkingDirectory'],
  'sessionLimits': ['execution', 'limits'],
  'autopilot': ['execution', 'autopilot'],
  'autopilotContinuation': ['execution', 'autopilotContinuation'],
  'instructionDirectories': ['instructions', 'directories'],
  'organizationCustomInstructions': ['instructions', 'organizationText'],
  'enableOnDemandInstructionDiscovery': ['instructions', 'onDemandDiscovery'],
  'skipCustomInstructions': ['instructions', 'skipCustomInstructions'],
  'refreshCustomInstructions': ['instructions', 'refreshCustomInstructions'],
  'hooks': ['hooks', 'hasHandlers'],
  'hookDirectories': ['hooks', 'directories'],
  'enableFileHooks': ['hooks', 'enableRepositoryHooks'],
  'coauthorEnabled': ['git', 'coauthorEnabled'],
  'enableHostGitOperations': ['git', 'enableHostGitOperations'],
  'infiniteSessions': ['state', 'infiniteSessions'],
  'memory': ['state', 'memory'],
  'hasMemoryStore': ['state', 'hasMemoryStore'],
  'enableSessionStore': ['state', 'enableCrossSessionStore'],
  'skipEmbeddingRetrieval': ['state', 'skipEmbeddingRetrieval'],
  'embeddingCacheStorage': ['state', 'embeddingCacheStorage'],
  'pluginDirectories': ['plugins', 'directories'],
  'installedPlugins': ['plugins', 'installed'],
  'canvases': ['canvases', 'definitions'],
  'canvasProvider': ['canvases', 'provider'],
  'requestCanvasRenderer': ['canvases', 'requestRenderer'],
  'openCanvases': ['canvases', 'open'],
  'requestExtensions': ['extensions', 'enabled'],
  'extensionSdkPath': ['extensions', 'sdkPath'],
  'extensionInfo': ['extensions', 'identity'],
  'enableExperimentalMode': ['experiments', 'enableExperimentalMode'],
  'isExperimentalMode': ['experiments', 'enableExperimentalMode'],
  'featureFlags': ['experiments', 'flags'],
  'expAssignments': ['experiments', 'assignments'],
  'featureFlagConfiguration': ['experiments', 'configuration'],
  'allowedModels': ['policy', 'allowedModels'],
  'managedSettings': ['policy', 'managedSettings'],
  'excludedManagedSettings': ['policy', 'excludedManagedSettings'],
  'enforceManagedModelDefaults': ['policy', 'enforceManagedModelDefaults'],
};

/// Legacy single-provider fields that describe the model, not the connection.
const Set<String> _legacyProviderModelFields = {
  'modelId',
  'wireModel',
  'maxPromptTokens',
  'maxContextWindowTokens',
  'maxOutputTokens',
  'capabilities',
};

/// Converts the flat `session.create` / `session.resume` payload into the
/// grouped SDK protocol 4 shape.
///
/// Known flat keys move into their option group (for example `streaming`
/// becomes `events.streaming` and a boolean `hooks` becomes
/// `hooks.hasHandlers`), root keys and unknown keys stay top level, null
/// values and empty groups are dropped (except an explicit
/// `defaultProvider: null` on resume, which clears the default provider), and
/// grouped objects already present in [flat] are merged with their members
/// winning. `configDir` becomes `configDirectory` and `disableResume` becomes
/// `suppressResumeEvent`. A legacy single `provider` becomes a `"default"`
/// named provider with Copilot API models disabled, unless explicit
/// `providers`, `defaultProvider` or `capi.enabled` are present.
Map<String, dynamic> groupSessionParams(
  Map<String, dynamic> flat, {
  required bool isResume,
}) {
  final payload = <String, dynamic>{};
  final groups = <String, Map<String, dynamic>>{};
  final explicitGroups = <String, Map<String, dynamic>>{};
  Map<String, dynamic>? legacyProvider;

  Map<String, dynamic> group(String name) =>
      groups.putIfAbsent(name, () => <String, dynamic>{});

  flat.forEach((key, value) {
    if (value == null && !(isResume && key == 'defaultProvider')) return;
    if (isResume && _createOnlyParams.contains(key)) return;
    if (!isResume && _resumeOnlyParams.contains(key)) return;

    final path = _sessionParamGroups[key];
    if (sessionParamGroupNames.contains(key) && value is Map) {
      explicitGroups[key] = Map<String, dynamic>.from(value);
    } else if (key == 'configDir') {
      payload['configDirectory'] = value;
    } else if (key == 'disableResume') {
      payload['suppressResumeEvent'] = value;
    } else if (key == 'provider' && value is Map) {
      legacyProvider = Map<String, dynamic>.from(value);
    } else if (path != null) {
      group(path[0])[path[1]] = value;
    } else {
      payload[key] = value;
    }
  });

  explicitGroups.forEach((name, members) {
    final target = group(name);
    members.forEach((member, value) {
      if (value != null || member == 'defaultProvider') target[member] = value;
    });
  });

  // Grouped members that only exist on the other operation are rejected by
  // the runtime.
  if (isResume) {
    groups['filesystem']?.remove('trustWorkingDirectory');
    groups['instructions']?.remove('refreshCustomInstructions');
  } else {
    groups['canvases']?.remove('open');
  }

  final provider = legacyProvider;
  if (provider != null) {
    _groupLegacyProvider(provider, payload['model'], groups);
  }

  final mcp = groups['mcp'];
  if (mcp != null && mcp.containsKey('servers')) {
    mcp.putIfAbsent('envValueMode', () => 'direct');
  }

  groups.forEach((name, members) {
    if (members.isNotEmpty) payload[name] = members;
  });
  return payload;
}

/// Translates a legacy single `provider` into `inference.providers`,
/// `inference.defaultProvider`, `github.capi.enabled = false` and, when the
/// session model is set and the provider carries model fields,
/// `inference.models`.
void _groupLegacyProvider(
  Map<String, dynamic> provider,
  Object? model,
  Map<String, Map<String, dynamic>> groups,
) {
  final inference = groups.putIfAbsent('inference', () => <String, dynamic>{});
  final github = groups.putIfAbsent('github', () => <String, dynamic>{});
  final capi = github['capi'];
  final explicitCapi = capi is Map && capi['enabled'] != null;
  if (inference.containsKey('providers') ||
      inference.containsKey('defaultProvider') ||
      explicitCapi) {
    return;
  }

  final connection = <String, dynamic>{'name': 'default'};
  final modelFields = <String, dynamic>{};
  provider.forEach((key, value) {
    if (value == null || key == 'name') return;
    if (_legacyProviderModelFields.contains(key)) {
      modelFields[key] = value;
    } else {
      connection[key] = value;
    }
  });

  inference['providers'] = [connection];
  inference['defaultProvider'] = 'default';
  github['capi'] = <String, dynamic>{
    if (capi is Map) ...Map<String, dynamic>.from(capi),
    'enabled': false,
  };
  if (modelFields.isNotEmpty && model is String) {
    inference['models'] = [
      <String, dynamic>{'provider': 'default', 'id': model, ...modelFields},
    ];
  }
}

/// Session defaults applied under the caller's values in `"empty"` mode.
const List<List<Object>> _emptyModeSessionDefaults = [
  ['experiments', 'enableExperimentalMode', false],
  ['github', 'telemetryEnabled', false],
  ['mcp', 'oauthTokenStorage', 'in-memory'],
  ['state', 'skipEmbeddingRetrieval', true],
  ['state', 'embeddingCacheStorage', 'in-memory'],
  ['state', 'enableCrossSessionStore', false],
  ['state', 'memory', <String, Object>{'enabled': false}],
  ['instructions', 'onDemandDiscovery', false],
  ['hooks', 'enableRepositoryHooks', false],
  ['git', 'enableHostGitOperations', false],
];

/// Applies the `"empty"` client mode defaults to a grouped payload; values
/// the caller already set win.
Map<String, dynamic> applyEmptyModeSessionDefaults(
    Map<String, dynamic> payload) {
  for (final entry in _emptyModeSessionDefaults) {
    final groupName = entry[0] as String;
    final member = entry[1] as String;
    final existing = payload[groupName];
    final Map<String, dynamic> target;
    if (existing is Map<String, dynamic>) {
      target = existing;
    } else {
      target = <String, dynamic>{};
      payload[groupName] = target;
    }
    if (target[member] == null) {
      final value = entry[2];
      target[member] =
          value is Map ? Map<String, dynamic>.from(value) : value;
    }
  }
  return payload;
}
