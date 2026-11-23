// Copyright (c) Microsoft Corporation. All rights reserved.
// V SDK - SDK protocol 4 session payload translation

module copilot

import x.json2

// session_root_keys stay at the top level of session.create / session.resume.
const session_root_keys = ['sessionId', 'model', 'name', 'clientName', 'workingDirectory',
	'configDirectory', 'enableConfigDiscovery', 'systemMessage', 'diagnostics',
	'lifecycleDiagnostics', 'cloud', 'traceparent', 'tracestate', 'suppressResumeEvent',
	'continuePendingWork', 'allowTranscriptRecovery', 'requestedEnvironmentVariables']

// session_group_names lists the protocol 4 option groups.
const session_group_names = ['inference', 'tools', 'permissions', 'interaction', 'events',
	'agents', 'skills', 'mcp', 'github', 'filesystem', 'execution', 'instructions', 'hooks',
	'git', 'state', 'plugins', 'canvases', 'extensions', 'experiments', 'policy']

const create_only_grouped_keys = ['trustWorkingDirectory', 'refreshCustomInstructions']

const resume_only_grouped_keys = ['openCanvases']

// Legacy `provider` members that describe the model rather than the connection.
const legacy_provider_model_fields = ['modelId', 'wireModel', 'maxPromptTokens',
	'maxContextWindowTokens', 'maxOutputTokens', 'capabilities']

// session_grouped_keys maps a flat wire key to its 'group.member' path.
const session_grouped_keys = {
	'reasoningEffort':                    'inference.reasoningEffort'
	'reasoningSummary':                   'inference.reasoningSummary'
	'contextTier':                        'inference.contextTier'
	'allowMultipleToolCalls':             'inference.allowMultipleToolCalls'
	'enableCitations':                    'inference.enableCitations'
	'modelCapabilities':                  'inference.capabilities'
	'verbosity':                          'inference.verbosity'
	'providers':                          'inference.providers'
	'models':                             'inference.models'
	'defaultProvider':                    'inference.defaultProvider'
	'tools':                              'tools.definitions'
	'availableTools':                     'tools.availableTools'
	'excludedTools':                      'tools.excludedTools'
	'toolSearch':                         'tools.search'
	'largeOutput':                        'tools.largeOutput'
	'imageGeneration':                    'tools.imageGeneration'
	'manageScheduleEnabled':              'tools.manageScheduleEnabled'
	'webSearch':                          'tools.webSearch'
	'shell':                              'tools.shell'
	'maxInlineBinaryBytes':               'tools.maxInlineBinaryBytes'
	'requestPermission':                  'permissions.request'
	'sandboxConfig':                      'permissions.sandbox'
	'sandbox':                            'permissions.sandbox'
	'permissionMode':                     'permissions.mode'
	'sandboxConfigSource':                'permissions.sandboxSource'
	'commands':                           'interaction.commands'
	'askUserVariant':                     'interaction.askUserVariant'
	'requestUserInput':                   'interaction.requestUserInput'
	'requestElicitation':                 'interaction.requestElicitation'
	'requestShellAuthentication':         'interaction.requestShellAuthentication'
	'requestExitPlanMode':                'interaction.requestExitPlanMode'
	'requestAutoModeSwitch':              'interaction.requestAutoModeSwitch'
	'requestAutoModeRecovery':            'interaction.requestAutoModeRecovery'
	'streaming':                          'events.streaming'
	'includeSubAgentStreamingEvents':     'events.includeSubAgentStreamingEvents'
	'customAgents':                       'agents.definitions'
	'defaultAgent':                       'agents.default'
	'agent':                              'agents.selected'
	'includedBuiltinAgents':              'agents.includedBuiltinAgents'
	'excludedBuiltinAgents':              'agents.excludedBuiltinAgents'
	'customAgentsLocalOnly':              'agents.localDiscoveryOnly'
	'agentDirectories':                   'agents.directories'
	'skillDirectories':                   'skills.directories'
	'enableSkills':                       'skills.enabled'
	'includedBuiltinSkills':              'skills.includedBuiltinSkills'
	'disabledSkills':                     'skills.disabledSkills'
	'hasSkillProvider':                   'skills.hasProvider'
	'ignoredSkillsLocations':             'skills.ignoredLocations'
	'mcpServers':                         'mcp.servers'
	'disabledMcpServers':                 'mcp.disabledServers'
	'requestMcpApps':                     'mcp.enableApps'
	'enableMcpApps':                      'mcp.enableApps'
	'mcpOAuthTokenStorage':               'mcp.oauthTokenStorage'
	'authClientIdMetadataUrl':            'mcp.authClientIdMetadataUrl'
	'envValueMode':                       'mcp.envValueMode'
	'managedMcpServers':                  'mcp.managedServers'
	'allowAllMcpServerInstructions':      'mcp.allowAllServerInstructions'
	'gitHubToken':                        'github.token'
	'gitHubTokenProviderRegistrationId':  'github.tokenProviderRegistrationId'
	'githubMcpToolConfig':                'github.mcp'
	'capi':                               'github.capi'
	'enableSessionTelemetry':             'github.telemetryEnabled'
	'enableManagedSettings':              'github.fetchManagedSettings'
	'interactiveUser':                    'github.interactiveUser'
	'enableGitHubTelemetryForwarding':    'github.telemetryForwarding'
	'authInfo':                           'github.authInfo'
	'copilotUrl':                         'github.copilotUrl'
	'additionalDirectories':              'filesystem.additionalDirectories'
	'enableFileChangeTracking':           'filesystem.trackFileChanges'
	'trustWorkingDirectory':              'filesystem.trustWorkingDirectory'
	'sessionLimits':                      'execution.limits'
	'autopilot':                          'execution.autopilot'
	'autopilotContinuation':              'execution.autopilotContinuation'
	'instructionDirectories':             'instructions.directories'
	'organizationCustomInstructions':     'instructions.organizationText'
	'enableOnDemandInstructionDiscovery': 'instructions.onDemandDiscovery'
	'skipCustomInstructions':             'instructions.skipCustomInstructions'
	'refreshCustomInstructions':          'instructions.refreshCustomInstructions'
	'hooks':                              'hooks.hasHandlers'
	'hookDirectories':                    'hooks.directories'
	'enableFileHooks':                    'hooks.enableRepositoryHooks'
	'coauthorEnabled':                    'git.coauthorEnabled'
	'enableHostGitOperations':            'git.enableHostGitOperations'
	'infiniteSessions':                   'state.infiniteSessions'
	'memory':                             'state.memory'
	'hasMemoryStore':                     'state.hasMemoryStore'
	'enableSessionStore':                 'state.enableCrossSessionStore'
	'skipEmbeddingRetrieval':             'state.skipEmbeddingRetrieval'
	'embeddingCacheStorage':              'state.embeddingCacheStorage'
	'pluginDirectories':                  'plugins.directories'
	'installedPlugins':                   'plugins.installed'
	'canvases':                           'canvases.definitions'
	'canvasProvider':                     'canvases.provider'
	'requestCanvasRenderer':              'canvases.requestRenderer'
	'openCanvases':                       'canvases.open'
	'requestExtensions':                  'extensions.enabled'
	'extensionSdkPath':                   'extensions.sdkPath'
	'extensionInfo':                      'extensions.identity'
	'enableExperimentalMode':             'experiments.enableExperimentalMode'
	'isExperimentalMode':                 'experiments.enableExperimentalMode'
	'featureFlags':                       'experiments.flags'
	'expAssignments':                     'experiments.assignments'
	'featureFlagConfiguration':           'experiments.configuration'
	'allowedModels':                      'policy.allowedModels'
	'managedSettings':                    'policy.managedSettings'
	'excludedManagedSettings':            'policy.excludedManagedSettings'
	'enforceManagedModelDefaults':        'policy.enforceManagedModelDefaults'
}

// GroupedSessionParams is a protocol 4 session payload: root values plus
// option groups. Every value is a raw JSON fragment.
pub struct GroupedSessionParams {
pub mut:
	root   map[string]string
	groups map[string]map[string]string
}

// put sets `group.member` to a raw JSON value.
pub fn (mut g GroupedSessionParams) put(group string, member string, value string) {
	if group !in g.groups {
		g.groups[group] = map[string]string{}
	}
	g.groups[group][member] = value
}

// has reports whether `group.member` is set.
pub fn (g GroupedSessionParams) has(group string, member string) bool {
	if group !in g.groups {
		return false
	}
	return member in g.groups[group]
}

// get returns the raw JSON value of `group.member`, or '' when unset.
pub fn (g GroupedSessionParams) get(group string, member string) string {
	if !g.has(group, member) {
		return ''
	}
	return g.groups[group][member]
}

fn (mut g GroupedSessionParams) put_default(group string, member string, value string) {
	if !g.has(group, member) {
		g.put(group, member, value)
	}
}

// apply_empty_mode_defaults fills the conservative 'empty' client mode
// defaults without overriding values the caller supplied.
pub fn (mut g GroupedSessionParams) apply_empty_mode_defaults() {
	g.put_default('experiments', 'enableExperimentalMode', 'false')
	g.put_default('github', 'telemetryEnabled', 'false')
	g.put_default('mcp', 'oauthTokenStorage', '"in-memory"')
	g.put_default('state', 'skipEmbeddingRetrieval', 'true')
	g.put_default('state', 'embeddingCacheStorage', '"in-memory"')
	g.put_default('state', 'enableCrossSessionStore', 'false')
	g.put_default('state', 'memory', '{"enabled":false}')
	g.put_default('instructions', 'onDemandDiscovery', 'false')
	g.put_default('hooks', 'enableRepositoryHooks', 'false')
	g.put_default('git', 'enableHostGitOperations', 'false')
}

// to_json renders the grouped payload. Empty groups are omitted.
pub fn (g GroupedSessionParams) to_json() string {
	mut out := map[string]string{}
	for key, value in g.root {
		out[key] = value
	}
	for group, members in g.groups {
		if members.len > 0 {
			out[group] = build_params(members)
		}
	}
	return build_params(out)
}

fn unquote_key(key string) string {
	if key.len >= 2 && key.starts_with('"') && key.ends_with('"') {
		return key[1..key.len - 1]
	}
	return key
}

// decode_json_string returns the string value of a raw JSON string fragment.
fn decode_json_string(raw string) string {
	value := json2.decode[json2.Any](raw) or { return '' }
	if value is string {
		return value
	}
	return ''
}

// group_session_params converts the flat session.create / session.resume
// payload built by this SDK (raw JSON values, keys with or without quotes)
// into the grouped SDK protocol 4 payload. Root keys stay at the top level,
// known flat keys move into their group, and unknown keys stay at the root.
// Null values are skipped and explicit grouped members win over flat keys.
pub fn group_session_params(flat map[string]string, is_resume bool) GroupedSessionParams {
	mut g := GroupedSessionParams{}
	mut normalized := map[string]string{}
	for key, value in flat {
		normalized[unquote_key(key)] = value
	}
	mut explicit := map[string]string{}
	mut legacy_provider := ''
	mut web_socket_responses := ''
	for key, raw in normalized {
		value := raw.trim_space()
		if value.len == 0 {
			continue
		}
		if value == 'null' && !(is_resume && key == 'defaultProvider') {
			continue
		}
		if key in session_group_names && value.starts_with('{') {
			explicit[key] = value
			continue
		}
		if key == 'configDir' {
			if 'configDirectory' !in normalized {
				g.root['configDirectory'] = value
			}
		} else if key == 'disableResume' {
			if 'suppressResumeEvent' !in normalized {
				g.root['suppressResumeEvent'] = value
			}
		} else if key == 'provider' {
			if value.starts_with('{') {
				legacy_provider = value
			}
		} else if key == 'enableWebSocketResponses' {
			web_socket_responses = value
		} else if key in session_root_keys {
			g.root[key] = value
		} else if is_resume && key in create_only_grouped_keys {
			continue
		} else if !is_resume && key in resume_only_grouped_keys {
			continue
		} else if path := session_grouped_keys[key] {
			g.put(path.all_before('.'), path.all_after('.'), value)
		} else {
			g.root[key] = value
		}
	}
	for group, raw in explicit {
		parsed := json2.decode[json2.Any](raw) or { continue }
		if parsed is map[string]json2.Any {
			for member, value in parsed {
				if value is json2.Null
					&& !(is_resume && group == 'inference' && member == 'defaultProvider') {
					continue
				}
				g.put(group, member, value.json_str())
			}
		}
	}
	if legacy_provider.len > 0 && !g.has('inference', 'providers')
		&& !g.has('inference', 'defaultProvider') && !g.has('github', 'capi') {
		g.translate_legacy_provider(legacy_provider, normalized['model'] or { '' })
	}
	// The legacy WebSocket flag is a Copilot API option in protocol 4.
	if web_socket_responses.len > 0 {
		mut capi := raw_object_members(g.get('github', 'capi'))
		if 'enableWebSocketResponses' !in capi {
			capi['enableWebSocketResponses'] = web_socket_responses
		}
		g.put('github', 'capi', build_params(capi))
	}
	if g.has('mcp', 'servers') && !g.has('mcp', 'envValueMode') {
		g.put('mcp', 'envValueMode', '"direct"')
	}
	return g
}

// raw_object_members returns the members of a raw JSON object as raw JSON
// values, or an empty map when `raw` is not an object.
fn raw_object_members(raw string) map[string]string {
	mut members := map[string]string{}
	if raw.len == 0 {
		return members
	}
	parsed := json2.decode[json2.Any](raw) or { return members }
	if parsed is map[string]json2.Any {
		for key, value in parsed {
			members[key] = value.json_str()
		}
	}
	return members
}

// translate_legacy_provider maps the v1 single `provider` to the named
// "default" connection with Copilot models disabled, plus a model definition
// for model-specific fields when the session model is known.
fn (mut g GroupedSessionParams) translate_legacy_provider(provider_raw string, model_raw string) {
	parsed := json2.decode[json2.Any](provider_raw) or { return }
	if parsed !is map[string]json2.Any {
		return
	}
	mut connection := map[string]string{}
	connection['name'] = '"default"'
	mut model_fields := map[string]string{}
	for key, value in parsed.as_map() {
		if value is json2.Null || key == 'name' {
			continue
		}
		if key in legacy_provider_model_fields {
			model_fields[key] = value.json_str()
		} else {
			connection[key] = value.json_str()
		}
	}
	g.put('inference', 'providers', '[${build_params(connection)}]')
	g.put('inference', 'defaultProvider', '"default"')
	g.put('github', 'capi', '{"enabled":false}')
	model := decode_json_string(model_raw)
	if model_fields.len > 0 && model.len > 0 && !g.has('inference', 'models') {
		mut entry := map[string]string{}
		entry['provider'] = '"default"'
		entry['id'] = json_str(model)
		for key, value in model_fields {
			entry[key] = value
		}
		g.put('inference', 'models', '[${build_params(entry)}]')
	}
}
