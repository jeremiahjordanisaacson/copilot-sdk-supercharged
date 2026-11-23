<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot;

/**
 * Translates the flat session.create / session.resume payload built by
 * SessionConfig::toServerParams() and ResumeSessionConfig::toServerParams()
 * into the grouped SDK protocol 4 shape.
 *
 * @internal
 */
final class SessionParams
{
    /** Option groups of the SDK protocol 4 session payloads. */
    private const GROUP_NAMES = [
        'inference', 'tools', 'permissions', 'interaction', 'events', 'agents', 'skills', 'mcp',
        'github', 'filesystem', 'execution', 'instructions', 'hooks', 'git', 'state', 'plugins',
        'canvases', 'extensions', 'experiments', 'policy',
    ];

    /** Flat wire keys only valid on session.create (dropped on resume). */
    private const CREATE_ONLY = ['name', 'cloud', 'trustWorkingDirectory', 'refreshCustomInstructions'];

    /** Flat wire keys only valid on session.resume (dropped on create). */
    private const RESUME_ONLY = [
        'disableResume', 'suppressResumeEvent', 'continuePendingWork', 'allowTranscriptRecovery',
        'requestedEnvironmentVariables', 'openCanvases',
    ];

    /**
     * Flat (SDK protocol 3) wire keys and the SDK protocol 4 [group, member] they move to.
     * Root keys (sessionId, model, name, clientName, workingDirectory, configDirectory,
     * enableConfigDiscovery, systemMessage, diagnostics, lifecycleDiagnostics, cloud,
     * traceparent, tracestate and the resume controls) and unknown keys stay top level.
     */
    private const GROUPS = [
        'reasoningEffort' => ['inference', 'reasoningEffort'],
        'reasoningSummary' => ['inference', 'reasoningSummary'],
        'contextTier' => ['inference', 'contextTier'],
        'allowMultipleToolCalls' => ['inference', 'allowMultipleToolCalls'],
        'enableCitations' => ['inference', 'enableCitations'],
        'modelCapabilities' => ['inference', 'capabilities'],
        'verbosity' => ['inference', 'verbosity'],
        'providers' => ['inference', 'providers'],
        'models' => ['inference', 'models'],
        'defaultProvider' => ['inference', 'defaultProvider'],
        'tools' => ['tools', 'definitions'],
        'availableTools' => ['tools', 'availableTools'],
        'excludedTools' => ['tools', 'excludedTools'],
        'toolSearch' => ['tools', 'search'],
        'largeOutput' => ['tools', 'largeOutput'],
        'imageGeneration' => ['tools', 'imageGeneration'],
        'manageScheduleEnabled' => ['tools', 'manageScheduleEnabled'],
        'webSearch' => ['tools', 'webSearch'],
        'shell' => ['tools', 'shell'],
        'maxInlineBinaryBytes' => ['tools', 'maxInlineBinaryBytes'],
        'requestPermission' => ['permissions', 'request'],
        'sandboxConfig' => ['permissions', 'sandbox'],
        'sandbox' => ['permissions', 'sandbox'],
        'permissionMode' => ['permissions', 'mode'],
        'sandboxConfigSource' => ['permissions', 'sandboxSource'],
        'commands' => ['interaction', 'commands'],
        'askUserVariant' => ['interaction', 'askUserVariant'],
        'requestUserInput' => ['interaction', 'requestUserInput'],
        'requestElicitation' => ['interaction', 'requestElicitation'],
        'requestShellAuthentication' => ['interaction', 'requestShellAuthentication'],
        'requestExitPlanMode' => ['interaction', 'requestExitPlanMode'],
        'requestAutoModeSwitch' => ['interaction', 'requestAutoModeSwitch'],
        'requestAutoModeRecovery' => ['interaction', 'requestAutoModeRecovery'],
        'streaming' => ['events', 'streaming'],
        'includeSubAgentStreamingEvents' => ['events', 'includeSubAgentStreamingEvents'],
        'customAgents' => ['agents', 'definitions'],
        'defaultAgent' => ['agents', 'default'],
        'agent' => ['agents', 'selected'],
        'includedBuiltinAgents' => ['agents', 'includedBuiltinAgents'],
        'excludedBuiltinAgents' => ['agents', 'excludedBuiltinAgents'],
        'customAgentsLocalOnly' => ['agents', 'localDiscoveryOnly'],
        'agentDirectories' => ['agents', 'directories'],
        'skillDirectories' => ['skills', 'directories'],
        'enableSkills' => ['skills', 'enabled'],
        'includedBuiltinSkills' => ['skills', 'includedBuiltinSkills'],
        'disabledSkills' => ['skills', 'disabledSkills'],
        'hasSkillProvider' => ['skills', 'hasProvider'],
        'ignoredSkillsLocations' => ['skills', 'ignoredLocations'],
        'mcpServers' => ['mcp', 'servers'],
        'disabledMcpServers' => ['mcp', 'disabledServers'],
        'requestMcpApps' => ['mcp', 'enableApps'],
        'enableMcpApps' => ['mcp', 'enableApps'],
        'mcpOAuthTokenStorage' => ['mcp', 'oauthTokenStorage'],
        'authClientIdMetadataUrl' => ['mcp', 'authClientIdMetadataUrl'],
        'envValueMode' => ['mcp', 'envValueMode'],
        'managedMcpServers' => ['mcp', 'managedServers'],
        'allowAllMcpServerInstructions' => ['mcp', 'allowAllServerInstructions'],
        'gitHubToken' => ['github', 'token'],
        'gitHubTokenProviderRegistrationId' => ['github', 'tokenProviderRegistrationId'],
        'githubMcpToolConfig' => ['github', 'mcp'],
        'capi' => ['github', 'capi'],
        'enableSessionTelemetry' => ['github', 'telemetryEnabled'],
        'enableManagedSettings' => ['github', 'fetchManagedSettings'],
        'interactiveUser' => ['github', 'interactiveUser'],
        'enableGitHubTelemetryForwarding' => ['github', 'telemetryForwarding'],
        'authInfo' => ['github', 'authInfo'],
        'copilotUrl' => ['github', 'copilotUrl'],
        'additionalDirectories' => ['filesystem', 'additionalDirectories'],
        'enableFileChangeTracking' => ['filesystem', 'trackFileChanges'],
        'trustWorkingDirectory' => ['filesystem', 'trustWorkingDirectory'],
        'sessionLimits' => ['execution', 'limits'],
        'autopilot' => ['execution', 'autopilot'],
        'autopilotContinuation' => ['execution', 'autopilotContinuation'],
        'instructionDirectories' => ['instructions', 'directories'],
        'organizationCustomInstructions' => ['instructions', 'organizationText'],
        'enableOnDemandInstructionDiscovery' => ['instructions', 'onDemandDiscovery'],
        'skipCustomInstructions' => ['instructions', 'skipCustomInstructions'],
        'refreshCustomInstructions' => ['instructions', 'refreshCustomInstructions'],
        'hooks' => ['hooks', 'hasHandlers'],
        'hookDirectories' => ['hooks', 'directories'],
        'enableFileHooks' => ['hooks', 'enableRepositoryHooks'],
        'coauthorEnabled' => ['git', 'coauthorEnabled'],
        'enableHostGitOperations' => ['git', 'enableHostGitOperations'],
        'infiniteSessions' => ['state', 'infiniteSessions'],
        'memory' => ['state', 'memory'],
        'hasMemoryStore' => ['state', 'hasMemoryStore'],
        'enableSessionStore' => ['state', 'enableCrossSessionStore'],
        'skipEmbeddingRetrieval' => ['state', 'skipEmbeddingRetrieval'],
        'embeddingCacheStorage' => ['state', 'embeddingCacheStorage'],
        'pluginDirectories' => ['plugins', 'directories'],
        'installedPlugins' => ['plugins', 'installed'],
        'canvases' => ['canvases', 'definitions'],
        'canvasProvider' => ['canvases', 'provider'],
        'requestCanvasRenderer' => ['canvases', 'requestRenderer'],
        'openCanvases' => ['canvases', 'open'],
        'requestExtensions' => ['extensions', 'enabled'],
        'extensionSdkPath' => ['extensions', 'sdkPath'],
        'extensionInfo' => ['extensions', 'identity'],
        'enableExperimentalMode' => ['experiments', 'enableExperimentalMode'],
        'isExperimentalMode' => ['experiments', 'enableExperimentalMode'],
        'featureFlags' => ['experiments', 'flags'],
        'expAssignments' => ['experiments', 'assignments'],
        'featureFlagConfiguration' => ['experiments', 'configuration'],
        'allowedModels' => ['policy', 'allowedModels'],
        'managedSettings' => ['policy', 'managedSettings'],
        'excludedManagedSettings' => ['policy', 'excludedManagedSettings'],
        'enforceManagedModelDefaults' => ['policy', 'enforceManagedModelDefaults'],
    ];

    /** Legacy single-provider fields that describe the model rather than the connection. */
    private const LEGACY_PROVIDER_MODEL_FIELDS = [
        'modelId', 'wireModel', 'maxPromptTokens', 'maxContextWindowTokens', 'maxOutputTokens', 'capabilities',
    ];

    /**
     * Session defaults applied under the caller's values when the client mode is "empty".
     * The memory default is built in applyEmptyModeDefaults() because it is an object.
     */
    private const EMPTY_MODE_DEFAULTS = [
        ['experiments', 'enableExperimentalMode', false],
        ['github', 'telemetryEnabled', false],
        ['mcp', 'oauthTokenStorage', 'in-memory'],
        ['state', 'skipEmbeddingRetrieval', true],
        ['state', 'embeddingCacheStorage', 'in-memory'],
        ['state', 'enableCrossSessionStore', false],
        ['state', 'memory', null],
        ['instructions', 'onDemandDiscovery', false],
        ['hooks', 'enableRepositoryHooks', false],
        ['git', 'enableHostGitOperations', false],
    ];

    /**
     * Convert a flat payload into the grouped SDK protocol 4 shape.
     *
     * Root keys stay top level, known flat keys move into their option group (for
     * example `streaming` becomes `events.streaming`), and unknown keys pass through.
     * Null values and empty groups are dropped, except an explicit
     * `defaultProvider: null` on resume, which clears the default provider. Group
     * objects already present are merged and their members win. A legacy single
     * `provider` becomes a "default" named provider with Copilot API models disabled,
     * unless explicit `providers`, `defaultProvider` or `capi.enabled` are present.
     *
     * @param array<string,mixed> $flat Flat payload
     * @param bool $resume True for session.resume, false for session.create
     * @return array<string,mixed> Grouped payload
     */
    public static function group(array $flat, bool $resume = false): array
    {
        $payload = [];
        $groups = [];
        $explicitGroups = [];
        $legacyProvider = null;

        foreach ($flat as $key => $value) {
            $key = (string) $key;
            if ($value === null && !($resume && $key === 'defaultProvider')) {
                continue;
            }
            if ($resume && in_array($key, self::CREATE_ONLY, true)) {
                continue;
            }
            if (!$resume && in_array($key, self::RESUME_ONLY, true)) {
                continue;
            }

            // An empty PHP array is ambiguous: for keys that are also flat lists
            // (tools, canvases) it is the flat list, otherwise an empty group.
            $isGroupObject = in_array($key, self::GROUP_NAMES, true) && self::isMap($value)
                && !($value === [] && isset(self::GROUPS[$key]));
            if ($isGroupObject) {
                $explicitGroups[$key] = self::toMap($value);
            } elseif ($key === 'configDir') {
                $payload['configDirectory'] = $value;
            } elseif ($key === 'disableResume') {
                $payload['suppressResumeEvent'] = $value;
            } elseif ($key === 'provider') {
                $legacyProvider = $value;
            } elseif (isset(self::GROUPS[$key])) {
                [$group, $member] = self::GROUPS[$key];
                $groups[$group][$member] = $value;
            } else {
                $payload[$key] = $value;
            }
        }

        foreach ($explicitGroups as $name => $members) {
            foreach ($members as $member => $value) {
                if ($value !== null) {
                    $groups[$name][(string) $member] = $value;
                }
            }
        }

        if (is_object($legacyProvider) && method_exists($legacyProvider, 'toArray')) {
            $legacyProvider = $legacyProvider->toArray();
        }
        if (self::isMap($legacyProvider)) {
            self::groupLegacyProvider(self::toMap($legacyProvider), $payload['model'] ?? null, $groups);
        }

        if (isset($groups['mcp']) && array_key_exists('servers', $groups['mcp'])
            && !isset($groups['mcp']['envValueMode'])) {
            $groups['mcp']['envValueMode'] = 'direct';
        }

        foreach ($groups as $name => $members) {
            if ($members !== []) {
                $payload[$name] = $members;
            }
        }
        return $payload;
    }

    /**
     * Apply the "empty" client mode session defaults to a grouped payload. Values the
     * caller already set win.
     *
     * @param array<string,mixed> $payload Grouped payload from group()
     * @return array<string,mixed>
     */
    public static function applyEmptyModeDefaults(array $payload): array
    {
        foreach (self::EMPTY_MODE_DEFAULTS as [$group, $member, $value]) {
            $target = isset($payload[$group]) && self::isMap($payload[$group]) ? self::toMap($payload[$group]) : [];
            if (!isset($target[$member])) {
                $target[$member] = $member === 'memory' ? ['enabled' => false] : $value;
            }
            $payload[$group] = $target;
        }
        return $payload;
    }

    /**
     * Translate a legacy single provider into inference.providers / inference.models /
     * inference.defaultProvider and github.capi.enabled = false.
     *
     * @param array<string,mixed> $provider
     * @param array<string,array<string,mixed>> $groups
     */
    private static function groupLegacyProvider(array $provider, mixed $model, array &$groups): void
    {
        $inference = $groups['inference'] ?? [];
        $github = $groups['github'] ?? [];
        $capi = isset($github['capi']) && self::isMap($github['capi']) ? self::toMap($github['capi']) : null;
        if (array_key_exists('providers', $inference) || array_key_exists('defaultProvider', $inference)
            || ($capi !== null && array_key_exists('enabled', $capi))) {
            return;
        }

        $connection = ['name' => 'default'];
        $modelFields = [];
        foreach ($provider as $key => $value) {
            $key = (string) $key;
            if ($value === null || $key === 'name') {
                continue;
            }
            if (in_array($key, self::LEGACY_PROVIDER_MODEL_FIELDS, true)) {
                $modelFields[$key] = $value;
            } else {
                $connection[$key] = $value;
            }
        }

        $inference['providers'] = [$connection];
        $inference['defaultProvider'] = 'default';
        $github['capi'] = array_merge($capi ?? [], ['enabled' => false]);
        if ($modelFields !== [] && $model !== null) {
            $inference['models'] = [array_merge(['provider' => 'default', 'id' => $model], $modelFields)];
        }
        $groups['inference'] = $inference;
        $groups['github'] = $github;
    }

    /** True for associative arrays and plain objects (JSON objects). */
    private static function isMap(mixed $value): bool
    {
        if ($value instanceof \stdClass) {
            return true;
        }
        return is_array($value) && ($value === [] || !array_is_list($value));
    }

    /** @return array<string,mixed> */
    private static function toMap(mixed $value): array
    {
        return $value instanceof \stdClass ? get_object_vars($value) : (array) $value;
    }
}
