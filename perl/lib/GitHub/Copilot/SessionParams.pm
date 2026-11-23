package GitHub::Copilot::SessionParams;
# Copyright (c) Microsoft Corporation. All rights reserved.

use strict;
use warnings;
use Scalar::Util qw(blessed);
use Exporter 'import';

our @EXPORT_OK = qw(group_session_params apply_empty_mode_defaults);

=head1 NAME

GitHub::Copilot::SessionParams - SDK protocol 4 grouping of session payloads

=head1 DESCRIPTION

Internal helper that converts the flat C<session.create> / C<session.resume>
payload built by C<GitHub::Copilot::Types::SessionConfig-E<gt>to_wire> into the
grouped SDK protocol 4 shape (for example C<streaming> becomes
C<events.streaming>).

=cut

# Option groups of the SDK protocol 4 session payloads.
my %GROUP_NAMES = map { $_ => 1 } qw(
    inference tools permissions interaction events agents skills mcp github filesystem
    execution instructions hooks git state plugins canvases extensions experiments policy
);

# Flat wire keys only valid on session.create (dropped on resume).
my %CREATE_ONLY = map { $_ => 1 } qw(name cloud trustWorkingDirectory refreshCustomInstructions);

# Flat wire keys only valid on session.resume (dropped on create).
my %RESUME_ONLY = map { $_ => 1 } qw(
    disableResume suppressResumeEvent continuePendingWork allowTranscriptRecovery
    requestedEnvironmentVariables openCanvases
);

# Flat (SDK protocol 3) wire keys and the SDK protocol 4 [group, member] they move to.
# Root keys (sessionId, model, name, clientName, workingDirectory, configDirectory,
# enableConfigDiscovery, systemMessage, diagnostics, lifecycleDiagnostics, cloud,
# traceparent, tracestate and the resume controls) and unknown keys stay top level.
my %GROUPS = (
    reasoningEffort                    => [qw(inference reasoningEffort)],
    reasoningSummary                   => [qw(inference reasoningSummary)],
    contextTier                        => [qw(inference contextTier)],
    allowMultipleToolCalls             => [qw(inference allowMultipleToolCalls)],
    enableCitations                    => [qw(inference enableCitations)],
    modelCapabilities                  => [qw(inference capabilities)],
    verbosity                          => [qw(inference verbosity)],
    providers                          => [qw(inference providers)],
    models                             => [qw(inference models)],
    defaultProvider                    => [qw(inference defaultProvider)],
    tools                              => [qw(tools definitions)],
    availableTools                     => [qw(tools availableTools)],
    excludedTools                      => [qw(tools excludedTools)],
    toolSearch                         => [qw(tools search)],
    largeOutput                        => [qw(tools largeOutput)],
    imageGeneration                    => [qw(tools imageGeneration)],
    manageScheduleEnabled              => [qw(tools manageScheduleEnabled)],
    webSearch                          => [qw(tools webSearch)],
    shell                              => [qw(tools shell)],
    maxInlineBinaryBytes               => [qw(tools maxInlineBinaryBytes)],
    requestPermission                  => [qw(permissions request)],
    sandboxConfig                      => [qw(permissions sandbox)],
    sandbox                            => [qw(permissions sandbox)],
    permissionMode                     => [qw(permissions mode)],
    sandboxConfigSource                => [qw(permissions sandboxSource)],
    commands                           => [qw(interaction commands)],
    askUserVariant                     => [qw(interaction askUserVariant)],
    requestUserInput                   => [qw(interaction requestUserInput)],
    requestElicitation                 => [qw(interaction requestElicitation)],
    requestShellAuthentication         => [qw(interaction requestShellAuthentication)],
    requestExitPlanMode                => [qw(interaction requestExitPlanMode)],
    requestAutoModeSwitch              => [qw(interaction requestAutoModeSwitch)],
    requestAutoModeRecovery            => [qw(interaction requestAutoModeRecovery)],
    streaming                          => [qw(events streaming)],
    includeSubAgentStreamingEvents     => [qw(events includeSubAgentStreamingEvents)],
    customAgents                       => [qw(agents definitions)],
    defaultAgent                       => [qw(agents default)],
    agent                              => [qw(agents selected)],
    includedBuiltinAgents              => [qw(agents includedBuiltinAgents)],
    excludedBuiltinAgents              => [qw(agents excludedBuiltinAgents)],
    customAgentsLocalOnly              => [qw(agents localDiscoveryOnly)],
    agentDirectories                   => [qw(agents directories)],
    skillDirectories                   => [qw(skills directories)],
    enableSkills                       => [qw(skills enabled)],
    includedBuiltinSkills              => [qw(skills includedBuiltinSkills)],
    disabledSkills                     => [qw(skills disabledSkills)],
    hasSkillProvider                   => [qw(skills hasProvider)],
    ignoredSkillsLocations             => [qw(skills ignoredLocations)],
    mcpServers                         => [qw(mcp servers)],
    disabledMcpServers                 => [qw(mcp disabledServers)],
    requestMcpApps                     => [qw(mcp enableApps)],
    enableMcpApps                      => [qw(mcp enableApps)],
    mcpOAuthTokenStorage               => [qw(mcp oauthTokenStorage)],
    authClientIdMetadataUrl            => [qw(mcp authClientIdMetadataUrl)],
    envValueMode                       => [qw(mcp envValueMode)],
    managedMcpServers                  => [qw(mcp managedServers)],
    allowAllMcpServerInstructions      => [qw(mcp allowAllServerInstructions)],
    gitHubToken                        => [qw(github token)],
    gitHubTokenProviderRegistrationId  => [qw(github tokenProviderRegistrationId)],
    githubMcpToolConfig                => [qw(github mcp)],
    capi                               => [qw(github capi)],
    enableSessionTelemetry             => [qw(github telemetryEnabled)],
    enableManagedSettings              => [qw(github fetchManagedSettings)],
    interactiveUser                    => [qw(github interactiveUser)],
    enableGitHubTelemetryForwarding    => [qw(github telemetryForwarding)],
    authInfo                           => [qw(github authInfo)],
    copilotUrl                         => [qw(github copilotUrl)],
    additionalDirectories              => [qw(filesystem additionalDirectories)],
    enableFileChangeTracking           => [qw(filesystem trackFileChanges)],
    trustWorkingDirectory              => [qw(filesystem trustWorkingDirectory)],
    sessionLimits                      => [qw(execution limits)],
    autopilot                          => [qw(execution autopilot)],
    autopilotContinuation              => [qw(execution autopilotContinuation)],
    instructionDirectories             => [qw(instructions directories)],
    organizationCustomInstructions     => [qw(instructions organizationText)],
    enableOnDemandInstructionDiscovery => [qw(instructions onDemandDiscovery)],
    skipCustomInstructions             => [qw(instructions skipCustomInstructions)],
    refreshCustomInstructions          => [qw(instructions refreshCustomInstructions)],
    hooks                              => [qw(hooks hasHandlers)],
    hookDirectories                    => [qw(hooks directories)],
    enableFileHooks                    => [qw(hooks enableRepositoryHooks)],
    coauthorEnabled                    => [qw(git coauthorEnabled)],
    enableHostGitOperations            => [qw(git enableHostGitOperations)],
    infiniteSessions                   => [qw(state infiniteSessions)],
    memory                             => [qw(state memory)],
    hasMemoryStore                     => [qw(state hasMemoryStore)],
    enableSessionStore                 => [qw(state enableCrossSessionStore)],
    skipEmbeddingRetrieval             => [qw(state skipEmbeddingRetrieval)],
    embeddingCacheStorage              => [qw(state embeddingCacheStorage)],
    pluginDirectories                  => [qw(plugins directories)],
    installedPlugins                   => [qw(plugins installed)],
    canvases                           => [qw(canvases definitions)],
    canvasProvider                     => [qw(canvases provider)],
    requestCanvasRenderer              => [qw(canvases requestRenderer)],
    openCanvases                       => [qw(canvases open)],
    requestExtensions                  => [qw(extensions enabled)],
    extensionSdkPath                   => [qw(extensions sdkPath)],
    extensionInfo                      => [qw(extensions identity)],
    enableExperimentalMode             => [qw(experiments enableExperimentalMode)],
    isExperimentalMode                 => [qw(experiments enableExperimentalMode)],
    featureFlags                       => [qw(experiments flags)],
    expAssignments                     => [qw(experiments assignments)],
    featureFlagConfiguration           => [qw(experiments configuration)],
    allowedModels                      => [qw(policy allowedModels)],
    managedSettings                    => [qw(policy managedSettings)],
    excludedManagedSettings            => [qw(policy excludedManagedSettings)],
    enforceManagedModelDefaults        => [qw(policy enforceManagedModelDefaults)],
);

# Legacy single-provider fields that describe the model rather than the connection.
my %LEGACY_PROVIDER_MODEL_FIELDS = map { $_ => 1 } qw(
    modelId wireModel maxPromptTokens maxContextWindowTokens maxOutputTokens capabilities
);

# Session defaults applied under the caller's values when the client mode is "empty".
my @EMPTY_MODE_DEFAULTS = (
    [experiments  => enableExperimentalMode  => sub { \0 }],
    [github       => telemetryEnabled        => sub { \0 }],
    [mcp          => oauthTokenStorage       => sub { 'in-memory' }],
    [state        => skipEmbeddingRetrieval  => sub { \1 }],
    [state        => embeddingCacheStorage   => sub { 'in-memory' }],
    [state        => enableCrossSessionStore => sub { \0 }],
    [state        => memory                  => sub { { enabled => \0 } }],
    [instructions => onDemandDiscovery       => sub { \0 }],
    [hooks        => enableRepositoryHooks   => sub { \0 }],
    [git          => enableHostGitOperations => sub { \0 }],
);

# Unwraps blessed config objects (TO_JSON) into plain values.
sub _plain {
    my ($value) = @_;
    return $value->TO_JSON if blessed($value) && $value->can('TO_JSON');
    return $value;
}

=head2 group_session_params($flat, resume => $bool)

Returns the grouped payload for C<$flat> (a hashref). Root keys stay top level,
known flat keys move into their option group, and unknown keys pass through
unchanged. Undefined values and empty groups are dropped, except an explicit
C<defaultProvider =E<gt> undef> on resume, which clears the default provider.
Group objects already present in C<$flat> are merged and their members win.
A legacy single C<provider> becomes a C<default> named provider with Copilot API
models disabled, unless explicit C<providers>, C<defaultProvider> or
C<capi.enabled> are present. A non-object value under a group name that has no
flat mapping (such as the fork-specific C<skills> name list, which the runtime
never honored) cannot be represented in protocol 4 and is dropped.

=cut

sub group_session_params {
    my ($flat, %opts) = @_;
    my $resume = $opts{resume} ? 1 : 0;

    my (%payload, %groups, %explicit);
    my $legacy_provider;

    for my $key (sort keys %$flat) {
        my $value = $flat->{$key};
        next if !defined $value && !($resume && $key eq 'defaultProvider');
        next if $resume && $CREATE_ONLY{$key};
        next if !$resume && $RESUME_ONLY{$key};

        my $plain = _plain($value);
        if ($GROUP_NAMES{$key} && ref($plain) eq 'HASH') {
            $explicit{$key} = $plain;
        } elsif ($GROUP_NAMES{$key} && !$GROUPS{$key}) {
            next;
        } elsif ($key eq 'configDir') {
            $payload{configDirectory} = $value;
        } elsif ($key eq 'disableResume') {
            $payload{suppressResumeEvent} = $value;
        } elsif ($key eq 'provider') {
            $legacy_provider = $plain;
        } elsif (my $path = $GROUPS{$key}) {
            $groups{ $path->[0] }{ $path->[1] } = $value;
        } else {
            $payload{$key} = $value;
        }
    }

    for my $name (keys %explicit) {
        my $members = $explicit{$name};
        for my $member (keys %$members) {
            next unless defined $members->{$member};
            $groups{$name}{$member} = $members->{$member};
        }
    }

    _group_legacy_provider($legacy_provider, $payload{model}, \%groups)
        if ref($legacy_provider) eq 'HASH';

    if ($groups{mcp} && exists $groups{mcp}{servers} && !defined $groups{mcp}{envValueMode}) {
        $groups{mcp}{envValueMode} = 'direct';
    }

    for my $name (keys %groups) {
        $payload{$name} = $groups{$name} if %{ $groups{$name} };
    }
    return \%payload;
}

=head2 apply_empty_mode_defaults($payload)

Applies the "empty" client mode session defaults to a grouped payload in place
and returns it. Values the caller already set win.

=cut

sub apply_empty_mode_defaults {
    my ($payload) = @_;
    for my $default (@EMPTY_MODE_DEFAULTS) {
        my ($group, $member, $value) = @$default;
        $payload->{$group} = {} unless ref($payload->{$group}) eq 'HASH';
        $payload->{$group}{$member} = $value->() unless defined $payload->{$group}{$member};
    }
    return $payload;
}

# Translates a legacy single provider into inference.providers / inference.models /
# inference.defaultProvider and github.capi.enabled = false.
sub _group_legacy_provider {
    my ($provider, $model, $groups) = @_;

    my $inference = $groups->{inference} //= {};
    my $github    = $groups->{github}    //= {};
    my $capi      = _plain($github->{capi});
    my $explicit_capi = ref($capi) eq 'HASH' && exists $capi->{enabled};
    return if exists $inference->{providers} || exists $inference->{defaultProvider} || $explicit_capi;

    my %connection = (name => 'default');
    my %model_fields;
    for my $key (keys %$provider) {
        my $value = $provider->{$key};
        next if !defined $value || $key eq 'name';
        if ($LEGACY_PROVIDER_MODEL_FIELDS{$key}) {
            $model_fields{$key} = $value;
        } else {
            $connection{$key} = $value;
        }
    }

    $inference->{providers}       = [ \%connection ];
    $inference->{defaultProvider} = 'default';
    $github->{capi} = { %{ ref($capi) eq 'HASH' ? $capi : {} }, enabled => \0 };
    if (%model_fields && defined $model) {
        $inference->{models} = [ { provider => 'default', id => $model, %model_fields } ];
    }
    return;
}

1;
