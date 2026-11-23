! -------------------------------------------------------------------------------------
!  Copyright (c) Microsoft Corporation. All rights reserved.
! -------------------------------------------------------------------------------------

!> SDK protocol 4 session payload translation.
!! The client builds the same flat session.create / session.resume payload it always has;
!! group_session_params converts it to the grouped v4 wire shape (for example
!! streaming -> events.streaming) right before the request is sent.
module copilot_session_wire
  use copilot_json
  implicit none
  private

  public :: group_session_params, apply_empty_mode_defaults, session_member_allowed

  integer, parameter :: GROUP_COUNT = 20
  integer, parameter :: GROUP_NAME_LEN = 12
  integer, parameter :: MAPPING_LEN = 72

  !> v4 option groups, in the order they are emitted.
  character(len=GROUP_NAME_LEN), parameter :: GROUP_NAMES(GROUP_COUNT) = [character(len=GROUP_NAME_LEN) :: &
    'inference', 'tools', 'permissions', 'interaction', 'events', 'agents', 'skills', 'mcp', &
    'github', 'filesystem', 'execution', 'instructions', 'hooks', 'git', 'state', 'plugins', &
    'canvases', 'extensions', 'experiments', 'policy']

  !> Keys that stay at the top level of the payload.
  character(len=32), parameter :: ROOT_KEYS(*) = [character(len=32) :: &
    'sessionId', 'model', 'name', 'clientName', 'workingDirectory', 'configDirectory', &
    'enableConfigDiscovery', 'systemMessage', 'diagnostics', 'lifecycleDiagnostics', 'cloud', &
    'traceparent', 'tracestate', 'suppressResumeEvent', 'continuePendingWork', &
    'allowTranscriptRecovery', 'requestedEnvironmentVariables']

  !> Flat wire key = v4 group.member. Group members are exactly the targets listed here.
  character(len=MAPPING_LEN), parameter :: MAPPINGS(*) = [character(len=MAPPING_LEN) :: &
    'reasoningEffort=inference.reasoningEffort', &
    'reasoningSummary=inference.reasoningSummary', &
    'contextTier=inference.contextTier', &
    'allowMultipleToolCalls=inference.allowMultipleToolCalls', &
    'enableCitations=inference.enableCitations', &
    'modelCapabilities=inference.capabilities', &
    'verbosity=inference.verbosity', &
    'providers=inference.providers', &
    'models=inference.models', &
    'defaultProvider=inference.defaultProvider', &
    'tools=tools.definitions', &
    'availableTools=tools.availableTools', &
    'excludedTools=tools.excludedTools', &
    'toolSearch=tools.search', &
    'largeOutput=tools.largeOutput', &
    'imageGeneration=tools.imageGeneration', &
    'manageScheduleEnabled=tools.manageScheduleEnabled', &
    'webSearch=tools.webSearch', &
    'shell=tools.shell', &
    'maxInlineBinaryBytes=tools.maxInlineBinaryBytes', &
    'requestPermission=permissions.request', &
    'sandboxConfig=permissions.sandbox', &
    'sandbox=permissions.sandbox', &
    'permissionMode=permissions.mode', &
    'sandboxConfigSource=permissions.sandboxSource', &
    'commands=interaction.commands', &
    'askUserVariant=interaction.askUserVariant', &
    'requestUserInput=interaction.requestUserInput', &
    'requestElicitation=interaction.requestElicitation', &
    'requestShellAuthentication=interaction.requestShellAuthentication', &
    'requestExitPlanMode=interaction.requestExitPlanMode', &
    'requestAutoModeSwitch=interaction.requestAutoModeSwitch', &
    'requestAutoModeRecovery=interaction.requestAutoModeRecovery', &
    'streaming=events.streaming', &
    'includeSubAgentStreamingEvents=events.includeSubAgentStreamingEvents', &
    'customAgents=agents.definitions', &
    'defaultAgent=agents.default', &
    'agent=agents.selected', &
    'includedBuiltinAgents=agents.includedBuiltinAgents', &
    'excludedBuiltinAgents=agents.excludedBuiltinAgents', &
    'customAgentsLocalOnly=agents.localDiscoveryOnly', &
    'agentDirectories=agents.directories', &
    'skillDirectories=skills.directories', &
    'enableSkills=skills.enabled', &
    'includedBuiltinSkills=skills.includedBuiltinSkills', &
    'disabledSkills=skills.disabledSkills', &
    'hasSkillProvider=skills.hasProvider', &
    'ignoredSkillsLocations=skills.ignoredLocations', &
    'mcpServers=mcp.servers', &
    'disabledMcpServers=mcp.disabledServers', &
    'requestMcpApps=mcp.enableApps', &
    'enableMcpApps=mcp.enableApps', &
    'mcpOAuthTokenStorage=mcp.oauthTokenStorage', &
    'authClientIdMetadataUrl=mcp.authClientIdMetadataUrl', &
    'envValueMode=mcp.envValueMode', &
    'managedMcpServers=mcp.managedServers', &
    'allowAllMcpServerInstructions=mcp.allowAllServerInstructions', &
    'gitHubToken=github.token', &
    'gitHubTokenProviderRegistrationId=github.tokenProviderRegistrationId', &
    'githubMcpToolConfig=github.mcp', &
    'capi=github.capi', &
    'enableSessionTelemetry=github.telemetryEnabled', &
    'enableManagedSettings=github.fetchManagedSettings', &
    'interactiveUser=github.interactiveUser', &
    'enableGitHubTelemetryForwarding=github.telemetryForwarding', &
    'authInfo=github.authInfo', &
    'copilotUrl=github.copilotUrl', &
    'additionalDirectories=filesystem.additionalDirectories', &
    'enableFileChangeTracking=filesystem.trackFileChanges', &
    'trustWorkingDirectory=filesystem.trustWorkingDirectory', &
    'sessionLimits=execution.limits', &
    'autopilot=execution.autopilot', &
    'autopilotContinuation=execution.autopilotContinuation', &
    'instructionDirectories=instructions.directories', &
    'organizationCustomInstructions=instructions.organizationText', &
    'enableOnDemandInstructionDiscovery=instructions.onDemandDiscovery', &
    'skipCustomInstructions=instructions.skipCustomInstructions', &
    'refreshCustomInstructions=instructions.refreshCustomInstructions', &
    'hooks=hooks.hasHandlers', &
    'hookDirectories=hooks.directories', &
    'enableFileHooks=hooks.enableRepositoryHooks', &
    'coauthorEnabled=git.coauthorEnabled', &
    'enableHostGitOperations=git.enableHostGitOperations', &
    'infiniteSessions=state.infiniteSessions', &
    'memory=state.memory', &
    'hasMemoryStore=state.hasMemoryStore', &
    'enableSessionStore=state.enableCrossSessionStore', &
    'skipEmbeddingRetrieval=state.skipEmbeddingRetrieval', &
    'embeddingCacheStorage=state.embeddingCacheStorage', &
    'pluginDirectories=plugins.directories', &
    'installedPlugins=plugins.installed', &
    'canvases=canvases.definitions', &
    'canvasProvider=canvases.provider', &
    'requestCanvasRenderer=canvases.requestRenderer', &
    'openCanvases=canvases.open', &
    'requestExtensions=extensions.enabled', &
    'extensionSdkPath=extensions.sdkPath', &
    'extensionInfo=extensions.identity', &
    'enableExperimentalMode=experiments.enableExperimentalMode', &
    'isExperimentalMode=experiments.enableExperimentalMode', &
    'featureFlags=experiments.flags', &
    'expAssignments=experiments.assignments', &
    'featureFlagConfiguration=experiments.configuration', &
    'allowedModels=policy.allowedModels', &
    'managedSettings=policy.managedSettings', &
    'excludedManagedSettings=policy.excludedManagedSettings', &
    'enforceManagedModelDefaults=policy.enforceManagedModelDefaults']

  !> Legacy single-provider fields that describe the model rather than the connection.
  character(len=24), parameter :: PROVIDER_MODEL_FIELDS(*) = [character(len=24) :: &
    'modelId', 'wireModel', 'maxPromptTokens', 'maxContextWindowTokens', 'maxOutputTokens', &
    'capabilities']

contains

  !> Convert a flat session.create (is_resume = .false.) or session.resume (is_resume = .true.)
  !! payload into the grouped SDK protocol 4 payload. Root keys stay at the top level, known
  !! flat keys move to group.member, unknown keys stay at the root, null values are skipped,
  !! empty groups are not emitted, and members of already-grouped objects win over flat keys.
  !! The runtime rejects unknown group members, so only members from the mapping table are
  !! sent, honoring create-only and resume-only members.
  function group_session_params(flat_json, is_resume) result(grouped_json)
    character(len=*), intent(in) :: flat_json
    logical, intent(in) :: is_resume
    character(len=:), allocatable :: grouped_json
    type(json_object) :: flat, root, explicit_group
    type(json_object) :: groups(GROUP_COUNT)
    character(len=:), allocatable :: key, value, path, group_name, member_name, provider_json
    integer :: i, j, g, dot
    logical :: has_provider, first

    call json_parse_object(flat_json, flat)
    has_provider = .false.
    provider_json = ''
    do i = 1, flat%count
      key = flat%items(i)%name
      value = flat%items(i)%value
      if (len(value) == 0) cycle
      if (json_is_null(value)) cycle
      if (is_root_key(key)) then
        call json_object_put(root, key, value, .true.)
      else if (key == 'configDir') then
        call json_object_put(root, 'configDirectory', value, .false.)
      else if (key == 'disableResume') then
        if (is_resume) call json_object_put(root, 'suppressResumeEvent', value, .false.)
      else if (key == 'provider') then
        if (json_is_object(value)) then
          provider_json = value
          has_provider = .true.
        end if
      else if (group_index(key) > 0 .and. json_is_object(value)) then
        g = group_index(key)
        call json_parse_object(value, explicit_group)
        do j = 1, explicit_group%count
          member_name = explicit_group%items(j)%name
          if (session_member_allowed(key, member_name, is_resume)) then
            call json_object_put(groups(g), member_name, explicit_group%items(j)%value, .true.)
          end if
        end do
      else
        path = flat_key_target(key)
        if (len(path) == 0) then
          call json_object_put(root, key, value, .true.)
        else
          dot = index(path, '.')
          group_name = path(1:dot - 1)
          member_name = path(dot + 1:)
          g = group_index(group_name)
          if (session_member_allowed(group_name, member_name, is_resume)) then
            call json_object_put(groups(g), member_name, value, .false.)
            if (key == 'mcpServers') call json_object_put(groups(g), 'envValueMode', '"direct"', .false.)
          end if
        end if
      end if
    end do
    if (has_provider) call translate_legacy_provider(provider_json, root, groups)

    grouped_json = '{'
    first = .true.
    do i = 1, root%count
      call append_member(grouped_json, first, root%items(i)%name, root%items(i)%value)
    end do
    do g = 1, GROUP_COUNT
      if (groups(g)%count == 0) cycle
      call append_member(grouped_json, first, trim(GROUP_NAMES(g)), json_object_text(groups(g)))
    end do
    grouped_json = grouped_json // '}'
  end function group_session_params

  !> Add the client mode "empty" defaults under the caller's values in a flat payload.
  !! Members the caller already set (with a non-null value) are kept.
  function apply_empty_mode_defaults(flat_json) result(json)
    character(len=*), intent(in) :: flat_json
    character(len=:), allocatable :: json
    type(json_object) :: flat

    call json_parse_object(flat_json, flat)
    call put_default(flat, 'enableExperimentalMode', 'false')
    call put_default(flat, 'enableSessionTelemetry', 'false')
    call put_default(flat, 'mcpOAuthTokenStorage', '"in-memory"')
    call put_default(flat, 'skipEmbeddingRetrieval', 'true')
    call put_default(flat, 'embeddingCacheStorage', '"in-memory"')
    call put_default(flat, 'enableSessionStore', 'false')
    call put_default(flat, 'memory', '{"enabled":false}')
    call put_default(flat, 'enableOnDemandInstructionDiscovery', 'false')
    call put_default(flat, 'enableFileHooks', 'false')
    call put_default(flat, 'enableHostGitOperations', 'false')
    json = json_object_text(flat)
  end function apply_empty_mode_defaults

  !> True when group.member is a v4 member the runtime accepts for this request kind.
  function session_member_allowed(group_name, member_name, is_resume) result(allowed)
    character(len=*), intent(in) :: group_name, member_name
    logical, intent(in) :: is_resume
    logical :: allowed
    character(len=:), allocatable :: path
    integer :: i, eq

    path = group_name // '.' // member_name
    allowed = .false.
    do i = 1, size(MAPPINGS)
      eq = index(MAPPINGS(i), '=')
      if (trim(MAPPINGS(i)(eq + 1:)) == path) then
        allowed = .true.
        exit
      end if
    end do
    if (.not. allowed) return
    if (is_resume) then
      if (path == 'filesystem.trustWorkingDirectory') allowed = .false.
      if (path == 'instructions.refreshCustomInstructions') allowed = .false.
    else
      if (path == 'canvases.open') allowed = .false.
    end if
  end function session_member_allowed

  ! --------------------------------------------------------------------------
  ! Internal helpers
  ! --------------------------------------------------------------------------

  !> Add a default value unless the key already has a non-null value.
  subroutine put_default(flat, key, value)
    type(json_object), intent(inout) :: flat
    character(len=*), intent(in) :: key, value
    integer :: idx

    idx = json_object_find(flat, key)
    if (idx == 0) then
      call json_object_add(flat, key, value)
    else if (json_is_null(flat%items(idx)%value)) then
      flat%items(idx)%value = value
    end if
  end subroutine put_default

  !> Append "name":value to a JSON object being built.
  subroutine append_member(text, first, name, value)
    character(len=:), allocatable, intent(inout) :: text
    logical, intent(inout) :: first
    character(len=*), intent(in) :: name, value

    if (.not. first) text = text // ','
    text = text // json_quote(name) // ':' // value
    first = .false.
  end subroutine append_member

  !> True for keys that stay at the top level.
  function is_root_key(key) result(is_root)
    character(len=*), intent(in) :: key
    logical :: is_root
    integer :: i

    is_root = .false.
    do i = 1, size(ROOT_KEYS)
      if (trim(ROOT_KEYS(i)) == key .and. len_trim(ROOT_KEYS(i)) == len(key)) then
        is_root = .true.
        return
      end if
    end do
  end function is_root_key

  !> Index of a v4 group name, or 0.
  function group_index(name) result(idx)
    character(len=*), intent(in) :: name
    integer :: idx
    integer :: i

    idx = 0
    do i = 1, GROUP_COUNT
      if (trim(GROUP_NAMES(i)) == name .and. len_trim(GROUP_NAMES(i)) == len(name)) then
        idx = i
        return
      end if
    end do
  end function group_index

  !> v4 path (group.member) of a known flat key, or '' for root/unknown keys.
  function flat_key_target(key) result(path)
    character(len=*), intent(in) :: key
    character(len=:), allocatable :: path
    integer :: i, eq

    path = ''
    do i = 1, size(MAPPINGS)
      eq = index(MAPPINGS(i), '=')
      if (MAPPINGS(i)(1:eq - 1) == key .and. eq - 1 == len(key)) then
        path = trim(MAPPINGS(i)(eq + 1:))
        return
      end if
    end do
  end function flat_key_target

  !> True for legacy provider fields that belong on inference.models.
  function is_provider_model_field(name) result(is_model)
    character(len=*), intent(in) :: name
    logical :: is_model
    integer :: i

    is_model = .false.
    do i = 1, size(PROVIDER_MODEL_FIELDS)
      if (trim(PROVIDER_MODEL_FIELDS(i)) == name .and. len_trim(PROVIDER_MODEL_FIELDS(i)) == len(name)) then
        is_model = .true.
        return
      end if
    end do
  end function is_provider_model_field

  !> Translate the legacy single BYOK provider into a named "default" provider.
  !! Explicit providers, defaultProvider, or capi.enabled win over the legacy option.
  subroutine translate_legacy_provider(provider_json, root, groups)
    character(len=*), intent(in) :: provider_json
    type(json_object), intent(in) :: root
    type(json_object), intent(inout) :: groups(:)
    type(json_object) :: provider, capi
    character(len=:), allocatable :: connection, model_fields, model_value
    integer :: i, gi, gg, capi_idx, model_idx

    gi = group_index('inference')
    gg = group_index('github')
    capi_idx = json_object_find(groups(gg), 'capi')
    if (capi_idx > 0) then
      call json_parse_object(groups(gg)%items(capi_idx)%value, capi)
      if (json_object_find(capi, 'enabled') > 0) return
    end if
    if (json_object_find(groups(gi), 'providers') > 0) return
    if (json_object_find(groups(gi), 'defaultProvider') > 0) return

    call json_parse_object(provider_json, provider)
    connection = '{"name":"default"'
    model_fields = ''
    do i = 1, provider%count
      if (json_is_null(provider%items(i)%value)) cycle
      if (is_provider_model_field(provider%items(i)%name)) then
        model_fields = model_fields // ',' // json_quote(provider%items(i)%name) // ':' // &
                       provider%items(i)%value
      else if (provider%items(i)%name /= 'name') then
        connection = connection // ',' // json_quote(provider%items(i)%name) // ':' // &
                     provider%items(i)%value
      end if
    end do
    connection = connection // '}'
    call json_object_put(groups(gi), 'providers', '[' // connection // ']', .false.)
    call json_object_put(groups(gi), 'defaultProvider', '"default"', .false.)

    ! v1 BYOK sessions did not use Copilot API models.
    if (capi_idx > 0) then
      call json_object_put(capi, 'enabled', 'false', .true.)
      groups(gg)%items(capi_idx)%value = json_object_text(capi)
    else
      call json_object_put(groups(gg), 'capi', '{"enabled":false}', .false.)
    end if

    model_idx = json_object_find(root, 'model')
    if (model_idx == 0 .or. len(model_fields) == 0) return
    model_value = root%items(model_idx)%value
    if (.not. json_is_string(model_value) .or. len(model_value) <= 2) return
    call json_object_put(groups(gi), 'models', '[{"provider":"default","id":' // model_value // &
                         model_fields // '}]', .false.)
  end subroutine translate_legacy_provider

end module copilot_session_wire
