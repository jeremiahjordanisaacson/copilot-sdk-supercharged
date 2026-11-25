"""Tests for generated RPC method behavior."""

import json
import subprocess
import sys
import typing
from copy import deepcopy
from datetime import timedelta
from pathlib import Path
from unittest.mock import AsyncMock, Mock

import pytest

from copilot._generated.rpc import (
    ClientGlobalApiHandlers,
    CustomizationReloadOutcome,
    CustomizationReloadStatus,
    CustomizationReloadSubsystem,
    FetchResponseMode,
    FetchSendResult,
    PlanSQLTodosRow,
    ToolsApi,
    ToolsExecuteRequest,
    UsageGetMetricsResult,
    UserSettingsApplyTopKeyRequest,
    UserSettingsNormalizeRequest,
    UserSettingsProblemsRequest,
    UserSettingsSetRequest,
    UserSettingsValidateRequest,
    UserSettingsWriteAllRequest,
    UserSettingsWriteKeyRequest,
    _InternalServerRpc,
    _SettingsRedactValueForDisplayRequest,
    _UserSettingsBuildRuntimeRequest,
    _UserSettingsEditRequest,
    _UserSettingsLoadAndMergeRepoRequest,
    _UserSettingsMergeManagedAndUserParams,
    _UserSettingsMergeRepoValueParams,
    _UserSettingsUpdateRequest,
    register_client_global_api_handlers,
)
from copilot._jsonrpc import JsonRpcClient
from copilot.rpc import (
    AccountsGetActiveResult,
    AutopilotObjectiveState,
    AutopilotObjectiveStatus,
    BuiltinToolInputSchemaType,
    CatalogInvalidRequestError,
    CommandsApi,
    CommandsInvokeRequest,
    CommandsRespondToQueuedCommandRequest,
    ExternalToolTextResultForLlm,
    HandlePendingToolCallRequest,
    HostDisposeRequest,
    HostEmptyResult,
    LocalSessionMetadataValue,
    LogResult,
    MCPDiscoverRequest,
    MCPDiscoverResult,
    MCPGrantType,
    MCPSecretSaveRequest,
    MCPSerializableServerConfig,
    MCPServerConfigHTTP,
    MCPServerConfigLocal,
    MCPStartServerRequest,
    PendingPermissionRequest,
    QueuedCommandHandled,
    QueuedCommandNotHandled,
    QuotaWarningProjection,
    RemoteControlStatusOff,
    RemoteControlStatusResult,
    RemoteSessionMetadataValue,
    SandboxConfig,
    SandboxConfigSource,
    ServerRpc,
    SessionList,
    SessionModelList,
    SessionRpc,
    SessionSandboxResetSessionChoicesRequest,
    ShellBuildInteractiveEnvironmentRequest,
    ShellBuildInteractiveEnvironmentResult,
    SlashCommandInfo,
    SlashCommandKind,
    SlashCommandTextResult,
    TaskAgentInfo,
    UIElicitationSchemaType,
    UpdateSubagentSettingsRequest,
)


@pytest.mark.asyncio
async def test_pending_tool_result_preserves_existing_positional_arguments():
    client = Mock(request=AsyncMock(return_value={"success": True}))
    result = ExternalToolTextResultForLlm(
        "large output", None, None, None, "success", "log", ["view"], {"source": "test"}
    )
    expected = {
        "textResultForLlm": "large output",
        "resultType": "success",
        "sessionLog": "log",
        "toolReferences": ["view"],
        "toolTelemetry": {"source": "test"},
    }

    await SessionRpc(client, "tools-session").tools.handle_pending_tool_call(
        HandlePendingToolCallRequest(request_id="request-1", result=result)
    )

    client.request.assert_awaited_once_with(
        "session.tools.handlePendingToolCall",
        {"sessionId": "tools-session", "requestId": "request-1", "result": expected},
    )
    expected["skipLargeOutputProcessing"] = True
    decoded = ExternalToolTextResultForLlm.from_dict(expected)
    assert decoded.tool_references == ["view"]
    assert decoded.tool_telemetry == {"source": "test"}
    assert decoded.skip_large_output_processing is True
    assert decoded.to_dict() == expected


@pytest.mark.parametrize("model", [MCPDiscoverRequest, MCPDiscoverResult])
def test_mcp_discovery_repr_redacts_configuration_without_changing_wire(model):
    configuration = {
        "type": "http",
        "url": "https://example.com/mcp",
        "env": {"TOKEN": "fake-discovery-token"},
        "headers": {"Authorization": "Bearer fake-discovery-authorization"},
    }
    if model is MCPDiscoverRequest:
        wire = {
            "includeConfiguration": True,
            "options": {
                "additionalConfig": [json.dumps({"mcpServers": {"configured": configuration}})],
                "disabledServers": ["disabled"],
            },
        }
    else:
        wire = {
            "servers": [
                {
                    "name": "configured",
                    "source": "user",
                    "enabled": True,
                    "configuration": configuration,
                }
            ],
            "diagnostics": [],
        }
    value = model.from_dict(wire)

    diagnostic = repr(value)
    assert "fake-discovery-token" not in diagnostic
    assert "fake-discovery-authorization" not in diagnostic
    assert ("disabled" if model is MCPDiscoverRequest else "configured") in diagnostic
    assert value.to_dict() == wire


def test_mcp_secret_save_repr_hides_credential_without_changing_wire():
    wire = {"secretId": "server/key", "value": "synthetic-mcp-credential"}
    request = MCPSecretSaveRequest.from_dict(wire)

    assert wire["value"] not in repr(request)
    assert wire["secretId"] in repr(request)
    assert request.to_dict() == wire


@pytest.mark.asyncio
@pytest.mark.parametrize("integer", [-(2**63) - 1, 2**64 + 1])
@pytest.mark.parametrize(
    "method,request_type,arguments,field",
    [
        ("set", UserSettingsSetRequest, {}, "settings"),
        ("_update", _UserSettingsUpdateRequest, {"key": "statusLine"}, "value"),
        ("_write_all", UserSettingsWriteAllRequest, {}, "settings"),
        ("_write_key", UserSettingsWriteKeyRequest, {"key": "statusLine"}, "value"),
        (
            "_apply_top_key",
            UserSettingsApplyTopKeyRequest,
            {"canonicalPath": ["statusLine"], "topKey": "statusLine"},
            "value",
        ),
        (
            "_edit",
            _UserSettingsEditRequest,
            {"canonicalPath": ["statusLine", "padding"], "operation": "set"},
            "value",
        ),
        ("_normalize", UserSettingsNormalizeRequest, {}, "settings"),
        ("_validate", UserSettingsValidateRequest, {"topKey": "statusLine"}, "value"),
        (
            "_build_runtime",
            _UserSettingsBuildRuntimeRequest,
            {"environment": {}, "platform": "linux", "workingDirectory": "/workspace"},
            "overrides",
        ),
        (
            "_load_and_merge_repo",
            _UserSettingsLoadAndMergeRepoRequest,
            {"repoRoot": "/workspace"},
            "userSettings",
        ),
        (
            "_merge_managed_and_user",
            _UserSettingsMergeManagedAndUserParams,
            {"managed": {}},
            "user",
        ),
        (
            "_merge_repo_value",
            _UserSettingsMergeRepoValueParams,
            {"repoSettings": {}},
            "userSettings",
        ),
        ("_diagnose_problems", UserSettingsProblemsRequest, {}, "sources"),
        (
            "_redact_value_for_display",
            _SettingsRedactValueForDisplayRequest,
            {"basePath": []},
            "value",
        ),
    ],
)
async def test_settings_inputs_reject_oversized_integers(
    method, request_type, arguments, field, integer
):
    client = Mock(
        request=AsyncMock(side_effect=AssertionError("An oversized integer reached the transport"))
    )
    value = integer if method == "_edit" else {"nested": [integer]}
    if method == "_diagnose_problems":
        value = [{"filePath": "/settings.json", "kind": "user", "label": "User", "config": value}]
    payload = {**arguments, field: value}
    params = request_type.from_dict(payload)
    if method == "set":
        api = ServerRpc(client).user.settings
    elif method == "_redact_value_for_display":
        api = _InternalServerRpc(client).settings
    else:
        api = _InternalServerRpc(client).user.settings

    with pytest.raises(ValueError, match="integer.*range"):
        await getattr(api, method)(params)

    client.request.assert_not_awaited()
    assert params.to_dict() == payload


@pytest.mark.asyncio
async def test_settings_set_preserves_supported_values_and_caller_objects():
    client = Mock(request=AsyncMock())
    shared = [-(2**63), 2**64 - 1, 1e20, "-9223372036854775809", None, True]
    value = {"first": shared, "second": shared, "tuple": (1e20,)}
    params = UserSettingsSetRequest(settings=value)

    await ServerRpc(client).user.settings.set(params)

    client.request.assert_awaited_once_with("user.settings.set", {"settings": value})
    assert params.settings is value
    assert value["first"] is shared
    assert value["second"] is shared
    assert value["tuple"] == (1e20,)


@pytest.mark.asyncio
@pytest.mark.parametrize("integer", [-(2**63) - 1, 2**64])
async def test_settings_update_rejects_oversized_condition_integers(integer):
    client = Mock(request=AsyncMock(return_value={"value": {}}))
    conditions = {"nested": [{"padding": integer}]}
    params = _UserSettingsUpdateRequest(
        key="statusLine", value={"command": "echo after"}, if_fields_match=conditions
    )

    with pytest.raises(ValueError, match="integer.*range"):
        await _InternalServerRpc(client).user.settings._update(params)

    client.request.assert_not_awaited()
    assert params.if_fields_match == {"nested": [{"padding": integer}]}


@pytest.mark.asyncio
async def test_settings_update_preserves_supported_condition_values():
    client = Mock(request=AsyncMock(return_value={"value": {}}))
    shared = [-(2**63), 2**64 - 1, 1e20, "-9223372036854775809", None, True]
    conditions = {"first": shared, "second": shared, "tuple": (1e20,)}
    params = _UserSettingsUpdateRequest(
        key="statusLine", value={"command": "echo after"}, if_fields_match=conditions
    )

    await _InternalServerRpc(client).user.settings._update(params)

    client.request.assert_awaited_once_with(
        "user.settings.update",
        {"key": "statusLine", "value": {"command": "echo after"}, "ifFieldsMatch": conditions},
    )
    assert conditions["first"] is shared
    assert conditions["second"] is shared
    assert conditions["tuple"] == (1e20,)


@pytest.mark.asyncio
async def test_settings_update_preserves_circular_reference_error():
    client = Mock(request=AsyncMock())
    conditions = {"nested": []}
    conditions["nested"].append(conditions)

    with pytest.raises(ValueError, match="Circular reference detected"):
        await _InternalServerRpc(client).user.settings._update(
            _UserSettingsUpdateRequest(key="statusLine", value={}, if_fields_match=conditions)
        )

    client.request.assert_not_awaited()


@pytest.mark.parametrize(
    "config,source,expected",
    [
        (
            SandboxConfig(enabled=True, add_current_working_directory=False),
            "user_enabled",
            {"enabled": True, "addCurrentWorkingDirectory": False},
        ),
        (
            {"enabled": True, "futurePreference": {"value": "kept"}},
            SandboxConfigSource.USER_ENABLED,
            {"enabled": True, "futurePreference": {"value": "kept"}},
        ),
        (None, SandboxConfigSource.USER_ENABLED, None),
    ],
)
@pytest.mark.asyncio
async def test_sandbox_reset_accepts_typed_config_and_source_without_changing_raw_json(
    config, source, expected
):
    original = deepcopy(config)
    client = Mock(
        request=AsyncMock(
            return_value={
                "config": expected,
                "configSource": "user_enabled",
                "autoApproveBypass": False,
                "disabledForSession": False,
                "incarnation": 1,
                "revision": 1,
                "sessionOverride": None,
            }
        )
    )
    api = SessionRpc(client, "sandbox-session").sandbox
    request = SessionSandboxResetSessionChoicesRequest(config=config, config_source=source)

    result = await api.reset_session_choices(request)

    client.request.assert_awaited_once()
    assert client.request.call_args.args[0] == "session.sandbox.resetSessionChoices"
    assert json.loads(json.dumps(client.request.call_args.args[1])) == {
        "sessionId": "sandbox-session",
        "config": expected,
        "configSource": "user_enabled",
    }
    assert result.config == expected
    assert request.config is config
    assert config == original
    assert request.config_source is source


@pytest.mark.parametrize(
    "model,environment_field",
    [
        (ShellBuildInteractiveEnvironmentRequest, "hostEnvironment"),
        (ShellBuildInteractiveEnvironmentResult, "environment"),
    ],
)
def test_shell_environment_repr_redacts_values_without_changing_wire(model, environment_field):
    environment = {
        "GITHUB_TOKEN": "fake-shell-token",
        "CUSTOM_CREDENTIAL": "fake-shell-custom-credential",
    }
    wire = {environment_field: environment}
    if model is ShellBuildInteractiveEnvironmentRequest:
        wire.update(additionalBlockedNames=[], caseInsensitive=False)
    value = model.from_dict(wire)

    for secret in environment.values():
        assert secret not in repr(value)
    assert value.to_dict() == wire


@pytest.mark.parametrize("mode", list(FetchResponseMode))
def test_fetch_response_mode_matches_request_mode_and_round_trips(mode):
    wire = {
        "responseMode": mode.value,
        "url": "https://example.com",
        "status": 200,
        "statusText": "OK",
        "headers": [],
    }
    if mode == FetchResponseMode.TEXT:
        wire.update(bodyText="hello", endedAtUnverifiedEof=False)
    elif mode == FetchResponseMode.FILE:
        wire.update(path="response.txt", sizeBytes=5, endedAtUnverifiedEof=False)
    result = FetchSendResult.from_dict(wire)
    assert result.response_mode == mode
    assert result.to_dict() == wire


@pytest.mark.parametrize("value", ["anthropic", "azure", "openai"])
def test_legacy_provider_type_remains_compatible_with_named_config_and_endpoint(value):
    from copilot import rpc

    kind = rpc.ProviderType(value)
    config = rpc.NamedProviderConfig(name="local", base_url="http://localhost:1234", type=kind)
    endpoint = rpc.ProviderEndpoint(base_url="http://localhost:1234", headers={}, type=kind)

    assert "ProviderType" in rpc.__all__
    assert config.to_dict()["type"] == value
    assert endpoint.to_dict()["type"] == value
    assert rpc.NamedProviderConfig.from_dict(config.to_dict()).type is kind
    assert rpc.ProviderEndpoint.from_dict(endpoint.to_dict()).type is kind
    assert config.type is kind
    assert endpoint.type is kind
    named = rpc.NamedProviderConfig.from_dict(
        {"name": "local", "baseUrl": config.base_url, "type": value}
    )
    assert named.type is kind
    assert named.to_dict()["type"] == value
    current = rpc.NamedProviderConfig(
        name="local", base_url=config.base_url, type=rpc.ProviderConfigType(value)
    )
    assert current.to_dict()["type"] == value
    assert current.type is rpc.ProviderConfigType(value)
    restored = rpc.NamedProviderConfig.from_dict(current.to_dict())
    assert restored.type is current.type
    assert restored == current
    named_current = rpc.NamedProviderConfig(
        name="local", base_url=config.base_url, type=current.type
    )
    assert rpc.NamedProviderConfig.from_dict(named_current.to_dict()) == named_current


def test_winml_provider_type_keeps_its_named_config_representation():
    from copilot import rpc

    config = rpc.NamedProviderConfig(
        name="local", base_url="model.gguf", type=rpc.ProviderConfigType.WINML
    )
    assert config.to_dict()["type"] == "winml"
    assert rpc.NamedProviderConfig.from_dict(config.to_dict()).type is rpc.ProviderConfigType.WINML
    with pytest.raises(ValueError):
        rpc.ProviderEndpoint.from_dict({"baseUrl": config.base_url, "headers": {}, "type": "winml"})
    with pytest.raises(ValueError):
        rpc.ProviderEndpoint(
            base_url=config.base_url, headers={}, type=rpc.ProviderConfigType.WINML
        ).to_dict()


@pytest.mark.parametrize(
    "model_name,enum_name,field,payload",
    [
        (
            "LocalModelGPURequirement",
            "LocalModelGPURequirementKind",
            "kind",
            {"kind": "unknown"},
        ),
        (
            "LocalModelTimelineMessage",
            "SessionLogLevel",
            "type",
            {"type": "info", "text": "Model ready"},
        ),
    ],
)
def test_local_model_values_preserve_published_types_and_enum_identities(
    model_name, enum_name, field, payload
):
    from copilot import rpc

    restored = getattr(rpc, model_name).from_dict(payload)

    assert getattr(restored, field) is getattr(rpc, enum_name)(payload[field])
    assert restored.to_dict() == payload


def test_http_oauth_grant_uses_the_published_shared_enum():
    payload = {"url": "https://example.test/mcp", "oauthGrantType": "client_credentials"}
    restored = MCPServerConfigHTTP.from_dict(payload)

    assert restored.oauth_grant_type is MCPGrantType.CLIENT_CREDENTIALS
    configured = MCPServerConfigHTTP(
        url=payload["url"], oauth_grant_type=MCPGrantType.CLIENT_CREDENTIALS
    )
    assert configured.to_dict() == payload


def test_shared_permission_union_rejects_an_unrelated_dataclass():
    request = PendingPermissionRequest(request=HostEmptyResult(), request_id="request")

    with pytest.raises(AssertionError):
        request.to_dict()


@pytest.mark.parametrize("value,error", [(42, AttributeError), (None, TypeError)])
def test_uuid_rpc_field_preserves_stdlib_conversion_errors(value, error):
    with pytest.raises(error):
        LogResult.from_dict({"eventId": value})


@pytest.mark.asyncio
async def test_raw_setting_write_keeps_required_null():
    client = Mock()
    client.request = AsyncMock(return_value=None)
    await _InternalServerRpc(client).user.settings._write_key(
        UserSettingsWriteKeyRequest(key="showReasoning", value=None)
    )
    client.request.assert_awaited_once_with(
        "user.settings.writeKey", {"key": "showReasoning", "value": None}
    )


def test_catalog_product_validation_refusal_preserves_typed_field():
    payload = {
        "kind": "invalid-request",
        "field": "product",
        "message": "The product must contain between 1 and 256 characters.",
    }

    result = CatalogInvalidRequestError.from_dict(payload)

    assert result.kind == "invalid-request"
    assert result.field.value == "product"
    assert result.field.name == "PRODUCT"
    assert result.to_dict() == payload


async def test_settings_redaction_keeps_required_opaque_null():
    client = Mock(request=AsyncMock(return_value={"value": None}))

    result = await _InternalServerRpc(client).settings._redact_value_for_display(
        _SettingsRedactValueForDisplayRequest(value=None, base_path=[])
    )

    client.request.assert_awaited_once_with(
        "settings.redactValueForDisplay", {"value": None, "basePath": []}
    )
    assert result.value is None


@pytest.mark.parametrize(
    "context_settings",
    [{}, {"contextManagementTools": True}, {"contextManagementTools": False}],
    ids=["absent", "enabled", "disabled"],
)
async def test_subagent_settings_getter_setter_preserve_context_management_tools(context_settings):
    settings = {
        "agents": {
            "rubber-duck": {
                "model": "model-id",
                "providerId": "provider-id",
                "autoInvoke": False,
            },
            "task": {"model": "task-model", "contextTier": "long_context"},
        },
        "disabledSubagents": ["explore"],
        "maxConcurrency": 7,
        "maxDepth": 3,
        **context_settings,
    }
    client = Mock(request=AsyncMock(side_effect=[{"subagents": settings}, {}]))
    api = SessionRpc(client, "settings-session").tools

    result = await api.get_subagent_settings()
    assert result.subagents is not None
    assert result.subagents.agents is not None
    result.subagents.agents["rubber-duck"].auto_invoke = True
    await api.update_subagent_settings(UpdateSubagentSettingsRequest(subagents=result.subagents))

    expected = {
        **settings,
        "agents": {
            **settings["agents"],
            "rubber-duck": {**settings["agents"]["rubber-duck"], "autoInvoke": True},
        },
    }
    assert client.request.await_count == 2
    client.request.assert_awaited_with(
        "session.tools.updateSubagentSettings",
        {"sessionId": "settings-session", "subagents": expected},
    )


async def test_session_usage_get_metrics_deserializes_shared_event_type():
    payload = {
        "codeChanges": {
            "filesModified": ["example.py"],
            "filesModifiedCount": 1,
            "linesAdded": 4,
            "linesRemoved": 2,
        },
        "lastCallInputTokens": 12,
        "lastCallOutputTokens": 7,
        "modelMetrics": {},
        "sessionStartTime": "2026-10-07T00:00:00Z",
        "totalApiDurationMs": 125,
        "totalPremiumRequestCost": 1.5,
        "totalUserRequests": 2,
        "aiCreditsStatus": "partial",
    }
    client = Mock(request=AsyncMock(return_value=payload))

    result = await SessionRpc(client, "usage-session").usage.get_metrics(timeout=2.5)

    assert isinstance(result, UsageGetMetricsResult)
    assert result.code_changes.lines_added == 4
    assert result.last_call_input_tokens == 12
    assert result.total_user_requests == 2
    assert result.session_start_time.isoformat() == "2026-10-07T00:00:00+00:00"
    assert result.total_api_duration == timedelta(milliseconds=125)
    assert result.ai_credits_status.value == "partial"
    client.request.assert_awaited_once_with(
        "session.usage.getMetrics", {"sessionId": "usage-session"}, timeout=2.5
    )


async def test_quota_warnings_deserialize_records_and_empty_lists():
    payload = [
        {
            "warningType": "quota",
            "message": "Usage is almost exhausted",
            "url": "https://example.test",
        },
        {"warningType": "limit", "message": "A limit was reached"},
    ]
    client = Mock(request=AsyncMock(side_effect=[payload, []]))
    api = SessionRpc(client, "quota-session").quota

    warnings = await api.take_warnings()

    assert all(isinstance(warning, QuotaWarningProjection) for warning in warnings)
    assert [warning.warning_type for warning in warnings] == ["quota", "limit"]
    assert warnings[0].message == payload[0]["message"]
    assert warnings[0].url == payload[0]["url"]
    assert warnings[1].url is None
    assert [warning.to_dict() for warning in warnings] == payload
    assert await api.take_warnings() == []
    assert client.request.await_count == 2
    client.request.assert_awaited_with("session.quota.takeWarnings", {"sessionId": "quota-session"})


def test_customization_reload_outcome_preserves_future_enum_values():
    payload = {"status": "newStatus", "subsystem": "newSubsystem", "detail": "new component"}
    outcome = CustomizationReloadOutcome.from_dict(payload)

    assert outcome.status.value == payload["status"]
    assert outcome.subsystem.value == payload["subsystem"]
    assert outcome.to_dict() == payload
    assert outcome.status is CustomizationReloadStatus("newStatus")
    assert outcome.subsystem is CustomizationReloadSubsystem("newSubsystem")
    assert CustomizationReloadOutcome.from_dict(payload).status is outcome.status
    assert CustomizationReloadOutcome.from_dict(payload).subsystem is outcome.subsystem
    assert CustomizationReloadStatus("anotherStatus") is not outcome.status
    assert CustomizationReloadSubsystem("anotherSubsystem") is not outcome.subsystem
    assert CustomizationReloadStatus("reloaded") is CustomizationReloadStatus.RELOADED
    assert CustomizationReloadSubsystem("skills") is CustomizationReloadSubsystem.SKILLS
    with pytest.raises(ValueError):
        CustomizationReloadStatus(None)


async def test_host_dispose_deserializes_empty_acknowledgement():
    client = Mock(request=AsyncMock(return_value={}))

    result = await ServerRpc(client).host.dispose(HostDisposeRequest(host_id="host"))

    assert isinstance(result, HostEmptyResult)
    assert result.to_dict() == {}
    client.request.assert_awaited_once_with("host.dispose", {"hostId": "host"})


async def test_host_shutdown_handler_round_trips_empty_acknowledgement():
    client = JsonRpcClient(Mock())
    client._send_message = AsyncMock()
    host = Mock(shutdown=AsyncMock(return_value=HostEmptyResult()))
    register_client_global_api_handlers(client, ClientGlobalApiHandlers(host=host))

    await client._dispatch_request(
        {"jsonrpc": "2.0", "id": "shutdown", "method": "host.shutdown", "params": {}},
        client.request_handlers["host.shutdown"],
        cancellation_id=None,
    )

    host.shutdown.assert_awaited_once()
    assert isinstance(host.shutdown.call_args.args[0], HostEmptyResult)
    client._send_message.assert_awaited_once_with(
        {"jsonrpc": "2.0", "id": "shutdown", "result": {}}
    )


def test_slash_command_info_preserves_existing_positional_parameters():
    command = SlashCommandInfo(
        True, "Select a model", SlashCommandKind.BUILTIN, "model", ["md"], False, None, True
    )
    wire = {
        "allowDuringAgentExecution": True,
        "description": "Select a model",
        "kind": "builtin",
        "name": "model",
        "aliases": ["md"],
        "experimental": False,
        "schedulable": True,
    }
    assert command.experimental is False
    assert command.schedulable is True
    assert command.to_dict() == wire
    assert SlashCommandInfo.from_dict(wire) == command

    extended = SlashCommandInfo(
        True,
        "Select a model",
        SlashCommandKind.BUILTIN,
        "model",
        ["md"],
        False,
        None,
        True,
        available_at_session_start=True,
        cli_args_json='{"model": {}}',
        display_name="/model",
        plugin_name="catalog-plugin",
        skill_name="catalog-skill",
        source="runtime",
        stops_queue_processing=False,
    )
    extended_wire = {
        **wire,
        "availableAtSessionStart": True,
        "cliArgsJson": '{"model": {}}',
        "displayName": "/model",
        "pluginName": "catalog-plugin",
        "skillName": "catalog-skill",
        "source": "runtime",
        "stopsQueueProcessing": False,
    }
    assert extended.to_dict() == extended_wire
    assert SlashCommandInfo.from_dict(extended_wire) == extended


def test_autopilot_objective_state_preserves_existing_positional_parameters():
    state = AutopilotObjectiveState(
        "0", 1, "task", AutopilotObjectiveStatus.PAUSED, 0, None, None, "autopilot_off"
    )
    wire = {
        "creditCountNanoAiu": "0",
        "id": 1,
        "objective": "task",
        "status": "paused",
        "turnCount": 0,
        "pauseReason": "autopilot_off",
    }
    assert state.pause_reason == "autopilot_off"
    assert state.credit_limit is None
    assert state.to_dict() == wire
    assert AutopilotObjectiveState.from_dict(wire) == state

    extended = AutopilotObjectiveState(
        "0",
        1,
        "task",
        AutopilotObjectiveStatus.PAUSED,
        0,
        None,
        None,
        "autopilot_off",
        created_at="2026-11-14T17:00:00Z",
        resume_count=2,
    )
    extended_wire = {**wire, "createdAt": "2026-11-14T17:00:00Z", "resumeCount": 2}
    assert extended.to_dict() == extended_wire
    assert AutopilotObjectiveState.from_dict(extended_wire) == extended


def test_sandbox_config_round_trips_allow_bypass_and_omits_when_absent():
    configured = SandboxConfig(enabled=True, allow_bypass=True)

    assert configured.to_dict() == {"enabled": True, "allowBypass": True}
    assert SandboxConfig.from_dict(configured.to_dict()).allow_bypass is True
    assert SandboxConfig(enabled=True).to_dict() == {"enabled": True}


def test_sandbox_config_rejects_null_allow_temp_storage_but_accepts_omission():
    assert SandboxConfig.from_dict({"enabled": True}).allow_temp_storage is None

    with pytest.raises(ValueError, match="null is not allowed"):
        SandboxConfig.from_dict({"enabled": True, "allowTempStorage": None})


def test_sandbox_config_rejects_null_allow_temp_storage_with_optimized_python():
    script = """
from copilot._generated.rpc import SandboxConfig

try:
    SandboxConfig.from_dict({"enabled": True, "allowTempStorage": None})
except ValueError:
    pass
else:
    raise SystemExit("null allowTempStorage was accepted")
"""
    subprocess.run([sys.executable, "-O", "-c", script], check=True, cwd=Path(__file__).parent)


def test_optional_rpc_fields_without_non_null_contract_accept_null():
    row = PlanSQLTodosRow.from_dict({"id": "todo", "description": None})

    assert row.description is None


@pytest.mark.asyncio
@pytest.mark.parametrize(
    "model,payload",
    [
        (
            MCPSerializableServerConfig,
            {"type": "stdio", "command": "example", "args": []},
        ),
        (
            MCPServerConfigLocal,
            {
                "type": "local-http",
                "socketPath": "/runtime/ide.sock",
                "headers": {},
                "ide": {
                    "ideName": "Test IDE",
                    "workspaceFolder": "/workspace",
                    "pid": 123,
                    "lockFilePath": "/runtime/ide.lock",
                    "clientVersion": "1.0.0",
                    "clientPid": 456,
                    "parentPid": 1,
                },
            },
        ),
    ],
)
async def test_mcp_start_accepts_existing_and_local_config_models(model, payload):
    client = Mock()
    client.request = AsyncMock(return_value=None)
    config = model.from_dict(payload)

    await SessionRpc(client, "ide-session").mcp.start_server(
        MCPStartServerRequest(server_name="ide", config=config)
    )

    client.request.assert_awaited_once_with(
        "session.mcp.startServer",
        {"sessionId": "ide-session", "serverName": "ide", "config": payload},
    )
    assert config.to_dict() == payload


@pytest.mark.asyncio
@pytest.mark.parametrize(
    "config",
    [
        {
            "type": "http",
            "url": "https://example.test/mcp",
            "headers": {"Authorization": "Bearer test-token", "X-Workspace": "test"},
        },
        {
            "type": "local-http",
            "socketPath": "/runtime/ide.sock",
            "headers": {"Authorization": "Bearer test-token"},
        },
    ],
)
async def test_mcp_start_request_preserves_header_values(config):
    client = Mock()
    client.request = AsyncMock(return_value=None)
    payload = {"serverName": "ide", "config": config}
    request = MCPStartServerRequest.from_dict(payload)

    await SessionRpc(client, "ide-session").mcp.start_server(request)

    assert request.to_dict() == payload
    client.request.assert_awaited_once_with(
        "session.mcp.startServer", {"sessionId": "ide-session", **payload}
    )


def test_active_account_result_preserves_null_and_identity_metadata():
    payload = {
        "account": None,
        "authInfo": {"type": "user", "host": "https://github.com", "login": "octocat"},
        "authSelectionId": "active-credential",
    }
    result = AccountsGetActiveResult.from_dict(payload)
    assert result.auth_info is not None
    assert result.auth_info.login == "octocat"
    assert result.auth_selection_id == "active-credential"
    assert result.account is None
    assert result.to_dict() == payload
    assert AccountsGetActiveResult().to_dict() == {"account": None}


def test_model_list_round_trips_metadata_without_remapping_existing_fields():
    payload = {
        "list": [{"id": "fixture-model"}],
        "modelPriceCategories": [],
        "providers": [],
        "quotaSnapshots": {"premium": {"remaining": 42}},
        "auto": {
            "defaultTier": "premium-v2",
            "tiers": [
                {
                    "id": "premium-v2",
                    "displayName": "Premium",
                    "description": "Provider tier",
                    "type": "auto",
                    "status": {"enabled": True},
                }
            ],
        },
    }

    result = SessionModelList.from_dict(payload)

    assert result.auto is not None
    assert result.auto.default_tier == "premium-v2"
    assert result.auto.tiers[0].id == "premium-v2"
    assert result.model_price_categories == []
    assert result.providers == []
    assert result.quota_snapshots == payload["quotaSnapshots"]
    assert result.to_dict() == payload


@pytest.mark.asyncio
async def test_commands_invoke_deserializes_slash_command_result():
    client = AsyncMock()
    client.request = AsyncMock(return_value={"kind": "text", "text": "hello", "markdown": True})
    api = CommandsApi(client, "sess-1")

    result = await api.invoke(CommandsInvokeRequest(name="help"))

    assert isinstance(result, SlashCommandTextResult)
    assert result.text == "hello"
    assert result.markdown is True


@pytest.mark.parametrize(
    "payload",
    [
        "plain tool output",
        {
            "textResultForLlm": "Tool 'missing' is not available.",
            "resultType": "failure",
            "error": "tool not available",
        },
    ],
)
async def test_tools_execute_returns_a_value_of_its_annotated_type(payload):
    client = Mock(request=AsyncMock(return_value=payload))

    result = await SessionRpc(client, "tools-session").tools.execute(
        ToolsExecuteRequest(name="missing", arguments={})
    )

    assert result == payload
    annotation = typing.get_type_hints(ToolsApi.execute)["return"]
    allowed = tuple(typing.get_origin(t) or t for t in typing.get_args(annotation) or (annotation,))
    assert isinstance(result, allowed), f"{type(result).__name__} is not {annotation}"
    client.request.assert_awaited_once_with(
        "session.tools.execute",
        {"name": "missing", "arguments": {}, "sessionId": "tools-session"},
    )


def test_remote_control_status_deserializes_string_discriminated_union():
    result = RemoteControlStatusResult.from_dict({"status": {"state": "off"}})

    assert isinstance(result.status, RemoteControlStatusOff)
    assert result.status.state == "off"
    assert result.status.to_dict() == {"state": "off"}


def test_ui_elicitation_schema_type_preserves_public_alias():
    assert UIElicitationSchemaType is BuiltinToolInputSchemaType


def test_session_list_deserializes_boolean_discriminated_entries():
    payload = {
        "sessions": [
            {
                "sessionId": "example-local",
                "startTime": "2026-07-26T10:00:00.000Z",
                "modifiedTime": "2026-07-26T10:05:00.000Z",
                "isRemote": False,
            },
            {
                "sessionId": "example-remote",
                "startTime": "2026-07-26T11:00:00.000Z",
                "modifiedTime": "2026-07-26T11:05:00.000Z",
                "isRemote": True,
                "remoteSessionIds": ["example-remote"],
                "repository": {"owner": "github", "name": "copilot-sdk", "branch": "main"},
            },
        ]
    }

    result = SessionList.from_dict(payload)

    local, remote = result.sessions
    assert isinstance(local, LocalSessionMetadataValue)
    assert local.session_id == "example-local"
    assert local.is_remote is False
    assert isinstance(remote, RemoteSessionMetadataValue)
    assert remote.session_id == "example-remote"
    assert remote.is_remote is True
    assert remote.repository.owner == "github"


def test_task_agent_info_deserializes_integral_float_milliseconds():
    task = TaskAgentInfo.from_dict(
        {
            "agentType": "general-purpose",
            "description": "Example task",
            "id": "agent-1",
            "prompt": "Do the task",
            "startedAt": "2026-08-19T12:00:00Z",
            "status": "running",
            "toolCallId": "tool-1",
            "type": "agent",
            "activeTimeMs": 43.0,
        }
    )

    assert task.active_time_ms == 43
    assert task.to_dict()["activeTimeMs"] == 43


@pytest.mark.parametrize(
    ("handled", "expected_type"),
    [(True, QueuedCommandHandled), (False, QueuedCommandNotHandled)],
)
def test_queued_command_result_deserializes_boolean_discriminator(handled, expected_type):
    request = CommandsRespondToQueuedCommandRequest.from_dict(
        {"requestId": "example-request", "result": {"handled": handled}}
    )

    assert isinstance(request.result, expected_type)


@pytest.mark.parametrize(
    ("variant", "expected_handled", "expected_json"),
    [
        (QueuedCommandHandled(), True, '{"handled": true}'),
        (QueuedCommandNotHandled(), False, '{"handled": false}'),
    ],
)
def test_queued_command_result_serializes_boolean_discriminator(
    variant, expected_handled, expected_json
):
    encoded = variant.to_dict()

    assert encoded["handled"] is expected_handled
    assert json.dumps(encoded) == expected_json

    request = CommandsRespondToQueuedCommandRequest(request_id="example-request", result=variant)
    round_tripped = CommandsRespondToQueuedCommandRequest.from_dict(request.to_dict())

    assert request.to_dict()["result"]["handled"] is expected_handled
    assert isinstance(round_tripped.result, type(variant))
