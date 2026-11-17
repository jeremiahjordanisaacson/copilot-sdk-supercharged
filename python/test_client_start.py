"""Startup concurrency regressions without a live CLI runtime."""

import asyncio
import subprocess
import sys
import threading
from datetime import UTC, datetime
from unittest.mock import AsyncMock, Mock

import pytest

import copilot
from copilot import (
    CopilotClient,
    HostDiagnostics,
    HostErrorReportingStatus,
    PingResult,
    RuntimeConnection,
)
from copilot._jsonrpc import JsonRpcClient, JsonRpcError, ProcessExitedError
from copilot._sdk_protocol_version import get_sdk_protocol_version
from copilot.client import _InternalTestOptions


@pytest.fixture
def client():
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_spawned_stdio(path="unused-cli")
    )
    client._start_cli_server = AsyncMock()
    client._connect_to_server = AsyncMock()
    client._verify_protocol_version = AsyncMock()
    return client


@pytest.mark.parametrize(
    "name,options",
    [
        ("github.token", {"github": {"token": ""}}),
        ("github.use_logged_in_user", {"github": {"use_logged_in_user": False}}),
        ("observability.telemetry", {"observability": {"telemetry": {}}}),
        ("observability.log_level", {"observability": {"log_level": "debug"}}),
        ("observability.log_level", {"observability": {"log_level": "info"}}),
        ("_test_options", {"_test_options": _InternalTestOptions()}),
    ],
)
def test_attached_runtime_rejects_runtime_only_options(name, options):
    with pytest.raises(ValueError, match=name):
        CopilotClient(
            mode="coding-agent",
            runtime=RuntimeConnection.for_tcp("localhost", 4321),
            **options,
        )


@pytest.mark.parametrize("options", [{"base_directory": ""}, {"session_idle_timeout_seconds": 0}])
def test_attached_runtime_has_no_owned_startup_options(options):
    with pytest.raises(TypeError, match="unexpected keyword argument"):
        RuntimeConnection.for_tcp("localhost", 4321, **options)


@pytest.mark.parametrize("options", [{}, {"observability": {"log_level": None}}])
async def test_attached_runtime_accepts_defaults_and_connection_callbacks(options):
    client = CopilotClient(
        mode="empty",
        runtime=RuntimeConnection.for_tcp("localhost", 4321),
        models={"on_list_models": lambda: []},
        **options,
    )
    assert await client.list_models() == []
    await client.force_stop()


@pytest.mark.parametrize("inprocess", [False, True])
@pytest.mark.parametrize("log_level", [None, "info"])
async def test_runtime_log_level_is_forwarded_only_when_configured(
    monkeypatch, inprocess, log_level
):
    process = Mock(poll=Mock(return_value=0))
    spawn = Mock(return_value=process)
    create_host = Mock(return_value=Mock(process=process))
    monkeypatch.setattr("copilot.client.subprocess.Popen", spawn)
    monkeypatch.setattr("copilot.client.FfiRuntimeHost.create", create_host)
    monkeypatch.setattr(CopilotClient, "_resolve_inprocess_runtime", lambda self: sys.executable)
    connection = (
        RuntimeConnection.for_inprocess()
        if inprocess
        else RuntimeConnection.for_spawned_stdio(path=sys.executable)
    )
    options = {} if log_level is None else {"observability": {"log_level": log_level}}
    client = CopilotClient(mode="coding-agent", runtime=connection, **options)
    try:
        await client._start_cli_server()
        args = create_host.call_args.kwargs["args"] if inprocess else spawn.call_args.args[0]
        if log_level is None:
            assert "--log-level" not in args
        else:
            assert args[args.index("--log-level") + 1] == log_level
    finally:
        await client.force_stop()


async def test_auth_status_uses_snake_case_attributes_and_preserves_wire_keys(client):
    payload = {
        "isAuthenticated": True,
        "authType": "token",
        "host": "github.com",
        "login": "test-user",
        "statusMessage": "Authenticated",
    }
    client._client = Mock(request=AsyncMock(return_value=payload))

    status = await client.get_auth_status()

    client._client.request.assert_awaited_once_with("auth.getStatus", {})
    assert status.is_authenticated is True
    assert status.auth_type == "token"
    assert status.status_message == "Authenticated"
    assert status.to_dict() == payload


@pytest.mark.parametrize("timestamp", ["2026-01-01T00:00:00Z", 1767225600000])
async def test_ping_preserves_legacy_timestamps_in_generated_result(client, timestamp):
    client._client = Mock(
        request=AsyncMock(
            return_value={
                "message": "echo",
                "timestamp": timestamp,
                "protocolVersion": get_sdk_protocol_version(),
            }
        )
    )
    response = await client.ping("echo")
    assert isinstance(response, PingResult)
    assert response.message == "echo"
    assert response.timestamp == datetime(2026, 1, 1, tzinfo=UTC)
    assert response.protocol_version == get_sdk_protocol_version()
    client._client.request.assert_awaited_once_with("ping", {"message": "echo"})


@pytest.mark.parametrize(
    ("category", "price", "policy", "thinking"),
    [
        ("future-capability", "high", "enabled", "optional"),
        ("powerful", "future-price", "enabled", "optional"),
        ("powerful", "high", "future-policy", "optional"),
        ("powerful", "high", "enabled", "future-thinking"),
    ],
)
async def test_list_models_preserves_unknown_metadata_values(
    client, category, price, policy, thinking
):
    payload = {
        "id": "future-model",
        "name": "Future model",
        "capabilities": {"supports": {"adaptive_thinking": thinking}},
        "policy": {"state": policy},
        "modelPickerCategory": category,
        "modelPickerPriceCategory": price,
    }
    client._client = Mock(request=AsyncMock(return_value={"models": [payload]}))

    models = await client.list_models()

    assert len(models) == 1
    assert isinstance(models[0], copilot.Model)
    assert models[0].model_picker_category.value == category
    assert models[0].model_picker_price_category.value == price
    assert models[0].policy.state.value == policy
    assert models[0].capabilities.supports.adaptive_thinking.value == thinking
    assert models[0].to_dict() == payload
    client._client.request.assert_awaited_once_with("models.list", {})


@pytest.mark.parametrize(
    "vision",
    [
        {"supported_media_types": ["image/png"]},
        {"max_prompt_images": 2, "max_prompt_image_size": 4096},
    ],
)
async def test_list_models_accepts_partial_vision_metadata(client, vision):
    client._client = Mock(
        request=AsyncMock(
            return_value={
                "models": [
                    {
                        "id": "partial-vision",
                        "name": "Partial vision",
                        "capabilities": {"limits": {"vision": vision}},
                    }
                ]
            }
        )
    )

    models = await client.list_models()

    assert len(models) == 1
    limits = models[0].capabilities.limits.vision
    assert limits.supported_media_types == vision.get("supported_media_types", [])
    assert limits.max_prompt_images == vision.get("max_prompt_images", 0)
    assert limits.max_prompt_image_size == vision.get("max_prompt_image_size", 0)
    client._client.request.assert_awaited_once_with("models.list", {})


@pytest.mark.parametrize("error_reporting", [None, "pending", "enabled", "disabled"])
async def test_get_status_preserves_optional_host_diagnostics(client, error_reporting):
    payload = {"version": "1.2.3", "protocolVersion": 2}
    if error_reporting is not None:
        payload["hostDiagnostics"] = {"errorReporting": error_reporting}
    client._client = Mock()
    client._client.request = AsyncMock(return_value=payload)

    status = await client.get_status()

    client._client.request.assert_awaited_once_with("status.get", {})
    assert status.version == "1.2.3"
    assert status.protocol_version == 2
    if error_reporting is None:
        assert status.host_diagnostics is None
    else:
        assert isinstance(status.host_diagnostics, HostDiagnostics)
        assert status.host_diagnostics.error_reporting is HostErrorReportingStatus(error_reporting)
        assert status.host_diagnostics.to_dict() == {"errorReporting": error_reporting}
    assert status.to_dict() == payload


def test_host_error_reporting_status_is_public():
    assert "HostErrorReportingStatus" in copilot.__all__
    diagnostics = HostDiagnostics(error_reporting=HostErrorReportingStatus.ENABLED)

    assert diagnostics.to_dict() == {"errorReporting": "enabled"}


@pytest.mark.parametrize(
    "phase", ["_start_cli_server", "_connect_to_server", "_verify_protocol_version"]
)
async def test_concurrent_start_initializes_transport_once(client, phase):
    entered = asyncio.Event()
    release = asyncio.Event()

    async def block():
        entered.set()
        await release.wait()

    getattr(client, phase).side_effect = block
    first = asyncio.create_task(client.start())
    await asyncio.wait_for(entered.wait(), timeout=1)
    second = asyncio.create_task(client.start())
    try:
        await asyncio.sleep(0)
        assert not second.done()
    finally:
        release.set()
        await asyncio.wait_for(asyncio.gather(first, second), timeout=1)

    client._start_cli_server.assert_awaited_once()
    client._connect_to_server.assert_awaited_once()
    client._verify_protocol_version.assert_awaited_once()
    await client.start()
    client._start_cli_server.assert_awaited_once()


async def test_start_can_retry_after_failure(client):
    client._start_cli_server.side_effect = [RuntimeError("startup failed"), None]

    with pytest.raises(RuntimeError, match="startup failed"):
        await client.start()
    await asyncio.wait_for(client.start(), timeout=1)

    assert client._start_cli_server.await_count == 2
    client._connect_to_server.assert_awaited_once()
    client._verify_protocol_version.assert_awaited_once()


@pytest.mark.parametrize("owner", ["process", "ffi"])
async def test_restart_after_connection_loss_disposes_previous_owner(client, owner):
    old_rpc = Mock(stop=AsyncMock(), _loop=asyncio.get_running_loop())
    # A running owner: the connection is lost only when its reader closes.
    old_owner = Mock(stdin=None, poll=Mock(return_value=None))
    client._client = old_rpc
    client._state = "connected"
    if owner == "process":
        client._process = client._cli_process = old_owner
    else:
        client._ffi_host = old_owner
    reader_waiting = asyncio.Event()
    release_reader = threading.Event()
    loop = asyncio.get_running_loop()
    sessions_lock = client._sessions_lock
    reader = threading.Thread(target=client._handle_connection_close)

    class GatedLock:
        def __enter__(self):
            if threading.current_thread() is reader:
                loop.call_soon_threadsafe(reader_waiting.set)
                release_reader.wait()
            return sessions_lock.__enter__()

        def __exit__(self, *args):
            return sessions_lock.__exit__(*args)

    client._sessions_lock = GatedLock()

    async def restart():
        old_rpc.stop.assert_awaited_once()
        if owner == "process":
            old_owner.kill.assert_called_once_with()
            old_owner.wait.assert_called_once()
        else:
            old_owner.dispose.assert_called_once_with()
        client._client = Mock(stop=AsyncMock())

    reader.start()
    try:
        await asyncio.wait_for(reader_waiting.wait(), timeout=1)
        await client.start()
        client._start_cli_server.assert_not_awaited()
        assert client._client is old_rpc

        release_reader.set()
        await asyncio.to_thread(reader.join)
        client._start_cli_server.side_effect = restart
        await client.start()
        client._verify_protocol_version.assert_awaited_once()
    finally:
        release_reader.set()
        await asyncio.to_thread(reader.join)
        await client.force_stop()


async def test_list_models_joins_restart_of_exited_stdio_owner(client):
    loop = asyncio.get_running_loop()
    closing = asyncio.Event()
    release_close = threading.Event()
    old_rpc = Mock(request=AsyncMock(), stop=AsyncMock(), _loop=loop)
    old_owner = Mock(stdin=None, poll=Mock(return_value=0))
    new_rpc = Mock(
        request=AsyncMock(
            return_value={
                "models": [{"id": "replacement", "name": "Replacement", "capabilities": {}}]
            }
        ),
        stop=AsyncMock(),
    )

    def report_close():
        loop.call_soon_threadsafe(closing.set)
        release_close.wait()
        client._handle_connection_close()

    async def connect():
        client._client = new_rpc

    old_rpc._report_close.side_effect = report_close
    client._client = old_rpc
    client._process = client._cli_process = old_owner
    client._state = "connected"
    client._connect_to_server.side_effect = connect
    starting = asyncio.create_task(client.start())
    listing = None
    try:
        await asyncio.wait_for(closing.wait(), timeout=1)
        listing = asyncio.create_task(client.list_models())
        await _settle(listing)
        assert not listing.done()
        release_close.set()
        await asyncio.wait_for(starting, timeout=1)
        models = await asyncio.wait_for(listing, timeout=1)
        assert [model.id for model in models] == ["replacement"]
        assert await client.list_models() == models
        old_rpc.request.assert_not_awaited()
        new_rpc.request.assert_awaited_once_with("models.list", {})
    finally:
        release_close.set()
        tasks = [starting, *([listing] if listing is not None else [])]
        await asyncio.gather(*tasks, return_exceptions=True)
        await client.force_stop()


@pytest.mark.parametrize("operation", ["pending-request", "request", "notify"])
async def test_start_after_request_fails_on_lost_connection_reconnects(client, operation):
    # The runtime stand-in exits when it receives the first request byte.
    with subprocess.Popen(
        [sys._base_executable, "-c", "import sys; sys.stdin.buffer.read(1)"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0),
    ) as process:
        rpc = JsonRpcClient(process)
        caller_started = threading.Event()
        closing = asyncio.Event()
        loop = asyncio.get_running_loop()

        def on_close():
            loop.call_soon_threadsafe(closing.set)
            # Connection teardown can take a while; a caller that sees the failed
            # request must not get ahead of it.
            caller_started.wait(timeout=0.5)
            client._handle_connection_close()

        rpc.on_close = on_close
        client._client = rpc
        client._process = client._cli_process = process
        client._state = "connected"
        rpc.start()
        pending = asyncio.create_task(rpc.request("ping"))
        try:
            if operation != "pending-request":
                await asyncio.wait_for(closing.wait(), timeout=5)
            with pytest.raises(ProcessExitedError):
                await asyncio.wait_for(
                    pending if operation == "pending-request" else getattr(rpc, operation)("ping"),
                    timeout=5,
                )
            await client.start()
            caller_started.set()

            client._start_cli_server.assert_awaited_once()
            assert client._client is not rpc
        finally:
            caller_started.set()
            await client.force_stop()
            pending.cancel()
            await asyncio.gather(pending, return_exceptions=True)


@pytest.mark.parametrize("owner", ["process", "ffi"])
@pytest.mark.parametrize("operation", ["start", "stop"])
async def test_restart_retries_owner_cleanup_after_cancellation(client, owner, operation):
    old_rpc = Mock(stop=AsyncMock(), _loop=asyncio.get_running_loop())
    old_owner = Mock(stdin=None, poll=Mock(return_value=None))
    client._client = old_rpc
    client._state = "disconnected"
    client._connection_lost = True
    entered = asyncio.Event()
    finished = asyncio.Event()
    release = threading.Event()
    loop = asyncio.get_running_loop()

    def cleanup(*args, **kwargs):
        loop.call_soon_threadsafe(entered.set)
        release.wait()
        loop.call_soon_threadsafe(finished.set)

    if owner == "process":
        client._process = client._cli_process = old_owner
        old_owner.wait.side_effect = cleanup
    else:
        client._ffi_host = old_owner
        old_owner.dispose.side_effect = cleanup

    async def restart():
        old_rpc.stop.assert_awaited_once()
        if owner == "process":
            assert old_owner.wait.call_count == 2
        else:
            assert old_owner.dispose.call_count == 2
        client._client = Mock(stop=AsyncMock())

    task = asyncio.create_task(getattr(client, operation)())
    try:
        await asyncio.wait_for(entered.wait(), timeout=1)
        task.cancel()
        with pytest.raises(asyncio.CancelledError):
            await task
        release.set()
        await asyncio.wait_for(finished.wait(), timeout=1)
        client._start_cli_server.side_effect = restart
        await client.start()
    finally:
        release.set()
        if not task.done():
            task.cancel()
        await asyncio.gather(task, return_exceptions=True)
        if entered.is_set():
            await asyncio.wait_for(finished.wait(), timeout=1)
        await client.force_stop()


async def test_cancelling_waiting_start_does_not_cancel_active_start(client):
    entered = asyncio.Event()
    release = asyncio.Event()

    async def block():
        entered.set()
        await release.wait()

    client._start_cli_server.side_effect = block
    first = asyncio.create_task(client.start())
    await asyncio.wait_for(entered.wait(), timeout=1)
    second = asyncio.create_task(client.start())
    try:
        await asyncio.sleep(0)
        second.cancel()
        with pytest.raises(asyncio.CancelledError):
            _ = await second
        assert not first.done()
    finally:
        release.set()
        await asyncio.wait_for(first, timeout=1)

    await client.start()
    client._start_cli_server.assert_awaited_once()
    client._verify_protocol_version.assert_awaited_once()


STATUS_CALLS = {
    "ping": (
        "ping",
        {"message": None},
        {"message": "pong", "timestamp": "2026-01-01T00:00:00Z", "protocolVersion": 3},
    ),
    "get_status": ("status.get", {}, {"version": "1.2.3", "protocolVersion": 3}),
    "get_auth_status": ("auth.getStatus", {}, {"isAuthenticated": False}),
    "list_models": ("models.list", {}, {"models": []}),
}

CONVENIENCE_CALLS = {
    **{method: ((), *request) for method, request in STATUS_CALLS.items()},
    "list_sessions": ((), "session.list", {}, {"sessions": []}),
    "get_last_session_id": ((), "session.getLastId", {}, {}),
    "get_session_metadata": (("one",), "session.getMetadata", {"sessionId": "one"}, {}),
    "delete_session": (("one",), "session.delete", {"sessionId": "one"}, {"success": True}),
    "get_foreground_session_id": ((), "session.getForeground", {}, {}),
    "set_foreground_session_id": (
        ("one",),
        "session.setForeground",
        {"sessionId": "one"},
        {"success": True},
    ),
}


@pytest.mark.parametrize("method", list(CONVENIENCE_CALLS))
async def test_convenience_call_lazily_starts_before_first_request(client, method):
    args, wire_method, params, response = CONVENIENCE_CALLS[method]
    rpc = Mock(request=AsyncMock(return_value=response), stop=AsyncMock())
    entered = asyncio.Event()
    release = asyncio.Event()

    async def connect():
        client._client = rpc

    async def verify():
        entered.set()
        await release.wait()

    client._connect_to_server.side_effect = connect
    client._verify_protocol_version.side_effect = verify
    with pytest.raises(RuntimeError, match="Client is not connected"):
        _ = client.rpc
    call = asyncio.create_task(getattr(client, method)(*args))
    try:
        await _settle(call)
        if call.done():
            await call
        await asyncio.wait_for(entered.wait(), timeout=1)
        rpc.request.assert_not_awaited()
    finally:
        release.set()
    await asyncio.wait_for(call, timeout=1)
    rpc.request.assert_awaited_once_with(wire_method, params)
    client._start_cli_server.assert_awaited_once()
    if method == "list_models":
        assert await client.list_models() == []
        rpc.request.assert_awaited_once()
    await client.force_stop()


@pytest.mark.parametrize("shutdown_method", ["stop", "force_stop"])
async def test_convenience_calls_fail_during_shutdown_and_restart_afterward(
    client, shutdown_method
):
    entered = asyncio.Event()
    release = asyncio.Event()

    async def stop():
        entered.set()
        await release.wait()

    rpc = Mock(request=AsyncMock(), stop=AsyncMock(side_effect=stop))
    client._client = rpc
    client._state = "connected"
    shutdown = asyncio.create_task(getattr(client, shutdown_method)())
    try:
        await asyncio.wait_for(entered.wait(), timeout=1)
        for method, (args, *_) in CONVENIENCE_CALLS.items():
            with pytest.raises(RuntimeError, match="disconnected|not connected"):
                await getattr(client, method)(*args)
        client._start_cli_server.assert_not_awaited()
        rpc.request.assert_not_awaited()
    finally:
        release.set()
        await asyncio.wait_for(shutdown, timeout=1)

    replacement = Mock(
        request=AsyncMock(return_value={"version": "restarted", "protocolVersion": 3}),
        stop=AsyncMock(),
    )

    async def connect():
        client._client = replacement

    client._connect_to_server.side_effect = connect
    assert (await client.get_status()).version == "restarted"
    client._start_cli_server.assert_awaited_once()
    await client.force_stop()


async def test_convenience_call_does_not_restart_lost_connection(client):
    old_rpc = Mock(
        request=AsyncMock(side_effect=RuntimeError("Connection closed")),
        stop=AsyncMock(),
        _loop=None,
    )
    replacement = Mock(
        request=AsyncMock(return_value={"version": "replacement", "protocolVersion": 3}),
        stop=AsyncMock(),
    )

    async def connect():
        client._client = replacement

    client._connect_to_server.side_effect = connect
    client._client = old_rpc
    client._state = "connected"
    client._handle_connection_close()
    try:
        with pytest.raises(RuntimeError, match="Connection closed"):
            await client.get_status()
        client._start_cli_server.assert_not_awaited()
        old_rpc.stop.assert_not_awaited()
        assert client._client is old_rpc

        await client.start()
        assert (await client.get_status()).version == "replacement"
        client._start_cli_server.assert_awaited_once()
        client._connect_to_server.assert_awaited_once()
        old_rpc.stop.assert_awaited_once()
    finally:
        await client.force_stop()


async def test_lazy_start_failure_is_not_retried_within_the_same_call(client):
    failure = RuntimeError("runtime launch failed")
    client._start_cli_server.side_effect = [failure, None]
    with pytest.raises(RuntimeError, match="runtime launch failed") as exc:
        await client.list_sessions()
    assert exc.value is failure
    client._start_cli_server.assert_awaited_once()

    rpc = Mock(request=AsyncMock(return_value={"sessions": []}), stop=AsyncMock())

    async def connect():
        client._client = rpc

    client._connect_to_server.side_effect = connect
    assert await client.list_sessions() == []
    assert client._start_cli_server.await_count == 2
    rpc.request.assert_awaited_once_with("session.list", {})
    await client.force_stop()


async def test_custom_model_callback_never_starts_the_runtime_after_stop(client):
    client._on_list_models = Mock(return_value=[])
    assert await client.list_models() == []
    await client.force_stop()
    assert await client.list_models() == []
    assert client._on_list_models.call_count == 2
    client._start_cli_server.assert_not_awaited()


@pytest.mark.parametrize("retirement", ["stop", "force_stop", "connection_loss"])
async def test_queued_model_listings_cannot_restart_after_retirement(
    client, retirement, monkeypatch
):
    fetching = asyncio.Event()
    release = asyncio.Event()
    queued_waiting = asyncio.Event()
    cache_lock = client._models_cache_lock
    acquire = cache_lock.acquire

    async def acquire_cache():
        if cache_lock.locked():
            queued_waiting.set()
        return await acquire()

    monkeypatch.setattr(cache_lock, "acquire", acquire_cache)

    async def fetch(method, params):
        assert (method, params) == ("models.list", {})
        fetching.set()
        await release.wait()
        raise RuntimeError("retired transport")

    old_rpc = Mock(
        request=AsyncMock(side_effect=fetch),
        stop=AsyncMock(),
        _loop=asyncio.get_running_loop(),
    )
    replacement = Mock(request=AsyncMock(return_value={"models": []}), stop=AsyncMock())
    client._client = old_rpc
    client._state = "connected"

    async def connect():
        client._client = replacement

    client._connect_to_server.side_effect = connect
    first = asyncio.create_task(client.list_models())
    queued = None
    try:
        await asyncio.wait_for(fetching.wait(), timeout=1)
        queued = asyncio.create_task(client.list_models())
        await asyncio.wait_for(queued_waiting.wait(), timeout=1)
        assert not queued.done()
        if retirement == "connection_loss":
            client._handle_connection_close()
        else:
            await getattr(client, retirement)()
        release.set()
        with pytest.raises(RuntimeError, match="retired transport"):
            await asyncio.wait_for(first, timeout=1)
        with pytest.raises(RuntimeError, match="disconnected|not connected"):
            await asyncio.wait_for(queued, timeout=1)
        client._start_cli_server.assert_not_awaited()
        old_rpc.request.assert_awaited_once_with("models.list", {})
        replacement.request.assert_not_awaited()

        if retirement == "connection_loss":
            await client.start()
        assert await client.list_models() == []
        client._start_cli_server.assert_awaited_once()
        replacement.request.assert_awaited_once_with("models.list", {})
    finally:
        release.set()
        tasks = [task for task in (first, queued) if task is not None]
        await asyncio.gather(*tasks, return_exceptions=True)
        await client.force_stop()


@pytest.mark.parametrize("shutdown_method", ["stop", "force_stop"])
async def test_custom_model_callback_stays_offline_during_shutdown(client, shutdown_method):
    disposing = asyncio.Event()
    release = asyncio.Event()

    async def stop():
        disposing.set()
        await release.wait()

    rpc = Mock(request=AsyncMock(), stop=AsyncMock(side_effect=stop))
    client._client = rpc
    client._on_list_models = Mock(return_value=[])
    shutdown = asyncio.create_task(getattr(client, shutdown_method)())
    try:
        await asyncio.wait_for(disposing.wait(), timeout=1)
        assert await client.list_models() == []
        client._on_list_models.assert_called_once_with()
        client._start_cli_server.assert_not_awaited()
        rpc.request.assert_not_awaited()
    finally:
        release.set()
        await asyncio.wait_for(shutdown, timeout=1)


async def _settle(task: asyncio.Task) -> None:
    # Enough turns for a call that does not wait for start() to send or fail.
    for _ in range(10):
        if task.done():
            return
        await asyncio.sleep(0)


@pytest.mark.parametrize("phase", ["_start_cli_server", "_verify_protocol_version"])
@pytest.mark.parametrize("method", list(STATUS_CALLS))
async def test_status_call_waits_for_in_progress_start(client, method, phase):
    """_start_cli_server holds start() before the transport exists, _verify_protocol_version
    after it exists but before the handshake has completed."""
    wire_method, params, payload = STATUS_CALLS[method]
    rpc = Mock(request=AsyncMock(return_value=payload), stop=AsyncMock())
    entered = asyncio.Event()
    release = asyncio.Event()

    async def connect():
        client._client = rpc

    async def block():
        entered.set()
        await release.wait()

    client._connect_to_server.side_effect = connect
    getattr(client, phase).side_effect = block
    start = asyncio.create_task(client.start())
    await asyncio.wait_for(entered.wait(), timeout=1)
    call = asyncio.create_task(getattr(client, method)())
    try:
        await _settle(call)
        assert not call.done()
        rpc.request.assert_not_awaited()
    finally:
        release.set()
        await asyncio.wait_for(start, timeout=1)

    await asyncio.wait_for(call, timeout=1)
    rpc.request.assert_awaited_once_with(wire_method, params)


@pytest.mark.parametrize("method", list(STATUS_CALLS))
async def test_status_call_reports_failure_of_the_start_it_waited_for(client, method):
    rpc = Mock(request=AsyncMock(), stop=AsyncMock())
    entered = asyncio.Event()
    release = asyncio.Event()

    async def connect():
        client._client = rpc

    async def refuse():
        entered.set()
        await release.wait()
        raise RuntimeError("handshake refused")

    client._connect_to_server.side_effect = connect
    client._verify_protocol_version.side_effect = refuse
    start = asyncio.create_task(client.start())
    await asyncio.wait_for(entered.wait(), timeout=1)
    call = asyncio.create_task(getattr(client, method)())
    await _settle(call)
    release.set()

    with pytest.raises(RuntimeError, match="handshake refused"):
        await asyncio.wait_for(start, timeout=1)
    with pytest.raises(RuntimeError, match="Client failed to start: handshake refused"):
        await asyncio.wait_for(call, timeout=1)
    client._start_cli_server.assert_awaited_once()
    rpc.request.assert_not_awaited()
    client._verify_protocol_version.side_effect = None
    rpc.request.return_value = {"version": "retried", "protocolVersion": 3}
    assert (await client.get_status()).version == "retried"
    assert client._start_cli_server.await_count == 2
    rpc.request.assert_awaited_once_with("status.get", {})
    await client.force_stop()


async def test_status_call_keeps_original_start_failure_when_retry_is_queued(client):
    entered = asyncio.Event()
    release = asyncio.Event()
    failure = RuntimeError("first handshake refused")
    rpc = Mock(
        request=AsyncMock(return_value={"version": "1.2.3", "protocolVersion": 3}),
        stop=AsyncMock(),
    )

    async def connect():
        client._client = rpc

    async def refuse():
        entered.set()
        await release.wait()
        raise failure

    client._connect_to_server.side_effect = connect

    async def verify():
        if not entered.is_set():
            await refuse()

    client._verify_protocol_version.side_effect = verify
    start = asyncio.create_task(client.start())
    await asyncio.wait_for(entered.wait(), timeout=1)
    retry = asyncio.create_task(client.start())
    await asyncio.sleep(0)
    call = asyncio.create_task(client.get_status())
    await _settle(call)
    release.set()
    try:
        with pytest.raises(RuntimeError, match="first handshake refused"):
            await asyncio.wait_for(start, timeout=1)
        await asyncio.wait_for(retry, timeout=1)
        with pytest.raises(
            RuntimeError, match="Client failed to start: first handshake refused"
        ) as exc:
            await asyncio.wait_for(call, timeout=1)
        assert exc.value.__cause__ is failure
        assert (await client.get_status()).version == "1.2.3"
    finally:
        for task in (start, retry, call):
            task.cancel()
        await asyncio.gather(start, retry, call, return_exceptions=True)
        await client.force_stop()


async def test_cancelling_status_waiter_preserves_start_for_other_requests(client):
    entered = asyncio.Event()
    release = asyncio.Event()
    rpc = Mock(
        request=AsyncMock(return_value={"version": "1.2.3", "protocolVersion": 3}),
        stop=AsyncMock(),
    )

    async def connect():
        client._client = rpc
        entered.set()
        await release.wait()

    client._connect_to_server.side_effect = connect
    start = asyncio.create_task(client.start())
    await asyncio.wait_for(entered.wait(), timeout=1)
    cancelled = asyncio.create_task(client.get_status())
    remaining = asyncio.create_task(client.get_status())
    try:
        await _settle(cancelled)
        cancelled.cancel()
        with pytest.raises(asyncio.CancelledError):
            await cancelled
        assert not start.done()
        release.set()
        await asyncio.wait_for(start, timeout=1)
        assert (await asyncio.wait_for(remaining, timeout=1)).version == "1.2.3"
        rpc.request.assert_awaited_once_with("status.get", {})
    finally:
        release.set()
        for task in (start, cancelled, remaining):
            task.cancel()
        await asyncio.gather(start, cancelled, remaining, return_exceptions=True)
        await client.force_stop()


@pytest.mark.parametrize("method", list(STATUS_CALLS))
async def test_status_call_follows_replacement_start_queued_after_success(client, method):
    wire_method, params, response = STATUS_CALLS[method]
    entered = asyncio.Event()
    release = asyncio.Event()
    replacing = asyncio.Event()
    release_replacement = asyncio.Event()
    old_rpc = Mock(request=AsyncMock(), stop=AsyncMock())
    new_rpc = Mock(request=AsyncMock(return_value=response), stop=AsyncMock())
    connects = 0

    async def connect():
        nonlocal connects
        connects += 1
        if connects == 1:
            client._client = old_rpc
            entered.set()
            await release.wait()
        else:
            client._client = new_rpc

    client._connect_to_server.side_effect = connect

    real_start = client._start
    starts = 0

    async def start_then_lose_connection():
        nonlocal starts
        starts += 1
        await real_start()
        if starts == 1:
            # The reader thread closes the first connection right after startup succeeds.
            client._state = "disconnected"
            client._connection_lost = True

    client._start = start_then_lose_connection
    real_force_stop = client.force_stop

    async def gated_force_stop():
        replacing.set()
        await release_replacement.wait()
        client._client = None

    client.force_stop = gated_force_stop
    first = asyncio.create_task(client.start())
    await asyncio.wait_for(entered.wait(), timeout=1)
    second = asyncio.create_task(client.start())
    await asyncio.sleep(0)
    call = asyncio.create_task(getattr(client, method)())
    await _settle(call)
    release.set()
    try:
        await asyncio.wait_for(replacing.wait(), timeout=1)
        await _settle(call)
        assert not call.done()
        release_replacement.set()
        await asyncio.wait_for(first, timeout=1)
        await asyncio.wait_for(second, timeout=1)
        await asyncio.wait_for(call, timeout=1)
        old_rpc.request.assert_not_awaited()
        new_rpc.request.assert_awaited_once_with(wire_method, params)
    finally:
        release.set()
        release_replacement.set()
        for task in (first, second, call):
            task.cancel()
        await asyncio.gather(first, second, call, return_exceptions=True)
        client.force_stop = real_force_stop
        await client.force_stop()


async def test_stop_and_force_stop_do_not_wait_for_a_model_callback(client):
    started = asyncio.Event()
    release = asyncio.Event()
    calls = 0

    async def on_list_models():
        nonlocal calls
        calls += 1
        started.set()
        await release.wait()
        return []

    client._on_list_models = on_list_models
    listing = asyncio.create_task(client.list_models())
    try:
        await asyncio.wait_for(started.wait(), timeout=1)
        await asyncio.wait_for(client.force_stop(), timeout=1)
        await asyncio.wait_for(client.stop(), timeout=1)
        release.set()
        await asyncio.wait_for(listing, timeout=1)
        # The fetch that straddled the stop must not repopulate the cleared cache.
        await asyncio.wait_for(client.list_models(), timeout=1)
        assert calls == 2
    finally:
        release.set()
        listing.cancel()
        await asyncio.gather(listing, return_exceptions=True)


async def test_list_models_rejects_replaced_client_after_waiting_for_the_cache_lock(client):
    old_rpc = Mock(request=AsyncMock(side_effect=RuntimeError("closed")), stop=AsyncMock())
    new_rpc = Mock(request=AsyncMock(return_value={"models": []}), stop=AsyncMock())
    client._client = old_rpc
    await client._models_cache_lock.acquire()
    call = asyncio.create_task(client.list_models())
    try:
        await _settle(call)
        assert not call.done()
        # Another fetch is still holding the lock while the client is restarted.
        client._client = new_rpc
        client._models_cache_lock.release()
        with pytest.raises(RuntimeError, match="disconnected"):
            await asyncio.wait_for(call, timeout=1)
        old_rpc.request.assert_not_awaited()
        new_rpc.request.assert_not_awaited()
        assert await client.list_models() == []
        new_rpc.request.assert_awaited_once_with("models.list", {})
    finally:
        if client._models_cache_lock.locked():
            client._models_cache_lock.release()
        call.cancel()
        await asyncio.gather(call, return_exceptions=True)


async def test_queued_list_models_does_not_join_restart_of_its_old_client(client):
    fetching = asyncio.Event()
    release_fetch = asyncio.Event()
    stopping = asyncio.Event()
    release_stop = asyncio.Event()

    async def fetch(method, params):
        assert (method, params) == ("models.list", {})
        fetching.set()
        await release_fetch.wait()
        raise RuntimeError("closed transport")

    async def stop():
        stopping.set()
        await release_stop.wait()

    old_rpc = Mock(request=AsyncMock(side_effect=fetch), stop=AsyncMock(side_effect=stop))
    new_rpc = Mock(request=AsyncMock(return_value={"models": []}), stop=AsyncMock())
    client._client = old_rpc
    client._state = "connected"

    async def connect():
        client._client = new_rpc

    client._connect_to_server.side_effect = connect
    first = asyncio.create_task(client.list_models())
    queued = restart = None
    try:
        await asyncio.wait_for(fetching.wait(), timeout=1)
        queued = asyncio.create_task(client.list_models())
        await _settle(queued)
        assert not queued.done()
        client._state = "disconnected"
        client._connection_lost = True
        restart = asyncio.create_task(client.start())
        await asyncio.wait_for(stopping.wait(), timeout=1)
        assert client._client is old_rpc
        release_fetch.set()
        with pytest.raises(RuntimeError, match="closed transport"):
            await asyncio.wait_for(first, timeout=1)
        with pytest.raises(RuntimeError, match="disconnected"):
            await asyncio.wait_for(queued, timeout=1)
        old_rpc.request.assert_awaited_once_with("models.list", {})
        release_stop.set()
        await asyncio.wait_for(restart, timeout=1)
        assert await client.list_models() == []
        new_rpc.request.assert_awaited_once_with("models.list", {})
    finally:
        release_fetch.set()
        release_stop.set()
        tasks = [task for task in (first, queued, restart) if task is not None]
        for task in tasks:
            task.cancel()
        await asyncio.gather(*tasks, return_exceptions=True)
        await client.force_stop()


async def test_list_models_after_stop_does_not_wait_for_a_hung_model_callback(client):
    started = asyncio.Event()
    hang = asyncio.Event()
    calls = 0

    async def on_list_models():
        nonlocal calls
        calls += 1
        if calls == 1:
            started.set()
            await hang.wait()
        return []

    client._on_list_models = on_list_models
    hung = asyncio.create_task(client.list_models())
    queued = None
    try:
        await asyncio.wait_for(started.wait(), timeout=1)
        queued = asyncio.create_task(client.list_models())
        await _settle(queued)
        await asyncio.wait_for(client.force_stop(), timeout=1)
        # A call made after the stop must not queue behind the hung fetch.
        assert await asyncio.wait_for(client.list_models(), timeout=1) == []
        assert client._models_cache == []
        # The old queue drains once the hung fetch ends; its calls refetch rather than
        # caching a result from before the stop.
        hang.set()
        await asyncio.wait_for(hung, timeout=1)
        assert await asyncio.wait_for(queued, timeout=1) == []
    finally:
        hang.set()
        tasks = [t for t in (hung, queued) if t is not None]
        for task in tasks:
            task.cancel()
        await asyncio.gather(*tasks, return_exceptions=True)


@pytest.mark.parametrize("timestamp", ["2026-01-01T00:00:00Z", 1767225600000])
async def test_legacy_ping_fallback_does_not_wait_for_its_own_start(timestamp):
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_spawned_stdio(path="unused-cli")
    )

    async def request(method, params):
        if method == "connect":
            raise JsonRpcError(-32601, "Unhandled method connect")
        assert (method, params) == ("ping", {"message": None})
        return {
            "message": "pong",
            "timestamp": timestamp,
            "protocolVersion": get_sdk_protocol_version(),
        }

    rpc = Mock(request=AsyncMock(side_effect=request), stop=AsyncMock())

    async def connect():
        client._client = rpc

    client._start_cli_server = AsyncMock()
    client._connect_to_server = AsyncMock(side_effect=connect)

    await asyncio.wait_for(client.start(), timeout=1)

    assert client._state == "connected"
    assert [call.args[0] for call in rpc.request.await_args_list] == ["connect", "ping"]


@pytest.mark.parametrize("method", ["stop", "force_stop"])
async def test_shutdown_cancels_pending_inference_requests(client, method):
    adapter = Mock()
    client._llm_inference_adapter = adapter

    await getattr(client, method)()

    adapter.cancel_pending.assert_called_once_with()
