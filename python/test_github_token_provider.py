from __future__ import annotations

import asyncio
from copy import deepcopy
from typing import Any, cast, get_args, get_type_hints

import pytest

from copilot import (
    CopilotClient,
    GitHubTokenAcquireReason,
    RuntimeConnection,
)
from copilot._jsonrpc import JsonRpcClient, JsonRpcError
from copilot.rpc import (
    UNSET_SENTINEL,
    GitHubTokenAcquireRequest,
    SessionOpenInferenceOptions,
    SessionRpc,
    SessionUpdateInferenceOptions,
    SessionUpdateOptionsParams,
    Unset,
)


class FakeJsonRpcClient:
    def __init__(self, *, fail_method: str | None = None) -> None:
        self.fail_method = fail_method
        self.requests: list[tuple[str, dict[str, Any]]] = []
        self.request_handlers: dict[str, Any] = {}
        self.raw_request_handlers: dict[str, Any] = {}
        self.notification_method_handlers: dict[str, Any] = {}

    async def request(self, method: str, params: dict[str, Any], **kwargs: Any) -> dict[str, Any]:
        self.requests.append((method, params))
        if method == self.fail_method:
            raise RuntimeError(f"{method} failed")
        if method in {"session.create", "session.resume"}:
            response = {"sessionId": params["sessionId"]}
            callback = kwargs.get("on_response_inline")
            if callback is not None:
                callback(response)
            return response
        if method == "session.detach":
            return {"success": True}
        if method == "session.delete":
            return {"success": True}
        if method == "session.options.update":
            return {"success": True}
        raise RuntimeError(f"Unexpected method: {method}")

    async def stop(self) -> None:
        pass

    def set_request_handler(self, method: str, handler: Any, **_options: Any) -> None:
        self.request_handlers[method] = handler

    def set_raw_request_handler(self, method: str, handler: Any) -> None:
        self.raw_request_handlers[method] = handler

    def set_notification_method_handler(self, method: str, handler: Any) -> None:
        self.notification_method_handlers[method] = handler


class ConcurrentResumeJsonRpcClient(FakeJsonRpcClient):
    def __init__(self) -> None:
        super().__init__()
        self.resume_responses: list[asyncio.Future[dict[str, Any]]] = []
        self.both_resumes_started = asyncio.Event()

    async def request(self, method: str, params: dict[str, Any], **kwargs: Any) -> dict[str, Any]:
        self.requests.append((method, params))
        if method == "session.create":
            response = {"sessionId": params["sessionId"]}
            callback = kwargs.get("on_response_inline")
            if callback is not None:
                callback(response)
            return response
        if method == "session.resume":
            response = asyncio.get_running_loop().create_future()
            self.resume_responses.append(response)
            if len(self.resume_responses) >= 2:
                self.both_resumes_started.set()
            return await response
        raise RuntimeError(f"Unexpected method: {method}")


@pytest.mark.asyncio
@pytest.mark.parametrize("resume", [False, True])
@pytest.mark.parametrize("routing", [{}, {"default_provider": None}, {"default_provider": "named"}])
async def test_default_provider_presence_on_wire(resume, routing):
    fake = FakeJsonRpcClient()
    client = make_client(fake)
    options = {
        "model": "model",
        "github": {"token": "identity", "capi": {"enabled": False}},
        "inference": {
            **routing,
            "providers": [
                {
                    "name": "named",
                    "base_url": "https://provider.invalid",
                    "transport": "websockets",
                    "api_key_command": "credential-helper",
                    "bearer_token_provider": lambda _: "token",
                }
            ],
            "models": [],
        },
    }
    try:
        if resume:
            await client.resume_session("routing", **options)
        else:
            await client.create_session(session_id="routing", **options)
        method = "session.resume" if resume else "session.create"
        payload = next(params for name, params in fake.requests if name == method)
        inference = payload["inference"]
        if "default_provider" in routing:
            assert inference["defaultProvider"] == routing["default_provider"]
        else:
            assert "defaultProvider" not in inference
        assert "provider" not in inference
        assert inference["models"] == []
        assert inference["providers"] == [
            {
                "name": "named",
                "baseUrl": "https://provider.invalid",
                "transport": "websockets",
                "apiKeyCommand": "credential-helper",
                "hasBearerTokenProvider": True,
            }
        ]
        assert payload["github"] == {"token": "identity", "capi": {"enabled": False}}
    finally:
        await client.force_stop()


@pytest.mark.asyncio
@pytest.mark.parametrize("resume", [False, True])
@pytest.mark.parametrize("inference", [{}, {"providers": [], "models": []}])
async def test_empty_provider_configuration_preserves_wire_presence(resume, inference):
    fake = FakeJsonRpcClient()
    client = make_client(fake)
    try:
        if resume:
            await client.resume_session("routing", inference=inference)
        else:
            await client.create_session(session_id="routing", inference=inference)
        method = "session.resume" if resume else "session.create"
        payload = next(params for name, params in fake.requests if name == method)
        actual = payload.get("inference", {})
        for key in ("providers", "models"):
            assert (key in actual) == (key in inference)
            if key in inference:
                assert actual[key] == []
    finally:
        await client.force_stop()


@pytest.mark.asyncio
@pytest.mark.parametrize("routing", [{}, {"defaultProvider": None}, {"defaultProvider": "named"}])
async def test_copied_options_update_preserves_default_provider_presence(routing):
    fake = FakeJsonRpcClient()
    rpc = SessionRpc(cast(JsonRpcClient, fake), "routing")
    params = SessionUpdateOptionsParams(inference=SessionUpdateInferenceOptions.from_dict(routing))
    await rpc.options.update(deepcopy(params))
    assert fake.requests == [
        ("session.options.update", {"sessionId": "routing", "inference": routing})
    ]


@pytest.mark.parametrize(
    "options_type", [SessionOpenInferenceOptions, SessionUpdateInferenceOptions]
)
@pytest.mark.parametrize("routing", [{}, {"defaultProvider": None}, {"defaultProvider": "named"}])
def test_generated_default_provider_annotation_covers_omission(options_type, routing):
    assert Unset in get_args(get_type_hints(options_type)["default_provider"])
    value = deepcopy(options_type.from_dict(routing)).default_provider
    if isinstance(value, Unset):
        assert "defaultProvider" not in routing
        assert value is UNSET_SENTINEL
    elif value is None:
        assert routing["defaultProvider"] is None
    else:
        assert value.upper() == "NAMED"


@pytest.mark.asyncio
async def test_removed_singular_provider_is_rejected():
    client = make_client(FakeJsonRpcClient())
    try:
        with pytest.raises(TypeError, match="Unknown inference option.*provider"):
            await client.create_session(inference={"provider": {}})
        with pytest.raises(TypeError, match="Unknown inference option.*provider"):
            await client.resume_session("routing", inference={"provider": {}})
    finally:
        await client.force_stop()


def make_client(fake: FakeJsonRpcClient) -> CopilotClient:
    client = CopilotClient(
        mode="coding-agent", runtime=RuntimeConnection.for_tcp("localhost", 1234)
    )
    client._client = cast(JsonRpcClient, fake)
    return client


class TestGitHubTokenProvider:
    @pytest.mark.parametrize("token", ["", "static"])
    @pytest.mark.parametrize("resume", [False, True])
    async def test_mutual_exclusion(self, token: str, resume: bool) -> None:
        fake = FakeJsonRpcClient()
        client = make_client(fake)
        github = {"token": token, "token_provider": lambda _: {"kind": "cancelled"}}

        with pytest.raises(
            ValueError, match="github_token and github_token_provider are mutually exclusive"
        ):
            if resume:
                await client.resume_session("session", github=github)
            else:
                await client.create_session(github=github)
        assert fake.requests == []
        assert client._github_token_providers == {}

    async def test_wire_mapping_token_and_cancelled(self) -> None:
        fake = FakeJsonRpcClient()
        client = make_client(fake)
        observed: list[dict[str, Any]] = []

        async def provider(args):
            observed.append(dict(args))
            if len(observed) == 1:
                return {
                    "kind": "token",
                    "accessToken": "secret-token",
                    "tokenType": "Bearer",
                    "expiresIn": 28_800,
                }
            return {"kind": "cancelled"}

        await client.create_session(
            session_id="python-session", github={"token_provider": provider}
        )
        create_payload = fake.requests[0][1]
        registration_id = create_payload["github"]["tokenProviderRegistrationId"]
        assert "github_token_provider" not in create_payload
        assert "token" not in create_payload.get("github", {})
        client._register_client_global_handlers()
        get_token = fake.request_handlers["gitHubToken.getToken"]

        token = await get_token(
            {
                "registrationId": registration_id,
                "host": "github.example.com",
                "reason": "initial",
            }
        )
        cancelled = await get_token(
            {
                "registrationId": registration_id,
                "host": "github.example.com",
                "reason": "refresh",
                "sessionId": "python-session",
            }
        )

        assert token == {
            "kind": "token",
            "accessToken": "secret-token",
            "tokenType": "Bearer",
            "expiresIn": 28_800,
        }
        assert cancelled == {"kind": "cancelled"}
        assert observed == [
            {
                "host": "github.example.com",
                "session_id": "python-session",
                "reason": GitHubTokenAcquireReason.INITIAL,
            },
            {
                "host": "github.example.com",
                "session_id": "python-session",
                "reason": GitHubTokenAcquireReason.REFRESH,
            },
        ]

    async def test_callback_and_unknown_registration_errors(self) -> None:
        fake = FakeJsonRpcClient()
        client = make_client(fake)
        failure = RuntimeError("credential broker failed")

        def provider(_args):
            raise failure

        await client.create_session(session_id="error-session", github={"token_provider": provider})
        registration_id = fake.requests[0][1]["github"]["tokenProviderRegistrationId"]

        with pytest.raises(RuntimeError, match="credential broker failed") as exc:
            await client._github_token_provider_adapter.get_token(
                GitHubTokenAcquireRequest(
                    registration_id=registration_id,
                    host="github.com",
                    reason=GitHubTokenAcquireReason.INITIAL,
                )
            )
        assert exc.value is failure

        with pytest.raises(JsonRpcError, match="No GitHub token provider registered"):
            await client._github_token_provider_adapter.get_token(
                GitHubTokenAcquireRequest(
                    registration_id="unknown",
                    host="github.com",
                    reason=GitHubTokenAcquireReason.REFRESH,
                )
            )

    async def test_failure_session_close_and_client_close_cleanup(self) -> None:
        failing = make_client(FakeJsonRpcClient(fail_method="session.create"))
        with pytest.raises(RuntimeError, match="session.create failed"):
            await failing.create_session(github={"token_provider": lambda _: {"kind": "cancelled"}})
        assert failing._github_token_providers == {}

        fake = FakeJsonRpcClient()
        client = make_client(fake)
        first = await client.create_session(
            session_id="first", github={"token_provider": lambda _: {"kind": "cancelled"}}
        )
        await client.create_session(
            session_id="second", github={"token_provider": lambda _: {"kind": "cancelled"}}
        )
        assert len(client._github_token_providers) == 2

        await first.disconnect()
        assert len(client._github_token_providers) == 1
        await client.delete_session("second")
        assert client._github_token_providers == {}
        await client.force_stop()
        assert client._github_token_providers == {}

    async def test_resume_rotates_provider(self) -> None:
        fake = FakeJsonRpcClient()
        client = make_client(fake)
        calls: list[str] = []

        await client.create_session(
            session_id="resumed",
            github={"token_provider": lambda _: calls.append("first") or {"kind": "cancelled"}},
        )
        first_registration = fake.requests[0][1]["github"]["tokenProviderRegistrationId"]
        await client.resume_session(
            "resumed",
            github={"token_provider": lambda _: calls.append("second") or {"kind": "cancelled"}},
        )
        second_registration = fake.requests[1][1]["github"]["tokenProviderRegistrationId"]

        with pytest.raises(JsonRpcError):
            await client._github_token_provider_adapter.get_token(
                GitHubTokenAcquireRequest(
                    registration_id=first_registration,
                    host="github.com",
                    reason=GitHubTokenAcquireReason.REFRESH,
                )
            )
        assert await client._github_token_provider_adapter.get_token(
            GitHubTokenAcquireRequest(
                registration_id=second_registration,
                host="github.com",
                reason=GitHubTokenAcquireReason.REFRESH,
            )
        ) == {"kind": "cancelled"}
        assert calls == ["second"]

    async def test_concurrent_resume_keeps_pending_registration(self) -> None:
        fake = ConcurrentResumeJsonRpcClient()
        client = make_client(fake)
        await client.create_session(
            session_id="concurrent", github={"token_provider": lambda _: {"kind": "cancelled"}}
        )

        first_resume = asyncio.create_task(
            client.resume_session(
                "concurrent", github={"token_provider": lambda _: {"kind": "cancelled"}}
            )
        )
        second_resume = asyncio.create_task(
            client.resume_session(
                "concurrent", github={"token_provider": lambda _: {"kind": "cancelled"}}
            )
        )
        await asyncio.wait_for(fake.both_resumes_started.wait(), timeout=5.0)

        fake.resume_responses[0].set_result({"sessionId": "concurrent"})
        await first_resume
        assert len(client._github_token_providers) == 2

        fake.resume_responses[1].set_result({"sessionId": "concurrent"})
        await second_resume
        assert len(client._github_token_providers) == 1
        second_registration = fake.requests[2][1]["github"]["tokenProviderRegistrationId"]
        assert second_registration in client._github_token_providers
