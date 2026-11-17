"""Unit tests for the ``ToolSet`` builder and empty-mode helpers."""

from __future__ import annotations

import pytest

from copilot import (
    BUILTIN_TOOLS_ISOLATED,
    CopilotClient,
    RuntimeConnection,
    TcpRuntimeConnection,
    ToolSet,
)
from copilot._mode import (
    _custom_agents_local_only_default,
    _embedding_cache_storage_default,
    _enable_file_hooks_default,
    _enable_host_git_operations_default,
    _enable_on_demand_instruction_discovery_default,
    _enable_session_store_default,
    _enable_session_telemetry_default,
    _enable_skills_default,
    _post_create_options_patch,
    _require_available_tools_for_empty_mode,
    _require_storage_for_empty_mode,
    _skip_embedding_retrieval_default,
    _system_message_for_mode,
    _validate_tool_filter_list,
)


class TestToolSet:
    def test_add_builtin_string(self):
        ts = ToolSet().add_builtin("bash")
        assert ts.to_list() == ["builtin:bash"]

    def test_add_builtin_wildcard(self):
        ts = ToolSet().add_builtin("*")
        assert ts.to_list() == ["builtin:*"]

    def test_add_builtin_iterable(self):
        ts = ToolSet().add_builtin(["bash", "edit"])
        assert ts.to_list() == ["builtin:bash", "builtin:edit"]

    def test_add_builtin_isolated(self):
        ts = ToolSet().add_builtin(BUILTIN_TOOLS_ISOLATED)
        assert ts.to_list() == [f"builtin:{name}" for name in BUILTIN_TOOLS_ISOLATED]

    def test_add_mcp(self):
        ts = ToolSet().add_mcp("github-list_issues")
        assert ts.to_list() == ["mcp:github-list_issues"]

    def test_add_mcp_wildcard(self):
        assert ToolSet().add_mcp("*").to_list() == ["mcp:*"]

    def test_add_custom(self):
        assert ToolSet().add_custom("my_tool").to_list() == ["custom:my_tool"]

    def test_chained(self):
        ts = ToolSet().add_builtin(BUILTIN_TOOLS_ISOLATED).add_mcp("*").add_custom("*")
        assert ts.to_list()[-2:] == ["mcp:*", "custom:*"]

    def test_rejects_bad_name(self):
        with pytest.raises(ValueError, match="tool names must match"):
            ToolSet().add_builtin("has space")

    def test_rejects_empty(self):
        with pytest.raises(ValueError, match="must not be empty"):
            ToolSet().add_custom("")

    def test_rejects_colon(self):
        with pytest.raises(ValueError, match="tool names must match"):
            ToolSet().add_mcp("server:tool")

    @pytest.mark.parametrize("method", ["add_builtin", "add_custom", "add_mcp"])
    def test_rejects_trailing_newline(self, method):
        with pytest.raises(ValueError, match="tool names must match"):
            getattr(ToolSet(), method)("bash\n")

    def test_iterable_protocol(self):
        ts = ToolSet().add_builtin("bash").add_mcp("*")
        assert list(ts) == ["builtin:bash", "mcp:*"]
        assert len(ts) == 2


class TestEmptyModeValidation:
    def test_empty_mode_requires_storage(self):
        with pytest.raises(ValueError, match="requires base_directory"):
            _require_storage_for_empty_mode(
                mode="empty",
                base_directory=None,
                session_fs_set=False,
                is_tcp_connection=False,
            )

    def test_empty_mode_accepts_base_directory(self):
        _require_storage_for_empty_mode(
            mode="empty",
            base_directory="/tmp/x",
            session_fs_set=False,
            is_tcp_connection=False,
        )

    def test_empty_mode_accepts_session_fs(self):
        _require_storage_for_empty_mode(
            mode="empty",
            base_directory=None,
            session_fs_set=True,
            is_tcp_connection=False,
        )

    def test_empty_mode_accepts_uri_connection(self):
        _require_storage_for_empty_mode(
            mode="empty",
            base_directory=None,
            session_fs_set=False,
            is_tcp_connection=True,
        )

    def test_coding_agent_mode_no_storage_required(self):
        _require_storage_for_empty_mode(
            mode="coding-agent",
            base_directory=None,
            session_fs_set=False,
            is_tcp_connection=False,
        )

    def test_empty_mode_requires_available_tools(self):
        with pytest.raises(ValueError, match="available_tools"):
            _require_available_tools_for_empty_mode("empty", None)

    def test_empty_mode_accepts_available_tools(self):
        _require_available_tools_for_empty_mode("empty", ["builtin:bash"])

    def test_coding_agent_mode_no_tool_filter_required(self):
        _require_available_tools_for_empty_mode("coding-agent", None)


class TestToolFilterListValidation:
    def test_rejects_bare_wildcard(self):
        with pytest.raises(ValueError, match="bare wildcard"):
            _validate_tool_filter_list("available_tools", ["*"])

    def test_accepts_source_qualified_wildcard(self):
        _validate_tool_filter_list("available_tools", ["builtin:*", "mcp:*"])

    def test_accepts_none(self):
        _validate_tool_filter_list("available_tools", None)


class TestSystemMessageForMode:
    def test_coding_agent_pass_through(self):
        assert _system_message_for_mode("coding-agent", None) is None
        msg = {"mode": "append", "content": "hi"}
        assert _system_message_for_mode("coding-agent", msg) is msg

    def test_empty_mode_none_supplied(self):
        out = _system_message_for_mode("empty", None)
        assert out == {
            "mode": "customize",
            "sections": {"environment_context": {"action": "remove"}},
        }

    def test_empty_mode_replace_pass_through(self):
        msg = {"mode": "replace", "content": "verbatim"}
        assert _system_message_for_mode("empty", msg) is msg

    def test_empty_mode_customize_adds_section(self):
        msg = {"mode": "customize", "sections": {"identity": {"action": "remove"}}}
        out = _system_message_for_mode("empty", msg)
        assert out["sections"]["environment_context"] == {"action": "remove"}
        assert out["sections"]["identity"] == {"action": "remove"}

    def test_empty_mode_customize_does_not_overwrite_existing(self):
        msg = {
            "mode": "customize",
            "sections": {"environment_context": {"action": "replace", "content": "X"}},
        }
        assert _system_message_for_mode("empty", msg) is msg

    def test_empty_mode_append_promoted_to_customize(self):
        msg = {"mode": "append", "content": "tip"}
        out = _system_message_for_mode("empty", msg)
        assert out["mode"] == "customize"
        assert out["content"] == "tip"
        assert out["sections"]["environment_context"] == {"action": "remove"}


class TestEmptyModeEmbeddingCacheStorageDefaults:
    def test_empty_mode_defaults_to_in_memory(self):
        assert _embedding_cache_storage_default("empty", None) == "in-memory"

    def test_caller_wins(self):
        assert _embedding_cache_storage_default("empty", "persistent") == "persistent"
        assert _embedding_cache_storage_default("empty", "in-memory") == "in-memory"

    def test_coding_agent_does_not_change(self):
        assert _embedding_cache_storage_default("coding-agent", None) is None
        assert _embedding_cache_storage_default("coding-agent", "persistent") == "persistent"


class TestEmptyModeBooleanDefaults:
    @pytest.mark.parametrize(
        ("helper", "empty_default"),
        [
            (_enable_session_telemetry_default, False),
            (_skip_embedding_retrieval_default, True),
            (_enable_on_demand_instruction_discovery_default, False),
            (_enable_file_hooks_default, False),
            (_enable_host_git_operations_default, False),
            (_enable_session_store_default, False),
            (_enable_skills_default, False),
            (_custom_agents_local_only_default, True),
        ],
    )
    def test_empty_mode_defaults(self, helper, empty_default):
        assert helper("empty", None) is empty_default

    @pytest.mark.parametrize(
        "helper",
        [
            _enable_session_telemetry_default,
            _skip_embedding_retrieval_default,
            _enable_on_demand_instruction_discovery_default,
            _enable_file_hooks_default,
            _enable_host_git_operations_default,
            _enable_session_store_default,
            _enable_skills_default,
            _custom_agents_local_only_default,
        ],
    )
    def test_caller_wins(self, helper):
        assert helper("empty", True) is True
        assert helper("empty", False) is False

    @pytest.mark.parametrize(
        "helper",
        [
            _enable_session_telemetry_default,
            _skip_embedding_retrieval_default,
            _enable_on_demand_instruction_discovery_default,
            _enable_file_hooks_default,
            _enable_host_git_operations_default,
            _enable_session_store_default,
            _enable_skills_default,
            _custom_agents_local_only_default,
        ],
    )
    def test_coding_agent_does_not_change(self, helper):
        assert helper("coding-agent", None) is None


class TestPostCreatePatch:
    def test_empty_mode_defaults(self):
        patch = _post_create_options_patch("empty", None, None, None, None)
        assert patch == {
            "instructions": {"skipCustomInstructions": True},
            "agents": {"localDiscoveryOnly": True},
            "git": {"coauthorEnabled": False},
            "tools": {"manageScheduleEnabled": False},
            "plugins": {"installed": []},
            "skills": {"includedBuiltinSkills": []},
        }

    def test_empty_mode_caller_wins(self):
        patch = _post_create_options_patch("empty", False, False, True, True)
        assert patch == {
            "instructions": {"skipCustomInstructions": False},
            "agents": {"localDiscoveryOnly": False},
            "git": {"coauthorEnabled": True},
            "tools": {"manageScheduleEnabled": True},
            "plugins": {"installed": []},
            "skills": {"includedBuiltinSkills": []},
        }

    def test_empty_mode_preserves_explicit_builtin_skill_allowlist(self):
        patch = _post_create_options_patch("empty", None, None, None, None, ["code-review"])
        assert patch is not None
        assert patch["skills"]["includedBuiltinSkills"] == ["code-review"]

    def test_coding_agent_returns_none_when_unset(self):
        assert _post_create_options_patch("coding-agent", None, None, None, None) is None
        # Non-empty mode never injects the built-in skill restriction.
        assert "includedBuiltinSkills" not in (
            _post_create_options_patch("coding-agent", True, None, False, None) or {}
        ).get("skills", {})

    def test_coding_agent_passes_through_explicit_values(self):
        patch = _post_create_options_patch("coding-agent", True, None, False, None, ["code-review"])
        assert patch == {
            "instructions": {"skipCustomInstructions": True},
            "git": {"coauthorEnabled": False},
            "skills": {"includedBuiltinSkills": ["code-review"]},
        }


class _CapturingOptions:
    """Captures the params passed to ``session.rpc.options.update``."""

    def __init__(self) -> None:
        self.captured: list = []

    async def update(self, params) -> None:
        self.captured.append(params)


class _CapturingRpc:
    def __init__(self) -> None:
        self.options = _CapturingOptions()


class _FakeSession:
    def __init__(self, session_id: str = "sid-1") -> None:
        self.session_id = session_id
        self.rpc = _CapturingRpc()

    async def disconnect(self) -> None:
        pass


class TestApplyPostCreateOptionsPatch:
    """Guards the translation from the patch dict to ``SessionUpdateOptionsParams``.

    This covers the wire request emitted on both create and resume, which both
    funnel through ``_apply_post_create_options_patch``.
    """

    def _make_client(self):
        return CopilotClient(
            mode="empty",
            runtime=TcpRuntimeConnection(host="localhost", port=1234),
        )

    async def test_empty_mode_sends_included_builtin_skills_empty(self):
        client = self._make_client()
        session = _FakeSession()
        await client._apply_post_create_options_patch(session, "empty", None, None, None, None)
        assert len(session.rpc.options.captured) == 1
        params = session.rpc.options.captured[0]
        # The Empty post-patch must reach the wire request as an empty list, not
        # be silently dropped during translation.
        assert params.to_dict()["skills"]["includedBuiltinSkills"] == []
        assert params.to_dict()["plugins"]["installed"] == []

    async def test_empty_mode_explicit_allowlist_reaches_wire(self):
        client = self._make_client()
        session = _FakeSession()
        await client._apply_post_create_options_patch(
            session, "empty", False, False, True, True, ["code-review"]
        )
        params = session.rpc.options.captured[0]
        assert params.to_dict()["skills"]["includedBuiltinSkills"] == ["code-review"]

    async def test_coding_agent_mode_omits_included_builtin_skills(self):
        client = self._make_client()
        session = _FakeSession()
        # Non-empty mode with no overrides sends no patch at all.
        await client._apply_post_create_options_patch(
            session, "coding-agent", None, None, None, None
        )
        assert session.rpc.options.captured == []

    async def test_coding_agent_mode_never_sets_included_builtin_skills(self):
        client = self._make_client()
        session = _FakeSession()
        await client._apply_post_create_options_patch(
            session, "coding-agent", True, None, False, None
        )
        params = session.rpc.options.captured[0]
        # coding-agent mode must not inject the built-in skill restriction.
        assert "skills" not in params.to_dict()


class TestClientConstruction:
    def test_empty_mode_without_storage_raises(self):
        with pytest.raises(ValueError, match="requires base_directory"):
            CopilotClient(mode="empty")

    def test_empty_mode_with_base_directory_ok(self, tmp_path):
        client = CopilotClient(
            mode="empty",
            runtime=RuntimeConnection.for_spawned_stdio(
                path="runtime", base_directory=str(tmp_path)
            ),
        )
        assert client._options.mode == "empty"

    def test_empty_mode_with_tcp_connection_ok(self):
        client = CopilotClient(
            mode="empty",
            runtime=TcpRuntimeConnection(host="localhost", port=1234),
        )
        assert client._options.mode == "empty"

    def test_requires_explicit_mode(self):
        with pytest.raises(TypeError, match="mode"):
            CopilotClient()

    @pytest.mark.parametrize("mode", [None, "copilot-cli", "invalid"])
    def test_rejects_invalid_mode(self, mode):
        with pytest.raises(ValueError, match="mode must be"):
            CopilotClient(mode=mode)

    def test_explicit_coding_agent_mode(self):
        client = CopilotClient(
            mode="coding-agent",
            runtime=TcpRuntimeConnection(host="localhost", port=1234),
        )
        assert client._options.mode == "coding-agent"
