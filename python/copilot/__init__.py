"""
Copilot SDK - Python Client for GitHub Copilot CLI

JSON-RPC based SDK for programmatic control of GitHub Copilot CLI
"""

from .client import (
    CopilotClient,
    ExternalServerConfig,
    ModelCapabilitiesOverride,
    ModelLimitsOverride,
    ModelSupportsOverride,
    ModelVisionLimitsOverride,
    SubprocessConfig,
)
from .session import (
    AssistantImageData,
    AutoModeSwitchHandler,
    AutoModeSwitchRequest,
    AutoModeSwitchResponse,
    CommandContext,
    CommandDefinition,
    ContentBlock,
    CopilotSession,
    CreateSessionFsHandler,
    ElicitationContext,
    ElicitationHandler,
    ElicitationParams,
    ElicitationResult,
    ExitPlanModeHandler,
    ExitPlanModeRequest,
    ExitPlanModeResult,
    ImageBlock,
    ImageOptions,
    InputOptions,
    ProviderConfig,
    ResponseFormat,
    SessionCapabilities,
    SessionFsConfig,
    SessionUiApi,
    SessionUiCapabilities,
    TextBlock,
)
from .session_fs_provider import (
    SessionFsFileInfo,
    SessionFsProvider,
    create_session_fs_adapter,
)
from .tools import convert_mcp_call_tool_result, define_tool

__version__ = "2.0.0"

__all__ = [
    "AssistantImageData",
    "CommandContext",
    "AutoModeSwitchHandler",
    "AutoModeSwitchRequest",
    "AutoModeSwitchResponse",
    "CommandDefinition",
    "ContentBlock",
    "CopilotClient",
    "CopilotSession",
    "CreateSessionFsHandler",
    "ElicitationHandler",
    "ElicitationParams",
    "ElicitationContext",
    "ElicitationResult",
    "ExitPlanModeHandler",
    "ExitPlanModeRequest",
    "ExitPlanModeResult",
    "ExternalServerConfig",
    "ImageBlock",
    "ImageOptions",
    "InputOptions",
    "ModelCapabilitiesOverride",
    "ModelLimitsOverride",
    "ModelSupportsOverride",
    "ModelVisionLimitsOverride",
    "ProviderConfig",
    "ResponseFormat",
    "SessionCapabilities",
    "SessionFsConfig",
    "SessionFsFileInfo",
    "SessionFsProvider",
    "create_session_fs_adapter",
    "SessionUiApi",
    "SessionUiCapabilities",
    "SubprocessConfig",
    "TextBlock",
    "convert_mcp_call_tool_result",
    "define_tool",
]
