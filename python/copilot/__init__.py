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
    CommandContext,
    CommandDefinition,
    ContentBlock,
    CopilotSession,
    CreateSessionFsHandler,
    ElicitationContext,
    ElicitationHandler,
    ElicitationParams,
    ElicitationResult,
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

__version__ = "0.1.0"

__all__ = [
    "AssistantImageData",
    "CommandContext",
    "CommandDefinition",
    "ContentBlock",
    "CopilotClient",
    "CopilotSession",
    "CreateSessionFsHandler",
    "ElicitationHandler",
    "ElicitationParams",
    "ElicitationContext",
    "ElicitationResult",
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
