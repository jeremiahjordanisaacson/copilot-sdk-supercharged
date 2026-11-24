/**
 * Copilot SDK Supercharged - Solidity / Web3 SDK
 *
 * TypeScript helpers for using GitHub Copilot in smart-contract development
 * workflows (Hardhat, Foundry, security auditing, gas optimization).
 */

// Client
export { CopilotSolidityClient, type SoliditySessionOptions } from "./client.js";

// Tools
export {
    createAuditTool,
    createGasOptimizationTool,
    createTestGenerationTool,
    createSecurityCheckTool,
    createSolidityTools,
    type SolidityToolsOptions,
} from "./tools.js";

// Types
export type {
    AuditFinding,
    AuditReport,
    AuditToolParams,
    CopilotSolidityClientOptions,
    GasCategory,
    GasOptimization,
    GasReport,
    GasToolParams,
    GeneratedTestSuite,
    SecurityCheckToolParams,
    SecurityPattern,
    Severity,
    SoliditySessionConfig,
    TestFramework,
    TestGenToolParams,
    // Upstream-sync feature types
    SessionLimitsConfig,
    MemoryConfiguration,
    ProviderTokenArgs,
    BearerTokenProvider,
    McpAuthHandler,
    PostToolUseHandler,
    PreMcpToolCallHandler,
    SolidityHooks,
    CopilotRequestHandler,
    SolidityMessageOptions,
    SolidityUpstreamSessionOptions,
    ToolDeferMode,
    SystemMessageSectionName,
    GitHubAttachmentType,
    // v2.6.0 upstream-sync types
    AskUserVariant,
    AutoTier,
    AutopilotOptions,
    ByokProviderOptions,
    CopilotClientInfo,
    GitHubTokenProvider,
    GitHubTokenProviderArgs,
    GitHubTokenProviderResult,
    ImageGenerationOptions,
    ModelLimitsOverride,
    ModelSwitchAutoTierResult,
    ProviderConfigModelProvider,
    SandboxConfig,
    SubagentStartHandler,
    SubagentStartHookInput,
    SubagentStartHookOutput,
    SubagentStopHandler,
    SubagentStopHookInput,
    SubagentStopHookOutput,
    WorkingDirectoryTrust,
    // v3.0.0 (SDK protocol 4) types
    CapiSessionOptions,
    ManagedSettingsResolvedData,
    MemoryStoreCallOptions,
    NamedProviderConfig,
    NetworkConfig,
    PermissionModeSetResult,
    PolicyHelperFailure,
    ProviderModelConfig,
    SessionStartError,
    ShellAuthenticationHandler,
    ShellAuthenticationRequest,
    ShellAuthenticationResponse,
    SolidityClientMode,
    SolidityProviderConfig,
    StoredMemory,
    ToolShellOutputData,
    ToolShellOutputEvent,
    ToolShellOutputStream,
    // v3.1.0 types (MCP secret store, plugin extension approvals, ask-user images)
    McpSecretsSaveResult,
    PermissionsExtensionApproval,
    PermissionsExtensionApprovalsGrantResult,
    PermissionsExtensionApprovalsListResult,
    PermissionsExtensionApprovalsRevokeResult,
    UserInputHandler,
    UserInputImageAttachment,
    UserInputImageAttachmentConstraints,
    UserInputRequest,
    UserInputResponse,
} from "./types.js";

// Upstream-sync feature constants
export {
    ToolDefer,
    SystemMessageSection,
    GitHubAttachment,
    MessageSource,
    PermissionModeRefusalReason,
    TOOL_SHELL_OUTPUT_EVENT,
} from "./types.js";

// SDK protocol 4
export { SDK_PROTOCOL_VERSION } from "./sdkProtocolVersion.js";
export { getPolicyHelperFailure } from "./client.js";
