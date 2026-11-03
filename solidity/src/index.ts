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
} from "./types.js";

// Upstream-sync feature constants
export { ToolDefer, SystemMessageSection, GitHubAttachment, MessageSource } from "./types.js";
