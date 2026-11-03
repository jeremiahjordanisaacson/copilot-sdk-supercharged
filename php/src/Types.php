<?php

declare(strict_types=1);

/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

namespace GitHub\Copilot;

/**
 * Type definitions for the Copilot SDK.
 *
 * All types are modeled as PHP classes with public properties, mirroring the
 * TypeScript/Python SDK interfaces.
 */

// ============================================================================
// Connection State
// ============================================================================

enum ConnectionState: string
{
    case Disconnected = 'disconnected';
    case Connecting = 'connecting';
    case Connected = 'connected';
    case Error = 'error';
}

// ============================================================================
// Log Level
// ============================================================================

enum LogLevel: string
{
    case None = 'none';
    case Error = 'error';
    case Warning = 'warning';
    case Info = 'info';
    case Debug = 'debug';
    case All = 'all';
}

// ============================================================================
// Tool Result Types
// ============================================================================

enum ToolResultType: string
{
    case Success = 'success';
    case Failure = 'failure';
    case Rejected = 'rejected';
    case Denied = 'denied';
}

class ToolBinaryResult
{
    public function __construct(
        public readonly string $data,
        public readonly string $mimeType,
        public readonly string $type,
        public readonly ?string $description = null,
    ) {}

    public function toArray(): array
    {
        $result = [
            'data' => $this->data,
            'mimeType' => $this->mimeType,
            'type' => $this->type,
        ];
        if ($this->description !== null) {
            $result['description'] = $this->description;
        }
        return $result;
    }

    public static function fromArray(array $data): self
    {
        return new self(
            data: $data['data'] ?? '',
            mimeType: $data['mimeType'] ?? '',
            type: $data['type'] ?? '',
            description: $data['description'] ?? null,
        );
    }
}

class ToolResultObject
{
    public function __construct(
        public readonly string $textResultForLlm,
        public readonly string $resultType = 'success',
        /** @var ToolBinaryResult[]|null */
        public readonly ?array $binaryResultsForLlm = null,
        public readonly ?string $error = null,
        public readonly ?string $sessionLog = null,
        public readonly ?array $toolTelemetry = null,
    ) {}

    public function toArray(): array
    {
        $result = [
            'textResultForLlm' => $this->textResultForLlm,
            'resultType' => $this->resultType,
        ];
        if ($this->binaryResultsForLlm !== null) {
            $result['binaryResultsForLlm'] = array_map(
                fn(ToolBinaryResult $b) => $b->toArray(),
                $this->binaryResultsForLlm,
            );
        }
        if ($this->error !== null) {
            $result['error'] = $this->error;
        }
        if ($this->sessionLog !== null) {
            $result['sessionLog'] = $this->sessionLog;
        }
        if ($this->toolTelemetry !== null) {
            $result['toolTelemetry'] = $this->toolTelemetry;
        }
        return $result;
    }

    public static function fromArray(array $data): self
    {
        $binaryResults = null;
        if (isset($data['binaryResultsForLlm'])) {
            $binaryResults = array_map(
                fn(array $b) => ToolBinaryResult::fromArray($b),
                $data['binaryResultsForLlm'],
            );
        }
        return new self(
            textResultForLlm: $data['textResultForLlm'] ?? '',
            resultType: $data['resultType'] ?? 'success',
            binaryResultsForLlm: $binaryResults,
            error: $data['error'] ?? null,
            sessionLog: $data['sessionLog'] ?? null,
            toolTelemetry: $data['toolTelemetry'] ?? null,
        );
    }
}

// ============================================================================
// Tool Types
// ============================================================================

class ToolInvocation
{
    public function __construct(
        public readonly string $sessionId,
        public readonly string $toolCallId,
        public readonly string $toolName,
        public readonly mixed $arguments,
    ) {}
}

class Tool
{
    /**
     * @param string $name Tool name
     * @param string|null $description Tool description
     * @param array|null $parameters JSON Schema parameters
     * @param callable $handler Function(mixed $args, ToolInvocation $invocation): mixed
     */
    public function __construct(
        public readonly string $name,
        public readonly ?string $description = null,
        public readonly ?array $parameters = null,
        public /* readonly */ $handler = null,
    ) {}

    public function toServerFormat(): array
    {
        $result = ['name' => $this->name];
        if ($this->description !== null) {
            $result['description'] = $this->description;
        }
        if ($this->parameters !== null) {
            $result['parameters'] = $this->parameters;
        }
        return $result;
    }
}

// ============================================================================
// System Message Configuration
// ============================================================================

class SystemMessageAppendConfig
{
    public function __construct(
        public readonly ?string $content = null,
    ) {}

    public function toArray(): array
    {
        $result = ['mode' => 'append'];
        if ($this->content !== null) {
            $result['content'] = $this->content;
        }
        return $result;
    }
}

class SystemMessageReplaceConfig
{
    public function __construct(
        public readonly string $content,
    ) {}

    public function toArray(): array
    {
        return [
            'mode' => 'replace',
            'content' => $this->content,
        ];
    }
}

// ============================================================================
// System Prompt Section Constants
// ============================================================================

/**
 * Known system prompt section identifiers for the "customize" mode.
 */
final class SystemPromptSection
{
    public const IDENTITY = 'identity';
    public const TONE = 'tone';
    public const TOOL_EFFICIENCY = 'tool_efficiency';
    public const ENVIRONMENT_CONTEXT = 'environment_context';
    public const CODE_CHANGE_RULES = 'code_change_rules';
    public const GUIDELINES = 'guidelines';
    public const SAFETY = 'safety';
    public const TOOL_INSTRUCTIONS = 'tool_instructions';
    public const CUSTOM_INSTRUCTIONS = 'custom_instructions';
    public const LAST_INSTRUCTIONS = 'last_instructions';
}

/**
 * Override action for a system prompt section.
 */
final class SectionOverrideAction
{
    public const REPLACE = 'replace';
    public const REMOVE = 'remove';
    public const APPEND = 'append';
    public const PREPEND = 'prepend';
}

/**
 * Override operation for a single system prompt section.
 */
class SectionOverride
{
    public function __construct(
        public readonly string $action,
        public readonly ?string $content = null,
    ) {}

    public function toArray(): array
    {
        $result = ['action' => $this->action];
        if ($this->content !== null) {
            $result['content'] = $this->content;
        }
        return $result;
    }
}

/**
 * System message in customize mode - override individual sections.
 */
class SystemMessageCustomizeConfig
{
    /**
     * @param array<string, SectionOverride>|null $sections
     */
    public function __construct(
        public readonly ?array $sections = null,
        public readonly ?string $content = null,
    ) {}

    public function toArray(): array
    {
        $result = ['mode' => 'customize'];
        if ($this->sections !== null) {
            $sectionsArray = [];
            foreach ($this->sections as $key => $override) {
                $sectionsArray[$key] = $override->toArray();
            }
            $result['sections'] = $sectionsArray;
        }
        if ($this->content !== null) {
            $result['content'] = $this->content;
        }
        return $result;
    }
}

// ============================================================================
// Permission Types
// ============================================================================

class PermissionRequest
{
    public function __construct(
        public readonly string $kind,
        public readonly ?string $toolCallId = null,
        public readonly array $extra = [],
    ) {}

    public static function fromArray(array $data): self
    {
        $kind = $data['kind'] ?? '';
        $toolCallId = $data['toolCallId'] ?? null;
        $extra = array_diff_key($data, array_flip(['kind', 'toolCallId']));
        return new self($kind, $toolCallId, $extra);
    }

    public function toArray(): array
    {
        $result = array_merge($this->extra, ['kind' => $this->kind]);
        if ($this->toolCallId !== null) {
            $result['toolCallId'] = $this->toolCallId;
        }
        return $result;
    }
}

class PermissionRequestResult
{
    public function __construct(
        public readonly string $kind,
        public readonly ?array $rules = null,
    ) {}

    public function toArray(): array
    {
        $result = ['kind' => $this->kind];
        if ($this->rules !== null) {
            $result['rules'] = $this->rules;
        }
        return $result;
    }
}

// ============================================================================
// User Input Types
// ============================================================================

class UserInputRequest
{
    public function __construct(
        public readonly string $question,
        /** @var string[]|null */
        public readonly ?array $choices = null,
        public readonly ?bool $allowFreeform = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            question: $data['question'] ?? '',
            choices: $data['choices'] ?? null,
            allowFreeform: $data['allowFreeform'] ?? null,
        );
    }
}

class UserInputResponse
{
    public function __construct(
        public readonly string $answer,
        public readonly bool $wasFreeform,
    ) {}

    public function toArray(): array
    {
        return [
            'answer' => $this->answer,
            'wasFreeform' => $this->wasFreeform,
        ];
    }
}

// ============================================================================
// Provider Configuration
// ============================================================================

class AzureProviderOptions
{
    public function __construct(
        public readonly ?string $apiVersion = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->apiVersion !== null) {
            $result['apiVersion'] = $this->apiVersion;
        }
        return $result;
    }
}

class ProviderConfig
{
    /**
     * @param string|null $modelProvider Provider family: "openai", "anthropic", "azure_openai",
     *        "ollama", "lm_studio", "foundry_local" or "llama_cpp" (see ModelProvider)
     * @param string|null $apiKeyCommand Command whose stdout is used as the API key
     */
    public function __construct(
        public readonly string $baseUrl,
        public readonly ?string $type = null,
        public readonly ?string $wireApi = null,
        public readonly ?string $apiKey = null,
        public readonly ?string $bearerToken = null,
        public readonly ?AzureProviderOptions $azure = null,
        public readonly ?string $modelProvider = null,
        public readonly ?string $apiKeyCommand = null,
    ) {}

    public function toArray(): array
    {
        $result = ['baseUrl' => $this->baseUrl];
        if ($this->type !== null) {
            $result['type'] = $this->type;
        }
        if ($this->wireApi !== null) {
            $result['wireApi'] = $this->wireApi;
        }
        if ($this->apiKey !== null) {
            $result['apiKey'] = $this->apiKey;
        }
        if ($this->bearerToken !== null) {
            $result['bearerToken'] = $this->bearerToken;
        }
        if ($this->azure !== null) {
            $result['azure'] = $this->azure->toArray();
        }
        if ($this->modelProvider !== null) {
            $result['modelProvider'] = $this->modelProvider;
        }
        if ($this->apiKeyCommand !== null) {
            $result['apiKeyCommand'] = $this->apiKeyCommand;
        }
        return $result;
    }
}

// ============================================================================
// MCP Server Configuration
// ============================================================================

class MCPLocalServerConfig
{
    /**
     * @param string[] $tools List of tools to include. [] means none.
     * @param string $command Command to run
     * @param string[] $args Command arguments
     * @param string|null $type Server type (local/stdio)
     * @param int|null $timeout Timeout in milliseconds
     * @param array<string,string>|null $env Environment variables
     * @param string|null $cwd Working directory
     */
    public function __construct(
        public readonly array $tools,
        public readonly string $command,
        public readonly array $args,
        public readonly ?string $type = null,
        public readonly ?int $timeout = null,
        public readonly ?array $env = null,
        public readonly ?string $cwd = null,
    ) {}

    public function toArray(): array
    {
        $result = [
            'tools' => $this->tools,
            'command' => $this->command,
            'args' => $this->args,
        ];
        if ($this->type !== null) {
            $result['type'] = $this->type;
        }
        if ($this->timeout !== null) {
            $result['timeout'] = $this->timeout;
        }
        if ($this->env !== null) {
            $result['env'] = $this->env;
        }
        if ($this->cwd !== null) {
            $result['cwd'] = $this->cwd;
        }
        return $result;
    }
}

class MCPRemoteServerConfig
{
    /**
     * @param string[] $tools List of tools to include
     * @param string $type Server type (http/sse)
     * @param string $url URL of the remote server
     * @param int|null $timeout Timeout in milliseconds
     * @param array<string,string>|null $headers HTTP headers
     */
    public function __construct(
        public readonly array $tools,
        public readonly string $type,
        public readonly string $url,
        public readonly ?int $timeout = null,
        public readonly ?array $headers = null,
    ) {}

    public function toArray(): array
    {
        $result = [
            'tools' => $this->tools,
            'type' => $this->type,
            'url' => $this->url,
        ];
        if ($this->timeout !== null) {
            $result['timeout'] = $this->timeout;
        }
        if ($this->headers !== null) {
            $result['headers'] = $this->headers;
        }
        return $result;
    }
}

// ============================================================================
// Custom Agent Configuration
// ============================================================================

class CustomAgentConfig
{
    /**
     * @param string $name Unique name
     * @param string $prompt The prompt content
     * @param string|null $displayName Display name
     * @param string|null $description Description
     * @param string[]|null $tools Tool names (null for all)
     * @param array<string,MCPLocalServerConfig|MCPRemoteServerConfig>|null $mcpServers
     * @param bool|null $infer Whether available for inference
     * @param string[]|null $skills Skill names to preload into this agent's context
     */
    public function __construct(
        public readonly string $name,
        public readonly string $prompt,
        public readonly ?string $displayName = null,
        public readonly ?string $description = null,
        public readonly ?array $tools = null,
        public readonly ?array $mcpServers = null,
        public readonly ?bool $infer = null,
        public readonly ?array $skills = null,
    ) {}

    public function toArray(): array
    {
        $result = [
            'name' => $this->name,
            'prompt' => $this->prompt,
        ];
        if ($this->displayName !== null) {
            $result['displayName'] = $this->displayName;
        }
        if ($this->description !== null) {
            $result['description'] = $this->description;
        }
        if ($this->tools !== null) {
            $result['tools'] = $this->tools;
        }
        if ($this->mcpServers !== null) {
            $result['mcpServers'] = array_map(
                fn($s) => $s->toArray(),
                $this->mcpServers,
            );
        }
        if ($this->infer !== null) {
            $result['infer'] = $this->infer;
        }
        if ($this->skills !== null) {
            $result['skills'] = $this->skills;
        }
        return $result;
    }
}

// ============================================================================
// Infinite Session Configuration
// ============================================================================

class InfiniteSessionConfig
{
    public function __construct(
        public readonly ?bool $enabled = null,
        public readonly ?float $backgroundCompactionThreshold = null,
        public readonly ?float $bufferExhaustionThreshold = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->enabled !== null) {
            $result['enabled'] = $this->enabled;
        }
        if ($this->backgroundCompactionThreshold !== null) {
            $result['backgroundCompactionThreshold'] = $this->backgroundCompactionThreshold;
        }
        if ($this->bufferExhaustionThreshold !== null) {
            $result['bufferExhaustionThreshold'] = $this->bufferExhaustionThreshold;
        }
        return $result;
    }
}

// ============================================================================
// Reasoning Effort
// ============================================================================

enum ReasoningEffort: string
{
    case Low = 'low';
    case Medium = 'medium';
    case High = 'high';
    case XHigh = 'xhigh';
}

// ============================================================================
// Commands
// ============================================================================

class CommandContext
{
    public function __construct(
        public readonly string $sessionId,
        public readonly string $command,
        public readonly string $commandName,
        public readonly string $args,
    ) {}
}

class CommandDefinition
{
    /**
     * @param string $name Command name (without leading /)
     * @param string|null $description Human-readable description
     * @param callable $handler Handler callback receiving CommandContext
     */
    public function __construct(
        public readonly string $name,
        public readonly ?string $description = null,
        public /* readonly */ $handler = null,
    ) {}
}

// ============================================================================
// UI Elicitation
// ============================================================================

class ElicitationContext
{
    /**
     * @param string $sessionId Session that triggered the request
     * @param string $message Prompt message
     * @param array<string,mixed>|null $requestedSchema JSON schema for expected input
     * @param string|null $mode Elicitation mode
     * @param string|null $elicitationSource Source that initiated the request
     * @param string|null $url Optional URL for context
     */
    public function __construct(
        public readonly string $sessionId,
        public readonly string $message,
        public readonly ?array $requestedSchema = null,
        public readonly ?string $mode = null,
        public readonly ?string $elicitationSource = null,
        public readonly ?string $url = null,
    ) {}
}

class ElicitationResult
{
    /**
     * @param string $action "accept", "decline", or "cancel"
     * @param array<string,mixed>|null $content Form values (present when action is "accept")
     */
    public function __construct(
        public readonly string $action,
        public readonly ?array $content = null,
    ) {}
}

// ============================================================================
// Upstream-sync feature types (parity with @github/copilot-sdk)
// ============================================================================

/**
 * Per-session AI-credit budget; set $maxAiCredits to cap spend.
 */
class SessionLimitsConfig
{
    public function __construct(
        public readonly ?int $maxAiCredits = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->maxAiCredits !== null) {
            $result['maxAiCredits'] = $this->maxAiCredits;
        }
        return $result;
    }
}

/**
 * Opt-in persistent session memory.
 */
class MemoryConfiguration
{
    public function __construct(
        public readonly bool $enabled = false,
    ) {}

    public function toArray(): array
    {
        return ['enabled' => $this->enabled];
    }
}

/**
 * Arguments passed to a BYOK bearer-token provider (per-session scoping).
 */
class ProviderTokenArgs
{
    public function __construct(
        public readonly string $sessionId,
    ) {}
}

/**
 * Intercepts outbound LLM inference HTTP/WebSocket requests. Extend this class
 * and override sendRequest() to mutate, replace, or forward the request, then
 * assign an instance to CopilotClientOptions::$requestHandler. BYOK providers
 * may also set CopilotClientOptions::$bearerTokenProvider.
 */
class CopilotRequestHandler
{
    /**
     * Transform an outbound request; override in a subclass.
     *
     * @param mixed $request
     * @param mixed $context
     * @return mixed
     */
    public function sendRequest($request, $context = null)
    {
        return $request;
    }
}

/**
 * Tool "defer" loading policy: eager pre-load ("never") or lazy via search ("auto").
 */
class ToolDefer
{
    public const AUTO = 'auto';
    public const NEVER = 'never';
}

/**
 * System-message section identifiers (used with system-message overrides).
 * The "preamble" section targets the identity preamble; the "preserve" action
 * protects an individually-addressable section from a group-level remove.
 */
class SystemMessageSection
{
    public const PREAMBLE = 'preamble';
    public const IDENTITY = 'identity';
    public const TOOL_INSTRUCTIONS = 'tool_instructions';
    public const PRESERVE = 'preserve';
}

/**
 * GitHub-anchored attachment variants.
 */
class GitHubAttachment
{
    public const GITHUB_COMMIT = 'GitHubCommit';
    public const GITHUB_RELEASE = 'GitHubRelease';
    public const GITHUB_ACTIONS_JOB = 'GitHubActionsJob';
    public const GITHUB_REPOSITORY = 'GitHubRepository';
    public const GITHUB_FILE_DIFF = 'GitHubFileDiff';
    public const GITHUB_TREE_COMPARISON = 'GitHubTreeComparison';
    public const GITHUB_URL = 'GitHubUrl';
    public const GITHUB_FILE = 'GitHubFile';
    public const GITHUB_SNIPPET = 'GitHubSnippet';
}

// ============================================================================
// v2.6.0 upstream-sync feature types (parity with @github/copilot-sdk)
// ============================================================================

/**
 * Wire helpers shared by the session config serializers.
 *
 * @internal
 */
final class WireFormat
{
    /**
     * Unwrap typed config objects (toArray()) and encode arrays as JSON objects,
     * so an empty map serializes as {} rather than [].
     */
    public static function toObject(mixed $value): mixed
    {
        if (is_object($value) && method_exists($value, 'toArray')) {
            $value = $value->toArray();
        }
        if (is_array($value)) {
            return (object) $value;
        }
        return $value;
    }
}

/**
 * Identity of the integrating application. When any field is set, the client sends
 * it on the `connect` handshake (as editorName/editorVersion/extensionName/extensionVersion)
 * so the runtime attributes its telemetry to your application.
 */
class ClientInfo
{
    public function __construct(
        public readonly ?string $applicationName = null,
        public readonly ?string $applicationVersion = null,
        public readonly ?string $integrationName = null,
        public readonly ?string $integrationVersion = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            applicationName: $data['applicationName'] ?? null,
            applicationVersion: $data['applicationVersion'] ?? null,
            integrationName: $data['integrationName'] ?? null,
            integrationVersion: $data['integrationVersion'] ?? null,
        );
    }

    /**
     * @return array<string,string>|null Wire shape with only non-empty fields, or null when all are empty
     */
    public function toWire(): ?array
    {
        $wire = [];
        if ($this->applicationName !== null && $this->applicationName !== '') {
            $wire['editorName'] = $this->applicationName;
        }
        if ($this->applicationVersion !== null && $this->applicationVersion !== '') {
            $wire['editorVersion'] = $this->applicationVersion;
        }
        if ($this->integrationName !== null && $this->integrationName !== '') {
            $wire['extensionName'] = $this->integrationName;
        }
        if ($this->integrationVersion !== null && $this->integrationVersion !== '') {
            $wire['extensionVersion'] = $this->integrationVersion;
        }
        return $wire === [] ? null : $wire;
    }
}

/**
 * Auto routing preference used when the session model is "auto" (experimental).
 */
final class AutoTier
{
    public const EFFICIENCY = 'efficiency';
    public const BALANCE = 'balance';
    public const INTELLIGENCE = 'intelligence';
    public const FAST = 'fast';
}

/**
 * Wire shape used for ask-user prompts.
 */
final class AskUserVariant
{
    public const LEGACY = 'legacy';
    public const ELICITATION = 'elicitation';
}

/**
 * Scope of the working-directory trust granted at session creation.
 */
final class WorkingDirectoryTrust
{
    public const SESSION = 'session';
    public const USER = 'user';
}

/**
 * BYOK provider families accepted by ProviderConfig::$modelProvider.
 */
final class ModelProvider
{
    public const OPENAI = 'openai';
    public const ANTHROPIC = 'anthropic';
    public const AZURE_OPENAI = 'azure_openai';
    public const OLLAMA = 'ollama';
    public const LM_STUDIO = 'lm_studio';
    public const FOUNDRY_LOCAL = 'foundry_local';
    public const LLAMA_CPP = 'llama_cpp';
}

/**
 * Message provenance for MessageOptions::$source.
 */
final class MessageSource
{
    public const USER = 'user';
    public const SYSTEM = 'system';

    /**
     * Build the "agent-<id>" source for a message produced by an identified agent.
     */
    public static function agent(string $agentId): string
    {
        return 'agent-' . $agentId;
    }
}

/**
 * Arguments passed to a session's gitHubTokenProvider callback.
 */
class GitHubTokenProviderArgs
{
    /**
     * @param string $host Effective GitHub host the token is for
     * @param string|null $sessionId Session receiving the token (null before a cloud session has an id)
     * @param string $reason "initial" or "refresh"
     */
    public function __construct(
        public readonly string $host,
        public readonly ?string $sessionId,
        public readonly string $reason,
    ) {}
}

/**
 * Result returned by a gitHubTokenProvider: a token or a cancellation.
 * Providers may also return the wire array directly.
 */
class GitHubTokenResult
{
    public function __construct(
        public readonly string $kind,
        public readonly ?string $accessToken = null,
        public readonly ?string $tokenType = null,
        public readonly ?int $expiresIn = null,
    ) {}

    /**
     * @param int $expiresIn Remaining token lifetime in seconds
     */
    public static function token(string $accessToken, int $expiresIn, ?string $tokenType = null): self
    {
        return new self('token', $accessToken, $tokenType, $expiresIn);
    }

    public static function cancelled(): self
    {
        return new self('cancelled');
    }

    public function toArray(): array
    {
        if ($this->kind === 'cancelled') {
            return ['kind' => 'cancelled'];
        }
        $result = [
            'kind' => 'token',
            'accessToken' => $this->accessToken,
            'expiresIn' => $this->expiresIn,
        ];
        if ($this->tokenType !== null) {
            $result['tokenType'] = $this->tokenType;
        }
        return $result;
    }
}

/**
 * Session sandbox configuration. $allowBypass opts in to letting the agent request
 * that an individual command run outside the sandbox through the normal permission flow.
 */
class SandboxConfig
{
    /**
     * @param array<string,mixed>|null $userPolicy Policy fragment merged into the base policy
     */
    public function __construct(
        public readonly bool $enabled,
        public readonly ?array $userPolicy = null,
        public readonly ?bool $addCurrentWorkingDirectory = null,
        public readonly ?bool $sandboxMcpServers = null,
        public readonly ?bool $sandboxLspServers = null,
        public readonly ?bool $sandboxExtensions = null,
        public readonly ?bool $allowBypass = null,
    ) {}

    public function toArray(): array
    {
        $result = ['enabled' => $this->enabled];
        if ($this->userPolicy !== null) {
            $result['userPolicy'] = (object) $this->userPolicy;
        }
        if ($this->addCurrentWorkingDirectory !== null) {
            $result['addCurrentWorkingDirectory'] = $this->addCurrentWorkingDirectory;
        }
        if ($this->sandboxMcpServers !== null) {
            $result['sandboxMcpServers'] = $this->sandboxMcpServers;
        }
        if ($this->sandboxLspServers !== null) {
            $result['sandboxLspServers'] = $this->sandboxLspServers;
        }
        if ($this->sandboxExtensions !== null) {
            $result['sandboxExtensions'] = $this->sandboxExtensions;
        }
        if ($this->allowBypass !== null) {
            $result['allowBypass'] = $this->allowBypass;
        }
        return $result;
    }
}

/**
 * Session-level image generation configuration.
 */
class ImageGenerationConfig
{
    public function __construct(
        public readonly ?bool $enabled = null,
        public readonly ?bool $watermark = null,
        public readonly ?bool $saveToDisk = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->enabled !== null) {
            $result['enabled'] = $this->enabled;
        }
        if ($this->watermark !== null) {
            $result['watermark'] = $this->watermark;
        }
        if ($this->saveToDisk !== null) {
            $result['saveToDisk'] = $this->saveToDisk;
        }
        return $result;
    }
}

/**
 * Runtime-owned Autopilot continuation options.
 */
class AutopilotOptions
{
    public function __construct(
        public readonly ?bool $continuationEnabled = null,
        public readonly ?int $maxContinues = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->continuationEnabled !== null) {
            $result['continuationEnabled'] = $this->continuationEnabled;
        }
        if ($this->maxContinues !== null) {
            $result['maxContinues'] = $this->maxContinues;
        }
        return $result;
    }
}

/**
 * Limit overrides for the SessionConfig::$modelCapabilities array, e.g.
 * `['limits' => (new ModelLimitsOverride(maxOutputTokens: 4096))->toArray()]`.
 * Wire keys are snake_case (max_prompt_tokens, max_output_tokens, max_context_window_tokens).
 */
class ModelLimitsOverride
{
    public function __construct(
        public readonly ?int $maxPromptTokens = null,
        public readonly ?int $maxOutputTokens = null,
        public readonly ?int $maxContextWindowTokens = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->maxPromptTokens !== null) {
            $result['max_prompt_tokens'] = $this->maxPromptTokens;
        }
        if ($this->maxOutputTokens !== null) {
            $result['max_output_tokens'] = $this->maxOutputTokens;
        }
        if ($this->maxContextWindowTokens !== null) {
            $result['max_context_window_tokens'] = $this->maxContextWindowTokens;
        }
        return $result;
    }
}

// ============================================================================
// Session Configuration
// ============================================================================

class SessionConfig
{
    /**
     * @param string|null $sessionId Optional custom session ID
     * @param string|null $model Model to use
     * @param ReasoningEffort|null $reasoningEffort Reasoning effort level
     * @param string|null $configDir Override config directory
     * @param Tool[]|null $tools Tools exposed to CLI server
     * @param SystemMessageAppendConfig|SystemMessageReplaceConfig|null $systemMessage
     * @param string[]|null $availableTools Tool names to allow
     * @param string[]|null $excludedTools Tool names to disable
     * @param ProviderConfig|null $provider Custom BYOK provider
     * @param callable|null $onPermissionRequest Permission handler
     * @param callable|null $onUserInputRequest User input handler
     * @param SessionHooks|null $hooks Hook handlers
     * @param string|null $workingDirectory Working directory
     * @param bool|null $streaming Enable streaming
     * @param array<string,MCPLocalServerConfig|MCPRemoteServerConfig>|null $mcpServers
     * @param CustomAgentConfig[]|null $customAgents
     * @param string[]|null $skillDirectories
     * @param string[]|null $disabledSkills
     * @param InfiniteSessionConfig|null $infiniteSessions
     * @param array<string,mixed>|null $modelCapabilities Per-property overrides for model capabilities
     * @param bool|null $enableConfigDiscovery Auto-discover MCP server configs from working directory
     */
    public function __construct(
        public readonly ?string $sessionId = null,
        public readonly ?string $model = null,
        public readonly ?ReasoningEffort $reasoningEffort = null,
        public readonly ?string $configDir = null,
        public readonly ?array $tools = null,
        public readonly SystemMessageAppendConfig|SystemMessageReplaceConfig|SystemMessageCustomizeConfig|null $systemMessage = null,
        public readonly ?array $availableTools = null,
        public readonly ?array $excludedTools = null,
        public readonly ?ProviderConfig $provider = null,
        public /* readonly */ $onPermissionRequest = null,
        public /* readonly */ $onUserInputRequest = null,
        public readonly ?SessionHooks $hooks = null,
        public readonly ?string $workingDirectory = null,
        public readonly ?bool $streaming = null,
        /** Include sub-agent streaming events in the event stream. Defaults to true. */
        public readonly ?bool $includeSubAgentStreamingEvents = null,
        public readonly ?array $mcpServers = null,
        public readonly ?array $customAgents = null,
        public readonly ?array $skillDirectories = null,
        public readonly ?array $disabledSkills = null,
        public readonly ?InfiniteSessionConfig $infiniteSessions = null,
        public readonly ?array $modelCapabilities = null,
        public readonly ?bool $enableConfigDiscovery = null,
        /** GitHub token for authentication. When set on session config, overrides the client-level token for this session only. */
        public readonly ?string $gitHubToken = null,
        /** @var CommandDefinition[]|null $commands Slash commands for this session */
        public readonly ?array $commands = null,
        /** @var callable|null $onElicitationRequest Elicitation handler */
        public /* readonly */ $onElicitationRequest = null,
        /** @var string[]|null $instructionDirectories Instruction directories for prompt customization */
        public readonly ?array $instructionDirectories = null,
        /** Enable inline source citations in assistant responses. */
        public readonly ?bool $enableCitations = null,
        /** @var string[]|null $excludedBuiltinAgents Built-in agents to exclude from this session */
        public readonly ?array $excludedBuiltinAgents = null,
        /** Per-session AI-credit spending limits. */
        public readonly ?SessionLimitsConfig $sessionLimits = null,
        /** Persistent session memory configuration. */
        public readonly ?MemoryConfiguration $memory = null,
        /** OTLP telemetry protocol ("grpc" or "http/protobuf"). */
        public readonly ?string $otlpProtocol = null,
        /** Stream assistant responses over a WebSocket transport. */
        public readonly ?bool $enableWebSocketResponses = null,
        /** @var array<string,mixed>|null $expAssignments Experiment assignments forwarded to the runtime */
        public readonly ?array $expAssignments = null,
        /** Enable session rewind. */
        public readonly ?bool $rewindEnabled = null,
        /** @var string[]|null $additionalDirectories Additional session working directories */
        public readonly ?array $additionalDirectories = null,
        /** @var string[]|null $disabledMcpServers MCP server names to disable */
        public readonly ?array $disabledMcpServers = null,
        /** @var array<string,mixed>|null $githubMcpToolConfig GitHub MCP tool configuration */
        public readonly ?array $githubMcpToolConfig = null,
        /** @var array<string,mixed>|string|null $canvasProvider Canvas provider config (object or provider name) */
        public /* readonly */ $canvasProvider = null,
        /** Restrict custom agents to locally-defined agents only. */
        public readonly ?bool $customAgentsLocalOnly = null,
        /** @var string[]|null $builtinPluginDirectories Built-in plugin directories */
        public readonly ?array $builtinPluginDirectories = null,
        /** @var array<string,mixed>|null $argsSchema Agent-factory authoring args schema */
        public readonly ?array $argsSchema = null,
        /** @var array<string,mixed>|null $decisionContext Permission decision context */
        public readonly ?array $decisionContext = null,
        /** @var array<string,mixed>|bool|null $toolSearch Tool search configuration */
        public /* readonly */ $toolSearch = null,
        /** Use the in-process FFI transport. */
        public readonly ?bool $inProcess = null,
        /** Enable experimental mode. */
        public readonly ?bool $experimentalMode = null,
        /** Enable content exclusion. */
        public readonly ?bool $contentExclusion = null,
        /** @var callable|null $onMcpAuthRequest MCP OAuth host-token handler fn(ProviderTokenArgs): string */
        public /* readonly */ $onMcpAuthRequest = null,
        // --- v2.6.0 upstream-sync options ---
        /** Auto routing preference when the model is "auto" (experimental; see AutoTier). Sent inside `capi`. */
        public readonly ?string $autoTier = null,
        /** Ask-user prompt variant: "legacy" or "elicitation" (see AskUserVariant). */
        public readonly ?string $askUserVariant = null,
        /** @var callable|null $gitHubTokenProvider fn(GitHubTokenProviderArgs): GitHubTokenResult|array. Mutually exclusive with $gitHubToken. */
        public /* readonly */ $gitHubTokenProvider = null,
        /** Sandbox configuration (SandboxConfig or wire array). */
        public readonly SandboxConfig|array|null $sandboxConfig = null,
        /** @var array<string,bool>|null $featureFlags Runtime feature flags (sent even when empty) */
        public readonly ?array $featureFlags = null,
        /** @var string[]|null $allowedModels Model allowlist for this session */
        public readonly ?array $allowedModels = null,
        /** OAuth client ID metadata document URL. */
        public readonly ?string $authClientIdMetadataUrl = null,
        /** Human-readable session name. */
        public readonly ?string $name = null,
        /** @var string[]|null $agentDirectories Directories to load custom agents from */
        public readonly ?array $agentDirectories = null,
        /** @var string[]|null $hookDirectories Directories to load hooks from */
        public readonly ?array $hookDirectories = null,
        /** Whether an interactive user is present. */
        public readonly ?bool $interactiveUser = null,
        /** @var string[]|null $excludedManagedSettings Managed settings to ignore */
        public readonly ?array $excludedManagedSettings = null,
        /** Enforce managed model defaults. */
        public readonly ?bool $enforceManagedModelDefaults = null,
        /** Working-directory trust: "session" or "user" (see WorkingDirectoryTrust). */
        public readonly ?string $trustWorkingDirectory = null,
        /** Refresh custom instructions on session start. */
        public readonly ?bool $refreshCustomInstructions = null,
        /** Session-level image generation configuration (ImageGenerationConfig or wire array). */
        public readonly ImageGenerationConfig|array|null $imageGeneration = null,
        /** Autopilot continuation options (AutopilotOptions or wire array). */
        public readonly AutopilotOptions|array|null $autopilot = null,
    ) {}

    public function toServerParams(): array
    {
        $params = [];
        if ($this->sessionId !== null) {
            $params['sessionId'] = $this->sessionId;
        }
        if ($this->model !== null) {
            $params['model'] = $this->model;
        }
        if ($this->reasoningEffort !== null) {
            $params['reasoningEffort'] = $this->reasoningEffort->value;
        }
        if ($this->configDir !== null) {
            $params['configDir'] = $this->configDir;
        }
        if ($this->tools !== null) {
            $params['tools'] = array_map(
                fn(Tool $t) => $t->toServerFormat(),
                $this->tools,
            );
        }
        if ($this->systemMessage !== null) {
            $params['systemMessage'] = $this->systemMessage->toArray();
        }
        if ($this->availableTools !== null) {
            $params['availableTools'] = $this->availableTools;
        }
        if ($this->excludedTools !== null) {
            $params['excludedTools'] = $this->excludedTools;
        }
        if ($this->provider !== null) {
            $params['provider'] = $this->provider->toArray();
        }
        $params['requestPermission'] = $this->onPermissionRequest !== null;
        $params['requestUserInput'] = $this->onUserInputRequest !== null;
        $params['hooks'] = $this->hooks !== null && $this->hooks->hasAnyHandler();
        if ($this->workingDirectory !== null) {
            $params['workingDirectory'] = $this->workingDirectory;
        }
        if ($this->streaming !== null) {
            $params['streaming'] = $this->streaming;
        }
        if ($this->includeSubAgentStreamingEvents !== null) {
            $params['includeSubAgentStreamingEvents'] = $this->includeSubAgentStreamingEvents;
        }
        if ($this->mcpServers !== null) {
            $mcpArr = [];
            foreach ($this->mcpServers as $name => $config) {
                $mcpArr[$name] = $config->toArray();
            }
            $params['mcpServers'] = $mcpArr;
        }
        if ($this->customAgents !== null) {
            $params['customAgents'] = array_map(
                fn(CustomAgentConfig $a) => $a->toArray(),
                $this->customAgents,
            );
        }
        if ($this->skillDirectories !== null) {
            $params['skillDirectories'] = $this->skillDirectories;
        }
        if ($this->disabledSkills !== null) {
            $params['disabledSkills'] = $this->disabledSkills;
        }
        if ($this->infiniteSessions !== null) {
            $params['infiniteSessions'] = $this->infiniteSessions->toArray();
        }
        if ($this->modelCapabilities !== null) {
            $params['modelCapabilities'] = $this->modelCapabilities;
        }
        if ($this->enableConfigDiscovery !== null) {
            $params['enableConfigDiscovery'] = $this->enableConfigDiscovery;
        }
        if ($this->gitHubToken !== null) {
            $params['gitHubToken'] = $this->gitHubToken;
        }
        if ($this->instructionDirectories !== null) {
            $params['instructionDirectories'] = $this->instructionDirectories;
        }
        // --- Upstream-sync session options (parity with @github/copilot-sdk) ---
        if ($this->enableCitations !== null) {
            $params['enableCitations'] = $this->enableCitations;
        }
        if ($this->excludedBuiltinAgents !== null) {
            $params['excludedBuiltinAgents'] = $this->excludedBuiltinAgents;
        }
        if ($this->sessionLimits !== null) {
            $params['sessionLimits'] = $this->sessionLimits->toArray();
        }
        if ($this->memory !== null) {
            $params['memory'] = $this->memory->toArray();
        }
        if ($this->otlpProtocol !== null) {
            $params['otlpProtocol'] = $this->otlpProtocol;
        }
        // enableWebSocketResponses is sent inside the `capi` object (see below).
        if ($this->expAssignments !== null) {
            $params['expAssignments'] = $this->expAssignments;
        }
        // --- Second upstream-sync batch (2026-08 parity with @github/copilot-sdk) ---
        if ($this->rewindEnabled !== null) {
            $params['rewindEnabled'] = $this->rewindEnabled;
        }
        if ($this->additionalDirectories !== null) {
            $params['additionalDirectories'] = $this->additionalDirectories;
        }
        if ($this->disabledMcpServers !== null) {
            $params['disabledMcpServers'] = $this->disabledMcpServers;
        }
        if ($this->githubMcpToolConfig !== null) {
            $params['githubMcpToolConfig'] = $this->githubMcpToolConfig;
        }
        if ($this->canvasProvider !== null) {
            $params['canvasProvider'] = $this->canvasProvider;
        }
        if ($this->customAgentsLocalOnly !== null) {
            $params['customAgentsLocalOnly'] = $this->customAgentsLocalOnly;
        }
        if ($this->builtinPluginDirectories !== null) {
            $params['builtinPluginDirectories'] = $this->builtinPluginDirectories;
        }
        if ($this->argsSchema !== null) {
            $params['argsSchema'] = $this->argsSchema;
        }
        if ($this->decisionContext !== null) {
            $params['decisionContext'] = $this->decisionContext;
        }
        if ($this->toolSearch !== null) {
            $params['toolSearch'] = $this->toolSearch;
        }
        if ($this->inProcess !== null) {
            $params['inProcess'] = $this->inProcess;
        }
        if ($this->experimentalMode !== null) {
            $params['experimentalMode'] = $this->experimentalMode;
        }
        if ($this->contentExclusion !== null) {
            $params['contentExclusion'] = $this->contentExclusion;
        }
        // Signal to the runtime that an MCP OAuth host-token handler is registered.
        if ($this->onMcpAuthRequest !== null) {
            $params['mcpAuthHandler'] = true;
        }
        // --- v2.6.0 upstream-sync session options (parity with @github/copilot-sdk) ---
        $capi = [];
        if ($this->autoTier !== null) {
            $capi['autoTier'] = $this->autoTier;
        }
        if ($this->enableWebSocketResponses !== null) {
            $capi['enableWebSocketResponses'] = $this->enableWebSocketResponses;
        }
        if ($capi !== []) {
            $params['capi'] = $capi;
        }
        if ($this->askUserVariant !== null) {
            $params['askUserVariant'] = $this->askUserVariant;
        }
        if ($this->sandboxConfig !== null) {
            $params['sandboxConfig'] = WireFormat::toObject($this->sandboxConfig);
        }
        if ($this->featureFlags !== null) {
            $params['featureFlags'] = WireFormat::toObject($this->featureFlags);
        }
        if ($this->allowedModels !== null) {
            $params['allowedModels'] = $this->allowedModels;
        }
        if ($this->authClientIdMetadataUrl !== null) {
            $params['authClientIdMetadataUrl'] = $this->authClientIdMetadataUrl;
        }
        if ($this->name !== null) {
            $params['name'] = $this->name;
        }
        if ($this->agentDirectories !== null) {
            $params['agentDirectories'] = $this->agentDirectories;
        }
        if ($this->hookDirectories !== null) {
            $params['hookDirectories'] = $this->hookDirectories;
        }
        if ($this->interactiveUser !== null) {
            $params['interactiveUser'] = $this->interactiveUser;
        }
        if ($this->excludedManagedSettings !== null) {
            $params['excludedManagedSettings'] = $this->excludedManagedSettings;
        }
        if ($this->enforceManagedModelDefaults !== null) {
            $params['enforceManagedModelDefaults'] = $this->enforceManagedModelDefaults;
        }
        if ($this->trustWorkingDirectory !== null) {
            $params['trustWorkingDirectory'] = $this->trustWorkingDirectory;
        }
        if ($this->refreshCustomInstructions !== null) {
            $params['refreshCustomInstructions'] = $this->refreshCustomInstructions;
        }
        if ($this->imageGeneration !== null) {
            $params['imageGeneration'] = WireFormat::toObject($this->imageGeneration);
        }
        if ($this->autopilot !== null) {
            $params['autopilot'] = WireFormat::toObject($this->autopilot);
        }
        return $params;
    }
}

class ResumeSessionConfig
{
    /**
     * @param string|null $model Model to use
     * @param ReasoningEffort|null $reasoningEffort Reasoning effort level
     * @param Tool[]|null $tools Tools exposed to CLI server
     * @param SystemMessageAppendConfig|SystemMessageReplaceConfig|null $systemMessage
     * @param string[]|null $availableTools
     * @param string[]|null $excludedTools
     * @param ProviderConfig|null $provider
     * @param callable|null $onPermissionRequest
     * @param callable|null $onUserInputRequest
     * @param SessionHooks|null $hooks
     * @param string|null $workingDirectory
     * @param string|null $configDir
     * @param bool|null $streaming
     * @param array|null $mcpServers
     * @param CustomAgentConfig[]|null $customAgents
     * @param string[]|null $skillDirectories
     * @param string[]|null $disabledSkills
     * @param InfiniteSessionConfig|null $infiniteSessions
     * @param array<string,mixed>|null $modelCapabilities Per-property overrides for model capabilities
     * @param bool|null $enableConfigDiscovery Auto-discover MCP server configs from working directory
     * @param bool|null $disableResume
     */
    public function __construct(
        public readonly ?string $model = null,
        public readonly ?ReasoningEffort $reasoningEffort = null,
        public readonly ?array $tools = null,
        public readonly SystemMessageAppendConfig|SystemMessageReplaceConfig|SystemMessageCustomizeConfig|null $systemMessage = null,
        public readonly ?array $availableTools = null,
        public readonly ?array $excludedTools = null,
        public readonly ?ProviderConfig $provider = null,
        public /* readonly */ $onPermissionRequest = null,
        public /* readonly */ $onUserInputRequest = null,
        public readonly ?SessionHooks $hooks = null,
        public readonly ?string $workingDirectory = null,
        public readonly ?string $configDir = null,
        public readonly ?bool $streaming = null,
        /** Include sub-agent streaming events in the event stream. Defaults to true. */
        public readonly ?bool $includeSubAgentStreamingEvents = null,
        public readonly ?array $mcpServers = null,
        public readonly ?array $customAgents = null,
        public readonly ?array $skillDirectories = null,
        public readonly ?array $disabledSkills = null,
        public readonly ?InfiniteSessionConfig $infiniteSessions = null,
        public readonly ?array $modelCapabilities = null,
        public readonly ?bool $enableConfigDiscovery = null,
        /** GitHub token for authentication. When set on session config, overrides the client-level token for this session only. */
        public readonly ?string $gitHubToken = null,
        /** @var CommandDefinition[]|null $commands Slash commands for this session */
        public readonly ?array $commands = null,
        /** @var callable|null $onElicitationRequest Elicitation handler */
        public /* readonly */ $onElicitationRequest = null,
        /** @var string[]|null $instructionDirectories Instruction directories for prompt customization */
        public readonly ?array $instructionDirectories = null,
        public readonly ?bool $disableResume = null,
        // --- v2.6.0 upstream-sync options ---
        /** Stream responses over the WebSocket transport. Sent inside `capi`. */
        public readonly ?bool $enableWebSocketResponses = null,
        /** Auto routing preference when the model is "auto" (experimental; see AutoTier). Sent inside `capi`. */
        public readonly ?string $autoTier = null,
        /** Ask-user prompt variant: "legacy" or "elicitation" (see AskUserVariant). */
        public readonly ?string $askUserVariant = null,
        /** @var callable|null $gitHubTokenProvider fn(GitHubTokenProviderArgs): GitHubTokenResult|array. Mutually exclusive with $gitHubToken. */
        public /* readonly */ $gitHubTokenProvider = null,
        /** Sandbox configuration (SandboxConfig or wire array). */
        public readonly SandboxConfig|array|null $sandboxConfig = null,
        /** @var array<string,bool>|null $featureFlags Runtime feature flags (sent even when empty) */
        public readonly ?array $featureFlags = null,
        /** @var string[]|null $allowedModels Model allowlist for this session */
        public readonly ?array $allowedModels = null,
        /** OAuth client ID metadata document URL. */
        public readonly ?string $authClientIdMetadataUrl = null,
        /** @var string[]|null $agentDirectories Directories to load custom agents from */
        public readonly ?array $agentDirectories = null,
        /** @var string[]|null $hookDirectories Directories to load hooks from */
        public readonly ?array $hookDirectories = null,
        /** Whether an interactive user is present. */
        public readonly ?bool $interactiveUser = null,
        /** @var string[]|null $excludedManagedSettings Managed settings to ignore */
        public readonly ?array $excludedManagedSettings = null,
        /** Enforce managed model defaults. */
        public readonly ?bool $enforceManagedModelDefaults = null,
        /** Allow recovering the conversation from the transcript when resuming. */
        public readonly ?bool $allowTranscriptRecovery = null,
        /** Session-level image generation configuration (ImageGenerationConfig or wire array). */
        public readonly ImageGenerationConfig|array|null $imageGeneration = null,
        /** Autopilot continuation options (AutopilotOptions or wire array). */
        public readonly AutopilotOptions|array|null $autopilot = null,
    ) {}

    public function toServerParams(string $sessionId): array
    {
        $params = ['sessionId' => $sessionId];
        if ($this->model !== null) {
            $params['model'] = $this->model;
        }
        if ($this->reasoningEffort !== null) {
            $params['reasoningEffort'] = $this->reasoningEffort->value;
        }
        if ($this->tools !== null) {
            $params['tools'] = array_map(
                fn(Tool $t) => $t->toServerFormat(),
                $this->tools,
            );
        }
        if ($this->systemMessage !== null) {
            $params['systemMessage'] = $this->systemMessage->toArray();
        }
        if ($this->availableTools !== null) {
            $params['availableTools'] = $this->availableTools;
        }
        if ($this->excludedTools !== null) {
            $params['excludedTools'] = $this->excludedTools;
        }
        if ($this->provider !== null) {
            $params['provider'] = $this->provider->toArray();
        }
        $params['requestPermission'] = $this->onPermissionRequest !== null;
        $params['requestUserInput'] = $this->onUserInputRequest !== null;
        $params['hooks'] = $this->hooks !== null && $this->hooks->hasAnyHandler();
        if ($this->workingDirectory !== null) {
            $params['workingDirectory'] = $this->workingDirectory;
        }
        if ($this->configDir !== null) {
            $params['configDir'] = $this->configDir;
        }
        if ($this->streaming !== null) {
            $params['streaming'] = $this->streaming;
        }
        if ($this->includeSubAgentStreamingEvents !== null) {
            $params['includeSubAgentStreamingEvents'] = $this->includeSubAgentStreamingEvents;
        }
        if ($this->mcpServers !== null) {
            $mcpArr = [];
            foreach ($this->mcpServers as $name => $config) {
                $mcpArr[$name] = $config->toArray();
            }
            $params['mcpServers'] = $mcpArr;
        }
        if ($this->customAgents !== null) {
            $params['customAgents'] = array_map(
                fn(CustomAgentConfig $a) => $a->toArray(),
                $this->customAgents,
            );
        }
        if ($this->skillDirectories !== null) {
            $params['skillDirectories'] = $this->skillDirectories;
        }
        if ($this->disabledSkills !== null) {
            $params['disabledSkills'] = $this->disabledSkills;
        }
        if ($this->infiniteSessions !== null) {
            $params['infiniteSessions'] = $this->infiniteSessions->toArray();
        }
        if ($this->modelCapabilities !== null) {
            $params['modelCapabilities'] = $this->modelCapabilities;
        }
        if ($this->enableConfigDiscovery !== null) {
            $params['enableConfigDiscovery'] = $this->enableConfigDiscovery;
        }
        if ($this->gitHubToken !== null) {
            $params['gitHubToken'] = $this->gitHubToken;
        }
        if ($this->instructionDirectories !== null) {
            $params['instructionDirectories'] = $this->instructionDirectories;
        }
        if ($this->disableResume !== null) {
            $params['disableResume'] = $this->disableResume;
        }
        // --- v2.6.0 upstream-sync session options (parity with @github/copilot-sdk) ---
        $capi = [];
        if ($this->autoTier !== null) {
            $capi['autoTier'] = $this->autoTier;
        }
        if ($this->enableWebSocketResponses !== null) {
            $capi['enableWebSocketResponses'] = $this->enableWebSocketResponses;
        }
        if ($capi !== []) {
            $params['capi'] = $capi;
        }
        if ($this->askUserVariant !== null) {
            $params['askUserVariant'] = $this->askUserVariant;
        }
        if ($this->sandboxConfig !== null) {
            $params['sandboxConfig'] = WireFormat::toObject($this->sandboxConfig);
        }
        if ($this->featureFlags !== null) {
            $params['featureFlags'] = WireFormat::toObject($this->featureFlags);
        }
        if ($this->allowedModels !== null) {
            $params['allowedModels'] = $this->allowedModels;
        }
        if ($this->authClientIdMetadataUrl !== null) {
            $params['authClientIdMetadataUrl'] = $this->authClientIdMetadataUrl;
        }
        if ($this->agentDirectories !== null) {
            $params['agentDirectories'] = $this->agentDirectories;
        }
        if ($this->hookDirectories !== null) {
            $params['hookDirectories'] = $this->hookDirectories;
        }
        if ($this->interactiveUser !== null) {
            $params['interactiveUser'] = $this->interactiveUser;
        }
        if ($this->excludedManagedSettings !== null) {
            $params['excludedManagedSettings'] = $this->excludedManagedSettings;
        }
        if ($this->enforceManagedModelDefaults !== null) {
            $params['enforceManagedModelDefaults'] = $this->enforceManagedModelDefaults;
        }
        if ($this->allowTranscriptRecovery !== null) {
            $params['allowTranscriptRecovery'] = $this->allowTranscriptRecovery;
        }
        if ($this->imageGeneration !== null) {
            $params['imageGeneration'] = WireFormat::toObject($this->imageGeneration);
        }
        if ($this->autopilot !== null) {
            $params['autopilot'] = WireFormat::toObject($this->autopilot);
        }
        return $params;
    }
}

// ============================================================================
// Session Event (generic container)
// ============================================================================

class SessionEvent
{
    public function __construct(
        public readonly string $type,
        public readonly string $id = '',
        public readonly string $timestamp = '',
        public readonly ?string $parentId = null,
        public readonly ?string $agentId = null,
        public readonly ?bool $ephemeral = null,
        public readonly array $data = [],
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            type: $data['type'] ?? 'unknown',
            id: $data['id'] ?? '',
            timestamp: $data['timestamp'] ?? '',
            parentId: $data['parentId'] ?? null,
            agentId: $data['agentId'] ?? null,
            ephemeral: $data['ephemeral'] ?? null,
            data: $data['data'] ?? $data,
        );
    }
}

// ============================================================================
// Response Format & Image Types
// ============================================================================

class ResponseFormat
{
    const TEXT = 'text';
    const IMAGE = 'image';
    const JSON_OBJECT = 'json_object';
}

class ImageOptions
{
    public function __construct(
        public readonly ?string $size = null,
        public readonly ?string $quality = null,
        public readonly ?string $style = null,
    ) {}

    public function toArray(): array
    {
        return array_filter([
            'size' => $this->size,
            'quality' => $this->quality,
            'style' => $this->style,
        ], fn($v) => $v !== null);
    }
}

class AssistantImageData
{
    public function __construct(
        public readonly string $format,
        public readonly string $base64,
        public readonly int $width,
        public readonly int $height,
        public readonly ?string $url = null,
        public readonly ?string $revisedPrompt = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            format: $data['format'] ?? '',
            base64: $data['base64'] ?? '',
            width: $data['width'] ?? 0,
            height: $data['height'] ?? 0,
            url: $data['url'] ?? null,
            revisedPrompt: $data['revisedPrompt'] ?? null,
        );
    }
}

class ContentBlock
{
    public function __construct(
        public readonly string $type,
        public readonly ?string $text = null,
        public readonly ?AssistantImageData $image = null,
    ) {}

    public static function fromArray(array $data): self
    {
        $image = isset($data['image']) ? AssistantImageData::fromArray($data['image']) : null;
        return new self(
            type: $data['type'] ?? '',
            text: $data['text'] ?? null,
            image: $image,
        );
    }
}

// ============================================================================
// Message Options
// ============================================================================

class FileAttachment
{
    public function __construct(
        public readonly string $path,
        public readonly ?string $displayName = null,
    ) {}

    public function toArray(): array
    {
        $result = ['type' => 'file', 'path' => $this->path];
        if ($this->displayName !== null) {
            $result['displayName'] = $this->displayName;
        }
        return $result;
    }
}

class DirectoryAttachment
{
    public function __construct(
        public readonly string $path,
        public readonly ?string $displayName = null,
    ) {}

    public function toArray(): array
    {
        $result = ['type' => 'directory', 'path' => $this->path];
        if ($this->displayName !== null) {
            $result['displayName'] = $this->displayName;
        }
        return $result;
    }
}

class SelectionRange
{
    public function __construct(
        public readonly int $line,
        public readonly int $character,
    ) {}

    public function toArray(): array
    {
        return ['line' => $this->line, 'character' => $this->character];
    }
}

class Selection
{
    public function __construct(
        public readonly SelectionRange $start,
        public readonly SelectionRange $end,
    ) {}

    public function toArray(): array
    {
        return ['start' => $this->start->toArray(), 'end' => $this->end->toArray()];
    }
}

class SelectionAttachment
{
    public function __construct(
        public readonly string $filePath,
        public readonly string $displayName,
        public readonly ?Selection $selection = null,
        public readonly ?string $text = null,
    ) {}

    public function toArray(): array
    {
        $result = [
            'type' => 'selection',
            'filePath' => $this->filePath,
            'displayName' => $this->displayName,
        ];
        if ($this->selection !== null) {
            $result['selection'] = $this->selection->toArray();
        }
        if ($this->text !== null) {
            $result['text'] = $this->text;
        }
        return $result;
    }
}

class MessageOptions
{
    /**
     * @param string $prompt The prompt/message to send
     * @param array<FileAttachment|DirectoryAttachment|SelectionAttachment>|null $attachments
     * @param string|null $mode "enqueue" (default) or "immediate"
     * @param string|null $responseFormat Response format (see ResponseFormat constants)
     * @param ImageOptions|null $imageOptions Image generation options
     * @param array<string,string>|null $requestHeaders Custom HTTP headers for outbound model requests
     * @param string|null $agentMode Agent-mode override for this message
     * @param string|null $displayPrompt Alternate prompt text shown to the user (the model still sees $prompt)
     * @param string|null $source Message provenance: "user", "system" or "agent-<id>" (see MessageSource)
     * @param array<string,mixed>|object|null $responseSchema JSON Schema requesting structured output;
     *        sent as a json_schema responseFormat and takes precedence over $responseFormat
     */
    public function __construct(
        public readonly string $prompt,
        public readonly ?array $attachments = null,
        public readonly ?string $mode = null,
        public readonly ?string $responseFormat = null,
        public readonly ?ImageOptions $imageOptions = null,
        public readonly ?array $requestHeaders = null,
        public readonly ?string $agentMode = null,
        public readonly ?string $displayPrompt = null,
        public readonly ?string $source = null,
        public readonly array|object|null $responseSchema = null,
    ) {}

    public function toArray(): array
    {
        $result = ['prompt' => $this->prompt];
        if ($this->source !== null) {
            $result['source'] = $this->source;
        }
        if ($this->attachments !== null) {
            $result['attachments'] = array_map(
                fn($a) => $a->toArray(),
                $this->attachments,
            );
        }
        if ($this->mode !== null) {
            $result['mode'] = $this->mode;
        }
        if ($this->responseSchema !== null) {
            $result['responseFormat'] = [
                'type' => 'json_schema',
                'jsonSchema' => [
                    'name' => 'response',
                    'strict' => true,
                    'schema' => $this->responseSchema,
                ],
            ];
        } elseif ($this->responseFormat !== null) {
            $result['responseFormat'] = $this->responseFormat;
        }
        if ($this->imageOptions !== null) {
            $result['imageOptions'] = $this->imageOptions->toArray();
        }
        if ($this->requestHeaders !== null) {
            $result['requestHeaders'] = $this->requestHeaders;
        }
        if ($this->agentMode !== null) {
            $result['agentMode'] = $this->agentMode;
        }
        if ($this->displayPrompt !== null) {
            $result['displayPrompt'] = $this->displayPrompt;
        }
        return $result;
    }
}

// ============================================================================
// Hook Types
// ============================================================================

class PreToolUseHookInput
{
    public function __construct(
        public readonly int $timestamp,
        public readonly string $cwd,
        public readonly string $toolName,
        public readonly mixed $toolArgs,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            timestamp: $data['timestamp'] ?? 0,
            cwd: $data['cwd'] ?? '',
            toolName: $data['toolName'] ?? '',
            toolArgs: $data['toolArgs'] ?? null,
        );
    }
}

class PreToolUseHookOutput
{
    public function __construct(
        public readonly ?string $permissionDecision = null,
        public readonly ?string $permissionDecisionReason = null,
        public readonly mixed $modifiedArgs = null,
        public readonly ?string $additionalContext = null,
        public readonly ?bool $suppressOutput = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->permissionDecision !== null) {
            $result['permissionDecision'] = $this->permissionDecision;
        }
        if ($this->permissionDecisionReason !== null) {
            $result['permissionDecisionReason'] = $this->permissionDecisionReason;
        }
        if ($this->modifiedArgs !== null) {
            $result['modifiedArgs'] = $this->modifiedArgs;
        }
        if ($this->additionalContext !== null) {
            $result['additionalContext'] = $this->additionalContext;
        }
        if ($this->suppressOutput !== null) {
            $result['suppressOutput'] = $this->suppressOutput;
        }
        return $result;
    }
}

class PostToolUseHookInput
{
    public function __construct(
        public readonly int $timestamp,
        public readonly string $cwd,
        public readonly string $toolName,
        public readonly mixed $toolArgs,
        public readonly mixed $toolResult,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            timestamp: $data['timestamp'] ?? 0,
            cwd: $data['cwd'] ?? '',
            toolName: $data['toolName'] ?? '',
            toolArgs: $data['toolArgs'] ?? null,
            toolResult: $data['toolResult'] ?? null,
        );
    }
}

class PostToolUseHookOutput
{
    public function __construct(
        public readonly mixed $modifiedResult = null,
        public readonly ?string $additionalContext = null,
        public readonly ?bool $suppressOutput = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->modifiedResult !== null) {
            $result['modifiedResult'] = $this->modifiedResult;
        }
        if ($this->additionalContext !== null) {
            $result['additionalContext'] = $this->additionalContext;
        }
        if ($this->suppressOutput !== null) {
            $result['suppressOutput'] = $this->suppressOutput;
        }
        return $result;
    }
}

class UserPromptSubmittedHookInput
{
    public function __construct(
        public readonly int $timestamp,
        public readonly string $cwd,
        public readonly string $prompt,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            timestamp: $data['timestamp'] ?? 0,
            cwd: $data['cwd'] ?? '',
            prompt: $data['prompt'] ?? '',
        );
    }
}

class UserPromptSubmittedHookOutput
{
    public function __construct(
        public readonly ?string $modifiedPrompt = null,
        public readonly ?string $additionalContext = null,
        public readonly ?bool $suppressOutput = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->modifiedPrompt !== null) {
            $result['modifiedPrompt'] = $this->modifiedPrompt;
        }
        if ($this->additionalContext !== null) {
            $result['additionalContext'] = $this->additionalContext;
        }
        if ($this->suppressOutput !== null) {
            $result['suppressOutput'] = $this->suppressOutput;
        }
        return $result;
    }
}

class SessionStartHookInput
{
    public function __construct(
        public readonly int $timestamp,
        public readonly string $cwd,
        public readonly string $source,
        public readonly ?string $initialPrompt = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            timestamp: $data['timestamp'] ?? 0,
            cwd: $data['cwd'] ?? '',
            source: $data['source'] ?? 'new',
            initialPrompt: $data['initialPrompt'] ?? null,
        );
    }
}

class SessionStartHookOutput
{
    public function __construct(
        public readonly ?string $additionalContext = null,
        public readonly ?array $modifiedConfig = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->additionalContext !== null) {
            $result['additionalContext'] = $this->additionalContext;
        }
        if ($this->modifiedConfig !== null) {
            $result['modifiedConfig'] = $this->modifiedConfig;
        }
        return $result;
    }
}

class SessionEndHookInput
{
    public function __construct(
        public readonly int $timestamp,
        public readonly string $cwd,
        public readonly string $reason,
        public readonly ?string $finalMessage = null,
        public readonly ?string $error = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            timestamp: $data['timestamp'] ?? 0,
            cwd: $data['cwd'] ?? '',
            reason: $data['reason'] ?? 'complete',
            finalMessage: $data['finalMessage'] ?? null,
            error: $data['error'] ?? null,
        );
    }
}

class SessionEndHookOutput
{
    public function __construct(
        public readonly ?bool $suppressOutput = null,
        /** @var string[]|null */
        public readonly ?array $cleanupActions = null,
        public readonly ?string $sessionSummary = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->suppressOutput !== null) {
            $result['suppressOutput'] = $this->suppressOutput;
        }
        if ($this->cleanupActions !== null) {
            $result['cleanupActions'] = $this->cleanupActions;
        }
        if ($this->sessionSummary !== null) {
            $result['sessionSummary'] = $this->sessionSummary;
        }
        return $result;
    }
}

class ErrorOccurredHookInput
{
    public function __construct(
        public readonly int $timestamp,
        public readonly string $cwd,
        public readonly string $error,
        public readonly string $errorContext,
        public readonly bool $recoverable,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            timestamp: $data['timestamp'] ?? 0,
            cwd: $data['cwd'] ?? '',
            error: $data['error'] ?? '',
            errorContext: $data['errorContext'] ?? 'system',
            recoverable: $data['recoverable'] ?? false,
        );
    }
}

class ErrorOccurredHookOutput
{
    public function __construct(
        public readonly ?bool $suppressOutput = null,
        public readonly ?string $errorHandling = null,
        public readonly ?int $retryCount = null,
        public readonly ?string $userNotification = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->suppressOutput !== null) {
            $result['suppressOutput'] = $this->suppressOutput;
        }
        if ($this->errorHandling !== null) {
            $result['errorHandling'] = $this->errorHandling;
        }
        if ($this->retryCount !== null) {
            $result['retryCount'] = $this->retryCount;
        }
        if ($this->userNotification !== null) {
            $result['userNotification'] = $this->userNotification;
        }
        return $result;
    }
}

class SessionHooks
{
    /**
     * @param callable|null $onPreToolUse fn(PreToolUseHookInput, array{sessionId:string}): ?PreToolUseHookOutput
     * @param callable|null $onPostToolUse fn(PostToolUseHookInput, array{sessionId:string}): ?PostToolUseHookOutput
     * @param callable|null $onUserPromptSubmitted fn(UserPromptSubmittedHookInput, array{sessionId:string}): ?UserPromptSubmittedHookOutput
     * @param callable|null $onUserPromptTransformed fn(array, array{sessionId:string}): mixed User-prompt-transformed hook
     * @param callable|null $onSessionStart fn(SessionStartHookInput, array{sessionId:string}): ?SessionStartHookOutput
     * @param callable|null $onSessionEnd fn(SessionEndHookInput, array{sessionId:string}): ?SessionEndHookOutput
     * @param callable|null $onErrorOccurred fn(ErrorOccurredHookInput, array{sessionId:string}): ?ErrorOccurredHookOutput
     * @param callable|null $onPreMcpToolCall fn(array, array{sessionId:string}): mixed Pre-MCP-tool-call hook
     * @param callable|null $onSubagentStart fn(array $input, array{sessionId:string}): SubagentStartHookOutput|array|null
     *        ("subagentStart"; see SubagentStartHookInput::fromArray())
     * @param callable|null $onSubagentStop fn(array $input, array{sessionId:string}): SubagentStopHookOutput|array|null
     *        ("subagentStop"; see SubagentStopHookInput::fromArray())
     */
    public function __construct(
        public /* readonly */ $onPreToolUse = null,
        public /* readonly */ $onPostToolUse = null,
        public /* readonly */ $onUserPromptSubmitted = null,
        public /* readonly */ $onUserPromptTransformed = null,
        public /* readonly */ $onSessionStart = null,
        public /* readonly */ $onSessionEnd = null,
        public /* readonly */ $onErrorOccurred = null,
        public /* readonly */ $onPreMcpToolCall = null,
        public /* readonly */ $onSubagentStart = null,
        public /* readonly */ $onSubagentStop = null,
    ) {}

    public function hasAnyHandler(): bool
    {
        return $this->onPreToolUse !== null
            || $this->onPostToolUse !== null
            || $this->onUserPromptSubmitted !== null
            || $this->onUserPromptTransformed !== null
            || $this->onSessionStart !== null
            || $this->onSessionEnd !== null
            || $this->onErrorOccurred !== null
            || $this->onPreMcpToolCall !== null
            || $this->onSubagentStart !== null
            || $this->onSubagentStop !== null;
    }
}

/**
 * Input for the "subagentStart" hook (session metadata belongs to the parent session).
 */
class SubagentStartHookInput
{
    public function __construct(
        public readonly mixed $timestamp,
        public readonly string $transcriptPath,
        public readonly string $agentName,
        public readonly ?string $agentDisplayName = null,
        public readonly ?string $agentDescription = null,
        public readonly ?string $sessionId = null,
        public readonly ?string $workingDirectory = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            timestamp: $data['timestamp'] ?? null,
            transcriptPath: $data['transcriptPath'] ?? '',
            agentName: $data['agentName'] ?? '',
            agentDisplayName: $data['agentDisplayName'] ?? null,
            agentDescription: $data['agentDescription'] ?? null,
            sessionId: $data['sessionId'] ?? null,
            workingDirectory: $data['workingDirectory'] ?? null,
        );
    }
}

/**
 * Output for the "subagentStart" hook: context prepended to the subagent's initial prompt.
 */
class SubagentStartHookOutput
{
    public function __construct(
        public readonly ?string $additionalContext = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->additionalContext !== null) {
            $result['additionalContext'] = $this->additionalContext;
        }
        return $result;
    }
}

/**
 * Input for the "subagentStop" hook, sent after a subagent turn completes.
 */
class SubagentStopHookInput
{
    public function __construct(
        public readonly mixed $timestamp,
        public readonly string $transcriptPath,
        public readonly string $agentName,
        public readonly string $agentType,
        public readonly string $stopReason,
        public readonly string $response,
        public readonly ?string $agentId = null,
        public readonly ?string $agentDisplayName = null,
        public readonly ?string $agentDescription = null,
        public readonly ?string $sessionId = null,
        public readonly ?string $workingDirectory = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            timestamp: $data['timestamp'] ?? null,
            transcriptPath: $data['transcriptPath'] ?? '',
            agentName: $data['agentName'] ?? '',
            agentType: $data['agentType'] ?? '',
            stopReason: $data['stopReason'] ?? 'end_turn',
            response: $data['response'] ?? '',
            agentId: $data['agentId'] ?? null,
            agentDisplayName: $data['agentDisplayName'] ?? null,
            agentDescription: $data['agentDescription'] ?? null,
            sessionId: $data['sessionId'] ?? null,
            workingDirectory: $data['workingDirectory'] ?? null,
        );
    }
}

/**
 * Output for the "subagentStop" hook: "block" (with a reason) or "allow", optionally
 * rewriting the subagent's response.
 */
class SubagentStopHookOutput
{
    public function __construct(
        public readonly ?string $decision = null,
        public readonly ?string $reason = null,
        public readonly ?string $modifiedResponse = null,
    ) {}

    public function toArray(): array
    {
        $result = [];
        if ($this->decision !== null) {
            $result['decision'] = $this->decision;
        }
        if ($this->reason !== null) {
            $result['reason'] = $this->reason;
        }
        if ($this->modifiedResponse !== null) {
            $result['modifiedResponse'] = $this->modifiedResponse;
        }
        return $result;
    }
}

// ============================================================================
// Ping Response
// ============================================================================

class PingResponse
{
    public function __construct(
        public readonly string $message,
        public readonly string $timestamp,
        public readonly ?int $protocolVersion = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            message: $data['message'] ?? '',
            timestamp: (string) ($data['timestamp'] ?? ''),
            protocolVersion: $data['protocolVersion'] ?? null,
        );
    }
}

// ============================================================================
// Status Responses
// ============================================================================

class GetStatusResponse
{
    public function __construct(
        public readonly string $version,
        public readonly int $protocolVersion,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            version: $data['version'] ?? '',
            protocolVersion: $data['protocolVersion'] ?? 0,
        );
    }
}

class GetAuthStatusResponse
{
    public function __construct(
        public readonly bool $isAuthenticated,
        public readonly ?string $authType = null,
        public readonly ?string $host = null,
        public readonly ?string $login = null,
        public readonly ?string $statusMessage = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            isAuthenticated: (bool) ($data['isAuthenticated'] ?? false),
            authType: $data['authType'] ?? null,
            host: $data['host'] ?? null,
            login: $data['login'] ?? null,
            statusMessage: $data['statusMessage'] ?? null,
        );
    }
}

// ============================================================================
// Model Types
// ============================================================================

class ModelVisionLimits
{
    public function __construct(
        /** @var string[]|null */
        public readonly ?array $supportedMediaTypes = null,
        public readonly ?int $maxPromptImages = null,
        public readonly ?int $maxPromptImageSize = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            supportedMediaTypes: $data['supported_media_types'] ?? null,
            maxPromptImages: $data['max_prompt_images'] ?? null,
            maxPromptImageSize: $data['max_prompt_image_size'] ?? null,
        );
    }
}

class ModelLimits
{
    public function __construct(
        public readonly ?int $maxPromptTokens = null,
        public readonly ?int $maxContextWindowTokens = null,
        public readonly ?ModelVisionLimits $vision = null,
        public readonly ?int $maxOutputTokens = null,
    ) {}

    public static function fromArray(array $data): self
    {
        $vision = isset($data['vision']) ? ModelVisionLimits::fromArray($data['vision']) : null;
        return new self(
            maxPromptTokens: $data['max_prompt_tokens'] ?? null,
            maxContextWindowTokens: $data['max_context_window_tokens'] ?? null,
            vision: $vision,
            maxOutputTokens: $data['max_output_tokens'] ?? null,
        );
    }
}

class ModelSupports
{
    public function __construct(
        public readonly bool $vision,
        public readonly bool $reasoningEffort = false,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            vision: (bool) ($data['vision'] ?? false),
            reasoningEffort: (bool) ($data['reasoningEffort'] ?? false),
        );
    }
}

class ModelCapabilities
{
    public function __construct(
        public readonly ModelSupports $supports,
        public readonly ModelLimits $limits,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            supports: ModelSupports::fromArray($data['supports'] ?? []),
            limits: ModelLimits::fromArray($data['limits'] ?? []),
        );
    }
}

class ModelPolicy
{
    public function __construct(
        public readonly string $state,
        public readonly string $terms,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            state: $data['state'] ?? 'unconfigured',
            terms: $data['terms'] ?? '',
        );
    }
}

class ModelBilling
{
    public function __construct(
        public readonly float $multiplier,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            multiplier: (float) ($data['multiplier'] ?? 1.0),
        );
    }
}

class ModelInfo
{
    public function __construct(
        public readonly string $id,
        public readonly string $name,
        public readonly ModelCapabilities $capabilities,
        public readonly ?ModelPolicy $policy = null,
        public readonly ?ModelBilling $billing = null,
        /** @var string[]|null */
        public readonly ?array $supportedReasoningEfforts = null,
        public readonly ?string $defaultReasoningEffort = null,
    ) {}

    public static function fromArray(array $data): self
    {
        $policy = isset($data['policy']) ? ModelPolicy::fromArray($data['policy']) : null;
        $billing = isset($data['billing']) ? ModelBilling::fromArray($data['billing']) : null;
        return new self(
            id: $data['id'] ?? '',
            name: $data['name'] ?? '',
            capabilities: ModelCapabilities::fromArray($data['capabilities'] ?? []),
            policy: $policy,
            billing: $billing,
            supportedReasoningEfforts: $data['supportedReasoningEfforts'] ?? null,
            defaultReasoningEffort: $data['defaultReasoningEffort'] ?? null,
        );
    }
}

// ============================================================================
// Session Metadata
// ============================================================================

class SessionMetadata
{
    public function __construct(
        public readonly string $sessionId,
        public readonly string $startTime,
        public readonly string $modifiedTime,
        public readonly bool $isRemote,
        public readonly ?string $summary = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            sessionId: $data['sessionId'] ?? '',
            startTime: $data['startTime'] ?? '',
            modifiedTime: $data['modifiedTime'] ?? '',
            isRemote: (bool) ($data['isRemote'] ?? false),
            summary: $data['summary'] ?? null,
        );
    }
}

// ============================================================================
// Session Lifecycle Event
// ============================================================================

class SessionLifecycleEventMetadata
{
    public function __construct(
        public readonly string $startTime,
        public readonly string $modifiedTime,
        public readonly ?string $summary = null,
    ) {}

    public static function fromArray(array $data): self
    {
        return new self(
            startTime: $data['startTime'] ?? '',
            modifiedTime: $data['modifiedTime'] ?? '',
            summary: $data['summary'] ?? null,
        );
    }
}

class SessionLifecycleEvent
{
    public function __construct(
        public readonly string $type,
        public readonly string $sessionId,
        public readonly ?SessionLifecycleEventMetadata $metadata = null,
    ) {}

    public static function fromArray(array $data): self
    {
        $metadata = isset($data['metadata']) && $data['metadata']
            ? SessionLifecycleEventMetadata::fromArray($data['metadata'])
            : null;
        return new self(
            type: $data['type'] ?? 'session.updated',
            sessionId: $data['sessionId'] ?? '',
            metadata: $metadata,
        );
    }
}

// ============================================================================
// Session Filesystem Types
// ============================================================================

/**
 * Configuration for a custom session filesystem provider.
 */
class SessionFsConfig
{
    public function __construct(
        public readonly string $initialCwd = '',
        public readonly string $sessionStatePath = '',
        public readonly string $conventions = '',
    ) {}

    public function toArray(): array
    {
        return [
            'initialCwd' => $this->initialCwd,
            'sessionStatePath' => $this->sessionStatePath,
            'conventions' => $this->conventions,
        ];
    }

    public static function fromArray(array $data): self
    {
        return new self(
            initialCwd: $data['initialCwd'] ?? '',
            sessionStatePath: $data['sessionStatePath'] ?? '',
            conventions: $data['conventions'] ?? '',
        );
    }
}

/**
 * File metadata returned by session filesystem operations.
 */
class SessionFsFileInfo
{
    public function __construct(
        public readonly string $name = '',
        public readonly int $size = 0,
        public readonly bool $isDirectory = false,
        public readonly bool $isFile = false,
        public readonly ?string $createdAt = null,
        public readonly ?string $modifiedAt = null,
    ) {}

    public function toArray(): array
    {
        $result = [
            'name' => $this->name,
            'size' => $this->size,
            'isDirectory' => $this->isDirectory,
            'isFile' => $this->isFile,
        ];
        if ($this->createdAt !== null) {
            $result['createdAt'] = $this->createdAt;
        }
        if ($this->modifiedAt !== null) {
            $result['modifiedAt'] = $this->modifiedAt;
        }
        return $result;
    }

    public static function fromArray(array $data): self
    {
        return new self(
            name: $data['name'] ?? '',
            size: $data['size'] ?? 0,
            isDirectory: $data['isDirectory'] ?? false,
            isFile: $data['isFile'] ?? false,
            createdAt: $data['createdAt'] ?? null,
            modifiedAt: $data['modifiedAt'] ?? null,
        );
    }
}

/**
 * Interface for session filesystem providers.
 *
 * Implementors provide file operations scoped to a session.
 */
interface SessionFsProvider
{
    public function readFile(string $sessionId, string $path): string;
    public function writeFile(string $sessionId, string $path, string $content): void;
    public function appendFile(string $sessionId, string $path, string $content): void;
    public function exists(string $sessionId, string $path): bool;
    public function stat(string $sessionId, string $path): SessionFsFileInfo;
    public function mkdir(string $sessionId, string $path, bool $recursive = false): void;
    /** @return string[] */
    public function readdir(string $sessionId, string $path): array;
    /** @return SessionFsFileInfo[] */
    public function readdirWithTypes(string $sessionId, string $path): array;
    public function rm(string $sessionId, string $path, bool $recursive = false): void;
    public function rename(string $sessionId, string $oldPath, string $newPath): void;
}

// ============================================================================
// Client Options
// ============================================================================

class CopilotClientOptions
{
    /**
     * @param string|null $cliPath Path to CLI executable
     * @param string[]|null $cliArgs Extra CLI arguments
     * @param string|null $cwd Working directory
     * @param int|null $port Port for TCP mode
     * @param bool $useStdio Use stdio transport (default: true)
     * @param string|null $cliUrl URL of existing server (mutually exclusive with cliPath/useStdio)
     * @param LogLevel $logLevel Log level
     * @param bool $autoStart Auto-start on first use
     * @param bool $autoRestart Auto-restart on crash
     * @param array<string,string>|null $env Environment variables
     * @param string|null $githubToken GitHub token for auth
     * @param bool|null $useLoggedInUser Use logged-in user auth
     * @param int|null $sessionIdleTimeoutSeconds Server-wide idle timeout for sessions in seconds
     * @param SessionFsConfig|null $sessionFs Custom session filesystem configuration
     * @param ClientInfo|array|null $clientInfo Identity of the integrating application, declared on
     *        the `connect` handshake when any field is set (array keys: applicationName,
     *        applicationVersion, integrationName, integrationVersion)
     */
    public function __construct(
        public readonly ?string $cliPath = null,
        public readonly ?array $cliArgs = null,
        public readonly ?string $cwd = null,
        public readonly ?int $port = null,
        public readonly bool $useStdio = true,
        public readonly ?string $cliUrl = null,
        public readonly LogLevel $logLevel = LogLevel::Info,
        public readonly bool $autoStart = true,
        public readonly bool $autoRestart = true,
        public readonly ?array $env = null,
        public readonly ?string $githubToken = null,
        public readonly ?bool $useLoggedInUser = null,
        public readonly ?int $sessionIdleTimeoutSeconds = null,
        public readonly ?SessionFsConfig $sessionFs = null,
        public readonly ?string $copilotHome = null,
        public readonly ?string $tcpConnectionToken = null,
        /** HTTP request handler for intercepting outbound LLM inference requests. */
        public readonly ?CopilotRequestHandler $requestHandler = null,
        /** @var callable|null $bearerTokenProvider BYOK bearer-token provider fn(ProviderTokenArgs): string */
        public /* readonly */ $bearerTokenProvider = null,
        public readonly ClientInfo|array|null $clientInfo = null,
    ) {}
}
