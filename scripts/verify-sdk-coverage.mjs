#!/usr/bin/env node
/**
 * SDK API Coverage Checker
 * 
 * Scans all 36 additional SDK client files to verify they implement
 * every required RPC method. Fails CI if any SDK is missing methods.
 * 
 * Usage: node scripts/verify-sdk-coverage.mjs
 */

import { readFileSync, readdirSync, statSync } from 'fs';
import { join, resolve } from 'path';
import { execSync } from 'child_process';

const ROOT = resolve(import.meta.dirname, '..');

// Directories skipped during recursive feature scanning: VCS, test, and build/vendor output.
// Excluding build/vendor dirs prevents false positives from compiled or vendored copies of source.
const SKIP_SCAN_DIRS = new Set([
  'node_modules', '.git', 'test', 'tests', 'e2e',
  'bin', 'obj', 'build', 'target', '.build', 'dist', 'out',
  'vendor', 'deps', '_build', '.dart_tool', '.gradle', '.stack-work', '.dub', '.cargo',
]);

// Gold standard: every RPC method that all SDKs must implement
const REQUIRED_RPC_METHODS = [
  'session.create',
  'session.resume',
  'session.delete',
  'session.list',
  'session.getLastId',
  'session.getMetadata',
  'session.getForeground',
  'session.setForeground',
  'sessionFs.setProvider',
  'models.list',
  'ping',
  'status.get',
  'auth.getStatus',
];

// Every feature we claim in the README must have evidence in the code.
// Each entry: [feature name, array of strings where ANY must appear in client code]
const REQUIRED_FEATURES = [
  ['Per-session auth tokens',       ['authToken', 'auth_token', 'AuthToken', 'auth-token', 'gitHubToken', 'github_token', 'GitHubToken']],
  ['Session idle timeout',          ['idleTimeout', 'idle_timeout', 'IdleTimeout', 'sessionIdleTimeout', 'session_idle_timeout']],
  ['SessionFs provider',            ['sessionFs', 'session_fs', 'SessionFs', 'sessionfs', 'setProvider']],
  ['Commands registration',         ['command', 'Command', 'registerCommand', 'register_command', 'slashCommand', 'slash_command']],
  ['UI Elicitation',                ['elicitation', 'Elicitation', 'ui_elicitation', 'uiElicitation', 'askUser', 'ask_user']],
  ['System prompt customization',   ['systemMessage', 'system_message', 'SystemMessage', 'systemPrompt', 'system_prompt', 'append', 'replace', 'customize']],
  ['Per-agent skills',              ['skill', 'Skill', 'skills', 'Skills', 'preloadSkill', 'preload_skill']],
  ['Per-agent tool visibility',     ['excludedTools', 'excluded_tools', 'ExcludedTools', 'toolVisibility', 'tool_visibility']],
  ['Runtime request headers',       ['requestHeaders', 'request_headers', 'RequestHeaders', 'runtimeHeaders', 'runtime_headers', 'extraHeaders', 'extra_headers']],
  ['Model capabilities override',   ['modelCapabilities', 'model_capabilities', 'ModelCapabilities', 'capabilityOverride', 'capability_override']],
  ['Config discovery',              ['configDiscovery', 'config_discovery', 'ConfigDiscovery', 'autoDiscover', 'auto_discover', 'discoverConfig', 'discover_config']],
  ['Sub-agent streaming events',    ['subAgentStreaming', 'sub_agent_streaming', 'SubAgentStreaming', 'streamingForward', 'streaming_forward', 'agentStreaming', 'agent_streaming']],
  ['MCP server config',             ['mcp', 'Mcp', 'MCP', 'mcpServer', 'mcp_server', 'McpServer', 'stdioServer', 'stdio_server', 'httpServer', 'http_server']],
  ['Image generation options',      ['imageGeneration', 'image_generation', 'ImageGeneration', 'responseFormat', 'response_format', 'ResponseFormat', 'imageOption', 'image_option']],
  // --- Upstream-sync features (parity with @github/copilot-sdk, 2026-07) ---
  ['MCP OAuth token handler',       ['onMcpAuthRequest', 'on_mcp_auth_request', 'OnMcpAuthRequest', 'mcpAuthHandler', 'mcp_auth_handler', 'McpAuthHandler']],
  ['BYOK bearer token provider',    ['bearerTokenProvider', 'bearer_token_provider', 'BearerTokenProvider', 'getBearerToken', 'get_bearer_token']],
  ['Session citations',             ['enableCitations', 'enable_citations', 'EnableCitations']],
  ['Excluded built-in agents',      ['excludedBuiltinAgents', 'excluded_builtin_agents', 'ExcludedBuiltinAgents', 'excludedBuiltInAgents', 'ExcludedBuiltInAgents']],
  ['Session spending limits',       ['sessionLimits', 'session_limits', 'SessionLimits', 'maxAiCredits', 'max_ai_credits', 'MaxAiCredits']],
  ['OTLP telemetry protocol',       ['otlpProtocol', 'otlp_protocol', 'OtlpProtocol', 'OTLPProtocol']],
  ['WebSocket transport',           ['enableWebSocketResponses', 'enable_web_socket_responses', 'EnableWebSocketResponses', 'webSocket', 'web_socket', 'WebSocket', 'websocket']],
  ['Tool defer loading',            ['defer', 'Defer', 'ToolDefer', 'toolDefer']],
  ['System message preamble',       ['preamble', 'Preamble', 'PREAMBLE']],
  ['System message preserve',       ['preserve', 'Preserve', 'PRESERVE']],
  ['Session memory config',         ['MemoryConfiguration', 'memoryConfig', 'memory_config', 'MemoryConfig', 'enableMemory', 'enable_memory']],
  ['Post-tool-use hook',            ['onPostToolUse', 'on_post_tool_use', 'OnPostToolUse', 'postToolUse', 'post_tool_use', 'PostToolUse']],
  ['Pre-MCP-tool-call hook',        ['preMcpToolCall', 'pre_mcp_tool_call', 'PreMcpToolCall', 'onPreMcpToolCall', 'on_pre_mcp_tool_call']],
  ['Message agent mode',            ['agentMode', 'agent_mode', 'AgentMode']],
  ['Display prompt',                ['displayPrompt', 'display_prompt', 'DisplayPrompt']],
  ['HTTP request handler',          ['requestHandler', 'request_handler', 'RequestHandler', 'CopilotRequestHandler']],
  ['GitHub attachment variants',    ['GitHubCommit', 'github_commit', 'githubCommit', 'GitHubRepository', 'github_repository', 'GITHUB_COMMIT']],
  ['Experiment assignments',        ['expAssignments', 'exp_assignments', 'ExpAssignments']],
  // --- Upstream-sync features (parity with @github/copilot-sdk, 2026-08) ---
  ['Session rewind',                ['rewind', 'Rewind', 'REWIND']],
  ['Additional session directories',['additionalDirectories', 'additional_directories', 'AdditionalDirectories', 'additionalDirs', 'additional_dirs']],
  ['Disabled MCP servers',          ['disabledMcpServers', 'disabled_mcp_servers', 'DisabledMcpServers', 'disabledMCPServers', 'DisabledMCPServers']],
  ['GitHub MCP tool config',        ['githubMcpToolConfig', 'github_mcp_tool_config', 'GitHubMcpToolConfig', 'githubMCPToolConfig', 'GithubMcpToolConfig']],
  ['Canvas provider',               ['canvasProvider', 'canvas_provider', 'CanvasProvider']],
  ['Custom agents local-only',      ['customAgentsLocalOnly', 'custom_agents_local_only', 'CustomAgentsLocalOnly', 'customAgentsLocalonly']],
  ['User-prompt-transformed hook',  ['userPromptTransformed', 'user_prompt_transformed', 'UserPromptTransformed', 'onUserPromptTransformed', 'on_user_prompt_transformed']],
  ['Permission decision context',   ['decisionContext', 'decision_context', 'DecisionContext']],
  ['Built-in plugin directories',   ['builtinPluginDirectories', 'builtin_plugin_directories', 'BuiltinPluginDirectories', 'pluginDirectories', 'plugin_directories', 'PluginDirectories']],
  ['Agent factories authoring',     ['argsSchema', 'args_schema', 'ArgsSchema', 'FactoryExecute', 'factory_execute', 'defineFactory', 'define_factory', 'DefineFactory']],
  ['Reasoning effort control',      ['reasoningEffort', 'reasoning_effort', 'ReasoningEffort']],
  ['Tool search configuration',     ['toolSearch', 'tool_search', 'ToolSearch']],
  ['In-process FFI transport',      ['inProcess', 'in_process', 'InProcess', 'InProcessTransport', 'ffiTransport', 'ffi_transport']],
  ['Experimental mode',             ['experimentalMode', 'experimental_mode', 'ExperimentalMode', 'enableExperimental', 'enable_experimental']],
  ['Content exclusion',             ['contentExclusion', 'content_exclusion', 'ContentExclusion']],
  // --- Upstream-sync features (parity with @github/copilot-sdk, 2026-11 / v2.6.0) ---
  ['Client info handshake',         ['clientInfo', 'client_info', 'ClientInfo']],
  ['Auto routing tier',             ['switchAutoTier']],
  ['Ask-user variant',              ['askUserVariant', 'ask_user_variant', 'AskUserVariant']],
  ['GitHub token provider',         ['gitHubToken.getToken']],
  ['Sandbox bypass',                ['allowBypass', 'allow_bypass', 'AllowBypass']],
  ['Feature flag overrides',        ['featureFlags', 'feature_flags', 'FeatureFlags']],
  ['Session detach',                ['session.detach']],
  ['Message source',                ['MessageSource', 'message_source', 'messageSource']],
  ['Managed settings cache clear',  ['managedSettings.clearCache']],
  ['Policy helper recheck',         ['recheckPolicyHelper']],
  ['Model allowlist',               ['allowedModels', 'allowed_models', 'AllowedModels']],
  ['OAuth client metadata URL',     ['authClientIdMetadataUrl', 'auth_client_id_metadata_url', 'AuthClientIdMetadataUrl']],
  ['Max output tokens override',    ['max_output_tokens', 'maxOutputTokens', 'MaxOutputTokens']],
  ['Structured output',             ['responseSchema', 'response_schema', 'ResponseSchema']],
  ['Agent/hook directories',        ['hookDirectories', 'hook_directories', 'HookDirectories']],
  ['Working directory trust',       ['trustWorkingDirectory', 'trust_working_directory', 'TrustWorkingDirectory']],
  ['Transcript recovery',           ['allowTranscriptRecovery', 'allow_transcript_recovery', 'AllowTranscriptRecovery']],
  ['Image generation config',       ['saveToDisk', 'save_to_disk', 'SaveToDisk']],
  ['Autopilot options',             ['maxContinues', 'max_continues', 'MaxContinues']],
  ['Subagent lifecycle hooks',      ['subagentStart', 'subagent_start', 'SubagentStart']],
  ['BYOK API key command',          ['apiKeyCommand', 'api_key_command', 'ApiKeyCommand']],
  // --- SDK protocol 4 / upstream SDK 2.0 parity (Microsoft Copilot SDK, 2026-11 / v3.0.0) ---
  ['Grouped v4 session payload',    ['hasHandlers']],
  ['Resume event suppression',      ['suppressResumeEvent']],
  ['Named BYOK providers',          ['defaultProvider', 'default_provider', 'DefaultProvider']],
  ['Idempotent tools',              ['idempotent', 'Idempotent']],
  ['Client turn ID',                ['clientTurnId', 'client_turn_id', 'ClientTurnId']],
  ['Client mode',                   ['coding-agent']],
  ['Shell output events',           ['tool.shell_output']],
  ['Shell authentication',          ['shellAuthentication.request']],
  ['Memory store',                  ['memoryStore.store']],
  ['Network callbacks',             ['network.setProvider']],
  ['Policy helper failure',         ['policyHelperFailure', 'policy_helper_failure', 'PolicyHelperFailure']],
  ['Permission mode refusal reason', ['managed_policy']],
];

// Every SDK must speak this SDK protocol version (canonical source: sdk-protocol-version.json).
const REQUIRED_PROTOCOL_VERSION = JSON.parse(
  readFileSync(join(ROOT, 'sdk-protocol-version.json'), 'utf-8'),
).version;

// SDK directories and their client file glob patterns
const SDK_CLIENT_FILES = {
  java: 'java/src/main/java/com/github/copilot/CopilotClient.java',
  rust: 'rust/src/client.rs',
  ruby: 'ruby/lib/copilot/client.rb',
  php: 'php/src/CopilotClient.php',
  swift: 'swift/Sources/CopilotSDK/CopilotClient.swift',
  kotlin: 'kotlin/src/main/kotlin/com/github/copilot/CopilotClient.kt',
  cpp: ['cpp/src/client.cpp', 'cpp/include/copilot/client.h'],
  c: ['c/src/client.c', 'c/include/copilot/copilot.h'],
  dart: 'dart/lib/src/client.dart',
  scala: 'scala/src/main/scala/com/github/copilot/CopilotClient.scala',
  r: 'r/R/client.R',
  perl: 'perl/lib/GitHub/Copilot/Client.pm',
  lua: 'lua/copilot/client.lua',
  shell: 'shell/lib/client.sh',
  elixir: 'elixir/lib/copilot/client.ex',
  haskell: 'haskell/src/Copilot/Client.hs',
  clojure: 'clojure/src/copilot/client.clj',
  visualbasic: 'visualbasic/src/CopilotClient.vb',
  delphi: 'delphi/src/Copilot.Client.pas',
  fortran: 'fortran/src/copilot_client.f90',
  matlab: 'matlab/+copilot/CopilotClient.m',
  ada: ['ada/src/copilot-client.adb', 'ada/src/copilot-client.ads'],
  objc: ['objc/src/CPCopilotClient.m', 'objc/src/CPCopilotClient.h'],
  fsharp: 'fsharp/src/Client.fs',
  groovy: 'groovy/src/main/groovy/com/github/copilot/CopilotClient.groovy',
  julia: 'julia/src/client.jl',
  cobol: 'cobol/src/COPILOT-CLIENT.cob',
  ocaml: 'ocaml/lib/client.ml',
  zig: 'zig/src/client.zig',
  nim: 'nim/src/copilot_sdk/client.nim',
  dlang: 'dlang/source/copilot/client.d',
  erlang: 'erlang/src/copilot_client.erl',
  crystal: 'crystal/src/copilot_sdk/client.cr',
  tcl: 'tcl/lib/copilot/client.tcl',
  solidity: 'solidity/src/client.ts',
  vlang: 'vlang/src/client.v',
};

function readClientFiles(sdk) {
  const paths = SDK_CLIENT_FILES[sdk];
  const files = Array.isArray(paths) ? paths : [paths];
  let combined = '';
  for (const f of files) {
    try {
      combined += readFileSync(join(ROOT, f), 'utf-8') + '\n';
    } catch {
      // File may not exist yet
    }
  }
  return combined;
}

function checkSdk(sdk) {
  const content = readClientFiles(sdk);
  if (!content.trim()) {
    return { sdk, missing: ['CLIENT FILE NOT FOUND'], found: [] };
  }

  const missing = [];
  const found = [];
  for (const method of REQUIRED_RPC_METHODS) {
    // Check for the RPC method string in any common format
    // e.g. "session.create", 'session.create', session\.create
    if (content.includes(method)) {
      found.push(method);
    } else {
      missing.push(method);
    }
  }
  return { sdk, missing, found };
}

function readSdkSources(sdk) {
  const content = readClientFiles(sdk);
  if (!content.trim()) return { content, allContent: '', files: [] };

  // Also read config/types/session files if they exist
  let allContent = content;
  const files = [];
  const sdkDir = sdk === 'dlang' ? 'dlang' : sdk;
  try {
    const dir = join(ROOT, sdkDir);
    // Deep enough to reach JVM-style package layouts (e.g. src/main/java/com/github/copilot).
    function scanDir(d, depth = 0) {
      if (depth > 10) return;
      try {
        for (const entry of readdirSync(d)) {
          const full = join(d, entry);
          // Skip VCS, test, and build/vendor output dirs (avoids false positives from compiled/vendored code).
          if (SKIP_SCAN_DIRS.has(entry)) continue;
          try {
            if (statSync(full).isDirectory()) {
              scanDir(full, depth + 1);
            } else if (/\.(ts|js|py|go|rs|rb|php|swift|kt|java|cpp|hpp|hh|cc|cxx|h|c|dart|scala|ex|hs|fs|lua|sh|clj|vb|pas|f90|m|mm|adb|ads|groovy|jl|cob|cpy|ml|zig|nim|d|erl|hrl|cr|tcl|sol|v|R|pm|pl)$/.test(entry)) {
              const text = readFileSync(full, 'utf-8');
              allContent += text + '\n';
              files.push({ path: full, text });
            }
          } catch {}
        }
      } catch {}
    }
    scanDir(dir);
  } catch {}
  return { content, allContent, files };
}

function checkSdkFeatures(sdk) {
  const { content, allContent } = readSdkSources(sdk);
  if (!content.trim()) {
    return { sdk, missing: REQUIRED_FEATURES.map(f => f[0]), found: [] };
  }

  const missing = [];
  const found = [];
  for (const [name, patterns] of REQUIRED_FEATURES) {
    if (patterns.some(p => allContent.includes(p))) {
      found.push(name);
    } else {
      missing.push(name);
    }
  }
  return { sdk, missing, found };
}

// Finds SDK protocol version constants across languages, for example
// `SDK_PROTOCOL_VERSION = 4`, `#define COPILOT_SDK_PROTOCOL_VERSION 4`,
// `-define(SDK_PROTOCOL_VERSION, 4).`, `WS-PROTO-MAJOR PIC 9(2) VALUE 4.`,
// `Public Const Version As String = "4"` inside SdkProtocolVersion.vb, and
// MATLAB-style `function v = sdkProtocolVersion()` followed by `v = 4;`.
const PROTOCOL_NAME = /protocol[\s_-]*version|proto[\s_-]*major/i;
const PROTOCOL_FILE = /protocol[_-]?version/i;
const TEST_FILE = /(^|[\\/])(spec|specs)[\\/]|(_test|_spec|Tests?|Spec)\.[A-Za-z]+$/;
const COMMENT_LINE = /^\s*(\/\/|\/\*|\*|--|%|;|!|'|\(\*|\{-|rem\b|#(?!\s*define\b))/i;
const TRAILING_INT = /(?:=|:=|=>|<-|::|,|\bvalue\b|\s)\s*["']?(\d+)["']?L?\s*[;.)\]]*\s*(?:(?:#|\/\/|--|%|!|').*)?$/i;

function checkProtocolVersion(sdk) {
  const { files } = readSdkSources(sdk);
  const values = [];
  for (const { path, text } of files) {
    if (TEST_FILE.test(path)) continue;
    const protocolFile = PROTOCOL_FILE.test(path.split(/[\\/]/).pop());
    const lines = text.split(/\r?\n/);
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (COMMENT_LINE.test(line) || (!protocolFile && !PROTOCOL_NAME.test(line))) continue;
      if (/strict_types|copyright|license|@since|@version/i.test(line)) continue;
      let match = line.match(TRAILING_INT);
      if (!match && /\bfunction\b/i.test(line)) {
        const next = lines.slice(i + 1).find((l) => l.trim() && !COMMENT_LINE.test(l));
        match = next?.match(/=\s*(\d+)\s*;?\s*$/);
      }
      // Zero is a field default (e.g. `int protocol_version = 0`), never a protocol constant.
      if (match && Number(match[1]) !== 0) {
        values.push({ value: Number(match[1]), where: `${path.slice(ROOT.length + 1)}:${i + 1}` });
      }
    }
  }
  const wrong = values.filter((v) => v.value !== REQUIRED_PROTOCOL_VERSION);
  return { sdk, ok: values.length > 0 && wrong.length === 0, values, wrong };
}

// Run the check
console.log('🔍 SDK API Coverage Check\n');

console.log(`Required RPC methods (${REQUIRED_RPC_METHODS.length}):`);
REQUIRED_RPC_METHODS.forEach(m => console.log(`  ✓ ${m}`));
console.log(`\nRequired features (${REQUIRED_FEATURES.length}):`);
REQUIRED_FEATURES.forEach(([name]) => console.log(`  ✓ ${name}`));
console.log('');

let rpcFailures = 0;
let featureFailures = 0;
let protocolFailures = 0;
const rpcResults = [];
const featureResults = [];
const protocolResults = [];

for (const sdk of Object.keys(SDK_CLIENT_FILES)) {
  const rpc = checkSdk(sdk);
  rpcResults.push(rpc);
  if (rpc.missing.length > 0) rpcFailures++;

  const feat = checkSdkFeatures(sdk);
  featureResults.push(feat);
  if (feat.missing.length > 0) featureFailures++;

  const proto = checkProtocolVersion(sdk);
  protocolResults.push(proto);
  if (!proto.ok) protocolFailures++;
}

// Print RPC results
console.log('━'.repeat(70));
console.log('RPC METHOD COVERAGE');
console.log('━'.repeat(70));
console.log(`${'SDK'.padEnd(15)} ${'Coverage'.padEnd(10)} Missing`);
console.log('─'.repeat(70));

for (const r of rpcResults) {
  const coverage = `${r.found.length}/${REQUIRED_RPC_METHODS.length}`;
  const status = r.missing.length === 0 ? '✅' : '❌';
  const missingStr = r.missing.length === 0 ? '' : r.missing.join(', ');
  console.log(`${status} ${r.sdk.padEnd(13)} ${coverage.padEnd(10)} ${missingStr}`);
}

// Print feature results
console.log('\n' + '━'.repeat(70));
console.log('FEATURE COVERAGE');
console.log('━'.repeat(70));
console.log(`${'SDK'.padEnd(15)} ${'Coverage'.padEnd(10)} Missing`);
console.log('─'.repeat(70));

for (const r of featureResults) {
  const coverage = `${r.found.length}/${REQUIRED_FEATURES.length}`;
  const status = r.missing.length === 0 ? '✅' : '❌';
  const missingStr = r.missing.length === 0 ? '' : r.missing.join(', ');
  console.log(`${status} ${r.sdk.padEnd(13)} ${coverage.padEnd(10)} ${missingStr}`);
}

// Print protocol results
console.log('\n' + '━'.repeat(70));
console.log(`SDK PROTOCOL VERSION (required: ${REQUIRED_PROTOCOL_VERSION})`);
console.log('━'.repeat(70));
for (const r of protocolResults) {
  const status = r.ok ? '✅' : '❌';
  const detail = r.values.length === 0
    ? 'no protocol version constant found'
    : r.ok
      ? `${r.values.length} constant(s) = ${REQUIRED_PROTOCOL_VERSION}`
      : r.wrong.map((w) => `${w.value} @ ${w.where}`).join(', ');
  console.log(`${status} ${r.sdk.padEnd(13)} ${detail}`);
}

// Summary
const totalSdks = Object.keys(SDK_CLIENT_FILES).length;
const totalFailures = new Set([
  ...rpcResults.filter(r => r.missing.length > 0).map(r => r.sdk),
  ...featureResults.filter(r => r.missing.length > 0).map(r => r.sdk),
  ...protocolResults.filter(r => !r.ok).map(r => r.sdk),
]).size;

console.log('\n' + '━'.repeat(70));
console.log(`RPC:      ${totalSdks - rpcFailures}/${totalSdks} SDKs fully covered`);
console.log(`Features: ${totalSdks - featureFailures}/${totalSdks} SDKs fully covered`);
console.log(`Protocol: ${totalSdks - protocolFailures}/${totalSdks} SDKs on protocol ${REQUIRED_PROTOCOL_VERSION}`);
console.log(`Overall:  ${totalSdks - totalFailures}/${totalSdks} SDKs pass all checks`);
console.log('━'.repeat(70));

if (totalFailures > 0) {
  console.error(`\n❌ ${totalFailures} SDK(s) have coverage gaps. Fix before merging.`);
  process.exit(1);
} else {
  console.log('\n✅ All SDKs have full API + feature coverage!');
  process.exit(0);
}
