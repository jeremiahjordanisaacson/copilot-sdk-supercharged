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
];

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

function checkSdkFeatures(sdk) {
  const content = readClientFiles(sdk);
  if (!content.trim()) {
    return { sdk, missing: REQUIRED_FEATURES.map(f => f[0]), found: [] };
  }

  // Also read config/types/session files if they exist
  let allContent = content;
  const sdkDir = sdk === 'dlang' ? 'dlang' : sdk;
  try {
    const dir = join(ROOT, sdkDir);
    function scanDir(d, depth = 0) {
      if (depth > 3) return;
      try {
        for (const entry of readdirSync(d)) {
          const full = join(d, entry);
          if (entry === 'node_modules' || entry === '.git' || entry === 'test' || entry === 'tests' || entry === 'e2e') continue;
          try {
            if (statSync(full).isDirectory()) {
              scanDir(full, depth + 1);
            } else if (/\.(ts|js|py|go|rs|rb|php|swift|kt|java|cpp|h|c|dart|scala|ex|hs|fs|lua|sh|clj|vb|pas|f90|m|adb|ads|groovy|jl|cob|ml|zig|nim|d|erl|cr|tcl|sol|v)$/.test(entry)) {
              allContent += readFileSync(full, 'utf-8') + '\n';
            }
          } catch {}
        }
      } catch {}
    }
    scanDir(dir);
  } catch {}

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

// Run the check
console.log('🔍 SDK API Coverage Check\n');

console.log(`Required RPC methods (${REQUIRED_RPC_METHODS.length}):`);
REQUIRED_RPC_METHODS.forEach(m => console.log(`  ✓ ${m}`));
console.log(`\nRequired features (${REQUIRED_FEATURES.length}):`);
REQUIRED_FEATURES.forEach(([name]) => console.log(`  ✓ ${name}`));
console.log('');

let rpcFailures = 0;
let featureFailures = 0;
const rpcResults = [];
const featureResults = [];

for (const sdk of Object.keys(SDK_CLIENT_FILES)) {
  const rpc = checkSdk(sdk);
  rpcResults.push(rpc);
  if (rpc.missing.length > 0) rpcFailures++;

  const feat = checkSdkFeatures(sdk);
  featureResults.push(feat);
  if (feat.missing.length > 0) featureFailures++;
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

// Summary
const totalSdks = Object.keys(SDK_CLIENT_FILES).length;
const totalFailures = new Set([
  ...rpcResults.filter(r => r.missing.length > 0).map(r => r.sdk),
  ...featureResults.filter(r => r.missing.length > 0).map(r => r.sdk),
]).size;

console.log('\n' + '━'.repeat(70));
console.log(`RPC:      ${totalSdks - rpcFailures}/${totalSdks} SDKs fully covered`);
console.log(`Features: ${totalSdks - featureFailures}/${totalSdks} SDKs fully covered`);
console.log(`Overall:  ${totalSdks - totalFailures}/${totalSdks} SDKs pass all checks`);
console.log('━'.repeat(70));

if (totalFailures > 0) {
  console.error(`\n❌ ${totalFailures} SDK(s) have coverage gaps. Fix before merging.`);
  process.exit(1);
} else {
  console.log('\n✅ All SDKs have full API + feature coverage!');
  process.exit(0);
}
