## 2.5.3
- Maintenance, security, and documentation release. Fixed the Python SDK's
  PyPI package name (now `copilot-sdk-supercharged`), resolved a High-severity
  ReDoS in the Python code generator, hardened the npm/PyPI/pub.dev publish
  workflows (OIDC trusted publishing + workflow_dispatch), and backfilled
  "Recent Features (v2.4–v2.5)" documentation across all 40 SDK READMEs.
  No public Dart API changes.

## 2.5.2
- Upstream sync: bundled CLI runtime pinned to the latest published
  @github/copilot (^1.0.80); upstream's 1.0.81-6 is not yet on public npm.
  Added the `user_setting` variant to the PermissionModeSource and
  PermissionsSetApproveAllSource enums (core generated SDKs), made
  AccountLoginRequest.login optional, and normalized interrupted shell replay
  results in the test harness. No public Dart API changes.

## 2.5.1
- Maintenance release: hardened the Lua, Perl, and Scala SDK end-to-end test
  suites. Perl now implements the sessionFs provider callbacks; Scala uses a
  byte-framed JSON-RPC transport and fixes a ping deadlock; Lua fixes
  process-timeout handling. No public API changes.

## 2.5.0
- Upstream sync: 210 commits from @github/copilot-sdk
- New features ported to all 40 SDKs: session rewind, additional session
  directories, disabled MCP servers, GitHub MCP tool config, canvas provider,
  custom-agents local-only, userPromptTransformed hook, permission decision
  context, built-in plugin directories, agent factory argsSchema,
  reasoningEffort: max, tool search config, in-process (FFI) transport,
  experimental mode, and content exclusion
- All 40 SDKs fully tested

## 2.4.0
- Upstream sync: 369 commits from @github/copilot-sdk (through java/v1.0.5-01)
- Feature parity across all 40 SDKs
- All 40 SDKs fully tested

## 2.3.0
- Upstream sync with @github/copilot 1.0.47
- Canvas, cloud sessions, experimental schema types support
- PingResponse.timestamp type fix (ISO 8601 string)
- All 40 SDKs fully tested

## 2.0.3
- Sync with upstream @github/copilot 1.0.39
- All 40 SDKs included in release

## 2.0.0
- Initial release with full v2.0 feature parity
- JSON-RPC 2.0 client over stdio
- Session management, tools, streaming, and event handling

