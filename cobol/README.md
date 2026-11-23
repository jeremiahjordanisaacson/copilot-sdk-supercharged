# Copilot Supercharged SDK for COBOL

COBOL SDK for the GitHub Copilot CLI. Communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio pipes using Content-Length header framing. Built for GnuCOBOL (the free, open-source COBOL compiler).

COBOL developers deserve nice things too.

## Requirements

- **GnuCOBOL** 3.0 or later (`cobc` compiler)
- **Make** (GNU Make recommended)
- **GitHub Copilot CLI** 1.0.113 or newer installed and available on PATH (or set `WS-CLI-PATH` or the `COPILOT_CLI_PATH` environment variable)

### Installing GnuCOBOL

**Debian/Ubuntu:**
```bash
sudo apt install gnucobol
```

**macOS (Homebrew):**
```bash
brew install gnucobol
```

**Fedora/RHEL:**
```bash
sudo dnf install gnucobol
```

**Windows (MSYS2):**
```bash
pacman -S mingw-w64-x86_64-gnucobol
```

## Building

```bash
cd cobol
make all
```

To build only the SDK modules:
```bash
make lib
```

To syntax-check without linking:
```bash
make check
```

## Quick Start

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-COPILOT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "COPILOT-TYPES.cpy".
       COPY "COPILOT-VERSION.cpy".

       PROCEDURE DIVISION.
       MAIN-PARA.
      *    Initialize the client (auto-starts the CLI)
           MOVE 1 TO WS-AUTO-START
           CALL "COPILOT-CLIENT-INIT"
               USING WS-CLIENT-OPTIONS WS-RETURN-CODE

      *    Create a session
           MOVE "my-cobol-app" TO WS-SESS-AGENT-NAME
           MOVE "1.0.0" TO WS-SESS-AGENT-VER
           CALL "COPILOT-SESSION" USING WS-SESSION-CONFIG
               WS-SESSION-STATE WS-RETURN-CODE

      *    Send a message
           MOVE "What is 2 + 2?" TO WS-SEND-MESSAGE
           CALL "COPILOT-SESSION" USING WS-SEND-OPTIONS
               WS-SEND-RESULT WS-RETURN-CODE

      *    Display the response
           DISPLAY "Response: "
               FUNCTION TRIM(WS-RESULT-MESSAGE)

      *    Clean up
           CALL "COPILOT-SESSION" USING WS-SESSION-STATE
               WS-RETURN-CODE
           CALL "COPILOT-CLIENT-STOP" USING WS-RETURN-CODE
           STOP RUN
           .
```

## What's New in v3.0.0

The COBOL SDK now speaks **SDK protocol 4** (the Microsoft Copilot SDK 2.0
protocol) and requires **Copilot CLI 1.0.113 or newer**. Starting the client
runs the `connect` handshake (falling back to `ping`) and rejects any other
protocol version: `WS-RETURN-CODE` is -4 and `WS-LAST-ERROR` holds
`SDK protocol version mismatch: SDK supports version 4, but server reports version N. Please update your SDK or server to ensure compatibility.`

Existing options keep working: you set the same flat working-storage items as
before. `BUILD-SESSION-PARAMS` (procedure copybook `COPILOT-PARAMS.cpy`, the
protocol 4 translation layer) writes them as the grouped payload
(`inference`, `tools`, `permissions`, `events`, `agents`, `github`, ...) for
both `session.create` (`CREATE-SESSION`) and `session.resume`
(`RESUME-SESSION`). Options without a protocol 4 group (for example
`otlpProtocol` or `rewindEnabled`) stay at the top level, and
`trustWorkingDirectory` is only sent on create. The agent name and version
(`WS-SESS-AGENT-NAME`, `WS-SESS-AGENT-VER`) are now sent as `clientName`,
because protocol 4 rejects the old `agent` identity object. Streaming
(`WS-SESS-STREAMING`) is sent as `events.streaming`.

Session options live in `COPILOT-SESSION-OPTS.cpy`; client options, callback
records and constants in `COPILOT-TYPES.cpy`. Tri-state flags hold `"Y"`,
`"N"` or SPACE (not sent) and have 88-level names.

### Client options

```cobol
      *    Client info handshake (clientInfo in the connect params)
           MOVE "MyEditor" TO WS-CI-EDITOR-NAME
           MOVE "1.2.0" TO WS-CI-EDITOR-VERSION
           MOVE "my-extension" TO WS-CI-EXTENSION-NAME
           MOVE "0.1.0" TO WS-CI-EXTENSION-VER
      *    Client mode: "coding-agent" (default) or "empty". Empty mode
      *    requires WS-AVAILABLE-TOOLS on every session and turns
      *    ambient features off (telemetry, embeddings, memory, ...).
           SET CLIENT-MODE-EMPTY TO TRUE
      *    Network callbacks, registered with network.setProvider
           SET NET-SCOPE-CONNECTION TO TRUE
           MOVE "MY-PROXY-RESOLVER" TO WS-NET-PROXY-PGM
           CALL "COPILOT-CLIENT-INIT" USING WS-CLIENT-OPTIONS
               WS-RETURN-CODE

      *    Clear cached enterprise managed settings
      *    (managedSettings.clearCache); WS-RPC-RESPONSE is PIC X(8192)
           CALL "COPILOT-CLEAR-SETTINGS-CACHE" USING WS-RPC-RESPONSE
               WS-RETURN-CODE
```

### Session options

```cobol
           MOVE '["view","grep"]' TO WS-AVAILABLE-TOOLS
           MOVE "elicitation" TO WS-ASK-USER-VARIANT
           SET SANDBOX-ON TO TRUE
           SET SANDBOX-BYPASS-ON TO TRUE
           MOVE '{"my-flag":true}' TO WS-FEATURE-FLAG-OVERRIDES
           MOVE '["claude-sonnet-4.5"]' TO WS-ALLOWED-MODELS
           MOVE "https://example.com/client.json"
               TO WS-AUTH-CLIENT-MD-URL
      *    capabilities.limits.max_output_tokens
           MOVE 4096 TO WS-MAX-OUTPUT-TOKENS
           MOVE '["/repo/.agents"]' TO WS-AGENT-DIRECTORIES
           MOVE '["/repo/.hooks"]' TO WS-HOOK-DIRECTORIES
           MOVE "/repo" TO WS-SESS-WORKING-DIR
      *    trustWorkingDirectory: "session" or "user" (create only)
           SET TRUST-WD-SESSION TO TRUE
           SET IMAGE-GEN-ON TO TRUE
           SET IMAGE-GEN-SAVE-TO-DISK TO TRUE
           SET AUTOPILOT-CONTINUE-ON TO TRUE
           MOVE 5 TO WS-AUTOPILOT-MAX-CONT
           PERFORM CREATE-SESSION THRU CREATE-SESSION-EXIT
```

### Named BYOK providers

```cobol
           MOVE 1 TO WS-PROVIDER-COUNT
           MOVE "local" TO WS-NP-NAME(1)
           MOVE "openai" TO WS-NP-TYPE(1)
           MOVE "http://localhost:11434/v1" TO WS-NP-BASE-URL(1)
      *    apiKeyCommand: the command's output is the API key
           MOVE "cat /keys/local.txt" TO WS-NP-API-KEY-CMD(1)
           MOVE 1 TO WS-PROVIDER-MODEL-COUNT
           MOVE "local" TO WS-PM-PROVIDER(1)
           MOVE "llama3" TO WS-PM-ID(1)
           MOVE 2048 TO WS-PM-MAX-OUTPUT(1)
           MOVE "local" TO WS-DEFAULT-PROVIDER
           MOVE "local/llama3" TO WS-SESS-MODEL
      *    github.capi enabled false: BYOK models only
           SET CAPI-MODELS-OFF TO TRUE
```

Up to 4 providers (`WS-PROVIDER-ENTRY`) and 8 models (`WS-PROVIDER-MODEL`)
are sent as `inference.providers` and `inference.models`.

### Idempotent tools

A tool definition (`WS-TOOL-DEF`) is sent in `tools.definitions` once a
handler program is named; `external_tool.requested` events CALL it with
`WS-TOOL-INVOCATION` and `WS-TOOL-RESULT`, and the result is returned with
`session.tools.handlePendingToolCall`.

```cobol
           MOVE "lookup" TO WS-TOOL-NAME
           MOVE "Read-only lookup" TO WS-TOOL-DESCRIPTION
           MOVE '{"type":"object","properties":{}}' TO WS-TOOL-SCHEMA
           SET TOOL-IDEMPOTENT-YES TO TRUE
           MOVE "MY-LOOKUP-TOOL" TO WS-TOOL-HANDLER-PGM
```

### Message options

`SEND-MESSAGE` uses `session.send` (the text goes out as `prompt`) and reads
`session.event` notifications until `session.idle`.

```cobol
           MOVE "Summarize the diff" TO WS-SEND-MESSAGE
      *    MessageSource: "user", "system" or "agent-<id>"
           MOVE "agent-reviewer" TO WS-SEND-SOURCE
           MOVE "turn-42" TO WS-SEND-CLIENT-TURN-ID
      *    responseSchema: sent as a json_schema responseFormat
           MOVE '{"type":"object"}' TO WS-SEND-RESPONSE-SCHEMA
           PERFORM SEND-MESSAGE THRU SEND-MESSAGE-EXIT
```

### Session operations

```cobol
      *    session.model.switchAutoTier (model "auto" only)
           SET AUTO-TIER-BALANCE TO TRUE
           PERFORM SWITCH-AUTO-TIER
      *    session.permissions.setMode: "manual", "assisted" or
      *    "allow-all"
           MOVE "allow-all" TO WS-PERMISSION-MODE
           PERFORM SET-PERMISSION-MODE
           IF PMR-MANAGED-POLICY
               DISPLAY "Refused by enterprise policy (managed_policy)"
           END-IF
      *    session.managedSettings.recheckPolicyHelper
           PERFORM RECHECK-POLICY-HELPER
      *    session.detach (the session stays on disk)
           PERFORM DETACH-SESSION
```

### Resume options

```cobol
           MOVE WS-SAVED-SESSION-ID TO WS-SESSION-ID
      *    suppressResumeEvent, allowTranscriptRecovery and
      *    continuePendingWork are resume-only root keys
           SET RESUME-EVENT-SUPPRESSED TO TRUE
           SET RESUME-TRANSCRIPT-RECOVERY TO TRUE
           SET RESUME-CONTINUE-PENDING TO TRUE
      *    Send defaultProvider null to clear it
           SET CLEAR-DEFAULT-PROVIDER TO TRUE
           PERFORM RESUME-SESSION THRU RESUME-SESSION-EXIT
```

### Callback programs

Name a CALLable program in the option; `COPILOT-SESSION` CALLs it when the
runtime sends the request and writes the JSON-RPC reply. Unknown requests get
a "Method not found" error.

| Option | Runtime request (and what the SDK sends) | Program receives |
|---|---|---|
| `WS-GH-TOKEN-PGM` | `gitHubToken.getToken` (`github.tokenProviderRegistrationId`, a generated UUID) | `WS-GITHUB-TOKEN-REQ`, `WS-GITHUB-TOKEN-RESP` |
| `WS-SHELL-AUTH-PGM` | `shellAuthentication.request` (`interaction.requestShellAuthentication`) | `WS-SHELL-AUTH-REQ`, `WS-SHELL-AUTH-RESP` |
| `WS-MEMORY-STORE-PGM` | `memoryStore.store` (`state.hasMemoryStore`) | `WS-STORED-MEMORY` |
| `WS-SUBAGENT-START-PGM`, `WS-SUBAGENT-STOP-PGM` | `hooks.invoke` for `subagentStart` / `subagentStop` (`hooks.hasHandlers`) | `WS-SUBAGENT-HOOK-INPUT`, `WS-SUBAGENT-HOOK-OUTPUT` |
| `WS-NET-PROXY-PGM`, `WS-NET-NEGOTIATE-PGM`, `WS-NET-BASIC-PGM` | `network.resolveProxy`, `network.authenticateNegotiate`, `network.authenticateBasic` (`network.setProvider`) | `WS-NETWORK-REQUEST`, `WS-NETWORK-RESPONSE` |
| `WS-TOOL-HANDLER-PGM` | `external_tool.requested` events | `WS-TOOL-INVOCATION`, `WS-TOOL-RESULT` |
| `WS-SHELL-OUTPUT-PGM` | `tool.shell_output` events | `WS-SHELL-OUTPUT-EVENT` |

The GitHub token provider is mutually exclusive with a static token
(`WS-AUTH-TOKEN` / `WS-SESS-AUTH-TOKEN`) and its registration is released by
`DETACH-SESSION`. A callback program can copy the SDK records into its
linkage section:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-TOKEN-PROVIDER.
       DATA DIVISION.
       LINKAGE SECTION.
       COPY "COPILOT-TYPES.cpy".
       PROCEDURE DIVISION USING WS-GITHUB-TOKEN-REQ
           WS-GITHUB-TOKEN-RESP.
           ACCEPT WS-GTP-ACCESS-TOKEN FROM ENVIRONMENT "GH_TOKEN"
           MOVE "Bearer" TO WS-GTP-TOKEN-TYPE
           MOVE 3600 TO WS-GTP-EXPIRES-IN
           GOBACK.
```

### Errors

JSON-RPC errors (including `session.delete` and `session.setForeground`
failures) set `WS-RETURN-CODE` to -1 with the message in `WS-LAST-ERROR`.
When `session.create` or `session.resume` fails because of the enterprise
Policy Helper, `WS-POLICY-HELPER-FAILURE` holds the `policyHelperFailure`
details (`PHF-PRESENT`, `WS-PHF-CATEGORY`, `WS-PHF-FAILURE-MODE`,
`WS-PHF-MESSAGE`, `PHF-PREVIOUS-RETAINED`, ...).

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*`) is not
  available: it is an experimental upstream surface that depends on the
  runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection: the COBOL
  SDK spawns the Copilot CLI over stdio.
- JSON is built and read with `STRING` and character scans over fixed-size
  fields: messages are limited to 8192 bytes, option fields to their `PIC`
  sizes, and `\uXXXX` escapes in received strings are kept as written.
- Callbacks are answered synchronously while messages are read, so
  `shellAuthentication.cancel` is acknowledged without further action.
- One tool definition (`WS-TOOL-DEF`) per session.

## Architecture

```
Your COBOL Program
    |
    | CALL "COPILOT-CLIENT-INIT"
    | CALL "COPILOT-SESSION"
    v
+---------------------+
| COPILOT-SESSION.cob |  Session lifecycle, message sending, protocol 4
+---------------------+  payloads (COPILOT-PARAMS.cpy), events, callbacks
    |
    | CALL "COPILOT-CLIENT-WRITE" / "COPILOT-CLIENT-READ"
    v
+---------------------+
| COPILOT-CLIENT.cob  |  CLI process, pipe I/O, framing, handshake
+---------------------+
    |
    | stdio pipes (Content-Length framing)
    v
+---------------------+
| Copilot CLI Server  |  JSON-RPC server (copilot --headless --stdio)
+---------------------+
```

`COPILOT-JSONRPC.cob` provides standalone JSON-RPC message build and parse
helpers.

## Module Reference

### COPILOT-CLIENT.cob

Main client module managing CLI process lifecycle and I/O.

| Entry Point | Parameters | Description |
|---|---|---|
| `COPILOT-CLIENT-INIT` | `WS-CLIENT-OPTIONS`, `WS-RETURN-CODE` | Initialize client with options |
| `COPILOT-CLIENT-START` | `WS-RETURN-CODE` | Start the CLI process and run the protocol 4 handshake |
| `COPILOT-CLIENT-STOP` | `WS-RETURN-CODE` | Stop the CLI process |
| `COPILOT-CLIENT-PING` | `WS-RETURN-CODE` | Health check (0 = healthy) |
| `COPILOT-CLIENT` | request, response, rc | Send/receive JSON-RPC |
| `COPILOT-CLIENT-WRITE` | request, rc | Write one JSON-RPC message (requests, callback replies) |
| `COPILOT-CLIENT-READ` | response, rc | Read the next JSON-RPC message |
| `COPILOT-CLEAR-SETTINGS-CACHE` | response, rc | Clear cached managed settings (`managedSettings.clearCache`) |

### COPILOT-SESSION.cob

Session management for conversations.

| Paragraph | Description |
|---|---|
| `CREATE-SESSION` | Create a new Copilot session (`session.create`, protocol 4 grouped payload) |
| `RESUME-SESSION` | Resume `WS-SESSION-ID` (`session.resume`, protocol 4 grouped payload) |
| `SEND-MESSAGE` | Send a message (`session.send`) and read events until `session.idle` |
| `DESTROY-SESSION` / `DETACH-SESSION` | Detach from the session (`session.detach`) |
| `SWITCH-AUTO-TIER` | Request an Auto routing tier (`session.model.switchAutoTier`) |
| `RECHECK-POLICY-HELPER` | Re-run the Policy Helper (`session.managedSettings.recheckPolicyHelper`) |
| `SET-PERMISSION-MODE` | Change the permission mode (`session.permissions.setMode`) |

### COPILOT-JSONRPC.cob

JSON-RPC 2.0 message construction and parsing.

| Paragraph | Description |
|---|---|
| `BUILD-REQUEST` | Build a JSON-RPC request with id |
| `BUILD-NOTIFICATION` | Build a JSON-RPC notification (no id) |
| `BUILD-CONTENT-FRAME` | Wrap message in Content-Length framing |
| `PARSE-RESPONSE` | Parse a JSON-RPC response |

### Copybooks

| File | Description |
|---|---|
| `COPILOT-TYPES.cpy` | Record layouts for all data structures (client options, session state, send options, results, tools, events, framing, protocol 4 callback records) |
| `COPILOT-VERSION.cpy` | Protocol version constants (SDK protocol 4) and SDK identity |
| `COPILOT-SESSION-OPTS.cpy` | Session options (`WS-UPSTREAM-CONFIG`, `WS-V4-SESSION-OPTIONS`) and builder work fields |
| `COPILOT-PARAMS.cpy` | Procedure copybook with `BUILD-SESSION-PARAMS`, the protocol 4 translation layer |

## Configuration Options

Set fields in `WS-CLIENT-OPTIONS` before calling `COPILOT-CLIENT-INIT`:

| Field | PIC | Default | Description |
|---|---|---|---|
| `WS-CLI-PATH` | X(256) | SPACES | Path to the Copilot CLI binary (`COPILOT_CLI_PATH` when blank; `.js` paths run through `node`) |
| `WS-CLI-URL` | X(256) | SPACES | URL of external CLI server |
| `WS-AUTO-START` | 9 | 1 | Auto-start CLI on init (1=yes, 0=no) |
| `WS-GITHUB-TOKEN` | X(256) | SPACES | GitHub authentication token |

`WS-CLIENT-MODE` (`CLIENT-MODE-CODING-AGENT`, `CLIENT-MODE-EMPTY`),
`WS-CLIENT-INFO` and `WS-NETWORK-OPTIONS` are separate client records in
`COPILOT-TYPES.cpy` (see What's New in v3.0.0).

## Session Configuration

Set fields in `WS-SESSION-CONFIG` before creating a session:

| Field | PIC | Default | Description |
|---|---|---|---|
| `WS-SESS-AGENT-NAME` | X(64) | SPACES | Your agent/app name |
| `WS-SESS-AGENT-VER` | X(16) | SPACES | Your agent/app version |
| `WS-SESS-SYS-PROMPT` | X(1024) | SPACES | System prompt for the assistant |
| `WS-SESS-STREAMING` | 9 | 0 | Enable streaming (1=on, 0=off) |
| `WS-SESS-HISTORY-LEN` | 9(5) | 100 | Conversation history length |

## Recent Features (v2.4–v2.5)

The session module (`COPILOT-SESSION.cob`) emits camelCase wire keys for the options
added in the v2.4 and v2.5 upstream syncs. Populate the corresponding working-storage
items (`WS-UPSTREAM-CONFIG`, `WS-UPSTREAM-HANDLERS`, `WS-SEND-OPTIONS-EXT`) before
creating a session; `BUILD-CREATE-PARAMS` includes any that are set. Since v3.0.0
the session keys below are placed in their SDK protocol 4 groups (for example
`reasoningEffort` in `inference`); see What's New in v3.0.0.

### v2.5 session options

| Field | Toggle / PIC | Wire key |
|---|---|---|
| `WS-REASONING-EFFORT` | X(16) | `reasoningEffort` |
| `WS-TOOL-SEARCH` | X(1024) | `toolSearch` |
| `WS-REWIND-ENABLED` | `SET REWIND-ON` | `rewindEnabled` |
| `WS-CONTENT-EXCLUSION` | `SET CONTENT-EXCLUSION-ON` | `contentExclusion` |
| `WS-ADDITIONAL-DIRS` | X(1024) | `additionalDirectories` |
| `WS-DISABLED-MCP` | X(1024) | `disabledMcpServers` |
| `WS-GITHUB-MCP-CONFIG` | X(1024) | `githubMcpToolConfig` |
| `WS-CANVAS-PROVIDER` | X(1024) | `canvasProvider` |
| `WS-CUSTOM-AGENTS-LCL` | `SET CUSTOM-AGENTS-LOCAL-ON` | `customAgentsLocalOnly` |
| `WS-DECISION-CONTEXT` | X(1024) | `decisionContext` |
| `WS-USER-PROMPT-XFORM` | `SET USER-PROMPT-XFORM-ON` | `userPromptTransformed` |
| `WS-BUILTIN-PLUGIN-DIR` | X(1024) | `builtinPluginDirectories` |
| `WS-ARGS-SCHEMA` | X(1024) | `argsSchema` |
| `WS-IN-PROCESS` | `SET IN-PROCESS-ON` | `inProcess` |
| `WS-EXPERIMENTAL-MODE` | `SET EXPERIMENTAL-ON` | `experimentalMode` |

### v2.4 session options and handlers

| Field | Toggle / PIC | Wire key |
|---|---|---|
| `WS-ENABLE-CITATIONS` | `SET CITATIONS-ON` | `enableCitations` |
| `WS-EXCLUDED-BUILTIN` | X(1024) | `excludedBuiltinAgents` |
| `WS-MAX-AI-CREDITS` | 9(10) | `sessionLimits.maxAiCredits` |
| `WS-MEMORY-ENABLED` | `SET MEMORY-ON` | `memory` |
| `WS-OTLP-PROTOCOL` | X(16) | `otlpProtocol` |
| `WS-ENABLE-WEBSOCKET` | `SET WEBSOCKET-ON` | `enableWebSocketResponses` |
| `WS-EXP-ASSIGNMENTS` | X(1024) | `expAssignments` |
| `WS-MCP-AUTH-HANDLER` | `SET MCP-AUTH-ON` | `mcpAuthHandler` |
| `WS-BEARER-PROVIDER` | `SET BEARER-PROVIDER-ON` | `bearer_token_provider` |
| `WS-REQUEST-HANDLER` | `SET REQUEST-HANDLER-ON` | `CopilotRequestHandler` |
| `WS-POST-TOOL-USE` | `SET POST-TOOL-USE-ON` | `post_tool_use` |
| `WS-PRE-MCP-TOOL-CALL` | `SET PRE-MCP-TOOL-CALL-ON` | `pre_mcp_tool_call` |
| `WS-SEND-AGENT-MODE` | X(32) | `agentMode` |
| `WS-SEND-DISPLAY-PROMPT` | X(4096) | `displayPrompt` |

Constants: `WS-TOOL-DEFER` (`TOOL-DEFER-AUTO`/`TOOL-DEFER-NEVER`),
`WS-SECTION-PREAMBLE`/`WS-SECTION-PRESERVE`, `WS-GH-COMMIT`/`WS-GH-REPOSITORY`.

### Example

```cobol
      *    Reasoning effort and tool search
           MOVE "high" TO WS-REASONING-EFFORT
           MOVE '{"enabled":true}' TO WS-TOOL-SEARCH

      *    Session rewind and content exclusion
           SET REWIND-ON TO TRUE
           SET CONTENT-EXCLUSION-ON TO TRUE

      *    Additional workspace directories (JSON array)
           MOVE '["/data/shared","/data/reference"]'
               TO WS-ADDITIONAL-DIRS

      *    Cap AI-credit spend for the session
           MOVE 500 TO WS-MAX-AI-CREDITS

           CALL "COPILOT-SESSION" USING WS-SESSION-CONFIG
               WS-SESSION-STATE WS-RETURN-CODE
```

## Return Codes

All operations set `WS-RETURN-CODE` with 88-level conditions:

| Code | 88-Level | Meaning |
|---|---|---|
| 0 | `RC-SUCCESS` | Operation succeeded |
| -1 | `RC-ERROR` | General error |
| -2 | `RC-TIMEOUT` | Operation timed out |
| -3 | `RC-PARSE-ERROR` | JSON parse failure |
| -4 | `RC-CONNECTION-ERROR` | CLI connection failure |

## Error Handling

```cobol
           CALL "COPILOT-SESSION" USING WS-SEND-OPTIONS
               WS-SEND-RESULT WS-RETURN-CODE

           EVALUATE TRUE
               WHEN RESULT-IS-OK
                   DISPLAY FUNCTION TRIM(WS-RESULT-MESSAGE)
               WHEN RESULT-HAS-ERROR
                   DISPLAY "Error: "
                       FUNCTION TRIM(WS-RESULT-ERROR-MSG)
               WHEN RC-TIMEOUT
                   DISPLAY "Request timed out"
               WHEN RC-CONNECTION-ERROR
                   DISPLAY "CLI connection lost"
           END-EVALUATE
```

## Running Tests

```bash
make test
```

The test suite validates:
- Version copybook values
- Type record initialization and defaults
- 88-level condition names
- Client, session, and send option defaults
- Return code value mapping
- JSON-RPC message building
- Tool and event type initialization
- Content framing defaults
- Protocol 4 grouped `session.create` / `session.resume` payloads (string
  escaping, unknown keys kept at the root, create-only members dropped on
  resume) and the client mode "empty" `availableTools` check

## Cookbook

See the [cookbook/](cookbook/) directory for practical recipes:

- [Error Handling](cookbook/error-handling.md)
- [Multiple Sessions](cookbook/multiple-sessions.md)
- [Persisting Sessions](cookbook/persisting-sessions.md)
- [Tools and Skills](cookbook/tools-and-skills.md)
- [Advanced Features](cookbook/advanced-features.md)
- [Batch Processing](cookbook/batch-processing.md)

## COBOL Idioms Used

This SDK follows standard COBOL conventions:

- **IDENTIFICATION/ENVIRONMENT/DATA/PROCEDURE DIVISIONs** in every program
- **01-level records** with 05/10-level subordinates for structured data
- **88-level condition names** for boolean flags and return codes
- **PERFORM paragraphs** for modular code organization
- **STRING/UNSTRING** for JSON construction and parsing
- **CALL** for inter-module communication
- **EVALUATE TRUE** for case/switch logic
- **COPY** for shared record layouts (copybooks)
- **FILE-CONTROL/FD** for pipe-based I/O

## License

See the [LICENSE](../LICENSE) file in the repository root.
