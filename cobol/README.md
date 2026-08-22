# Copilot Supercharged SDK for COBOL

COBOL SDK for the GitHub Copilot CLI. Communicates with the Copilot CLI server via JSON-RPC 2.0 over stdio pipes using Content-Length header framing. Built for GnuCOBOL (the free, open-source COBOL compiler).

COBOL developers deserve nice things too.

## Requirements

- **GnuCOBOL** 3.0 or later (`cobc` compiler)
- **Make** (GNU Make recommended)
- **GitHub Copilot CLI** installed and available on PATH (or specify path via `cli_url`)

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

## Architecture

```
Your COBOL Program
    |
    | CALL "COPILOT-CLIENT-INIT"
    | CALL "COPILOT-SESSION"
    v
+---------------------+
| COPILOT-SESSION.cob |  Session lifecycle, message sending
+---------------------+
    |
    | CALL "COPILOT-JSONRPC"
    v
+---------------------+
| COPILOT-JSONRPC.cob |  JSON-RPC 2.0 message build/parse
+---------------------+
    |
    | CALL "COPILOT-CLIENT"
    v
+---------------------+
| COPILOT-CLIENT.cob  |  CLI process, pipe I/O, framing
+---------------------+
    |
    | stdio pipes (Content-Length framing)
    v
+---------------------+
| Copilot CLI Server  |  JSON-RPC server (copilot server --stdio)
+---------------------+
```

## Module Reference

### COPILOT-CLIENT.cob

Main client module managing CLI process lifecycle and I/O.

| Entry Point | Parameters | Description |
|---|---|---|
| `COPILOT-CLIENT-INIT` | `WS-CLIENT-OPTIONS`, `WS-RETURN-CODE` | Initialize client with options |
| `COPILOT-CLIENT-START` | `WS-RETURN-CODE` | Start the CLI process |
| `COPILOT-CLIENT-STOP` | `WS-RETURN-CODE` | Stop the CLI process |
| `COPILOT-CLIENT-PING` | `WS-RETURN-CODE` | Health check (0 = healthy) |
| `COPILOT-CLIENT` | request, response, rc | Send/receive JSON-RPC |

### COPILOT-SESSION.cob

Session management for conversations.

| Paragraph | Description |
|---|---|
| `CREATE-SESSION` | Create a new Copilot session |
| `SEND-MESSAGE` | Send a user message and wait for response |
| `DESTROY-SESSION` | Close and clean up a session |

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
| `COPILOT-TYPES.cpy` | Record layouts for all data structures (client options, session state, send options, results, tools, events, framing) |
| `COPILOT-VERSION.cpy` | Protocol version constants and SDK identity |

## Configuration Options

Set fields in `WS-CLIENT-OPTIONS` before calling `COPILOT-CLIENT-INIT`:

| Field | PIC | Default | Description |
|---|---|---|---|
| `WS-CLI-PATH` | X(256) | SPACES | Path to the Copilot CLI binary |
| `WS-CLI-URL` | X(256) | SPACES | URL of external CLI server |
| `WS-AUTO-START` | 9 | 1 | Auto-start CLI on init (1=yes, 0=no) |
| `WS-GITHUB-TOKEN` | X(256) | SPACES | GitHub authentication token |

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
creating a session; `BUILD-CREATE-PARAMS` includes any that are set.

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
