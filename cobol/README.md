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
