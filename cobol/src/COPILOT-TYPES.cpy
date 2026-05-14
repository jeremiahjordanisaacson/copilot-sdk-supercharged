      *================================================================*
      * COPILOT-TYPES.cpy                                              *
      * Record layouts for Copilot SDK data structures.                *
      * Used by COPILOT-CLIENT, COPILOT-SESSION, COPILOT-JSONRPC.     *
      *================================================================*

      *----------------------------------------------------------------*
      * Client configuration options                                   *
      *----------------------------------------------------------------*
       01  WS-CLIENT-OPTIONS.
           05 WS-CLI-PATH           PIC X(256) VALUE SPACES.
           05 WS-CLI-URL            PIC X(256) VALUE SPACES.
           05 WS-AUTO-START         PIC 9      VALUE 1.
           05 WS-GITHUB-TOKEN       PIC X(256) VALUE SPACES.
           05 WS-COPILOT-HOME       PIC X(256) VALUE SPACES.
           05 WS-TCP-CONN-TOKEN     PIC X(256) VALUE SPACES.

      *----------------------------------------------------------------*
      * Client state                                                   *
      *----------------------------------------------------------------*
       01  WS-CLIENT-STATE.
           05 WS-CLIENT-ACTIVE      PIC 9      VALUE 0.
               88 CLIENT-IS-ACTIVE             VALUE 1.
               88 CLIENT-IS-STOPPED            VALUE 0.
           05 WS-CLIENT-PID         PIC 9(10)  VALUE 0.
           05 WS-REQUEST-ID         PIC 9(10)  VALUE 0.
           05 WS-STDIN-HANDLE       PIC 9(10)  VALUE 0.
           05 WS-STDOUT-HANDLE      PIC 9(10)  VALUE 0.
           05 WS-LAST-ERROR         PIC X(256) VALUE SPACES.

      *----------------------------------------------------------------*
      * JSON-RPC request message                                       *
      *----------------------------------------------------------------*
       01  WS-JSONRPC-REQUEST.
           05 WS-REQ-JSONRPC        PIC X(4)   VALUE "2.0".
           05 WS-REQ-METHOD         PIC X(64)  VALUE SPACES.
           05 WS-REQ-ID             PIC 9(10)  VALUE 0.
           05 WS-REQ-PARAMS         PIC X(4096) VALUE SPACES.
           05 WS-REQ-PARAMS-LEN     PIC 9(5)   VALUE 0.

      *----------------------------------------------------------------*
      * JSON-RPC response message                                      *
      *----------------------------------------------------------------*
       01  WS-JSONRPC-RESPONSE.
           05 WS-RESP-JSONRPC       PIC X(4)   VALUE SPACES.
           05 WS-RESP-ID            PIC 9(10)  VALUE 0.
           05 WS-RESP-HAS-ERROR     PIC 9      VALUE 0.
               88 RESP-IS-ERROR                VALUE 1.
               88 RESP-IS-OK                   VALUE 0.
           05 WS-RESP-ERROR-CODE    PIC S9(10) VALUE 0.
           05 WS-RESP-ERROR-MSG     PIC X(256) VALUE SPACES.
           05 WS-RESP-RESULT        PIC X(4096) VALUE SPACES.
           05 WS-RESP-RESULT-LEN    PIC 9(5)   VALUE 0.

      *----------------------------------------------------------------*
      * Session configuration                                          *
      *----------------------------------------------------------------*
       01  WS-SESSION-CONFIG.
           05 WS-SESS-AGENT-NAME    PIC X(64)  VALUE SPACES.
           05 WS-SESS-AGENT-VER     PIC X(16)  VALUE SPACES.
           05 WS-SESS-SYS-PROMPT    PIC X(1024) VALUE SPACES.
           05 WS-SESS-STREAMING     PIC 9      VALUE 0.
               88 STREAMING-ON                 VALUE 1.
               88 STREAMING-OFF                VALUE 0.
           05 WS-SESS-HISTORY-LEN   PIC 9(5)   VALUE 100.
           05 WS-SESS-INSTR-DIRS.
               10 WS-INSTR-DIR-COUNT PIC 9(3)  VALUE 0.
               10 WS-INSTR-DIR-ENTRY OCCURS 10 TIMES.
                   15 WS-INSTR-DIR-PATH PIC X(256) VALUE SPACES.

      *----------------------------------------------------------------*
      * Session state                                                  *
      *----------------------------------------------------------------*
       01  WS-SESSION-STATE.
           05 WS-SESSION-ID         PIC X(64)  VALUE SPACES.
           05 WS-SESSION-ACTIVE     PIC 9      VALUE 0.
               88 SESSION-IS-ACTIVE            VALUE 1.
               88 SESSION-IS-CLOSED            VALUE 0.
           05 WS-TURN-COUNT         PIC 9(10)  VALUE 0.

      *----------------------------------------------------------------*
      * Send options for a message                                     *
      *----------------------------------------------------------------*
       01  WS-SEND-OPTIONS.
           05 WS-SEND-MESSAGE       PIC X(4096) VALUE SPACES.
           05 WS-SEND-MSG-LEN       PIC 9(5)   VALUE 0.
           05 WS-SEND-TIMEOUT-MS    PIC 9(10)  VALUE 30000.

      *----------------------------------------------------------------*
      * Response result from send-and-wait                             *
      *----------------------------------------------------------------*
       01  WS-SEND-RESULT.
           05 WS-RESULT-MESSAGE     PIC X(4096) VALUE SPACES.
           05 WS-RESULT-MSG-LEN     PIC 9(5)   VALUE 0.
           05 WS-RESULT-IS-ERROR    PIC 9      VALUE 0.
               88 RESULT-HAS-ERROR             VALUE 1.
               88 RESULT-IS-OK                 VALUE 0.
           05 WS-RESULT-ERROR-MSG   PIC X(256) VALUE SPACES.
           05 WS-RESULT-TURN-ID     PIC X(64)  VALUE SPACES.

      *----------------------------------------------------------------*
      * Tool definition                                                *
      *----------------------------------------------------------------*
       01  WS-TOOL-DEF.
           05 WS-TOOL-NAME          PIC X(64)  VALUE SPACES.
           05 WS-TOOL-DESCRIPTION   PIC X(256) VALUE SPACES.
           05 WS-TOOL-SCHEMA        PIC X(1024) VALUE SPACES.

      *----------------------------------------------------------------*
      * Tool invocation (received from assistant)                      *
      *----------------------------------------------------------------*
       01  WS-TOOL-INVOCATION.
           05 WS-INVOKE-ID          PIC X(64)  VALUE SPACES.
           05 WS-INVOKE-NAME        PIC X(64)  VALUE SPACES.
           05 WS-INVOKE-ARGS        PIC X(2048) VALUE SPACES.

      *----------------------------------------------------------------*
      * Tool result (returned to assistant)                            *
      *----------------------------------------------------------------*
       01  WS-TOOL-RESULT.
           05 WS-TRES-SUCCESS       PIC 9      VALUE 1.
               88 TOOL-SUCCEEDED              VALUE 1.
               88 TOOL-FAILED                 VALUE 0.
           05 WS-TRES-OUTPUT        PIC X(2048) VALUE SPACES.

      *----------------------------------------------------------------*
      * Session event (streaming or notification)                      *
      *----------------------------------------------------------------*
       01  WS-SESSION-EVENT.
           05 WS-EVT-TYPE           PIC X(64)  VALUE SPACES.
           05 WS-EVT-ID             PIC X(64)  VALUE SPACES.
           05 WS-EVT-TIMESTAMP      PIC X(32)  VALUE SPACES.
           05 WS-EVT-PARENT-ID      PIC X(64)  VALUE SPACES.
           05 WS-EVT-AGENT-ID       PIC X(64)  VALUE SPACES.
           05 WS-EVT-EPHEMERAL      PIC 9      VALUE 0.
           05 WS-EVT-SESSION-ID     PIC X(64)  VALUE SPACES.
           05 WS-EVT-DATA           PIC X(4096) VALUE SPACES.
           05 WS-EVT-DATA-LEN       PIC 9(5)   VALUE 0.

      *----------------------------------------------------------------*
      * Content-Length framing fields                                   *
      *----------------------------------------------------------------*
       01  WS-FRAMING.
           05 WS-CONTENT-LENGTH     PIC 9(10)  VALUE 0.
           05 WS-FRAME-BUFFER       PIC X(8192) VALUE SPACES.
           05 WS-FRAME-BUF-LEN      PIC 9(5)   VALUE 0.

      *----------------------------------------------------------------*
      * Return code used across modules                                *
      *----------------------------------------------------------------*
       01  WS-RETURN-CODE           PIC S9(4)  VALUE 0.
           88 RC-SUCCESS                       VALUE 0.
           88 RC-ERROR                         VALUE -1.
           88 RC-TIMEOUT                       VALUE -2.
           88 RC-PARSE-ERROR                   VALUE -3.
           88 RC-CONNECTION-ERROR              VALUE -4.

      *----------------------------------------------------------------*
      * Session filesystem configuration                               *
      *----------------------------------------------------------------*
       01  WS-SESSION-FS-CONFIG.
           05 WS-SFS-INITIAL-CWD    PIC X(256) VALUE SPACES.
           05 WS-SFS-STATE-PATH     PIC X(256) VALUE SPACES.
           05 WS-SFS-CONVENTIONS    PIC X(16)  VALUE "posix".
           05 WS-SFS-ENABLED        PIC 9      VALUE 0.
               88 SESSION-FS-ON                VALUE 1.
               88 SESSION-FS-OFF               VALUE 0.

      *----------------------------------------------------------------*
      * MCP server configuration                                       *
      *----------------------------------------------------------------*
       01  WS-MCP-SERVER-CONFIG.
           05 WS-MCP-SERVER-NAME    PIC X(64)  VALUE SPACES.
           05 WS-MCP-SERVER-TYPE    PIC X(16)  VALUE "stdio".
           05 WS-MCP-SERVER-CMD     PIC X(256) VALUE SPACES.
           05 WS-MCP-SERVER-URL     PIC X(256) VALUE SPACES.

      *----------------------------------------------------------------*
      * Command definition                                             *
      *----------------------------------------------------------------*
       01  WS-COMMAND-DEF.
           05 WS-CMD-NAME           PIC X(64)  VALUE SPACES.
           05 WS-CMD-DESCRIPTION    PIC X(256) VALUE SPACES.

      *----------------------------------------------------------------*
      * Extended session options                                       *
      *----------------------------------------------------------------*
       01  WS-SESSION-EXTENDED.
           05 WS-SESS-MODEL         PIC X(64)  VALUE SPACES.
           05 WS-SESS-REASON-EFFORT PIC X(16)  VALUE SPACES.
           05 WS-SESS-AUTH-TOKEN    PIC X(256) VALUE SPACES.
           05 WS-SESS-WORKING-DIR   PIC X(256) VALUE SPACES.
           05 WS-SESS-RESP-FORMAT   PIC X(16)  VALUE SPACES.
           05 WS-SESS-CONFIG-DISC   PIC 9      VALUE 0.
               88 CONFIG-DISCOVERY-ON          VALUE 1.
               88 CONFIG-DISCOVERY-OFF         VALUE 0.
           05 WS-SESS-SUB-STREAM    PIC 9      VALUE 0.
               88 SUB-AGENT-STREAM-ON         VALUE 1.
               88 SUB-AGENT-STREAM-OFF        VALUE 0.

      *----------------------------------------------------------------*
      * Feature support flags                                          *
      * Maps to SDK features: elicitation, requestHeaders,             *
      * responseFormat, skills, availableTools, modelCapabilities,     *
      * enableConfigDiscovery, includeSubAgentStreamingEvents,         *
      * idleTimeout, authToken, imageGeneration                        *
      *----------------------------------------------------------------*
       01  WS-FEATURE-FLAGS.
      *    UI elicitation handler flag
           05 WS-FEAT-ELICITATION    PIC 9      VALUE 0.
               88 ELICITATION-ON               VALUE 1.
               88 ELICITATION-OFF              VALUE 0.
      *    Runtime request_headers (JSON object)
           05 WS-REQUEST-HEADERS     PIC X(1024) VALUE SPACES.
      *    Image generation / responseFormat
           05 WS-RESPONSE-FORMAT     PIC X(16)  VALUE SPACES.
      *    Per-agent skills (JSON array)
           05 WS-AGENT-SKILLS        PIC X(1024) VALUE SPACES.
      *    Per-agent tool visibility: availableTools
           05 WS-AVAILABLE-TOOLS     PIC X(1024) VALUE SPACES.
      *    Model capabilities override: modelCapabilities
           05 WS-MODEL-CAPABILITIES  PIC X(1024) VALUE SPACES.
      *    Config discovery: enableConfigDiscovery
           05 WS-ENABLE-CONFIG-DISCOVERY PIC 9   VALUE 0.
               88 CONFIG-DISC-ON               VALUE 1.
               88 CONFIG-DISC-OFF              VALUE 0.
      *    Sub-agent streaming: includeSubAgentStreamingEvents
           05 WS-SUB-AGENT-STREAMING PIC 9      VALUE 0.
               88 SUB-AGT-STREAM-ON           VALUE 1.
               88 SUB-AGT-STREAM-OFF          VALUE 0.
      *    Session idleTimeout in seconds
           05 WS-IDLE-TIMEOUT        PIC 9(10)  VALUE 0.
      *    Per-session authToken
           05 WS-AUTH-TOKEN          PIC X(256) VALUE SPACES.

      *----------------------------------------------------------------*
      * Client extended options                                        *
      *----------------------------------------------------------------*
       01  WS-CLIENT-EXTENDED.
           05 WS-CLI-USE-LOGGED-IN  PIC 9      VALUE 1.
               88 USE-LOGGED-IN-YES           VALUE 1.
               88 USE-LOGGED-IN-NO            VALUE 0.
           05 WS-CLI-IDLE-TIMEOUT   PIC 9(10)  VALUE 0.
           05 WS-CLI-LOG-LEVEL      PIC X(16)  VALUE "error".

      *----------------------------------------------------------------*
      * Remote option for CLI spawn                                    *
      *----------------------------------------------------------------*
       01  WS-REMOTE-OPTION.
           05 WS-REMOTE-ENABLED      PIC 9      VALUE 0.
               88 REMOTE-ON                     VALUE 1.
               88 REMOTE-OFF                    VALUE 0.

      *----------------------------------------------------------------*
      * Session telemetry                                               *
      *----------------------------------------------------------------*
       01  WS-SESSION-TELEMETRY.
           05 WS-ENABLE-SESS-TELEMETRY PIC 9    VALUE 0.
               88 SESS-TELEMETRY-ON             VALUE 1.
               88 SESS-TELEMETRY-OFF            VALUE 0.

      *----------------------------------------------------------------*
      * Exit Plan Mode request/response                                *
      *----------------------------------------------------------------*
       01  WS-EXIT-PLAN-MODE-REQ.
           05 WS-EPM-SUMMARY         PIC X(1024) VALUE SPACES.
           05 WS-EPM-PLAN-CONTENT    PIC X(2048) VALUE SPACES.
           05 WS-EPM-ACTIONS         PIC X(1024) VALUE SPACES.
           05 WS-EPM-RECOMMENDED     PIC X(64)  VALUE SPACES.
           05 WS-EPM-SESSION-ID      PIC X(64)  VALUE SPACES.

       01  WS-EXIT-PLAN-MODE-RESP.
           05 WS-EPM-APPROVED        PIC 9      VALUE 1.
               88 EPM-APPROVED                  VALUE 1.
               88 EPM-DENIED                    VALUE 0.
           05 WS-EPM-SELECTED-ACTION PIC X(64)  VALUE SPACES.
           05 WS-EPM-FEEDBACK        PIC X(256) VALUE SPACES.

       01  WS-EPM-HANDLER-SET        PIC 9      VALUE 0.
           88 EPM-HANDLER-ACTIVE                VALUE 1.
           88 EPM-HANDLER-INACTIVE              VALUE 0.

      *----------------------------------------------------------------*
      * W3C Trace Context                                              *
      *----------------------------------------------------------------*
       01  WS-TRACE-CONTEXT.
           05 WS-TRACEPARENT         PIC X(256) VALUE SPACES.
           05 WS-TRACESTATE          PIC X(256) VALUE SPACES.

       01  WS-TRACE-PROVIDER-SET     PIC 9      VALUE 0.
           88 TRACE-PROVIDER-ACTIVE             VALUE 1.
           88 TRACE-PROVIDER-INACTIVE           VALUE 0.

      *----------------------------------------------------------------*
      * Slash command input                                             *
      *----------------------------------------------------------------*
       01  WS-SLASH-CMD-INPUT.
           05 WS-SCI-HINT            PIC X(256) VALUE SPACES.
           05 WS-SCI-COMPLETION      PIC X(16)  VALUE SPACES.

      *----------------------------------------------------------------*
      * Slash command info                                              *
      *----------------------------------------------------------------*
       01  WS-SLASH-CMD-INFO.
           05 WS-SCMD-ALLOW-AGENT    PIC 9      VALUE 0.
               88 SCMD-ALLOW-AGENT-YES          VALUE 1.
               88 SCMD-ALLOW-AGENT-NO           VALUE 0.
           05 WS-SCMD-DESCRIPTION    PIC X(256) VALUE SPACES.
           05 WS-SCMD-KIND           PIC X(16)  VALUE SPACES.
           05 WS-SCMD-NAME           PIC X(64)  VALUE SPACES.
           05 WS-SCMD-ALIASES        PIC X(512) VALUE SPACES.
           05 WS-SCMD-EXPERIMENTAL   PIC 9      VALUE 0.
               88 SCMD-EXPERIMENTAL-YES         VALUE 1.
               88 SCMD-EXPERIMENTAL-NO          VALUE 0.
           05 WS-SCMD-INPUT          PIC X(256) VALUE SPACES.

      *----------------------------------------------------------------*
      * Commands invoke request                                         *
      *----------------------------------------------------------------*
       01  WS-CMD-INVOKE-REQ.
           05 WS-CINV-NAME           PIC X(64)  VALUE SPACES.
           05 WS-CINV-INPUT          PIC X(1024) VALUE SPACES.

      *----------------------------------------------------------------*
      * Commands list request                                           *
      *----------------------------------------------------------------*
       01  WS-CMD-LIST-REQ.
           05 WS-CLST-INCL-BUILTINS  PIC 9      VALUE 0.
               88 CLST-BUILTINS-YES             VALUE 1.
               88 CLST-BUILTINS-NO              VALUE 0.
           05 WS-CLST-INCL-CLIENT    PIC 9      VALUE 0.
               88 CLST-CLIENT-YES               VALUE 1.
               88 CLST-CLIENT-NO                VALUE 0.
           05 WS-CLST-INCL-SKILLS    PIC 9      VALUE 0.
               88 CLST-SKILLS-YES               VALUE 1.
               88 CLST-SKILLS-NO                VALUE 0.

      *----------------------------------------------------------------*
      * Model billing token prices                                      *
      *----------------------------------------------------------------*
       01  WS-MODEL-BILL-PRICES.
           05 WS-MBP-BATCH-SIZE      PIC 9(10)  VALUE 0.
           05 WS-MBP-CACHE-PRICE     PIC 9(10)  VALUE 0.
           05 WS-MBP-INPUT-PRICE     PIC 9(10)  VALUE 0.
           05 WS-MBP-OUTPUT-PRICE    PIC 9(10)  VALUE 0.

      *----------------------------------------------------------------*
      * Model billing                                                   *
      *----------------------------------------------------------------*
       01  WS-MODEL-BILLING.
           05 WS-MB-MULTIPLIER       PIC 9(5)V9(4) VALUE 0.
           05 WS-MB-HAS-PRICES       PIC 9      VALUE 0.
               88 MB-HAS-PRICES-YES             VALUE 1.
               88 MB-HAS-PRICES-NO              VALUE 0.
           05 WS-MB-PRICE-CATEGORY   PIC X(16)  VALUE SPACES.

      *----------------------------------------------------------------*
      * Experimental                                                    *
      * Diagnostics from loading skills                                 *
      *----------------------------------------------------------------*
       01  WS-SKILLS-DIAG.
           05 WS-SDIAG-ERRORS        PIC X(2048) VALUE SPACES.
           05 WS-SDIAG-WARNINGS      PIC X(2048) VALUE SPACES.

      *----------------------------------------------------------------*
      * Experimental                                                    *
      * Remote session mode enum                                        *
      * Values: "export", "off", "on"                                   *
      *----------------------------------------------------------------*
       01  WS-REMOTE-SESSION-MODE     PIC X(16)  VALUE SPACES.
           88 REMOTE-MODE-EXPORT                  VALUE "export".
           88 REMOTE-MODE-OFF                     VALUE "off".
           88 REMOTE-MODE-ON                      VALUE "on".

      *----------------------------------------------------------------*
      * Experimental                                                    *
      * Remote enable request                                           *
      *----------------------------------------------------------------*
       01  WS-REMOTE-ENABLE-REQ.
           05 WS-RER-MODE             PIC X(16)  VALUE SPACES.
           05 WS-RER-MODE-SET         PIC 9      VALUE 0.
               88 RER-MODE-SET-YES               VALUE 1.
               88 RER-MODE-SET-NO                VALUE 0.

      *----------------------------------------------------------------*
      * Experimental                                                    *
      * Remote enable result                                            *
      *----------------------------------------------------------------*
       01  WS-REMOTE-ENABLE-RESULT.
           05 WS-RERES-STEERABLE     PIC 9      VALUE 0.
               88 RERES-STEERABLE-YES            VALUE 1.
               88 RERES-STEERABLE-NO             VALUE 0.
           05 WS-RERES-URL            PIC X(256) VALUE SPACES.

      *----------------------------------------------------------------*
      * Model capabilities override                                    *
      *----------------------------------------------------------------*
       01  WS-MODEL-CAP-OVERRIDE.
           05 WS-MCO-SUPPORTS        PIC X(512) VALUE SPACES.
           05 WS-MCO-LIMITS          PIC X(512) VALUE SPACES.
           05 WS-MCO-VISION          PIC X(256) VALUE SPACES.
           05 WS-MCO-ENABLED         PIC 9      VALUE 0.
               88 MCO-ENABLED                   VALUE 1.
               88 MCO-DISABLED                  VALUE 0.
