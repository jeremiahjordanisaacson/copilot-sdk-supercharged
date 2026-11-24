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
      *    Idempotent tool (idempotent): "Y", "N" or SPACE (not sent)
           05 WS-TOOL-IDEMPOTENT    PIC X      VALUE SPACE.
               88 TOOL-IDEMPOTENT-YES          VALUE "Y".
               88 TOOL-IDEMPOTENT-NO           VALUE "N".
      *    CALLable program that runs the tool. The definition is sent
      *    (tools.definitions) only when a handler program is named.
           05 WS-TOOL-HANDLER-PGM   PIC X(30)  VALUE SPACES.

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
      * Image generation options                                       *
      *----------------------------------------------------------------*
       01  WS-IMAGE-OPTIONS.
           05 WS-IMG-SIZE           PIC X(32)  VALUE SPACES.
           05 WS-IMG-QUALITY        PIC X(16)  VALUE SPACES.
           05 WS-IMG-STYLE          PIC X(16)  VALUE SPACES.

      *----------------------------------------------------------------*
      * System message configuration                                   *
      *----------------------------------------------------------------*
       01  WS-SECTION-OVERRIDE.
           05 WS-SO-ACTION          PIC X(16)  VALUE SPACES.
           05 WS-SO-CONTENT         PIC X(2048) VALUE SPACES.

       01  WS-SYSTEM-MSG-APPEND.
           05 WS-SMA-MODE           PIC X(16)  VALUE SPACES.
           05 WS-SMA-CONTENT        PIC X(2048) VALUE SPACES.

       01  WS-SYSTEM-MSG-REPLACE.
           05 WS-SMR-MODE           PIC X(16)  VALUE "replace".
           05 WS-SMR-CONTENT        PIC X(4096) VALUE SPACES.

       01  WS-SYSTEM-MSG-CUSTOMIZE.
           05 WS-SMC-MODE           PIC X(16)  VALUE "customize".
           05 WS-SMC-SECTIONS       PIC X(4096) VALUE SPACES.
           05 WS-SMC-CONTENT        PIC X(4096) VALUE SPACES.

      *----------------------------------------------------------------*
      * User input request / response (userInput.request, the ask_user *
      * tool). COPILOT-SESSION CALLs the program named in              *
      * WS-USER-INPUT-PGM (COPILOT-SESSION-OPTS.cpy) with both records.*
      *----------------------------------------------------------------*
       01  WS-USER-INPUT-REQ.
           05 WS-UIR-QUESTION       PIC X(1024) VALUE SPACES.
      *    Choices as the raw JSON array (spaces when absent)
           05 WS-UIR-CHOICES        PIC X(2048) VALUE SPACES.
           05 WS-UIR-ALLOW-FREEFORM PIC 9      VALUE 0.
               88 UIR-ALLOW-FREEFORM-YES       VALUE 1.
               88 UIR-ALLOW-FREEFORM-NO        VALUE 0.
      *    Session id and pending request id (requestId), when sent
           05 WS-UIR-SESSION-ID     PIC X(64)  VALUE SPACES.
           05 WS-UIR-REQUEST-ID     PIC X(64)  VALUE SPACES.
      *    Image attachment constraints (imageAttachmentConstraints):
      *    UIR-IMAGES-ACCEPTED when the runtime accepts images in the
      *    answer; allowedMimeTypes is the raw JSON array.
           05 WS-UIR-IMAGES-FLAG    PIC 9      VALUE 0.
               88 UIR-IMAGES-ACCEPTED          VALUE 1.
               88 UIR-IMAGES-NOT-ACCEPTED      VALUE 0.
           05 WS-UIR-ALLOWED-MIME-TYPES PIC X(512) VALUE SPACES.
           05 WS-UIR-MAX-BYTES-PER-IMAGE PIC 9(10) VALUE 0.
           05 WS-UIR-MAX-COUNT      PIC 9(4)   VALUE 0.
      *    The request's opaque _meta object (raw JSON), when sent
           05 WS-UIR-META           PIC X(1024) VALUE SPACES.

       01  WS-USER-INPUT-RESP.
           05 WS-UIRESP-ANSWER      PIC X(1024) VALUE SPACES.
           05 WS-UIRESP-WAS-FREEFORM PIC 9     VALUE 0.
               88 UIRESP-WAS-FREEFORM-YES      VALUE 1.
               88 UIRESP-WAS-FREEFORM-NO       VALUE 0.
      *    Image attachments (attachments, UserInputImageAttachment),
      *    sent only when WS-UIRESP-ATTACH-COUNT > 0 and only valid
      *    when the request set UIR-IMAGES-ACCEPTED. The wire type is
      *    always "image"; WS-UIA-DATA holds the base64 image bytes.
      *    Image-only answers use a blank answer with
      *    UIRESP-WAS-FREEFORM-YES. The runtime enforces the limits.
           05 WS-UIRESP-ATTACH-COUNT PIC 9     VALUE 0.
           05 WS-UIRESP-ATTACHMENT  OCCURS 4 TIMES.
               10 WS-UIA-DATA       PIC X(3072) VALUE SPACES.
               10 WS-UIA-MIME-TYPE  PIC X(64)  VALUE SPACES.
               10 WS-UIA-DESCRIPTION PIC X(256) VALUE SPACES.
               10 WS-UIA-DISPLAY-NAME PIC X(128) VALUE SPACES.

      *----------------------------------------------------------------*
      * Cloud session metadata                                          *
      *----------------------------------------------------------------*
       01  WS-CLOUD-SESSION-REPOSITORY.
           05 WS-CSR-OWNER          PIC X(128) VALUE SPACES.
           05 WS-CSR-NAME           PIC X(128) VALUE SPACES.
           05 WS-CSR-BRANCH         PIC X(128) VALUE SPACES.

       01  WS-CLOUD-SESSION-OPTIONS.
           05 WS-CSO-HAS-REPOSITORY PIC 9      VALUE 0.
               88 CSO-HAS-REPOSITORY-YES       VALUE 1.
               88 CSO-HAS-REPOSITORY-NO        VALUE 0.
           05 WS-CSO-REPOSITORY.
               10 WS-CSO-OWNER      PIC X(128) VALUE SPACES.
               10 WS-CSO-NAME       PIC X(128) VALUE SPACES.
               10 WS-CSO-BRANCH     PIC X(128) VALUE SPACES.

      *----------------------------------------------------------------*
      * Canvas types                                                    *
      *----------------------------------------------------------------*
       01  WS-CANVAS-ACTION.
           05 WS-CVA-NAME           PIC X(128) VALUE SPACES.
           05 WS-CVA-DESCRIPTION    PIC X(256) VALUE SPACES.
           05 WS-CVA-INPUT-SCHEMA   PIC X(2048) VALUE SPACES.

       01  WS-CANVAS-DECLARATION.
           05 WS-CVD-ID             PIC X(128) VALUE SPACES.
           05 WS-CVD-DISPLAY-NAME   PIC X(128) VALUE SPACES.
           05 WS-CVD-DESCRIPTION    PIC X(256) VALUE SPACES.
           05 WS-CVD-INPUT-SCHEMA   PIC X(2048) VALUE SPACES.
           05 WS-CVD-ACTIONS        PIC X(4096) VALUE SPACES.

       01  WS-CANVAS-OPEN-RESP.
           05 WS-COR-URL            PIC X(256) VALUE SPACES.
           05 WS-COR-TITLE          PIC X(128) VALUE SPACES.
           05 WS-COR-STATUS         PIC X(128) VALUE SPACES.

       01  WS-CANVAS-HOST-CAPS.
           05 WS-CHC-CANVASES       PIC 9      VALUE 0.
               88 CHC-CANVASES-YES             VALUE 1.
               88 CHC-CANVASES-NO              VALUE 0.

       01  WS-CANVAS-HOST-CONTEXT.
           05 WS-CHCTX-CAPS.
               10 WS-CHCTX-CANVASES PIC 9      VALUE 0.

       01  WS-CANVAS-OPEN-CONTEXT.
           05 WS-COC-SESSION-ID     PIC X(64)  VALUE SPACES.
           05 WS-COC-EXTENSION-ID   PIC X(128) VALUE SPACES.
           05 WS-COC-CANVAS-ID      PIC X(128) VALUE SPACES.
           05 WS-COC-INSTANCE-ID    PIC X(128) VALUE SPACES.
           05 WS-COC-INPUT          PIC X(4096) VALUE SPACES.
           05 WS-COC-HAS-HOST       PIC 9      VALUE 0.
           05 WS-COC-HOST           PIC X(256) VALUE SPACES.

       01  WS-CANVAS-ACTION-CONTEXT.
           05 WS-CAC-SESSION-ID     PIC X(64)  VALUE SPACES.
           05 WS-CAC-EXTENSION-ID   PIC X(128) VALUE SPACES.
           05 WS-CAC-CANVAS-ID      PIC X(128) VALUE SPACES.
           05 WS-CAC-INSTANCE-ID    PIC X(128) VALUE SPACES.
           05 WS-CAC-ACTION-NAME    PIC X(128) VALUE SPACES.
           05 WS-CAC-INPUT          PIC X(4096) VALUE SPACES.
           05 WS-CAC-HAS-HOST       PIC 9      VALUE 0.
           05 WS-CAC-HOST           PIC X(256) VALUE SPACES.

       01  WS-CANVAS-LIFECYCLE-CONTEXT.
           05 WS-CLC-SESSION-ID     PIC X(64)  VALUE SPACES.
           05 WS-CLC-EXTENSION-ID   PIC X(128) VALUE SPACES.
           05 WS-CLC-CANVAS-ID      PIC X(128) VALUE SPACES.
           05 WS-CLC-INSTANCE-ID    PIC X(128) VALUE SPACES.
           05 WS-CLC-HAS-HOST       PIC 9      VALUE 0.
           05 WS-CLC-HOST           PIC X(256) VALUE SPACES.

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

      *================================================================*
      * SDK protocol 4 / v3.0.0 types (Microsoft Copilot SDK 2.0)      *
      * Callback programs named in these records are CALLed            *
      * dynamically by COPILOT-SESSION when the runtime sends the      *
      * matching server-to-client request.                             *
      *================================================================*

      *----------------------------------------------------------------*
      * Client info handshake (ClientInfo): sent as clientInfo in the  *
      * connect params. Blank fields are dropped and nothing is sent   *
      * when every field is blank.                                     *
      *----------------------------------------------------------------*
       01  WS-CLIENT-INFO.
           05 WS-CI-EDITOR-NAME      PIC X(64)  VALUE SPACES.
           05 WS-CI-EDITOR-VERSION   PIC X(32)  VALUE SPACES.
           05 WS-CI-EXTENSION-NAME   PIC X(64)  VALUE SPACES.
           05 WS-CI-EXTENSION-VER    PIC X(32)  VALUE SPACES.

      *----------------------------------------------------------------*
      * Client mode: "coding-agent" (default, the existing behavior)   *
      * or "empty". Empty mode requires WS-AVAILABLE-TOOLS on every    *
      * session and turns ambient features off. "copilot-cli" is an   *
      * alias of "coding-agent".                                       *
      *----------------------------------------------------------------*
       01  WS-CLIENT-MODE            PIC X(16)  VALUE "coding-agent".
           88 CLIENT-MODE-CODING-AGENT          VALUE "coding-agent"
                                                      "copilot-cli"
                                                      " ".
           88 CLIENT-MODE-EMPTY                 VALUE "empty".

      *----------------------------------------------------------------*
      * Network callbacks (network.setProvider). Name the CALLable     *
      * program that answers each request (blank = not registered).    *
      * Each program receives WS-NETWORK-REQUEST and fills             *
      * WS-NETWORK-RESPONSE; blank response fields decline.            *
      *----------------------------------------------------------------*
       01  WS-NETWORK-OPTIONS.
           05 WS-NET-SCOPE           PIC X(16)  VALUE SPACES.
               88 NET-SCOPE-CONNECTION          VALUE "connection".
               88 NET-SCOPE-RUNTIME             VALUE "runtime".
           05 WS-NET-PROXY-PGM       PIC X(30)  VALUE SPACES.
           05 WS-NET-NEGOTIATE-PGM   PIC X(30)  VALUE SPACES.
           05 WS-NET-BASIC-PGM       PIC X(30)  VALUE SPACES.

       01  WS-NETWORK-REQUEST.
           05 WS-NREQ-METHOD         PIC X(32)  VALUE SPACES.
           05 WS-NREQ-URL            PIC X(1024) VALUE SPACES.
           05 WS-NREQ-SPN            PIC X(256) VALUE SPACES.
           05 WS-NREQ-PROXY          PIC X(256) VALUE SPACES.
           05 WS-NREQ-REALM          PIC X(256) VALUE SPACES.
           05 WS-NREQ-PREV-REJECTED  PIC 9      VALUE 0.
               88 NREQ-PREVIOUS-REJECTED        VALUE 1.

       01  WS-NETWORK-RESPONSE.
           05 WS-NRESP-PROXY         PIC X(256) VALUE SPACES.
           05 WS-NRESP-TOKEN         PIC X(4096) VALUE SPACES.
           05 WS-NRESP-USERNAME      PIC X(128) VALUE SPACES.
           05 WS-NRESP-PASSWORD      PIC X(128) VALUE SPACES.

      *----------------------------------------------------------------*
      * GitHub token provider (gitHubToken.getToken). The provider     *
      * program receives WS-GITHUB-TOKEN-REQ and fills                 *
      * WS-GITHUB-TOKEN-RESP; SET GTP-CANCELLED to decline.            *
      *----------------------------------------------------------------*
       01  WS-GITHUB-TOKEN-REQ.
           05 WS-GTR-REGISTRATION-ID PIC X(36)  VALUE SPACES.
           05 WS-GTR-HOST            PIC X(256) VALUE SPACES.
           05 WS-GTR-SESSION-ID      PIC X(64)  VALUE SPACES.
           05 WS-GTR-REASON          PIC X(64)  VALUE SPACES.

       01  WS-GITHUB-TOKEN-RESP.
           05 WS-GTP-CANCEL-FLAG     PIC 9      VALUE 0.
               88 GTP-CANCELLED                 VALUE 1.
               88 GTP-TOKEN                     VALUE 0.
           05 WS-GTP-ACCESS-TOKEN    PIC X(1024) VALUE SPACES.
           05 WS-GTP-TOKEN-TYPE      PIC X(32)  VALUE SPACES.
           05 WS-GTP-EXPIRES-IN      PIC 9(10)  VALUE 0.

      *----------------------------------------------------------------*
      * Shell authentication prompts (shellAuthentication.request).    *
      * The handler fills WS-SHELL-AUTH-RESP: SAP-ACCEPT with          *
      * WS-SAP-VALUE replies {"action":"accept"}, SAP-CANCEL replies   *
      * {"action":"cancel"}.                                           *
      *----------------------------------------------------------------*
       01  WS-SHELL-AUTH-REQ.
           05 WS-SAR-SESSION-ID      PIC X(64)  VALUE SPACES.
           05 WS-SAR-REQUEST-ID      PIC X(64)  VALUE SPACES.
           05 WS-SAR-PROGRAM         PIC X(128) VALUE SPACES.
           05 WS-SAR-PROMPT          PIC X(512) VALUE SPACES.
           05 WS-SAR-INPUT-KIND      PIC X(8)   VALUE SPACES.
               88 SAR-INPUT-SECRET              VALUE "secret".
               88 SAR-INPUT-TEXT                VALUE "text".
               88 SAR-INPUT-CONFIRM             VALUE "confirm".
           05 WS-SAR-COMMAND         PIC X(1024) VALUE SPACES.
           05 WS-SAR-CWD             PIC X(256) VALUE SPACES.

       01  WS-SHELL-AUTH-RESP.
           05 WS-SAP-ACTION          PIC 9      VALUE 0.
               88 SAP-ACCEPT                    VALUE 1.
               88 SAP-CANCEL                    VALUE 0.
           05 WS-SAP-VALUE           PIC X(512) VALUE SPACES.

      *----------------------------------------------------------------*
      * Memory store callback (memoryStore.store). The store program   *
      * receives WS-STORED-MEMORY (citations as raw JSON); the SDK     *
      * replies null.                                                  *
      *----------------------------------------------------------------*
       01  WS-STORED-MEMORY.
           05 WS-SM-SESSION-ID       PIC X(64)  VALUE SPACES.
           05 WS-SM-SUBJECT          PIC X(256) VALUE SPACES.
           05 WS-SM-FACT             PIC X(2048) VALUE SPACES.
           05 WS-SM-CITATIONS        PIC X(1024) VALUE SPACES.
           05 WS-SM-REASON           PIC X(512) VALUE SPACES.

      *----------------------------------------------------------------*
      * Sub-agent lifecycle hooks (hooks.invoke with hookType          *
      * "subagentStart" or "subagentStop").                            *
      *----------------------------------------------------------------*
       01  WS-SUBAGENT-HOOK-INPUT.
           05 WS-SAH-HOOK-TYPE       PIC X(16)  VALUE SPACES.
               88 SAH-SUBAGENT-START            VALUE "subagentStart".
               88 SAH-SUBAGENT-STOP             VALUE "subagentStop".
           05 WS-SAH-SESSION-ID      PIC X(64)  VALUE SPACES.
           05 WS-SAH-AGENT-NAME      PIC X(128) VALUE SPACES.
           05 WS-SAH-AGENT-DISPLAY   PIC X(128) VALUE SPACES.
           05 WS-SAH-AGENT-DESC      PIC X(512) VALUE SPACES.
           05 WS-SAH-AGENT-ID        PIC X(64)  VALUE SPACES.
           05 WS-SAH-AGENT-TYPE      PIC X(64)  VALUE SPACES.
           05 WS-SAH-STOP-REASON     PIC X(64)  VALUE SPACES.
           05 WS-SAH-RESPONSE        PIC X(2048) VALUE SPACES.

      *    subagentStart: WS-SAO-ADDITIONAL-CTX is added to the child
      *    prompt. subagentStop: SAO-BLOCK-YES with WS-SAO-REASON
      *    blocks the stop; WS-SAO-MODIFIED-RESP replaces the response.
       01  WS-SUBAGENT-HOOK-OUTPUT.
           05 WS-SAO-ADDITIONAL-CTX  PIC X(2048) VALUE SPACES.
           05 WS-SAO-BLOCK           PIC 9      VALUE 0.
               88 SAO-BLOCK-YES                 VALUE 1.
               88 SAO-BLOCK-NO                  VALUE 0.
           05 WS-SAO-REASON          PIC X(512) VALUE SPACES.
           05 WS-SAO-MODIFIED-RESP   PIC X(2048) VALUE SPACES.

      *----------------------------------------------------------------*
      * tool.shell_output event (streamed shell command output).       *
      *----------------------------------------------------------------*
       01  WS-EVT-TOOL-SHELL-OUTPUT  PIC X(32)
           VALUE "tool.shell_output".
       01  WS-SHELL-OUTPUT-EVENT.
           05 WS-SOE-TOOL-CALL-ID    PIC X(64)  VALUE SPACES.
           05 WS-SOE-TEXT            PIC X(4096) VALUE SPACES.
           05 WS-SOE-SEQUENCE        PIC 9(10)  VALUE 0.
           05 WS-SOE-STREAM          PIC X(8)   VALUE SPACES.
               88 SOE-STREAM-STDOUT             VALUE "stdout".
               88 SOE-STREAM-STDERR             VALUE "stderr".

      *----------------------------------------------------------------*
      * Policy Helper failure (PolicyHelperFailure), read from         *
      * error.data.policyHelperFailure when session.create or          *
      * session.resume fails. PHF-PRESENT tells whether one was sent.  *
      *----------------------------------------------------------------*
       01  WS-POLICY-HELPER-FAILURE.
           05 WS-PHF-FOUND           PIC 9      VALUE 0.
               88 PHF-PRESENT                   VALUE 1.
               88 PHF-ABSENT                    VALUE 0.
           05 WS-PHF-CATEGORY        PIC X(64)  VALUE SPACES.
           05 WS-PHF-FAILURE-MODE    PIC X(64)  VALUE SPACES.
           05 WS-PHF-MESSAGE         PIC X(512) VALUE SPACES.
           05 WS-PHF-PREV-RETAINED   PIC 9      VALUE 0.
               88 PHF-PREVIOUS-RETAINED         VALUE 1.
           05 WS-PHF-REASON          PIC X(256) VALUE SPACES.
           05 WS-PHF-AUTHORITY       PIC X(64)  VALUE SPACES.
           05 WS-PHF-CODE            PIC X(64)  VALUE SPACES.
           05 WS-PHF-DIAGNOSTIC      PIC X(512) VALUE SPACES.
           05 WS-PHF-HELP-LINK       PIC X(512) VALUE SPACES.

      *----------------------------------------------------------------*
      * Permission mode (session.permissions.setMode) and its result.  *
      * WS-PMR-REASON is "managed_policy" when enterprise policy       *
      * refused the change.                                            *
      *----------------------------------------------------------------*
       01  WS-PERMISSION-MODE        PIC X(32)  VALUE SPACES.
       01  WS-PERMISSION-MODE-RESULT.
           05 WS-PMR-SUCCESS         PIC 9      VALUE 0.
               88 PMR-SUCCEEDED                 VALUE 1.
           05 WS-PMR-MODE            PIC X(32)  VALUE SPACES.
           05 WS-PMR-REASON          PIC X(32)  VALUE SPACES.
               88 PMR-MANAGED-POLICY            VALUE "managed_policy".

      *----------------------------------------------------------------*
      * Auto routing tier (session.model.switchAutoTier)               *
      *----------------------------------------------------------------*
       01  WS-AUTO-TIER              PIC X(16)  VALUE SPACES.
           88 AUTO-TIER-FAST                    VALUE "fast".
           88 AUTO-TIER-EFFICIENCY              VALUE "efficiency".
           88 AUTO-TIER-BALANCE                 VALUE "balance".
           88 AUTO-TIER-INTELLIGENCE            VALUE "intelligence".

      *================================================================*
      * v3.1.0 types (Microsoft Copilot SDK 2.0.3-preview.0). The      *
      * COPILOT-CLIENT entries that take these records send            *
      * server-scoped requests that require Copilot CLI 1.0.116 or     *
      * newer; older CLIs answer with a JSON-RPC method-not-found      *
      * error (return code -1, message in WS-LAST-ERROR).              *
      *================================================================*

      *----------------------------------------------------------------*
      * MCP secret store: COPILOT-MCP-SECRETS-SAVE (mcp.secrets.save), *
      * COPILOT-MCP-SECRETS-DELETE (mcp.secrets.delete) and            *
      * COPILOT-MCP-SECRETS-DEL-SERVER (mcp.secrets.deleteServer).     *
      * WS-MSQ-VALUE is a credential: it is never logged. A blank      *
      * WS-MSQ-CONFIG-DIR is not sent (the runtime's configuration     *
      * root). After a save, MSQ-USED-KEYCHAIN tells that the OS       *
      * keychain stored the credential (usedKeychain); otherwise the   *
      * runtime used its file backend.                                 *
      *----------------------------------------------------------------*
       01  WS-MCP-SECRETS-REQ.
           05 WS-MSQ-SECRET-ID       PIC X(256)  VALUE SPACES.
           05 WS-MSQ-VALUE           PIC X(2048) VALUE SPACES.
           05 WS-MSQ-SERVER-NAME     PIC X(256)  VALUE SPACES.
           05 WS-MSQ-CONFIG-DIR      PIC X(512)  VALUE SPACES.
           05 WS-MSQ-USED-KEYCHAIN   PIC 9       VALUE 0.
               88 MSQ-USED-KEYCHAIN             VALUE 1.
               88 MSQ-USED-FILE-BACKEND         VALUE 0.

      *----------------------------------------------------------------*
      * Plugin extension approvals (user-scoped approvals for          *
      * installed plugin extensions). COPILOT-EXT-APPROVALS-GRANT      *
      * (permissions.extensionApprovals.grant) persists the pending    *
      * extension permission request WS-EAQ-REQUEST-ID of session      *
      * WS-EAQ-SESSION-ID before the host answers it.                  *
      * COPILOT-EXT-APPROVALS-LIST (permissions.extensionApprovals.    *
      * list) and COPILOT-EXT-APPROVALS-REVOKE (permissions.           *
      * extensionApprovals.revoke) send WS-EAQ-SESSION-ID only when    *
      * set (an attached session's configuration profile). Revoke      *
      * takes the approval's extensionName and installationKey         *
      * unchanged. EAQ-GRANTED and EAQ-REVOKED report the outcome; the *
      * approvals (extensionName, pluginName, capabilities, grantedAt, *
      * installationKey, installationLabel) are in the raw response.   *
      *----------------------------------------------------------------*
       01  WS-EXT-APPROVALS-REQ.
           05 WS-EAQ-SESSION-ID      PIC X(64)   VALUE SPACES.
           05 WS-EAQ-REQUEST-ID      PIC X(64)   VALUE SPACES.
           05 WS-EAQ-EXTENSION-NAME  PIC X(256)  VALUE SPACES.
           05 WS-EAQ-INSTALL-KEY     PIC X(512)  VALUE SPACES.
           05 WS-EAQ-GRANTED         PIC 9       VALUE 0.
               88 EAQ-GRANTED                   VALUE 1.
           05 WS-EAQ-REVOKED         PIC 9       VALUE 0.
               88 EAQ-REVOKED                   VALUE 1.
