      *================================================================*
      * COPILOT-SESSION.cob                                            *
      * Session management for the Copilot SDK.                        *
      * Handles session creation, message sending, event handling,     *
      * and session destruction.                                       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPILOT-SESSION.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY "COPILOT-TYPES.cpy".
       COPY "COPILOT-VERSION.cpy".

       01  WS-PARAMS-BUFFER         PIC X(4096) VALUE SPACES.
       01  WS-PARAMS-PTR            PIC 9(5)    VALUE 1.
       01  WS-RESULT-JSON           PIC X(4096) VALUE SPACES.
       01  WS-MSG-KEY               PIC X(64)   VALUE SPACES.
       01  WS-MSG-VALUE             PIC X(4096) VALUE SPACES.
       01  WS-EVENT-METHOD          PIC X(64)   VALUE SPACES.
       01  WS-WAIT-COMPLETE         PIC 9       VALUE 0.
           88 WAIT-IS-DONE                      VALUE 1.
           88 WAIT-IN-PROGRESS                  VALUE 0.

      *----------------------------------------------------------------*
      * Upstream-sync session options (parity with @github/copilot).
      * Wire keys emitted by BUILD-CREATE-PARAMS are camelCase:
      *   enableCitations, excludedBuiltinAgents,
      *   sessionLimits / maxAiCredits, memory, otlpProtocol,
      *   enableWebSocketResponses, expAssignments, mcpAuthHandler.
      *----------------------------------------------------------------*
       01  WS-UPSTREAM-CONFIG.
      *    Inline citations: enableCitations
           05 WS-ENABLE-CITATIONS   PIC 9       VALUE 0.
               88 CITATIONS-ON                  VALUE 1.
               88 CITATIONS-OFF                 VALUE 0.
      *    Excluded built-in agents (JSON array): excludedBuiltinAgents
           05 WS-EXCLUDED-BUILTIN   PIC X(1024) VALUE SPACES.
      *    Session spending limits: sessionLimits / maxAiCredits
           05 WS-MAX-AI-CREDITS     PIC 9(10)   VALUE 0.
           05 WS-MAX-AI-CREDITS-ED  PIC Z(9)9.
      *    Session memory config: MemoryConfiguration / memory_config
           05 WS-MEMORY-ENABLED     PIC 9       VALUE 0.
               88 MEMORY-ON                     VALUE 1.
               88 MEMORY-OFF                    VALUE 0.
      *    OTLP telemetry protocol: otlpProtocol
           05 WS-OTLP-PROTOCOL      PIC X(16)   VALUE SPACES.
      *    WebSocket transport: enableWebSocketResponses
           05 WS-ENABLE-WEBSOCKET   PIC 9       VALUE 0.
               88 WEBSOCKET-ON                  VALUE 1.
               88 WEBSOCKET-OFF                 VALUE 0.
      *    Experiment assignments (JSON object): expAssignments
           05 WS-EXP-ASSIGNMENTS    PIC X(1024) VALUE SPACES.
      *    MCP OAuth token handler: mcpAuthHandler / on_mcp_auth_request
           05 WS-MCP-AUTH-HANDLER   PIC 9       VALUE 0.
               88 MCP-AUTH-ON                   VALUE 1.
               88 MCP-AUTH-OFF                  VALUE 0.
      *    Session rewind: rewindEnabled
           05 WS-REWIND-ENABLED     PIC 9       VALUE 0.
               88 REWIND-ON                     VALUE 1.
               88 REWIND-OFF                    VALUE 0.
      *    Additional session directories (JSON array): additionalDirectories
           05 WS-ADDITIONAL-DIRS    PIC X(1024) VALUE SPACES.
      *    Disabled MCP servers (JSON array): disabledMcpServers
           05 WS-DISABLED-MCP       PIC X(1024) VALUE SPACES.
      *    GitHub MCP tool config (JSON object): githubMcpToolConfig
           05 WS-GITHUB-MCP-CONFIG  PIC X(1024) VALUE SPACES.
      *    Canvas provider (JSON object): canvasProvider
           05 WS-CANVAS-PROVIDER    PIC X(1024) VALUE SPACES.
      *    Custom agents local-only: customAgentsLocalOnly
           05 WS-CUSTOM-AGENTS-LCL  PIC 9       VALUE 0.
               88 CUSTOM-AGENTS-LOCAL-ON        VALUE 1.
               88 CUSTOM-AGENTS-LOCAL-OFF       VALUE 0.
      *    Permission-reply decision context (JSON object): decisionContext
           05 WS-DECISION-CONTEXT   PIC X(1024) VALUE SPACES.
      *    User-prompt-transformed hook: userPromptTransformed
           05 WS-USER-PROMPT-XFORM  PIC 9       VALUE 0.
               88 USER-PROMPT-XFORM-ON          VALUE 1.
               88 USER-PROMPT-XFORM-OFF         VALUE 0.
      *    Built-in plugin directories (JSON array): builtinPluginDirectories
           05 WS-BUILTIN-PLUGIN-DIR PIC X(1024) VALUE SPACES.
      *    Agent-factory authoring args schema (JSON object): argsSchema
           05 WS-ARGS-SCHEMA        PIC X(1024) VALUE SPACES.
      *    Reasoning effort control: reasoningEffort
           05 WS-REASONING-EFFORT   PIC X(16)   VALUE SPACES.
      *    Tool search configuration (JSON object): toolSearch
           05 WS-TOOL-SEARCH        PIC X(1024) VALUE SPACES.
      *    In-process FFI transport: inProcess
           05 WS-IN-PROCESS         PIC 9       VALUE 0.
               88 IN-PROCESS-ON                 VALUE 1.
               88 IN-PROCESS-OFF                VALUE 0.
      *    Experimental mode: experimentalMode
           05 WS-EXPERIMENTAL-MODE  PIC 9       VALUE 0.
               88 EXPERIMENTAL-ON               VALUE 1.
               88 EXPERIMENTAL-OFF              VALUE 0.
      *    Content exclusion: contentExclusion
           05 WS-CONTENT-EXCLUSION  PIC 9       VALUE 0.
               88 CONTENT-EXCLUSION-ON          VALUE 1.
               88 CONTENT-EXCLUSION-OFF         VALUE 0.

      *----------------------------------------------------------------*
      * Upstream-sync host callbacks.  The host enables a callback by
      * setting the flag; the runtime is then notified.  Conventions
      * mirror @github/copilot-sdk:
      *   bearer_token_provider / get_bearer_token,
      *   request_handler / CopilotRequestHandler,
      *   on_post_tool_use / post_tool_use,
      *   on_pre_mcp_tool_call / pre_mcp_tool_call.
      *----------------------------------------------------------------*
       01  WS-UPSTREAM-HANDLERS.
           05 WS-BEARER-PROVIDER    PIC 9       VALUE 0.
               88 BEARER-PROVIDER-ON            VALUE 1.
               88 BEARER-PROVIDER-OFF           VALUE 0.
           05 WS-REQUEST-HANDLER    PIC 9       VALUE 0.
               88 REQUEST-HANDLER-ON            VALUE 1.
               88 REQUEST-HANDLER-OFF           VALUE 0.
           05 WS-POST-TOOL-USE      PIC 9       VALUE 0.
               88 POST-TOOL-USE-ON              VALUE 1.
               88 POST-TOOL-USE-OFF             VALUE 0.
           05 WS-PRE-MCP-TOOL-CALL  PIC 9       VALUE 0.
               88 PRE-MCP-TOOL-CALL-ON          VALUE 1.
               88 PRE-MCP-TOOL-CALL-OFF         VALUE 0.

      *----------------------------------------------------------------*
      * Per-message send options (upstream-sync): agentMode,
      * displayPrompt, requestHeaders.
      *----------------------------------------------------------------*
       01  WS-SEND-OPTIONS-EXT.
           05 WS-SEND-AGENT-MODE    PIC X(32)   VALUE SPACES.
           05 WS-SEND-DISPLAY-PROMPT PIC X(4096) VALUE SPACES.
           05 WS-SEND-REQ-HEADERS   PIC X(1024) VALUE SPACES.

      *----------------------------------------------------------------*
      * Tool defer-loading policy: ToolDefer / toolDefer
      *----------------------------------------------------------------*
       01  WS-TOOL-DEFER            PIC X(8)    VALUE SPACES.
           88 TOOL-DEFER-AUTO                   VALUE "auto".
           88 TOOL-DEFER-NEVER                  VALUE "never".

      *----------------------------------------------------------------*
      * System-message section identifiers
      *----------------------------------------------------------------*
       01  WS-SECTION-IDS.
           05 WS-SECTION-PREAMBLE   PIC X(16)   VALUE "preamble".
           05 WS-SECTION-IDENTITY   PIC X(16)   VALUE "identity".
           05 WS-SECTION-PRESERVE   PIC X(16)   VALUE "preserve".

      *----------------------------------------------------------------*
      * GitHub-anchored attachment variants
      *----------------------------------------------------------------*
       01  WS-GITHUB-ATTACH.
           05 WS-GH-COMMIT       PIC X(16) VALUE "GitHubCommit".
           05 WS-GH-RELEASE      PIC X(16) VALUE "GitHubRelease".
           05 WS-GH-REPOSITORY   PIC X(16) VALUE "GitHubRepository".
           05 WS-GH-URL          PIC X(16) VALUE "GitHubUrl".

       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
      * CREATE-SESSION: Initialize a new Copilot session.              *
      * Input:  WS-SESSION-CONFIG, WS-CLIENT-STATE                     *
      * Output: WS-SESSION-STATE, WS-RETURN-CODE                      *
      *----------------------------------------------------------------*
       CREATE-SESSION.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Client is not active" TO WS-LAST-ERROR
               GO TO CREATE-SESSION-EXIT
           END-IF

           PERFORM BUILD-CREATE-PARAMS
           MOVE "session.create" TO WS-REQ-METHOD
           ADD 1 TO WS-REQUEST-ID
           MOVE WS-REQUEST-ID TO WS-REQ-ID
           MOVE WS-PARAMS-BUFFER TO WS-REQ-PARAMS

           CALL "COPILOT-JSONRPC"
           PERFORM SEND-RPC-REQUEST

           IF WS-RETURN-CODE NOT = 0
               GO TO CREATE-SESSION-EXIT
           END-IF

           PERFORM EXTRACT-SESSION-ID
           SET SESSION-IS-ACTIVE TO TRUE
           MOVE 0 TO WS-TURN-COUNT
           MOVE 0 TO WS-RETURN-CODE
           .
       CREATE-SESSION-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * BUILD-CREATE-PARAMS: Build the JSON parameters for session     *
      * creation including agent info and system prompt.               *
      *----------------------------------------------------------------*
       BUILD-CREATE-PARAMS.
           MOVE SPACES TO WS-PARAMS-BUFFER
           MOVE 1 TO WS-PARAMS-PTR

           STRING
               '{"agent":{"name":"'
               FUNCTION TRIM(WS-SESS-AGENT-NAME)
               '","version":"'
               FUNCTION TRIM(WS-SESS-AGENT-VER)
               '","sdkName":"'
               WS-SDK-NAME
               '","sdkVersion":"'
               FUNCTION TRIM(WS-SDK-VERSION)
               '","protocolVersion":"'
               FUNCTION TRIM(WS-PROTO-STRING)
               '"}'
               DELIMITED SIZE
               INTO WS-PARAMS-BUFFER
               WITH POINTER WS-PARAMS-PTR
           END-STRING

           IF WS-SESS-SYS-PROMPT NOT = SPACES
               STRING
                   ',"systemPrompt":"'
                   FUNCTION TRIM(WS-SESS-SYS-PROMPT)
                   '"'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF STREAMING-ON
               STRING
                   ',"capabilities":{"streaming":true}'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

      *    Upstream-sync session-config passthroughs (camelCase keys).
           IF CITATIONS-ON
               STRING
                   ',"enableCitations":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-EXCLUDED-BUILTIN NOT = SPACES
               STRING
                   ',"excludedBuiltinAgents":'
                   FUNCTION TRIM(WS-EXCLUDED-BUILTIN)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-MAX-AI-CREDITS > 0
               MOVE WS-MAX-AI-CREDITS TO WS-MAX-AI-CREDITS-ED
               STRING
                   ',"sessionLimits":{"maxAiCredits":'
                   FUNCTION TRIM(WS-MAX-AI-CREDITS-ED)
                   '}'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF MEMORY-ON
               STRING
                   ',"memory":{"enabled":true}'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-OTLP-PROTOCOL NOT = SPACES
               STRING
                   ',"otlpProtocol":"'
                   FUNCTION TRIM(WS-OTLP-PROTOCOL)
                   '"'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WEBSOCKET-ON
               STRING
                   ',"enableWebSocketResponses":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-EXP-ASSIGNMENTS NOT = SPACES
               STRING
                   ',"expAssignments":'
                   FUNCTION TRIM(WS-EXP-ASSIGNMENTS)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF MCP-AUTH-ON
               STRING
                   ',"mcpAuthHandler":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

      *    Newly-synced session-config passthroughs (camelCase keys).
           IF REWIND-ON
               STRING
                   ',"rewindEnabled":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-ADDITIONAL-DIRS NOT = SPACES
               STRING
                   ',"additionalDirectories":'
                   FUNCTION TRIM(WS-ADDITIONAL-DIRS)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-DISABLED-MCP NOT = SPACES
               STRING
                   ',"disabledMcpServers":'
                   FUNCTION TRIM(WS-DISABLED-MCP)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-GITHUB-MCP-CONFIG NOT = SPACES
               STRING
                   ',"githubMcpToolConfig":'
                   FUNCTION TRIM(WS-GITHUB-MCP-CONFIG)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-CANVAS-PROVIDER NOT = SPACES
               STRING
                   ',"canvasProvider":'
                   FUNCTION TRIM(WS-CANVAS-PROVIDER)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF CUSTOM-AGENTS-LOCAL-ON
               STRING
                   ',"customAgentsLocalOnly":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-DECISION-CONTEXT NOT = SPACES
               STRING
                   ',"decisionContext":'
                   FUNCTION TRIM(WS-DECISION-CONTEXT)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF USER-PROMPT-XFORM-ON
               STRING
                   ',"userPromptTransformed":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-BUILTIN-PLUGIN-DIR NOT = SPACES
               STRING
                   ',"builtinPluginDirectories":'
                   FUNCTION TRIM(WS-BUILTIN-PLUGIN-DIR)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-ARGS-SCHEMA NOT = SPACES
               STRING
                   ',"argsSchema":'
                   FUNCTION TRIM(WS-ARGS-SCHEMA)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-REASONING-EFFORT NOT = SPACES
               STRING
                   ',"reasoningEffort":"'
                   FUNCTION TRIM(WS-REASONING-EFFORT)
                   '"'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-TOOL-SEARCH NOT = SPACES
               STRING
                   ',"toolSearch":'
                   FUNCTION TRIM(WS-TOOL-SEARCH)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF IN-PROCESS-ON
               STRING
                   ',"inProcess":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF EXPERIMENTAL-ON
               STRING
                   ',"experimentalMode":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF CONTENT-EXCLUSION-ON
               STRING
                   ',"contentExclusion":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           STRING
               '}'
               DELIMITED SIZE
               INTO WS-PARAMS-BUFFER
               WITH POINTER WS-PARAMS-PTR
           END-STRING
           .

      *----------------------------------------------------------------*
      * EXTRACT-SESSION-ID: Pull session ID from create response.      *
      *----------------------------------------------------------------*
       EXTRACT-SESSION-ID.
           MOVE SPACES TO WS-SESSION-ID
           MOVE WS-RESP-RESULT TO WS-RESULT-JSON

           MOVE SPACES TO WS-MSG-VALUE
           MOVE '"sessionId"' TO WS-MSG-KEY
           PERFORM FIND-VALUE-IN-RESULT

           IF WS-MSG-VALUE NOT = SPACES
               MOVE WS-MSG-VALUE TO WS-SESSION-ID
           END-IF
           .

      *----------------------------------------------------------------*
      * SEND-MESSAGE: Send a user message and wait for response.       *
      * Input:  WS-SEND-OPTIONS, WS-SESSION-STATE                     *
      * Output: WS-SEND-RESULT, WS-RETURN-CODE                        *
      *----------------------------------------------------------------*
       SEND-MESSAGE.
           IF NOT SESSION-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Session is not active" TO WS-LAST-ERROR
               GO TO SEND-MESSAGE-EXIT
           END-IF

           PERFORM BUILD-SEND-PARAMS
           MOVE "session.sendMessage" TO WS-REQ-METHOD
           ADD 1 TO WS-REQUEST-ID
           MOVE WS-REQUEST-ID TO WS-REQ-ID
           MOVE WS-PARAMS-BUFFER TO WS-REQ-PARAMS

           CALL "COPILOT-JSONRPC"
           PERFORM SEND-RPC-REQUEST

           IF WS-RETURN-CODE NOT = 0
               SET RESULT-HAS-ERROR TO TRUE
               MOVE WS-LAST-ERROR TO WS-RESULT-ERROR-MSG
               GO TO SEND-MESSAGE-EXIT
           END-IF

           ADD 1 TO WS-TURN-COUNT

           PERFORM WAIT-FOR-RESPONSE

           .
       SEND-MESSAGE-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * BUILD-SEND-PARAMS: Build params for sendMessage call.          *
      *----------------------------------------------------------------*
       BUILD-SEND-PARAMS.
           MOVE SPACES TO WS-PARAMS-BUFFER
           MOVE 1 TO WS-PARAMS-PTR

           STRING
               '{"sessionId":"'
               FUNCTION TRIM(WS-SESSION-ID)
               '","message":"'
               FUNCTION TRIM(WS-SEND-MESSAGE)
               '"'
               DELIMITED SIZE
               INTO WS-PARAMS-BUFFER
               WITH POINTER WS-PARAMS-PTR
           END-STRING

      *    Upstream-sync per-message send options (camelCase keys).
           IF WS-SEND-AGENT-MODE NOT = SPACES
               STRING
                   ',"agentMode":"'
                   FUNCTION TRIM(WS-SEND-AGENT-MODE)
                   '"'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-SEND-DISPLAY-PROMPT NOT = SPACES
               STRING
                   ',"displayPrompt":"'
                   FUNCTION TRIM(WS-SEND-DISPLAY-PROMPT)
                   '"'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           IF WS-SEND-REQ-HEADERS NOT = SPACES
               STRING
                   ',"requestHeaders":'
                   FUNCTION TRIM(WS-SEND-REQ-HEADERS)
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

           STRING
               '}'
               DELIMITED SIZE
               INTO WS-PARAMS-BUFFER
               WITH POINTER WS-PARAMS-PTR
           END-STRING
           .

      *----------------------------------------------------------------*
      * WAIT-FOR-RESPONSE: Read events until assistant.message or      *
      * an error event is received.                                    *
      *----------------------------------------------------------------*
       WAIT-FOR-RESPONSE.
           SET WAIT-IN-PROGRESS TO TRUE
           MOVE SPACES TO WS-RESULT-MESSAGE
           MOVE 0      TO WS-RESULT-MSG-LEN
           MOVE 0      TO WS-RESULT-IS-ERROR
           MOVE SPACES TO WS-RESULT-ERROR-MSG

           PERFORM READ-NEXT-EVENT
               UNTIL WAIT-IS-DONE
           .

      *----------------------------------------------------------------*
      * READ-NEXT-EVENT: Read and process a single event.              *
      *----------------------------------------------------------------*
       READ-NEXT-EVENT.
           PERFORM RECEIVE-FROM-CLI

           IF WS-RETURN-CODE NOT = 0
               SET WAIT-IS-DONE TO TRUE
               SET RESULT-HAS-ERROR TO TRUE
               MOVE "Read error from CLI"
                   TO WS-RESULT-ERROR-MSG
               EXIT PARAGRAPH
           END-IF

           PERFORM PARSE-EVENT-TYPE

           EVALUATE TRUE
               WHEN WS-EVENT-METHOD =
                   "session.assistant.message"
                   PERFORM HANDLE-ASSISTANT-MESSAGE
                   SET WAIT-IS-DONE TO TRUE
               WHEN WS-EVENT-METHOD =
                   "session.assistant.message_delta"
                   PERFORM HANDLE-MESSAGE-DELTA
               WHEN WS-EVENT-METHOD =
                   "session.turn.end"
                   SET WAIT-IS-DONE TO TRUE
               WHEN WS-EVENT-METHOD =
                   "session.error"
                   PERFORM HANDLE-ERROR-EVENT
                   SET WAIT-IS-DONE TO TRUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * PARSE-EVENT-TYPE: Extract the method from a notification.      *
      *----------------------------------------------------------------*
       PARSE-EVENT-TYPE.
           MOVE SPACES TO WS-EVENT-METHOD
           MOVE '"method"' TO WS-MSG-KEY
           MOVE WS-RESP-RESULT TO WS-RESULT-JSON
           PERFORM FIND-VALUE-IN-RESULT
           MOVE WS-MSG-VALUE TO WS-EVENT-METHOD
           .

      *----------------------------------------------------------------*
      * HANDLE-ASSISTANT-MESSAGE: Extract final assistant message.     *
      *----------------------------------------------------------------*
       HANDLE-ASSISTANT-MESSAGE.
           MOVE '"message"' TO WS-MSG-KEY
           PERFORM FIND-VALUE-IN-RESULT
           IF WS-MSG-VALUE NOT = SPACES
               MOVE WS-MSG-VALUE TO WS-RESULT-MESSAGE
               MOVE FUNCTION LENGTH(
                   FUNCTION TRIM(WS-MSG-VALUE TRAILING))
                   TO WS-RESULT-MSG-LEN
               SET RESULT-IS-OK TO TRUE
           END-IF
           .

      *----------------------------------------------------------------*
      * HANDLE-MESSAGE-DELTA: Accumulate streaming delta text.         *
      *----------------------------------------------------------------*
       HANDLE-MESSAGE-DELTA.
           MOVE '"delta"' TO WS-MSG-KEY
           PERFORM FIND-VALUE-IN-RESULT
           IF WS-MSG-VALUE NOT = SPACES
               STRING
                   FUNCTION TRIM(WS-RESULT-MESSAGE)
                   WS-MSG-VALUE
                   DELIMITED SIZE
                   INTO WS-RESULT-MESSAGE
               END-STRING
           END-IF
           .

      *----------------------------------------------------------------*
      * HANDLE-ERROR-EVENT: Process error notification.                *
      *----------------------------------------------------------------*
       HANDLE-ERROR-EVENT.
           SET RESULT-HAS-ERROR TO TRUE
           MOVE '"message"' TO WS-MSG-KEY
           PERFORM FIND-VALUE-IN-RESULT
           MOVE WS-MSG-VALUE TO WS-RESULT-ERROR-MSG
           .

      *----------------------------------------------------------------*
      * DESTROY-SESSION: Close and clean up a session.                 *
      * Input:  WS-SESSION-STATE                                       *
      * Output: WS-RETURN-CODE                                        *
      *----------------------------------------------------------------*
       DESTROY-SESSION.
           IF NOT SESSION-IS-ACTIVE
               MOVE 0 TO WS-RETURN-CODE
               GO TO DESTROY-SESSION-EXIT
           END-IF

           MOVE SPACES TO WS-PARAMS-BUFFER
           MOVE 1 TO WS-PARAMS-PTR

           STRING
               '{"sessionId":"'
               FUNCTION TRIM(WS-SESSION-ID)
               '"}'
               DELIMITED SIZE
               INTO WS-PARAMS-BUFFER
               WITH POINTER WS-PARAMS-PTR
           END-STRING

           MOVE "session.destroy" TO WS-REQ-METHOD
           ADD 1 TO WS-REQUEST-ID
           MOVE WS-REQUEST-ID TO WS-REQ-ID
           MOVE WS-PARAMS-BUFFER TO WS-REQ-PARAMS

           CALL "COPILOT-JSONRPC"
           PERFORM SEND-RPC-REQUEST

           SET SESSION-IS-CLOSED TO TRUE
           MOVE SPACES TO WS-SESSION-ID
           MOVE 0 TO WS-RETURN-CODE
           .
       DESTROY-SESSION-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * FIND-VALUE-IN-RESULT: Simple key-value extractor.              *
      * Input:  WS-MSG-KEY, WS-RESULT-JSON                            *
      * Output: WS-MSG-VALUE                                          *
      *----------------------------------------------------------------*
       FIND-VALUE-IN-RESULT.
           MOVE SPACES TO WS-MSG-VALUE
           INSPECT WS-RESULT-JSON TALLYING WS-PARAMS-PTR
               FOR CHARACTERS BEFORE INITIAL WS-MSG-KEY
           IF WS-PARAMS-PTR > 0
               ADD FUNCTION LENGTH(
                   FUNCTION TRIM(WS-MSG-KEY)) TO WS-PARAMS-PTR
               ADD 2 TO WS-PARAMS-PTR
               UNSTRING WS-RESULT-JSON
                   DELIMITED '"'
                   INTO WS-MSG-VALUE
                   WITH POINTER WS-PARAMS-PTR
               END-UNSTRING
           END-IF
           MOVE 1 TO WS-PARAMS-PTR
           .

      *----------------------------------------------------------------*
      * SEND-RPC-REQUEST: Stub for sending data to CLI process.        *
      * In production, this CALLs the client I/O layer.                *
      *----------------------------------------------------------------*
       SEND-RPC-REQUEST.
           CALL "COPILOT-CLIENT" USING WS-JSONRPC-REQUEST
               WS-JSONRPC-RESPONSE WS-RETURN-CODE
           .

      *----------------------------------------------------------------*
      * RECEIVE-FROM-CLI: Stub for receiving data from CLI process.    *
      *----------------------------------------------------------------*
       RECEIVE-FROM-CLI.
           CALL "COPILOT-CLIENT" USING WS-JSONRPC-RESPONSE
               WS-RETURN-CODE
           .

       STOP RUN.
