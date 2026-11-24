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
      * Session options (WS-UPSTREAM-CONFIG, WS-V4-SESSION-OPTIONS)    *
      * and the protocol 4 payload builder work fields.                *
      *----------------------------------------------------------------*
       COPY "COPILOT-SESSION-OPTS.cpy".

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
      * displayPrompt, requestHeaders. SDK protocol 4 adds the message
      * source (MessageSource: "user", "system" or "agent-<id>"), an
      * opaque clientTurnId and a JSON schema (responseSchema) that is
      * sent as a json_schema responseFormat for structured output.
      *----------------------------------------------------------------*
       01  WS-SEND-OPTIONS-EXT.
           05 WS-SEND-AGENT-MODE    PIC X(32)   VALUE SPACES.
           05 WS-SEND-DISPLAY-PROMPT PIC X(4096) VALUE SPACES.
           05 WS-SEND-REQ-HEADERS   PIC X(1024) VALUE SPACES.
           05 WS-SEND-SOURCE        PIC X(64)   VALUE SPACES.
               88 SEND-SOURCE-USER              VALUE "user".
               88 SEND-SOURCE-SYSTEM            VALUE "system".
           05 WS-SEND-CLIENT-TURN-ID PIC X(64)  VALUE SPACES.
           05 WS-SEND-RESPONSE-SCHEMA PIC X(2048) VALUE SPACES.

      *----------------------------------------------------------------*
      * SDK protocol 4 message handling: session.event notifications,  *
      * server-to-client requests and their replies.                   *
      *----------------------------------------------------------------*
       01  WS-EVENT-TYPE            PIC X(64)   VALUE SPACES.
       01  WS-RPC-IS-REQUEST        PIC 9       VALUE 0.
       01  WS-RPC-ID-TEXT           PIC X(64)   VALUE SPACES.
       01  WS-RPC-ID-ED             PIC Z(9)9.
       01  WS-REPLY-IS-ERROR        PIC 9       VALUE 0.
       01  WS-REPLY-ERROR-CODE      PIC S9(6)   VALUE -32601.
       01  WS-REPLY-CODE-ED         PIC -(6)9.
       01  WS-REPLY-ERROR-MSG       PIC X(128)  VALUE SPACES.
       01  WS-OUTBOUND-MSG          PIC X(8192) VALUE SPACES.
       01  WS-INBOUND-MSG           PIC X(8192) VALUE SPACES.
       01  WS-AWAIT-ID-ED           PIC Z(9)9.
       01  WS-OUTBOUND-PTR          PIC 9(5)    VALUE 1.
       01  WS-WRITE-RC              PIC S9(4)   VALUE 0.
       01  WS-NET-PGM-NAME          PIC X(30)   VALUE SPACES.
       01  WS-TOOL-REQUEST-ID       PIC X(64)   VALUE SPACES.

      *----------------------------------------------------------------*
      * JSON reader work fields (JSON-FIND-KEY and friends)            *
      *----------------------------------------------------------------*
       01  WS-JS-DOC                PIC X(8192) VALUE SPACES.
       01  WS-JS-KEY                PIC X(48)   VALUE SPACES.
       01  WS-JS-KEY-LEN            PIC 9(3)    VALUE 0.
       01  WS-JS-DEPTH              PIC 9(3)    VALUE 1.
       01  WS-JS-LEVEL              PIC S9(4)   VALUE 0.
       01  WS-JS-POS                PIC 9(5)    VALUE 0.
       01  WS-JS-END                PIC 9(5)    VALUE 0.
       01  WS-JS-I                  PIC 9(5)    VALUE 0.
       01  WS-JS-J                  PIC 9(5)    VALUE 0.
       01  WS-JS-CH                 PIC X       VALUE SPACE.
       01  WS-JS-IN-STR             PIC 9       VALUE 0.
       01  WS-JS-VALUE              PIC X(4096) VALUE SPACES.
       01  WS-JS-VALUE-LEN          PIC 9(5)    VALUE 0.

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
           IF PB-FAILED
               GO TO CREATE-SESSION-EXIT
           END-IF
           MOVE "session.create" TO WS-REQ-METHOD
           ADD 1 TO WS-REQUEST-ID
           MOVE WS-REQUEST-ID TO WS-REQ-ID
           MOVE WS-PARAMS-BUFFER TO WS-REQ-PARAMS

           PERFORM SEND-RPC-REQUEST

           IF WS-RETURN-CODE NOT = 0
               GO TO CREATE-SESSION-EXIT
           END-IF
           PERFORM CHECK-RPC-ERROR
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
      * BUILD-CREATE-PARAMS: Build the session.create params. The      *
      * flat session options are translated to the SDK protocol 4      *
      * grouped payload by BUILD-SESSION-PARAMS (COPILOT-PARAMS.cpy).  *
      *----------------------------------------------------------------*
       BUILD-CREATE-PARAMS.
           SET PB-FOR-CREATE TO TRUE
           PERFORM BUILD-SESSION-PARAMS
           .

      *----------------------------------------------------------------*
      * RESUME-SESSION: Resume the session in WS-SESSION-ID with the   *
      * current session options (protocol 4 grouped payload). Set      *
      * RESUME-EVENT-SUPPRESSED, RESUME-CONTINUE-PENDING or            *
      * RESUME-TRANSCRIPT-RECOVERY for the resume-only options.        *
      * Output: WS-SESSION-STATE, WS-RETURN-CODE                       *
      *----------------------------------------------------------------*
       RESUME-SESSION.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Client is not active" TO WS-LAST-ERROR
               GO TO RESUME-SESSION-EXIT
           END-IF

           PERFORM BUILD-RESUME-PARAMS
           IF PB-FAILED
               GO TO RESUME-SESSION-EXIT
           END-IF
           MOVE "session.resume" TO WS-REQ-METHOD
           ADD 1 TO WS-REQUEST-ID
           MOVE WS-REQUEST-ID TO WS-REQ-ID
           MOVE WS-PARAMS-BUFFER TO WS-REQ-PARAMS
           PERFORM SEND-RPC-REQUEST

           IF WS-RETURN-CODE NOT = 0
               GO TO RESUME-SESSION-EXIT
           END-IF
           PERFORM CHECK-RPC-ERROR
           IF WS-RETURN-CODE NOT = 0
               GO TO RESUME-SESSION-EXIT
           END-IF

           SET SESSION-IS-ACTIVE TO TRUE
           MOVE 0 TO WS-RETURN-CODE
           .
       RESUME-SESSION-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * BUILD-RESUME-PARAMS: Build the session.resume params through   *
      * the same protocol 4 translation (create-only members dropped). *
      *----------------------------------------------------------------*
       BUILD-RESUME-PARAMS.
           SET PB-FOR-RESUME TO TRUE
           PERFORM BUILD-SESSION-PARAMS
           .

      *----------------------------------------------------------------*
      * EXTRACT-SESSION-ID: Pull session ID from create response.      *
      *----------------------------------------------------------------*
       EXTRACT-SESSION-ID.
           MOVE SPACES TO WS-SESSION-ID
           MOVE WS-INBOUND-MSG TO WS-JS-DOC
           MOVE "sessionId" TO WS-JS-KEY
           MOVE 2 TO WS-JS-DEPTH
           PERFORM JSON-STRING-VALUE
           IF WS-JS-VALUE = SPACES
               MOVE 1 TO WS-JS-DEPTH
               PERFORM JSON-STRING-VALUE
           END-IF

           IF WS-JS-VALUE NOT = SPACES
               MOVE WS-JS-VALUE TO WS-SESSION-ID
           END-IF
           .

      *----------------------------------------------------------------*
      * CHECK-RPC-ERROR: A JSON-RPC error response (top-level "error") *
      * sets WS-RETURN-CODE -1 and copies error.message to             *
      * WS-LAST-ERROR. A Policy Helper failure in                      *
      * error.data.policyHelperFailure fills WS-POLICY-HELPER-FAILURE. *
      *----------------------------------------------------------------*
       CHECK-RPC-ERROR.
           INITIALIZE WS-POLICY-HELPER-FAILURE
           MOVE WS-INBOUND-MSG TO WS-JS-DOC
           MOVE "error" TO WS-JS-KEY
           MOVE 1 TO WS-JS-DEPTH
           PERFORM JSON-FIND-KEY
           IF WS-JS-POS = 0
               EXIT PARAGRAPH
           END-IF
           MOVE -1 TO WS-RETURN-CODE
           MOVE "message" TO WS-JS-KEY
           MOVE 2 TO WS-JS-DEPTH
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-LAST-ERROR
           PERFORM PARSE-POLICY-HELPER-FAILURE
           .

      *----------------------------------------------------------------*
      * PARSE-POLICY-HELPER-FAILURE: Read                              *
      * error.data.policyHelperFailure (PolicyHelperFailure) into      *
      * WS-POLICY-HELPER-FAILURE; PHF-PRESENT when one was sent.       *
      *----------------------------------------------------------------*
       PARSE-POLICY-HELPER-FAILURE.
           MOVE "policyHelperFailure" TO WS-JS-KEY
           MOVE 3 TO WS-JS-DEPTH
           PERFORM JSON-FIND-KEY
           IF WS-JS-POS = 0
               EXIT PARAGRAPH
           END-IF
           SET PHF-PRESENT TO TRUE
           MOVE 4 TO WS-JS-DEPTH
           MOVE "category" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-PHF-CATEGORY
           MOVE "failureMode" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-PHF-FAILURE-MODE
           MOVE "message" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-PHF-MESSAGE
           MOVE "reason" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-PHF-REASON
           MOVE "authority" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-PHF-AUTHORITY
           MOVE "code" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-PHF-CODE
           MOVE "diagnostic" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-PHF-DIAGNOSTIC
           MOVE "helpLink" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           MOVE WS-JS-VALUE TO WS-PHF-HELP-LINK
           MOVE "previousPolicyRetained" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           IF WS-JS-VALUE(1:4) = "true"
               SET PHF-PREVIOUS-RETAINED TO TRUE
           END-IF
           .

      *----------------------------------------------------------------*
      * SEND-MESSAGE: Send a user message and wait for response.       *
      * SDK protocol 4: session.send returns once the message is       *
      * admitted, then the reply streams in as session.event           *
      * notifications until session.idle.                              *
      * Input:  WS-SEND-OPTIONS, WS-SEND-OPTIONS-EXT, WS-SESSION-STATE *
      * Output: WS-SEND-RESULT, WS-RETURN-CODE                        *
      *----------------------------------------------------------------*
       SEND-MESSAGE.
           IF NOT SESSION-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Session is not active" TO WS-LAST-ERROR
               GO TO SEND-MESSAGE-EXIT
           END-IF

           SET WAIT-IN-PROGRESS TO TRUE
           MOVE SPACES TO WS-RESULT-MESSAGE
           MOVE 0      TO WS-RESULT-MSG-LEN
           MOVE 0      TO WS-RESULT-IS-ERROR
           MOVE SPACES TO WS-RESULT-ERROR-MSG

           PERFORM BUILD-SEND-PARAMS
           MOVE "session.send" TO WS-REQ-METHOD
           ADD 1 TO WS-REQUEST-ID
           MOVE WS-REQUEST-ID TO WS-REQ-ID
           MOVE WS-PARAMS-BUFFER TO WS-REQ-PARAMS

           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RPC-ERROR
           END-IF

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
      * BUILD-SEND-PARAMS: Build params for the session.send call.     *
      *----------------------------------------------------------------*
       BUILD-SEND-PARAMS.
           MOVE SPACES TO WS-PARAMS-BUFFER
           MOVE 1 TO WS-PARAMS-PTR
           MOVE 0 TO WS-PB-ROOT-COUNT
           MOVE "{" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR

           MOVE "sessionId" TO WS-PB-KEY
           MOVE WS-SESSION-ID TO WS-PB-TEXT
           PERFORM PB-ROOT-STRING
      *    Protocol 4 takes the message text as "prompt".
           MOVE "prompt" TO WS-PB-KEY
           MOVE WS-SEND-MESSAGE TO WS-PB-TEXT
           PERFORM PB-ROOT-STRING

      *    Message source (MessageSource): "user", "system" or
      *    "agent-<id>".
           IF WS-SEND-SOURCE NOT = SPACES
               MOVE "source" TO WS-PB-KEY
               MOVE WS-SEND-SOURCE TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
           END-IF

           IF WS-SEND-CLIENT-TURN-ID NOT = SPACES
               MOVE "clientTurnId" TO WS-PB-KEY
               MOVE WS-SEND-CLIENT-TURN-ID TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
           END-IF

      *    Structured output: the responseSchema JSON schema is sent
      *    as a json_schema responseFormat.
           IF WS-SEND-RESPONSE-SCHEMA NOT = SPACES
               MOVE "responseFormat" TO WS-PB-KEY
               PERFORM PB-ROOT-KEY
               STRING '{"type":"json_schema","jsonSchema":'
                   '{"name":"response","strict":true,"schema":'
                   FUNCTION TRIM(WS-SEND-RESPONSE-SCHEMA)
                   '}}'
                   DELIMITED SIZE INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

      *    Upstream-sync per-message send options (camelCase keys).
           IF WS-SEND-AGENT-MODE NOT = SPACES
               MOVE "agentMode" TO WS-PB-KEY
               MOVE WS-SEND-AGENT-MODE TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
           END-IF

           IF WS-SEND-DISPLAY-PROMPT NOT = SPACES
               MOVE "displayPrompt" TO WS-PB-KEY
               MOVE WS-SEND-DISPLAY-PROMPT TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
           END-IF

           IF WS-SEND-REQ-HEADERS NOT = SPACES
               MOVE "requestHeaders" TO WS-PB-KEY
               MOVE WS-SEND-REQ-HEADERS TO WS-PB-TEXT
               PERFORM PB-ROOT-RAW
           END-IF

           MOVE "}" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           .

      *----------------------------------------------------------------*
      * WAIT-FOR-RESPONSE: Read messages until session.idle or an      *
      * error event. Server requests are answered on the way.          *
      *----------------------------------------------------------------*
       WAIT-FOR-RESPONSE.
           PERFORM READ-NEXT-EVENT
               UNTIL WAIT-IS-DONE
           .

      *----------------------------------------------------------------*
      * READ-NEXT-EVENT: Read and process a single message.            *
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

           PERFORM PROCESS-INBOUND
           .

      *----------------------------------------------------------------*
      * PROCESS-INBOUND: Route the message in WS-RESP-RESULT: answer   *
      * server requests, handle session.event notifications and leave  *
      * responses (no method) to the caller.                           *
      *----------------------------------------------------------------*
       PROCESS-INBOUND.
           PERFORM PARSE-EVENT-TYPE
           IF WS-RPC-IS-REQUEST = 1
               PERFORM DISPATCH-SERVER-REQUEST
           ELSE
               IF WS-EVENT-TYPE NOT = SPACES
                   PERFORM HANDLE-SESSION-EVENT
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
      * HANDLE-SESSION-EVENT: React to one protocol 4 session event.   *
      * Unknown event types are ignored.                               *
      *----------------------------------------------------------------*
       HANDLE-SESSION-EVENT.
           EVALUATE TRUE
               WHEN WS-EVENT-TYPE = "assistant.message"
                   PERFORM HANDLE-ASSISTANT-MESSAGE
               WHEN WS-EVENT-TYPE = "assistant.message_delta"
                   PERFORM HANDLE-MESSAGE-DELTA
               WHEN WS-EVENT-TYPE = WS-EVT-TOOL-SHELL-OUTPUT
                   PERFORM HANDLE-SHELL-OUTPUT
               WHEN WS-EVENT-TYPE = "external_tool.requested"
                   PERFORM HANDLE-EXTERNAL-TOOL
               WHEN WS-EVENT-TYPE = "session.idle"
                   SET WAIT-IS-DONE TO TRUE
               WHEN WS-EVENT-TYPE = "session.error"
                   PERFORM HANDLE-ERROR-EVENT
                   SET WAIT-IS-DONE TO TRUE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * PARSE-EVENT-TYPE: Classify the message in WS-RESP-RESULT.      *
      * WS-EVENT-METHOD = method (spaces for a response),              *
      * WS-RPC-IS-REQUEST = 1 for a server-to-client request (its id   *
      * in WS-RPC-ID-TEXT), WS-EVENT-TYPE = params.event.type of a     *
      * session.event notification.                                   *
      *----------------------------------------------------------------*
       PARSE-EVENT-TYPE.
           MOVE SPACES TO WS-EVENT-METHOD
           MOVE SPACES TO WS-EVENT-TYPE
           MOVE SPACES TO WS-RPC-ID-TEXT
           MOVE 0 TO WS-RPC-IS-REQUEST
           MOVE WS-INBOUND-MSG TO WS-JS-DOC
           MOVE "method" TO WS-JS-KEY
           MOVE 1 TO WS-JS-DEPTH
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-EVENT-METHOD
           IF WS-EVENT-METHOD = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE "id" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           IF WS-JS-VALUE NOT = SPACES
               MOVE WS-JS-VALUE TO WS-RPC-ID-TEXT
               MOVE 1 TO WS-RPC-IS-REQUEST
               EXIT PARAGRAPH
           END-IF
           IF WS-EVENT-METHOD = "session.event"
               MOVE "type" TO WS-JS-KEY
               MOVE 3 TO WS-JS-DEPTH
               PERFORM JSON-STRING-VALUE
               MOVE WS-JS-VALUE TO WS-EVENT-TYPE
           END-IF
           .

      *----------------------------------------------------------------*
      * HANDLE-ASSISTANT-MESSAGE: Extract final assistant message.     *
      *----------------------------------------------------------------*
       HANDLE-ASSISTANT-MESSAGE.
           MOVE "content" TO WS-JS-KEY
           MOVE 4 TO WS-JS-DEPTH
           PERFORM JSON-STRING-VALUE
           IF WS-JS-VALUE NOT = SPACES
               MOVE WS-JS-VALUE TO WS-RESULT-MESSAGE
               MOVE WS-JS-VALUE-LEN TO WS-RESULT-MSG-LEN
               SET RESULT-IS-OK TO TRUE
           END-IF
           .

      *----------------------------------------------------------------*
      * HANDLE-MESSAGE-DELTA: Accumulate streaming delta text.         *
      *----------------------------------------------------------------*
       HANDLE-MESSAGE-DELTA.
           MOVE "deltaContent" TO WS-JS-KEY
           MOVE 4 TO WS-JS-DEPTH
           PERFORM JSON-STRING-VALUE
           IF WS-JS-VALUE-LEN > 0
              AND WS-RESULT-MSG-LEN + WS-JS-VALUE-LEN
                  <= FUNCTION LENGTH(WS-RESULT-MESSAGE)
               MOVE WS-JS-VALUE(1:WS-JS-VALUE-LEN)
                   TO WS-RESULT-MESSAGE(WS-RESULT-MSG-LEN + 1:
                                        WS-JS-VALUE-LEN)
               ADD WS-JS-VALUE-LEN TO WS-RESULT-MSG-LEN
           END-IF
           .

      *----------------------------------------------------------------*
      * HANDLE-ERROR-EVENT: Process error notification.                *
      *----------------------------------------------------------------*
       HANDLE-ERROR-EVENT.
           SET RESULT-HAS-ERROR TO TRUE
           MOVE "message" TO WS-JS-KEY
           MOVE 4 TO WS-JS-DEPTH
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-RESULT-ERROR-MSG
           .

      *----------------------------------------------------------------*
      * HANDLE-SHELL-OUTPUT: Decode a tool.shell_output event into     *
      * WS-SHELL-OUTPUT-EVENT and CALL WS-SHELL-OUTPUT-PGM when set.   *
      *----------------------------------------------------------------*
       HANDLE-SHELL-OUTPUT.
           INITIALIZE WS-SHELL-OUTPUT-EVENT
           MOVE 4 TO WS-JS-DEPTH
           MOVE "toolCallId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SOE-TOOL-CALL-ID
           MOVE "text" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SOE-TEXT
           MOVE "stream" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SOE-STREAM
           MOVE "sequence" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           IF WS-JS-VALUE-LEN > 0
               MOVE FUNCTION NUMVAL(WS-JS-VALUE(1:WS-JS-VALUE-LEN))
                   TO WS-SOE-SEQUENCE
           END-IF
           IF WS-SHELL-OUTPUT-PGM NOT = SPACES
               CALL WS-SHELL-OUTPUT-PGM USING WS-SHELL-OUTPUT-EVENT
                   ON EXCEPTION
                       CONTINUE
               END-CALL
           END-IF
           .

      *----------------------------------------------------------------*
      * HANDLE-EXTERNAL-TOOL: Run WS-TOOL-HANDLER-PGM for an           *
      * external_tool.requested event naming WS-TOOL-NAME and answer   *
      * with session.tools.handlePendingToolCall.                      *
      *----------------------------------------------------------------*
       HANDLE-EXTERNAL-TOOL.
           MOVE 4 TO WS-JS-DEPTH
           MOVE "toolName" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           IF WS-TOOL-HANDLER-PGM = SPACES
              OR WS-JS-VALUE NOT = WS-TOOL-NAME
               EXIT PARAGRAPH
           END-IF
           INITIALIZE WS-TOOL-INVOCATION
           MOVE WS-JS-VALUE TO WS-INVOKE-NAME
           MOVE "toolCallId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-INVOKE-ID
           MOVE "requestId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-TOOL-REQUEST-ID
           MOVE "arguments" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           MOVE WS-JS-VALUE TO WS-INVOKE-ARGS

           INITIALIZE WS-TOOL-RESULT
           SET TOOL-SUCCEEDED TO TRUE
           CALL WS-TOOL-HANDLER-PGM USING WS-TOOL-INVOCATION
               WS-TOOL-RESULT
               ON EXCEPTION
                   SET TOOL-FAILED TO TRUE
                   MOVE "Tool handler program not found"
                       TO WS-TRES-OUTPUT
           END-CALL
           IF TOOL-FAILED AND WS-TRES-OUTPUT = SPACES
               MOVE "Tool failed" TO WS-TRES-OUTPUT
           END-IF

           PERFORM BEGIN-SESSION-PARAMS
           MOVE "requestId" TO WS-PB-KEY
           MOVE WS-TOOL-REQUEST-ID TO WS-PB-TEXT
           PERFORM PB-ROOT-STRING
           IF TOOL-SUCCEEDED
               MOVE "result" TO WS-PB-KEY
           ELSE
               MOVE "error" TO WS-PB-KEY
           END-IF
           MOVE WS-TRES-OUTPUT TO WS-PB-TEXT
           PERFORM PB-ROOT-STRING
           PERFORM END-SESSION-PARAMS
           MOVE "session.tools.handlePendingToolCall" TO WS-REQ-METHOD
           PERFORM POST-SESSION-RPC
           .

      *----------------------------------------------------------------*
      * DESTROY-SESSION: Close and clean up a session. Protocol 4      *
      * detaches (session.detach); the session stays on disk and can   *
      * be resumed. The GitHub token provider registration is released.*
      * Input:  WS-SESSION-STATE                                       *
      * Output: WS-RETURN-CODE                                        *
      *----------------------------------------------------------------*
       DESTROY-SESSION.
           IF NOT SESSION-IS-ACTIVE
               MOVE 0 TO WS-RETURN-CODE
               GO TO DESTROY-SESSION-EXIT
           END-IF

           PERFORM BEGIN-SESSION-PARAMS
           PERFORM END-SESSION-PARAMS

           MOVE "session.detach" TO WS-REQ-METHOD
           ADD 1 TO WS-REQUEST-ID
           MOVE WS-REQUEST-ID TO WS-REQ-ID
           MOVE WS-PARAMS-BUFFER TO WS-REQ-PARAMS

           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RPC-ERROR
           END-IF
           IF WS-RETURN-CODE NOT = 0
               GO TO DESTROY-SESSION-EXIT
           END-IF

           SET SESSION-IS-CLOSED TO TRUE
           MOVE SPACES TO WS-SESSION-ID
           MOVE SPACES TO WS-GH-TOKEN-REG-ID
           MOVE 0 TO WS-RETURN-CODE
           .
       DESTROY-SESSION-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * DETACH-SESSION: Detach from the session (session.detach).      *
      *----------------------------------------------------------------*
       DETACH-SESSION.
           PERFORM DESTROY-SESSION THRU DESTROY-SESSION-EXIT
           .

      *----------------------------------------------------------------*
      * SWITCH-AUTO-TIER: Request an Auto routing tier for later turns *
      * that use the "auto" model (session.model.switchAutoTier).      *
      * Input: WS-AUTO-TIER (AUTO-TIER-FAST, AUTO-TIER-EFFICIENCY,     *
      * AUTO-TIER-BALANCE or AUTO-TIER-INTELLIGENCE)                   *
      *----------------------------------------------------------------*
       SWITCH-AUTO-TIER.
           IF NOT SESSION-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Session is not active" TO WS-LAST-ERROR
               EXIT PARAGRAPH
           END-IF
           PERFORM BEGIN-SESSION-PARAMS
           MOVE "autoTier" TO WS-PB-KEY
           MOVE WS-AUTO-TIER TO WS-PB-TEXT
           PERFORM PB-ROOT-STRING
           PERFORM END-SESSION-PARAMS
           MOVE "session.model.switchAutoTier" TO WS-REQ-METHOD
           PERFORM CALL-SESSION-RPC
           .

      *----------------------------------------------------------------*
      * RECHECK-POLICY-HELPER: Re-run the enterprise Policy Helper     *
      * (session.managedSettings.recheckPolicyHelper). The managed     *
      * settings snapshot is left in WS-RESP-RESULT.                   *
      *----------------------------------------------------------------*
       RECHECK-POLICY-HELPER.
           IF NOT SESSION-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Session is not active" TO WS-LAST-ERROR
               EXIT PARAGRAPH
           END-IF
           PERFORM BEGIN-SESSION-PARAMS
           PERFORM END-SESSION-PARAMS
           MOVE "session.managedSettings.recheckPolicyHelper"
               TO WS-REQ-METHOD
           PERFORM CALL-SESSION-RPC
           .

      *----------------------------------------------------------------*
      * SET-PERMISSION-MODE: Change the permission mode                *
      * (session.permissions.setMode). Input: WS-PERMISSION-MODE.      *
      * Output: WS-PERMISSION-MODE-RESULT; PMR-MANAGED-POLICY when     *
      * enterprise policy refused the change ("managed_policy").       *
      *----------------------------------------------------------------*
       SET-PERMISSION-MODE.
           INITIALIZE WS-PERMISSION-MODE-RESULT
           IF NOT SESSION-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Session is not active" TO WS-LAST-ERROR
               EXIT PARAGRAPH
           END-IF
           PERFORM BEGIN-SESSION-PARAMS
           MOVE "mode" TO WS-PB-KEY
           MOVE WS-PERMISSION-MODE TO WS-PB-TEXT
           PERFORM PB-ROOT-STRING
           PERFORM END-SESSION-PARAMS
           MOVE "session.permissions.setMode" TO WS-REQ-METHOD
           PERFORM CALL-SESSION-RPC
           IF WS-RETURN-CODE NOT = 0
               EXIT PARAGRAPH
           END-IF
           MOVE WS-INBOUND-MSG TO WS-JS-DOC
           MOVE 2 TO WS-JS-DEPTH
           MOVE "success" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           IF WS-JS-VALUE(1:4) = "true"
               SET PMR-SUCCEEDED TO TRUE
           END-IF
           MOVE "mode" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-PMR-MODE
           MOVE "reason" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-PMR-REASON
           .

      *----------------------------------------------------------------*
      * BEGIN-SESSION-PARAMS / END-SESSION-PARAMS: Open and close a    *
      * params object that starts with this session's sessionId.       *
      *----------------------------------------------------------------*
       BEGIN-SESSION-PARAMS.
           MOVE SPACES TO WS-PARAMS-BUFFER
           MOVE 1 TO WS-PARAMS-PTR
           MOVE 0 TO WS-PB-ROOT-COUNT
           MOVE "{" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           MOVE "sessionId" TO WS-PB-KEY
           MOVE WS-SESSION-ID TO WS-PB-TEXT
           PERFORM PB-ROOT-STRING
           .

       END-SESSION-PARAMS.
           MOVE "}" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           .

      *----------------------------------------------------------------*
      * CALL-SESSION-RPC: Send WS-REQ-METHOD with WS-PARAMS-BUFFER,    *
      * wait for its response and turn a JSON-RPC error into           *
      * WS-RETURN-CODE -1 (message in WS-LAST-ERROR).                  *
      *----------------------------------------------------------------*
       CALL-SESSION-RPC.
           ADD 1 TO WS-REQUEST-ID
           MOVE WS-REQUEST-ID TO WS-REQ-ID
           MOVE WS-PARAMS-BUFFER TO WS-REQ-PARAMS
           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RPC-ERROR
           END-IF
           .

      *----------------------------------------------------------------*
      * POST-SESSION-RPC: Write WS-REQ-METHOD with WS-PARAMS-BUFFER as *
      * a request without waiting; its response is skipped by the      *
      * event loop. Used while reading events.                         *
      *----------------------------------------------------------------*
       POST-SESSION-RPC.
           ADD 1 TO WS-REQUEST-ID
           MOVE WS-REQUEST-ID TO WS-RPC-ID-ED
           MOVE SPACES TO WS-OUTBOUND-MSG
           MOVE 1 TO WS-OUTBOUND-PTR
           STRING '{"jsonrpc":"2.0","id":'
               FUNCTION TRIM(WS-RPC-ID-ED)
               ',"method":"'
               FUNCTION TRIM(WS-REQ-METHOD)
               '","params":'
               WS-PARAMS-BUFFER(1:WS-PARAMS-PTR - 1)
               '}'
               DELIMITED SIZE INTO WS-OUTBOUND-MSG
               WITH POINTER WS-OUTBOUND-PTR
           END-STRING
           CALL "COPILOT-CLIENT-WRITE" USING WS-OUTBOUND-MSG
               WS-WRITE-RC
           .

      *================================================================*
      * Server-to-client requests (callbacks). Each one CALLs the      *
      * program named in the session or client options and writes the *
      * JSON-RPC reply; unknown methods get a "Method not found" error.*
      *================================================================*
       DISPATCH-SERVER-REQUEST.
           MOVE SPACES TO WS-PARAMS-BUFFER
           MOVE 1 TO WS-PARAMS-PTR
           MOVE 0 TO WS-PB-ROOT-COUNT
           MOVE 0 TO WS-REPLY-IS-ERROR
           MOVE -32601 TO WS-REPLY-ERROR-CODE
           MOVE "Method not found" TO WS-REPLY-ERROR-MSG
           EVALUATE WS-EVENT-METHOD
               WHEN "gitHubToken.getToken"
                   PERFORM ANSWER-GITHUB-TOKEN
               WHEN "userInput.request"
                   PERFORM ANSWER-USER-INPUT
               WHEN "shellAuthentication.request"
                   PERFORM ANSWER-SHELL-AUTH
               WHEN "shellAuthentication.cancel"
      *            Prompts are answered synchronously, so nothing is
      *            pending any more.
                   MOVE "{}" TO WS-PB-TEXT
                   PERFORM PB-PUT-RAW
               WHEN "memoryStore.store"
                   PERFORM ANSWER-MEMORY-STORE
               WHEN "hooks.invoke"
                   PERFORM ANSWER-HOOKS-INVOKE
               WHEN "network.resolveProxy"
               WHEN "network.authenticateNegotiate"
               WHEN "network.authenticateBasic"
                   PERFORM ANSWER-NETWORK
               WHEN OTHER
                   MOVE 1 TO WS-REPLY-IS-ERROR
           END-EVALUATE
           PERFORM POST-RPC-REPLY
           .

      *    gitHubToken.getToken {registrationId, host, sessionId,
      *    reason} -> {kind:"token", accessToken, tokenType?,
      *    expiresIn?} or {kind:"cancelled"}.
       ANSWER-GITHUB-TOKEN.
           INITIALIZE WS-GITHUB-TOKEN-REQ WS-GITHUB-TOKEN-RESP
           MOVE 2 TO WS-JS-DEPTH
           MOVE "registrationId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-GTR-REGISTRATION-ID
           MOVE "host" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-GTR-HOST
           MOVE "sessionId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-GTR-SESSION-ID
           MOVE "reason" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-GTR-REASON
           IF WS-GH-TOKEN-PGM = SPACES
              OR WS-GTR-REGISTRATION-ID NOT = WS-GH-TOKEN-REG-ID
               MOVE 1 TO WS-REPLY-IS-ERROR
               MOVE -32603 TO WS-REPLY-ERROR-CODE
               MOVE "Unknown GitHub token provider registration"
                   TO WS-REPLY-ERROR-MSG
               EXIT PARAGRAPH
           END-IF
           SET GTP-TOKEN TO TRUE
           CALL WS-GH-TOKEN-PGM USING WS-GITHUB-TOKEN-REQ
               WS-GITHUB-TOKEN-RESP
               ON EXCEPTION
                   SET GTP-CANCELLED TO TRUE
           END-CALL
           IF GTP-CANCELLED OR WS-GTP-ACCESS-TOKEN = SPACES
               MOVE '{"kind":"cancelled"}' TO WS-PB-TEXT
               PERFORM PB-PUT-RAW
               EXIT PARAGRAPH
           END-IF
           MOVE "{" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           MOVE "kind" TO WS-PB-KEY
           MOVE "token" TO WS-PB-TEXT
           PERFORM PB-ROOT-STRING
           MOVE "accessToken" TO WS-PB-KEY
           MOVE WS-GTP-ACCESS-TOKEN TO WS-PB-TEXT
           PERFORM PB-ROOT-STRING
           IF WS-GTP-TOKEN-TYPE NOT = SPACES
               MOVE "tokenType" TO WS-PB-KEY
               MOVE WS-GTP-TOKEN-TYPE TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
           END-IF
           IF WS-GTP-EXPIRES-IN > 0
               MOVE "expiresIn" TO WS-PB-KEY
               PERFORM PB-ROOT-KEY
               MOVE WS-GTP-EXPIRES-IN TO WS-PB-NUM
               PERFORM PB-PUT-NUMBER
           END-IF
           MOVE "}" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           .

      *    shellAuthentication.request {sessionId, requestId, program,
      *    prompt, inputKind, command, cwd} -> {action:"accept",
      *    value} or {action:"cancel"}.
       ANSWER-SHELL-AUTH.
           INITIALIZE WS-SHELL-AUTH-REQ WS-SHELL-AUTH-RESP
           MOVE 2 TO WS-JS-DEPTH
           MOVE "sessionId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAR-SESSION-ID
           MOVE "requestId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAR-REQUEST-ID
           MOVE "program" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAR-PROGRAM
           MOVE "prompt" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAR-PROMPT
           MOVE "inputKind" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAR-INPUT-KIND
           MOVE "command" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAR-COMMAND
           MOVE "cwd" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAR-CWD
           SET SAP-CANCEL TO TRUE
           IF WS-SHELL-AUTH-PGM NOT = SPACES
               CALL WS-SHELL-AUTH-PGM USING WS-SHELL-AUTH-REQ
                   WS-SHELL-AUTH-RESP
                   ON EXCEPTION
                       SET SAP-CANCEL TO TRUE
               END-CALL
           END-IF
           IF SAP-ACCEPT
               MOVE "{" TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
               MOVE "action" TO WS-PB-KEY
               MOVE "accept" TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
               MOVE "value" TO WS-PB-KEY
               MOVE WS-SAP-VALUE TO WS-PB-TEXT
               PERFORM PB-ROOT-STRING
               MOVE "}" TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
           ELSE
               MOVE '{"action":"cancel"}' TO WS-PB-TEXT
               PERFORM PB-PUT-RAW
           END-IF
           .

      *    userInput.request {sessionId, question, choices?,
      *    allowFreeform?, requestId?, imageAttachmentConstraints?,
      *    _meta?} (the ask_user tool) -> {answer, wasFreeform,
      *    attachments?}. Attachments are sent only when the program
      *    sets WS-UIRESP-ATTACH-COUNT.
       ANSWER-USER-INPUT.
           INITIALIZE WS-USER-INPUT-REQ WS-USER-INPUT-RESP
           MOVE 2 TO WS-JS-DEPTH
           MOVE "sessionId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-UIR-SESSION-ID
           MOVE "question" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-UIR-QUESTION
           MOVE "choices" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           MOVE WS-JS-VALUE TO WS-UIR-CHOICES
           SET UIR-ALLOW-FREEFORM-YES TO TRUE
           MOVE "allowFreeform" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           IF WS-JS-VALUE(1:5) = "false"
               SET UIR-ALLOW-FREEFORM-NO TO TRUE
           END-IF
           MOVE "requestId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-UIR-REQUEST-ID
           MOVE "_meta" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           MOVE WS-JS-VALUE TO WS-UIR-META
           MOVE "imageAttachmentConstraints" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           IF WS-JS-VALUE(1:1) = "{"
               SET UIR-IMAGES-ACCEPTED TO TRUE
               MOVE 3 TO WS-JS-DEPTH
               MOVE "allowedMimeTypes" TO WS-JS-KEY
               PERFORM JSON-RAW-VALUE
               MOVE WS-JS-VALUE TO WS-UIR-ALLOWED-MIME-TYPES
               MOVE "maxBytesPerImage" TO WS-JS-KEY
               PERFORM JSON-RAW-VALUE
               IF WS-JS-VALUE-LEN > 0
                   MOVE FUNCTION NUMVAL(WS-JS-VALUE(1:WS-JS-VALUE-LEN))
                       TO WS-UIR-MAX-BYTES-PER-IMAGE
               END-IF
               MOVE "maxCount" TO WS-JS-KEY
               PERFORM JSON-RAW-VALUE
               IF WS-JS-VALUE-LEN > 0
                   MOVE FUNCTION NUMVAL(WS-JS-VALUE(1:WS-JS-VALUE-LEN))
                       TO WS-UIR-MAX-COUNT
               END-IF
           END-IF
           IF WS-USER-INPUT-PGM = SPACES
               MOVE 1 TO WS-REPLY-IS-ERROR
               MOVE -32603 TO WS-REPLY-ERROR-CODE
               MOVE "User input requested but no handler registered"
                   TO WS-REPLY-ERROR-MSG
               EXIT PARAGRAPH
           END-IF
           CALL WS-USER-INPUT-PGM USING WS-USER-INPUT-REQ
               WS-USER-INPUT-RESP
               ON EXCEPTION
                   MOVE 1 TO WS-REPLY-IS-ERROR
                   MOVE -32603 TO WS-REPLY-ERROR-CODE
                   MOVE "User input program not found"
                       TO WS-REPLY-ERROR-MSG
           END-CALL
           IF WS-REPLY-IS-ERROR = 1
               EXIT PARAGRAPH
           END-IF
           MOVE "{" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           MOVE "answer" TO WS-PB-KEY
           MOVE WS-UIRESP-ANSWER TO WS-PB-TEXT
           PERFORM PB-ROOT-STRING
           MOVE "wasFreeform" TO WS-PB-KEY
           PERFORM PB-ROOT-KEY
           IF UIRESP-WAS-FREEFORM-YES
               PERFORM PB-PUT-TRUE
           ELSE
               PERFORM PB-PUT-FALSE
           END-IF
           IF WS-UIRESP-ATTACH-COUNT > 0
               MOVE "attachments" TO WS-PB-KEY
               PERFORM PB-ROOT-KEY
               MOVE "[" TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
               PERFORM VARYING WS-PB-ENTRY FROM 1 BY 1
                       UNTIL WS-PB-ENTRY > WS-UIRESP-ATTACH-COUNT
                          OR WS-PB-ENTRY > 4
                   IF WS-PB-ENTRY > 1
                       MOVE "," TO WS-PB-CHAR
                       PERFORM PB-PUT-CHAR
                   END-IF
                   PERFORM PB-OBJ-OPEN
                   MOVE "type" TO WS-PB-KEY
                   MOVE "image" TO WS-PB-TEXT
                   PERFORM PB-OBJ-STRING
                   MOVE "data" TO WS-PB-KEY
                   MOVE WS-UIA-DATA(WS-PB-ENTRY) TO WS-PB-TEXT
                   PERFORM PB-OBJ-STRING
                   MOVE "mimeType" TO WS-PB-KEY
                   MOVE WS-UIA-MIME-TYPE(WS-PB-ENTRY) TO WS-PB-TEXT
                   PERFORM PB-OBJ-STRING
                   MOVE "description" TO WS-PB-KEY
                   MOVE WS-UIA-DESCRIPTION(WS-PB-ENTRY) TO WS-PB-TEXT
                   PERFORM PB-OBJ-STRING
                   MOVE "displayName" TO WS-PB-KEY
                   MOVE WS-UIA-DISPLAY-NAME(WS-PB-ENTRY) TO WS-PB-TEXT
                   PERFORM PB-OBJ-STRING
                   PERFORM PB-OBJ-CLOSE
               END-PERFORM
               MOVE "]" TO WS-PB-CHAR
               PERFORM PB-PUT-CHAR
           END-IF
           MOVE "}" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           .

      *    memoryStore.store {sessionId, subject, fact, citations,
      *    reason} -> null once the store program has run.
       ANSWER-MEMORY-STORE.
           INITIALIZE WS-STORED-MEMORY
           MOVE 2 TO WS-JS-DEPTH
           MOVE "sessionId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SM-SESSION-ID
           MOVE "subject" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SM-SUBJECT
           MOVE "fact" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SM-FACT
           MOVE "citations" TO WS-JS-KEY
           PERFORM JSON-RAW-VALUE
           MOVE WS-JS-VALUE TO WS-SM-CITATIONS
           MOVE "reason" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SM-REASON
           IF WS-MEMORY-STORE-PGM = SPACES
               MOVE 1 TO WS-REPLY-IS-ERROR
               MOVE -32603 TO WS-REPLY-ERROR-CODE
               MOVE "Memory store owner is unavailable"
                   TO WS-REPLY-ERROR-MSG
               EXIT PARAGRAPH
           END-IF
           CALL WS-MEMORY-STORE-PGM USING WS-STORED-MEMORY
               ON EXCEPTION
                   MOVE 1 TO WS-REPLY-IS-ERROR
                   MOVE -32603 TO WS-REPLY-ERROR-CODE
                   MOVE "Memory store program not found"
                       TO WS-REPLY-ERROR-MSG
           END-CALL
           IF WS-REPLY-IS-ERROR = 0
               PERFORM PB-PUT-NULL
           END-IF
           .

      *    hooks.invoke {sessionId, hookType, input}: subagentStart and
      *    subagentStop go to their programs; results travel as
      *    {"output":{...}} and {} means no change.
       ANSWER-HOOKS-INVOKE.
           INITIALIZE WS-SUBAGENT-HOOK-INPUT WS-SUBAGENT-HOOK-OUTPUT
           MOVE 2 TO WS-JS-DEPTH
           MOVE "hookType" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAH-HOOK-TYPE
           MOVE "sessionId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAH-SESSION-ID
           EVALUATE TRUE
               WHEN SAH-SUBAGENT-START
                AND WS-SUBAGENT-START-PGM NOT = SPACES
                   PERFORM READ-SUBAGENT-INPUT
                   CALL WS-SUBAGENT-START-PGM
                       USING WS-SUBAGENT-HOOK-INPUT
                             WS-SUBAGENT-HOOK-OUTPUT
                       ON EXCEPTION
                           CONTINUE
                   END-CALL
                   IF WS-SAO-ADDITIONAL-CTX NOT = SPACES
                       STRING '{"output":' DELIMITED SIZE
                           INTO WS-PARAMS-BUFFER
                           WITH POINTER WS-PARAMS-PTR
                       END-STRING
                       PERFORM PB-OBJ-OPEN
                       MOVE "additionalContext" TO WS-PB-KEY
                       MOVE WS-SAO-ADDITIONAL-CTX TO WS-PB-TEXT
                       PERFORM PB-OBJ-STRING
                       PERFORM PB-OBJ-CLOSE
                       MOVE "}" TO WS-PB-CHAR
                       PERFORM PB-PUT-CHAR
                       EXIT PARAGRAPH
                   END-IF
               WHEN SAH-SUBAGENT-STOP
                AND WS-SUBAGENT-STOP-PGM NOT = SPACES
                   PERFORM READ-SUBAGENT-INPUT
                   CALL WS-SUBAGENT-STOP-PGM
                       USING WS-SUBAGENT-HOOK-INPUT
                             WS-SUBAGENT-HOOK-OUTPUT
                       ON EXCEPTION
                           CONTINUE
                   END-CALL
                   IF (SAO-BLOCK-YES AND WS-SAO-REASON NOT = SPACES)
                      OR WS-SAO-MODIFIED-RESP NOT = SPACES
                       STRING '{"output":' DELIMITED SIZE
                           INTO WS-PARAMS-BUFFER
                           WITH POINTER WS-PARAMS-PTR
                       END-STRING
                       PERFORM PB-OBJ-OPEN
                       IF SAO-BLOCK-YES AND WS-SAO-REASON NOT = SPACES
                           MOVE "decision" TO WS-PB-KEY
                           MOVE "block" TO WS-PB-TEXT
                           PERFORM PB-OBJ-STRING
                           MOVE "reason" TO WS-PB-KEY
                           MOVE WS-SAO-REASON TO WS-PB-TEXT
                           PERFORM PB-OBJ-STRING
                       END-IF
                       MOVE "modifiedResponse" TO WS-PB-KEY
                       MOVE WS-SAO-MODIFIED-RESP TO WS-PB-TEXT
                       PERFORM PB-OBJ-STRING
                       PERFORM PB-OBJ-CLOSE
                       MOVE "}" TO WS-PB-CHAR
                       PERFORM PB-PUT-CHAR
                       EXIT PARAGRAPH
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           MOVE "{}" TO WS-PB-TEXT
           PERFORM PB-PUT-RAW
           .

       READ-SUBAGENT-INPUT.
           MOVE 3 TO WS-JS-DEPTH
           MOVE "agentName" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAH-AGENT-NAME
           MOVE "agentDisplayName" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAH-AGENT-DISPLAY
           MOVE "agentDescription" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAH-AGENT-DESC
           MOVE "agentId" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAH-AGENT-ID
           MOVE "agentType" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAH-AGENT-TYPE
           MOVE "stopReason" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAH-STOP-REASON
           MOVE "response" TO WS-JS-KEY
           PERFORM JSON-STRING-VALUE
           MOVE WS-JS-VALUE TO WS-SAH-RESPONSE
           .

      *    network.resolveProxy {url} -> {proxy?},
      *    network.authenticateNegotiate {spn} -> {token?},
      *    network.authenticateBasic {proxy, realm?, previousRejected}
      *    -> {username?, password?}. Blank answers decline. Tokens
      *    and passwords are never logged.
       ANSWER-NETWORK.
           INITIALIZE WS-NETWORK-REQUEST WS-NETWORK-RESPONSE
           MOVE WS-EVENT-METHOD TO WS-NREQ-METHOD
           MOVE SPACES TO WS-NET-PGM-NAME
           MOVE 2 TO WS-JS-DEPTH
           EVALUATE WS-EVENT-METHOD
               WHEN "network.resolveProxy"
                   MOVE "url" TO WS-JS-KEY
                   PERFORM JSON-STRING-VALUE
                   MOVE WS-JS-VALUE TO WS-NREQ-URL
                   MOVE WS-NET-PROXY-PGM TO WS-NET-PGM-NAME
               WHEN "network.authenticateNegotiate"
                   MOVE "spn" TO WS-JS-KEY
                   PERFORM JSON-STRING-VALUE
                   MOVE WS-JS-VALUE TO WS-NREQ-SPN
                   MOVE WS-NET-NEGOTIATE-PGM TO WS-NET-PGM-NAME
               WHEN OTHER
                   MOVE "proxy" TO WS-JS-KEY
                   PERFORM JSON-STRING-VALUE
                   MOVE WS-JS-VALUE TO WS-NREQ-PROXY
                   MOVE "realm" TO WS-JS-KEY
                   PERFORM JSON-STRING-VALUE
                   MOVE WS-JS-VALUE TO WS-NREQ-REALM
                   MOVE "previousRejected" TO WS-JS-KEY
                   PERFORM JSON-RAW-VALUE
                   IF WS-JS-VALUE(1:4) = "true"
                       SET NREQ-PREVIOUS-REJECTED TO TRUE
                   END-IF
                   MOVE WS-NET-BASIC-PGM TO WS-NET-PGM-NAME
           END-EVALUATE
           IF WS-NET-PGM-NAME NOT = SPACES
               CALL WS-NET-PGM-NAME USING WS-NETWORK-REQUEST
                   WS-NETWORK-RESPONSE
                   ON EXCEPTION
                       INITIALIZE WS-NETWORK-RESPONSE
               END-CALL
           END-IF
           MOVE "{" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           EVALUATE WS-EVENT-METHOD
               WHEN "network.resolveProxy"
                   IF WS-NRESP-PROXY NOT = SPACES
                       MOVE "proxy" TO WS-PB-KEY
                       MOVE WS-NRESP-PROXY TO WS-PB-TEXT
                       PERFORM PB-ROOT-STRING
                   END-IF
               WHEN "network.authenticateNegotiate"
                   IF WS-NRESP-TOKEN NOT = SPACES
                       MOVE "token" TO WS-PB-KEY
                       MOVE WS-NRESP-TOKEN TO WS-PB-TEXT
                       PERFORM PB-ROOT-STRING
                   END-IF
               WHEN OTHER
                   IF WS-NRESP-USERNAME NOT = SPACES
                       MOVE "username" TO WS-PB-KEY
                       MOVE WS-NRESP-USERNAME TO WS-PB-TEXT
                       PERFORM PB-ROOT-STRING
                       MOVE "password" TO WS-PB-KEY
                       MOVE WS-NRESP-PASSWORD TO WS-PB-TEXT
                       PERFORM PB-ROOT-STRING
                   END-IF
           END-EVALUATE
           MOVE "}" TO WS-PB-CHAR
           PERFORM PB-PUT-CHAR
           .

      *    Write the reply: WS-PARAMS-BUFFER is the result value, or
      *    WS-REPLY-ERROR-CODE / -MSG when WS-REPLY-IS-ERROR = 1.
       POST-RPC-REPLY.
           MOVE SPACES TO WS-OUTBOUND-MSG
           MOVE 1 TO WS-OUTBOUND-PTR
           IF WS-REPLY-IS-ERROR = 1
               MOVE WS-REPLY-ERROR-CODE TO WS-REPLY-CODE-ED
               STRING '{"jsonrpc":"2.0","id":'
                   FUNCTION TRIM(WS-RPC-ID-TEXT)
                   ',"error":{"code":'
                   FUNCTION TRIM(WS-REPLY-CODE-ED)
                   ',"message":"'
                   FUNCTION TRIM(WS-REPLY-ERROR-MSG)
                   '"}}'
                   DELIMITED SIZE INTO WS-OUTBOUND-MSG
                   WITH POINTER WS-OUTBOUND-PTR
               END-STRING
           ELSE
               IF WS-PARAMS-PTR < 2
                   PERFORM PB-PUT-NULL
               END-IF
               STRING '{"jsonrpc":"2.0","id":'
                   FUNCTION TRIM(WS-RPC-ID-TEXT)
                   ',"result":'
                   WS-PARAMS-BUFFER(1:WS-PARAMS-PTR - 1)
                   '}'
                   DELIMITED SIZE INTO WS-OUTBOUND-MSG
                   WITH POINTER WS-OUTBOUND-PTR
               END-STRING
           END-IF
           CALL "COPILOT-CLIENT-WRITE" USING WS-OUTBOUND-MSG
               WS-WRITE-RC
           .

      *================================================================*
      * JSON reader. Keys are matched at an exact nesting depth        *
      * (1 = top-level object) so nested fields with the same name are *
      * not confused. \uXXXX escapes are kept as written.              *
      *================================================================*

      *----------------------------------------------------------------*
      * JSON-FIND-KEY: Find WS-JS-KEY (no quotes) at depth             *
      * WS-JS-DEPTH in WS-JS-DOC. WS-JS-POS is the first character of  *
      * its value, or 0 when the key is absent.                        *
      *----------------------------------------------------------------*
       JSON-FIND-KEY.
           MOVE 0 TO WS-JS-POS
           MOVE 0 TO WS-JS-LEVEL
           MOVE 0 TO WS-JS-IN-STR
           MOVE 0 TO WS-JS-KEY-LEN
           IF WS-JS-KEY NOT = SPACES
               MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-JS-KEY TRAILING))
                   TO WS-JS-KEY-LEN
           END-IF
           MOVE 0 TO WS-JS-END
           IF WS-JS-DOC NOT = SPACES
               MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-JS-DOC TRAILING))
                   TO WS-JS-END
           END-IF
           IF WS-JS-END >= FUNCTION LENGTH(WS-JS-DOC)
               COMPUTE WS-JS-END = FUNCTION LENGTH(WS-JS-DOC) - 1
           END-IF
           MOVE 1 TO WS-JS-I
           PERFORM UNTIL WS-JS-I > WS-JS-END OR WS-JS-POS > 0
               MOVE WS-JS-DOC(WS-JS-I:1) TO WS-JS-CH
               IF WS-JS-IN-STR = 1
                   EVALUATE WS-JS-CH
                       WHEN "\"
                           ADD 1 TO WS-JS-I
                       WHEN '"'
                           MOVE 0 TO WS-JS-IN-STR
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               ELSE
                   EVALUATE WS-JS-CH
                       WHEN "{"
                       WHEN "["
                           ADD 1 TO WS-JS-LEVEL
                       WHEN "}"
                       WHEN "]"
                           SUBTRACT 1 FROM WS-JS-LEVEL
                       WHEN '"'
                           PERFORM JSON-CHECK-KEY
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               END-IF
               ADD 1 TO WS-JS-I
           END-PERFORM
           .

      *    At an opening quote outside a string: either the wanted key
      *    (sets WS-JS-POS) or the start of a string to skip.
       JSON-CHECK-KEY.
           MOVE 1 TO WS-JS-IN-STR
           IF WS-JS-LEVEL NOT = WS-JS-DEPTH OR WS-JS-KEY-LEN = 0
               EXIT PARAGRAPH
           END-IF
           IF WS-JS-I + WS-JS-KEY-LEN + 1 > WS-JS-END
               EXIT PARAGRAPH
           END-IF
           IF WS-JS-DOC(WS-JS-I + 1:WS-JS-KEY-LEN)
                  NOT = WS-JS-KEY(1:WS-JS-KEY-LEN)
              OR WS-JS-DOC(WS-JS-I + WS-JS-KEY-LEN + 1:1) NOT = '"'
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-JS-J = WS-JS-I + WS-JS-KEY-LEN + 2
           PERFORM UNTIL WS-JS-J > WS-JS-END
                      OR WS-JS-DOC(WS-JS-J:1) NOT = SPACE
               ADD 1 TO WS-JS-J
           END-PERFORM
           IF WS-JS-J > WS-JS-END
               EXIT PARAGRAPH
           END-IF
           IF WS-JS-DOC(WS-JS-J:1) NOT = ":"
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-JS-J
           PERFORM UNTIL WS-JS-J > WS-JS-END
                      OR WS-JS-DOC(WS-JS-J:1) NOT = SPACE
               ADD 1 TO WS-JS-J
           END-PERFORM
           IF WS-JS-J <= WS-JS-END
               MOVE WS-JS-J TO WS-JS-POS
               MOVE 0 TO WS-JS-IN-STR
           END-IF
           .

      *----------------------------------------------------------------*
      * JSON-STRING-VALUE: WS-JS-VALUE / WS-JS-VALUE-LEN = the         *
      * unescaped string value of WS-JS-KEY at WS-JS-DEPTH (spaces     *
      * when absent). A non-string value is returned raw.              *
      *----------------------------------------------------------------*
       JSON-STRING-VALUE.
           PERFORM JSON-FIND-KEY
           MOVE SPACES TO WS-JS-VALUE
           MOVE 0 TO WS-JS-VALUE-LEN
           IF WS-JS-POS = 0
               EXIT PARAGRAPH
           END-IF
           IF WS-JS-DOC(WS-JS-POS:1) NOT = '"'
               PERFORM JSON-READ-RAW
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-JS-J = WS-JS-POS + 1
           PERFORM UNTIL WS-JS-J > WS-JS-END
                      OR WS-JS-DOC(WS-JS-J:1) = '"'
               MOVE WS-JS-DOC(WS-JS-J:1) TO WS-JS-CH
               IF WS-JS-CH = "\" AND WS-JS-J < WS-JS-END
                   ADD 1 TO WS-JS-J
                   MOVE WS-JS-DOC(WS-JS-J:1) TO WS-JS-CH
                   EVALUATE WS-JS-CH
                       WHEN "n"
                           MOVE X"0A" TO WS-JS-CH
                       WHEN "r"
                           MOVE X"0D" TO WS-JS-CH
                       WHEN "t"
                           MOVE X"09" TO WS-JS-CH
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               END-IF
               IF WS-JS-VALUE-LEN < FUNCTION LENGTH(WS-JS-VALUE)
                   ADD 1 TO WS-JS-VALUE-LEN
                   MOVE WS-JS-CH TO WS-JS-VALUE(WS-JS-VALUE-LEN:1)
               END-IF
               ADD 1 TO WS-JS-J
           END-PERFORM
           .

      *----------------------------------------------------------------*
      * JSON-RAW-VALUE: WS-JS-VALUE / WS-JS-VALUE-LEN = the raw JSON   *
      * text (object, array, string, number or literal) of WS-JS-KEY   *
      * at WS-JS-DEPTH (spaces when absent).                           *
      *----------------------------------------------------------------*
       JSON-RAW-VALUE.
           PERFORM JSON-FIND-KEY
           MOVE SPACES TO WS-JS-VALUE
           MOVE 0 TO WS-JS-VALUE-LEN
           IF WS-JS-POS > 0
               PERFORM JSON-READ-RAW
           END-IF
           .

       JSON-READ-RAW.
           MOVE 0 TO WS-JS-LEVEL
           MOVE 0 TO WS-JS-IN-STR
           MOVE WS-JS-POS TO WS-JS-J
           PERFORM UNTIL WS-JS-J > WS-JS-END
               MOVE WS-JS-DOC(WS-JS-J:1) TO WS-JS-CH
               IF WS-JS-IN-STR = 1
                   EVALUATE WS-JS-CH
                       WHEN "\"
                           ADD 1 TO WS-JS-J
                       WHEN '"'
                           MOVE 0 TO WS-JS-IN-STR
                           IF WS-JS-LEVEL = 0
                               EXIT PERFORM
                           END-IF
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               ELSE
                   EVALUATE WS-JS-CH
                       WHEN '"'
                           MOVE 1 TO WS-JS-IN-STR
                       WHEN "{"
                       WHEN "["
                           ADD 1 TO WS-JS-LEVEL
                       WHEN "}"
                       WHEN "]"
                           IF WS-JS-LEVEL = 0
                               SUBTRACT 1 FROM WS-JS-J
                               EXIT PERFORM
                           END-IF
                           SUBTRACT 1 FROM WS-JS-LEVEL
                           IF WS-JS-LEVEL = 0
                               EXIT PERFORM
                           END-IF
                       WHEN ","
                           IF WS-JS-LEVEL = 0
                               SUBTRACT 1 FROM WS-JS-J
                               EXIT PERFORM
                           END-IF
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE
               END-IF
               ADD 1 TO WS-JS-J
           END-PERFORM
           IF WS-JS-J > WS-JS-END
               MOVE WS-JS-END TO WS-JS-J
           END-IF
           IF WS-JS-J >= WS-JS-POS
               COMPUTE WS-JS-VALUE-LEN = WS-JS-J - WS-JS-POS + 1
               IF WS-JS-VALUE-LEN > FUNCTION LENGTH(WS-JS-VALUE)
                   MOVE FUNCTION LENGTH(WS-JS-VALUE) TO WS-JS-VALUE-LEN
               END-IF
               MOVE WS-JS-DOC(WS-JS-POS:WS-JS-VALUE-LEN) TO WS-JS-VALUE
           END-IF
           MOVE 0 TO WS-JS-VALUE-LEN
           IF WS-JS-VALUE NOT = SPACES
               MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-JS-VALUE TRAILING))
                   TO WS-JS-VALUE-LEN
           END-IF
           .

      *----------------------------------------------------------------*
      * SEND-RPC-REQUEST: Frame WS-REQ-METHOD / WS-REQ-ID /            *
      * WS-REQ-PARAMS as a JSON-RPC request, write it and read until   *
      * its response arrives (left in WS-RESP-RESULT). Server requests *
      * are answered and notifications handled while waiting.          *
      *----------------------------------------------------------------*
       SEND-RPC-REQUEST.
           MOVE WS-REQ-ID TO WS-AWAIT-ID-ED
           MOVE SPACES TO WS-OUTBOUND-MSG
           MOVE 1 TO WS-OUTBOUND-PTR
           STRING '{"jsonrpc":"2.0","id":'
               FUNCTION TRIM(WS-AWAIT-ID-ED)
               ',"method":"'
               FUNCTION TRIM(WS-REQ-METHOD)
               '","params":'
               FUNCTION TRIM(WS-REQ-PARAMS)
               '}'
               DELIMITED SIZE INTO WS-OUTBOUND-MSG
               WITH POINTER WS-OUTBOUND-PTR
           END-STRING
           CALL "COPILOT-CLIENT-WRITE" USING WS-OUTBOUND-MSG
               WS-RETURN-CODE
           PERFORM UNTIL WS-RETURN-CODE NOT = 0
               PERFORM RECEIVE-FROM-CLI
               IF WS-RETURN-CODE = 0
                   PERFORM PROCESS-INBOUND
                   IF WS-EVENT-METHOD = SPACES
                       MOVE "id" TO WS-JS-KEY
                       MOVE 1 TO WS-JS-DEPTH
                       PERFORM JSON-RAW-VALUE
                       IF WS-JS-VALUE = FUNCTION TRIM(WS-AWAIT-ID-ED)
                           EXIT PERFORM
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .

      *----------------------------------------------------------------*
      * RECEIVE-FROM-CLI: Read the next framed message (response,      *
      * notification or server request) into WS-RESP-RESULT.           *
      *----------------------------------------------------------------*
       RECEIVE-FROM-CLI.
           MOVE SPACES TO WS-INBOUND-MSG
           CALL "COPILOT-CLIENT-READ" USING WS-INBOUND-MSG
               WS-RETURN-CODE
           MOVE WS-INBOUND-MSG TO WS-RESP-RESULT
           .

       SESSION-PROGRAM-END.
           STOP RUN.

      *----------------------------------------------------------------*
      * SDK protocol 4 session payload builder (translation layer).    *
      *----------------------------------------------------------------*
       COPY "COPILOT-PARAMS.cpy".
