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

      *    Include remote option
           IF REMOTE-ON
               STRING
                   ',"remote":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

      *    Include session telemetry
           IF SESS-TELEMETRY-ON
               STRING
                   ',"enableSessionTelemetry":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

      *    Include exit plan mode handler flag
           IF EPM-HANDLER-ACTIVE
               STRING
                   ',"requestExitPlanMode":true'
                   DELIMITED SIZE
                   INTO WS-PARAMS-BUFFER
                   WITH POINTER WS-PARAMS-PTR
               END-STRING
           END-IF

      *    Include trace context
           IF TRACE-PROVIDER-ACTIVE
               IF WS-TRACEPARENT NOT = SPACES
                   STRING
                       ',"traceparent":"'
                       FUNCTION TRIM(WS-TRACEPARENT)
                       '"'
                       DELIMITED SIZE
                       INTO WS-PARAMS-BUFFER
                       WITH POINTER WS-PARAMS-PTR
                   END-STRING
               END-IF
               IF WS-TRACESTATE NOT = SPACES
                   STRING
                       ',"tracestate":"'
                       FUNCTION TRIM(WS-TRACESTATE)
                       '"'
                       DELIMITED SIZE
                       INTO WS-PARAMS-BUFFER
                       WITH POINTER WS-PARAMS-PTR
                   END-STRING
               END-IF
           END-IF

      *    Include model capabilities override
           IF MCO-ENABLED
               STRING
                   ',"modelCapabilities":'
                   FUNCTION TRIM(WS-MODEL-CAPABILITIES)
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
               '"}'
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

      *----------------------------------------------------------------*
      * HANDLE-EXIT-PLAN-MODE: Handle exit plan mode request           *
      * Input:  WS-EXIT-PLAN-MODE-REQ                                  *
      * Output: WS-EXIT-PLAN-MODE-RESP                                 *
      *----------------------------------------------------------------*
       HANDLE-EXIT-PLAN-MODE.
           IF NOT EPM-HANDLER-ACTIVE
               SET EPM-APPROVED TO TRUE
               MOVE SPACES TO WS-EPM-SELECTED-ACTION
               MOVE SPACES TO WS-EPM-FEEDBACK
           END-IF
           .
       HANDLE-EXIT-PLAN-MODE-EXIT.
           EXIT.

       STOP RUN.
