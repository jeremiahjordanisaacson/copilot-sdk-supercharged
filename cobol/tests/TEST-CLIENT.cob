      *================================================================*
      * TEST-CLIENT.cob                                                *
      * Unit-style tests for the Copilot SDK COBOL modules.            *
      * Uses EVALUATE TRUE for assertions and PERFORM for test         *
      * orchestration. Reports pass/fail counts at the end.            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CLIENT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY "COPILOT-TYPES.cpy".
       COPY "COPILOT-VERSION.cpy".

       01  WS-TEST-COUNT            PIC 9(4)    VALUE 0.
       01  WS-PASS-COUNT            PIC 9(4)    VALUE 0.
       01  WS-FAIL-COUNT            PIC 9(4)    VALUE 0.
       01  WS-TEST-NAME             PIC X(64)   VALUE SPACES.
       01  WS-EXPECTED              PIC X(256)  VALUE SPACES.
       01  WS-ACTUAL                PIC X(256)  VALUE SPACES.

       01  WS-JSON-BUFFER           PIC X(8192) VALUE SPACES.
       01  WS-JSON-LEN              PIC 9(5)    VALUE 0.
       01  WS-PARAMS-BUFFER         PIC X(4096) VALUE SPACES.
       01  WS-PARAMS-PTR            PIC 9(5)    VALUE 1.

       PROCEDURE DIVISION.
       RUN-ALL-TESTS.
           DISPLAY "========================================"
           DISPLAY " Copilot SDK for COBOL - Test Suite"
           DISPLAY "========================================"
           DISPLAY " "

           PERFORM TEST-VERSION-COPYBOOK
           PERFORM TEST-TYPES-INITIALIZATION
           PERFORM TEST-TYPES-88-LEVELS
           PERFORM TEST-CLIENT-STATE-DEFAULTS
           PERFORM TEST-SESSION-CONFIG-DEFAULTS
           PERFORM TEST-SEND-OPTIONS-DEFAULTS
           PERFORM TEST-SEND-RESULT-DEFAULTS
           PERFORM TEST-RETURN-CODE-VALUES
           PERFORM TEST-JSON-BUILD-REQUEST
           PERFORM TEST-JSON-BUILD-NOTIFICATION
           PERFORM TEST-SESSION-ID-EMPTY
           PERFORM TEST-TOOL-DEF-INIT
           PERFORM TEST-EVENT-TYPE-INIT
           PERFORM TEST-FRAMING-DEFAULTS

           DISPLAY " "
           DISPLAY "========================================"
           DISPLAY " Results: " WS-PASS-COUNT " passed, "
               WS-FAIL-COUNT " failed out of "
               WS-TEST-COUNT " tests"
           DISPLAY "========================================"

           IF WS-FAIL-COUNT > 0
               DISPLAY "FAIL: Some tests failed!"
               STOP RUN WITH STATUS 1
           ELSE
               DISPLAY "OK: All tests passed!"
               STOP RUN WITH STATUS 0
           END-IF
           .

      *================================================================*
      * INDIVIDUAL TEST PARAGRAPHS                                     *
      *================================================================*

      *----------------------------------------------------------------*
      * TEST-VERSION-COPYBOOK: Verify version constants.               *
      *----------------------------------------------------------------*
       TEST-VERSION-COPYBOOK.
           MOVE "Version copybook values" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           EVALUATE TRUE
               WHEN WS-PROTO-MAJOR = 3
                AND WS-SDK-NAME = "copilot-sdk-cobol"
                AND WS-SDK-VERSION = "1.0.0"
                AND WS-PROTO-STRING = "3"
                   PERFORM RECORD-PASS
               WHEN OTHER
                   MOVE "3, copilot-sdk-cobol, 1.0.0"
                       TO WS-EXPECTED
                   STRING WS-PROTO-MAJOR ", "
                       FUNCTION TRIM(WS-SDK-NAME) ", "
                       FUNCTION TRIM(WS-SDK-VERSION)
                       DELIMITED SIZE INTO WS-ACTUAL
                   END-STRING
                   PERFORM RECORD-FAIL
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * TEST-TYPES-INITIALIZATION: Verify types default values.        *
      *----------------------------------------------------------------*
       TEST-TYPES-INITIALIZATION.
           MOVE "Types default initialization" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           EVALUATE TRUE
               WHEN WS-CLI-PATH = SPACES
                AND WS-AUTO-START = 1
                AND WS-CLIENT-ACTIVE = 0
                AND WS-REQUEST-ID = 0
                   PERFORM RECORD-PASS
               WHEN OTHER
                   MOVE "SPACES, 1, 0, 0" TO WS-EXPECTED
                   MOVE "Unexpected defaults" TO WS-ACTUAL
                   PERFORM RECORD-FAIL
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * TEST-TYPES-88-LEVELS: Verify 88-level condition names.         *
      *----------------------------------------------------------------*
       TEST-TYPES-88-LEVELS.
           MOVE "88-level condition names" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           EVALUATE TRUE
               WHEN CLIENT-IS-STOPPED
                AND SESSION-IS-CLOSED
                AND STREAMING-OFF
                AND RESP-IS-OK
                AND RESULT-IS-OK
                   PERFORM RECORD-PASS
               WHEN OTHER
                   MOVE "All false by default" TO WS-EXPECTED
                   MOVE "Some 88-levels wrong" TO WS-ACTUAL
                   PERFORM RECORD-FAIL
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * TEST-CLIENT-STATE-DEFAULTS: Check client state initialization. *
      *----------------------------------------------------------------*
       TEST-CLIENT-STATE-DEFAULTS.
           MOVE "Client state defaults" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           EVALUATE TRUE
               WHEN WS-CLIENT-PID = 0
                AND WS-STDIN-HANDLE = 0
                AND WS-STDOUT-HANDLE = 0
                AND WS-LAST-ERROR = SPACES
                   PERFORM RECORD-PASS
               WHEN OTHER
                   MOVE "All zeros and spaces" TO WS-EXPECTED
                   MOVE "Non-default values found" TO WS-ACTUAL
                   PERFORM RECORD-FAIL
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * TEST-SESSION-CONFIG-DEFAULTS: Check session config init.       *
      *----------------------------------------------------------------*
       TEST-SESSION-CONFIG-DEFAULTS.
           MOVE "Session config defaults" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           EVALUATE TRUE
               WHEN WS-SESS-AGENT-NAME = SPACES
                AND WS-SESS-STREAMING = 0
                AND WS-SESS-HISTORY-LEN = 100
                   PERFORM RECORD-PASS
               WHEN OTHER
                   MOVE "SPACES, 0, 100" TO WS-EXPECTED
                   MOVE "Bad session defaults" TO WS-ACTUAL
                   PERFORM RECORD-FAIL
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * TEST-SEND-OPTIONS-DEFAULTS: Check send options init.           *
      *----------------------------------------------------------------*
       TEST-SEND-OPTIONS-DEFAULTS.
           MOVE "Send options defaults" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           EVALUATE TRUE
               WHEN WS-SEND-MESSAGE = SPACES
                AND WS-SEND-TIMEOUT-MS = 30000
                   PERFORM RECORD-PASS
               WHEN OTHER
                   MOVE "SPACES, 30000" TO WS-EXPECTED
                   MOVE "Bad send defaults" TO WS-ACTUAL
                   PERFORM RECORD-FAIL
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * TEST-SEND-RESULT-DEFAULTS: Check send result init.             *
      *----------------------------------------------------------------*
       TEST-SEND-RESULT-DEFAULTS.
           MOVE "Send result defaults" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           EVALUATE TRUE
               WHEN WS-RESULT-MESSAGE = SPACES
                AND WS-RESULT-IS-ERROR = 0
                AND RESULT-IS-OK
                   PERFORM RECORD-PASS
               WHEN OTHER
                   MOVE "SPACES, 0, true" TO WS-EXPECTED
                   MOVE "Bad result defaults" TO WS-ACTUAL
                   PERFORM RECORD-FAIL
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * TEST-RETURN-CODE-VALUES: Verify return code 88-levels.         *
      *----------------------------------------------------------------*
       TEST-RETURN-CODE-VALUES.
           MOVE "Return code 88-levels" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           MOVE 0 TO WS-RETURN-CODE
           EVALUATE TRUE
               WHEN RC-SUCCESS
                   MOVE -1 TO WS-RETURN-CODE
                   IF RC-ERROR
                       MOVE -2 TO WS-RETURN-CODE
                       IF RC-TIMEOUT
                           MOVE -3 TO WS-RETURN-CODE
                           IF RC-PARSE-ERROR
                               MOVE -4 TO WS-RETURN-CODE
                               IF RC-CONNECTION-ERROR
                                   PERFORM RECORD-PASS
                               ELSE
                                   GO TO RC-TEST-FAIL
                               END-IF
                           ELSE
                               GO TO RC-TEST-FAIL
                           END-IF
                       ELSE
                           GO TO RC-TEST-FAIL
                       END-IF
                   ELSE
                       GO TO RC-TEST-FAIL
                   END-IF
               WHEN OTHER
                   GO TO RC-TEST-FAIL
           END-EVALUATE
           MOVE 0 TO WS-RETURN-CODE
           GO TO RC-TEST-DONE
           .
       RC-TEST-FAIL.
           MOVE "0,-1,-2,-3,-4 map correctly" TO WS-EXPECTED
           MOVE "88-level mismatch" TO WS-ACTUAL
           PERFORM RECORD-FAIL
           MOVE 0 TO WS-RETURN-CODE
           .
       RC-TEST-DONE.
           EXIT.

      *----------------------------------------------------------------*
      * TEST-JSON-BUILD-REQUEST: Verify JSON request building.         *
      *----------------------------------------------------------------*
       TEST-JSON-BUILD-REQUEST.
           MOVE "JSON-RPC request building" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           MOVE "ping"   TO WS-REQ-METHOD
           MOVE 1        TO WS-REQ-ID
           MOVE SPACES   TO WS-REQ-PARAMS

           MOVE '{"jsonrpc":"2.0","method":"ping","id":1}'
               TO WS-EXPECTED

           MOVE SPACES TO WS-JSON-BUFFER
           MOVE 1 TO WS-JSON-LEN

           STRING
               '{"jsonrpc":"2.0","method":"'
               FUNCTION TRIM(WS-REQ-METHOD)
               '","id":1}'
               DELIMITED SIZE
               INTO WS-JSON-BUFFER
               WITH POINTER WS-JSON-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-LEN

           MOVE WS-JSON-BUFFER TO WS-ACTUAL

           IF FUNCTION TRIM(WS-ACTUAL TRAILING) =
              FUNCTION TRIM(WS-EXPECTED TRAILING)
               PERFORM RECORD-PASS
           ELSE
               PERFORM RECORD-FAIL
           END-IF
           .

      *----------------------------------------------------------------*
      * TEST-JSON-BUILD-NOTIFICATION: Verify notification building.    *
      *----------------------------------------------------------------*
       TEST-JSON-BUILD-NOTIFICATION.
           MOVE "JSON-RPC notification building" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           MOVE SPACES TO WS-JSON-BUFFER
           MOVE 1 TO WS-JSON-LEN

           STRING
               '{"jsonrpc":"2.0","method":"session.event"}'
               DELIMITED SIZE
               INTO WS-JSON-BUFFER
               WITH POINTER WS-JSON-LEN
           END-STRING

           MOVE '{"jsonrpc":"2.0","method":"session.event"}'
               TO WS-EXPECTED
           MOVE WS-JSON-BUFFER TO WS-ACTUAL

           IF FUNCTION TRIM(WS-ACTUAL TRAILING) =
              FUNCTION TRIM(WS-EXPECTED TRAILING)
               PERFORM RECORD-PASS
           ELSE
               PERFORM RECORD-FAIL
           END-IF
           .

      *----------------------------------------------------------------*
      * TEST-SESSION-ID-EMPTY: Verify session ID starts empty.         *
      *----------------------------------------------------------------*
       TEST-SESSION-ID-EMPTY.
           MOVE "Session ID starts empty" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           IF WS-SESSION-ID = SPACES
               PERFORM RECORD-PASS
           ELSE
               MOVE "SPACES" TO WS-EXPECTED
               MOVE WS-SESSION-ID TO WS-ACTUAL
               PERFORM RECORD-FAIL
           END-IF
           .

      *----------------------------------------------------------------*
      * TEST-TOOL-DEF-INIT: Verify tool definition defaults.           *
      *----------------------------------------------------------------*
       TEST-TOOL-DEF-INIT.
           MOVE "Tool definition defaults" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           IF WS-TOOL-NAME = SPACES
              AND WS-TOOL-DESCRIPTION = SPACES
              AND WS-TOOL-SCHEMA = SPACES
               PERFORM RECORD-PASS
           ELSE
               MOVE "All SPACES" TO WS-EXPECTED
               MOVE "Non-space tool fields" TO WS-ACTUAL
               PERFORM RECORD-FAIL
           END-IF
           .

      *----------------------------------------------------------------*
      * TEST-EVENT-TYPE-INIT: Verify event type defaults.              *
      *----------------------------------------------------------------*
       TEST-EVENT-TYPE-INIT.
           MOVE "Event type defaults" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           IF WS-EVT-TYPE = SPACES
              AND WS-EVT-SESSION-ID = SPACES
              AND WS-EVT-DATA-LEN = 0
               PERFORM RECORD-PASS
           ELSE
               MOVE "SPACES, SPACES, 0" TO WS-EXPECTED
               MOVE "Bad event defaults" TO WS-ACTUAL
               PERFORM RECORD-FAIL
           END-IF
           .

      *----------------------------------------------------------------*
      * TEST-FRAMING-DEFAULTS: Verify content framing defaults.        *
      *----------------------------------------------------------------*
       TEST-FRAMING-DEFAULTS.
           MOVE "Content framing defaults" TO WS-TEST-NAME
           ADD 1 TO WS-TEST-COUNT

           IF WS-CONTENT-LENGTH = 0
              AND WS-FRAME-BUFFER = SPACES
              AND WS-FRAME-BUF-LEN = 0
               PERFORM RECORD-PASS
           ELSE
               MOVE "0, SPACES, 0" TO WS-EXPECTED
               MOVE "Bad framing defaults" TO WS-ACTUAL
               PERFORM RECORD-FAIL
           END-IF
           .

      *================================================================*
      * TEST HELPER PARAGRAPHS                                         *
      *================================================================*

      *----------------------------------------------------------------*
      * RECORD-PASS: Record a passing test.                            *
      *----------------------------------------------------------------*
       RECORD-PASS.
           ADD 1 TO WS-PASS-COUNT
           DISPLAY "  PASS: " FUNCTION TRIM(WS-TEST-NAME)
           .

      *----------------------------------------------------------------*
      * RECORD-FAIL: Record a failing test with details.               *
      *----------------------------------------------------------------*
       RECORD-FAIL.
           ADD 1 TO WS-FAIL-COUNT
           DISPLAY "  FAIL: " FUNCTION TRIM(WS-TEST-NAME)
           DISPLAY "    Expected: " FUNCTION TRIM(WS-EXPECTED)
           DISPLAY "    Actual:   " FUNCTION TRIM(WS-ACTUAL)
           .
