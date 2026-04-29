      *================================================================*
      * COPILOT-JSONRPC.cob                                            *
      * JSON-RPC 2.0 message builder and parser for the Copilot SDK.   *
      * Uses STRING/UNSTRING for JSON construction and parsing.        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPILOT-JSONRPC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY "COPILOT-TYPES.cpy".

       01  WS-JSON-BUFFER           PIC X(8192) VALUE SPACES.
       01  WS-JSON-LEN              PIC 9(5)    VALUE 0.
       01  WS-TEMP-NUM              PIC X(10)   VALUE SPACES.
       01  WS-TALLY-CTR             PIC 9(5)    VALUE 0.
       01  WS-PARSE-POS             PIC 9(5)    VALUE 1.
       01  WS-PARSE-TOKEN           PIC X(256)  VALUE SPACES.
       01  WS-PARSE-KEY             PIC X(64)   VALUE SPACES.
       01  WS-PARSE-VALUE           PIC X(4096) VALUE SPACES.
       01  WS-DELIM-CHAR            PIC X       VALUE SPACES.
       01  WS-QUOTE                 PIC X       VALUE '"'.
       01  WS-BRACE-DEPTH           PIC 9(3)    VALUE 0.
       01  WS-SCAN-IDX              PIC 9(5)    VALUE 0.
       01  WS-SCAN-CHAR             PIC X       VALUE SPACES.

       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
      * BUILD-REQUEST: Build a JSON-RPC 2.0 request message.           *
      * Input:  WS-REQ-METHOD, WS-REQ-ID, WS-REQ-PARAMS               *
      * Output: WS-JSON-BUFFER, WS-JSON-LEN                           *
      *----------------------------------------------------------------*
       BUILD-REQUEST.
           MOVE SPACES TO WS-JSON-BUFFER
           MOVE SPACES TO WS-TEMP-NUM

           MOVE WS-REQ-ID TO WS-TEMP-NUM
           INSPECT WS-TEMP-NUM REPLACING LEADING
               SPACES BY ZEROES

           STRING
               '{"jsonrpc":"2.0","method":"'
               FUNCTION TRIM(WS-REQ-METHOD)
               '","id":'
               FUNCTION TRIM(WS-TEMP-NUM LEADING)
               DELIMITED SIZE
               INTO WS-JSON-BUFFER
               WITH POINTER WS-JSON-LEN
           END-STRING

           IF WS-REQ-PARAMS NOT = SPACES
               STRING
                   ',"params":'
                   FUNCTION TRIM(WS-REQ-PARAMS)
                   DELIMITED SIZE
                   INTO WS-JSON-BUFFER
                   WITH POINTER WS-JSON-LEN
               END-STRING
           END-IF

           STRING
               '}'
               DELIMITED SIZE
               INTO WS-JSON-BUFFER
               WITH POINTER WS-JSON-LEN
           END-STRING

           SUBTRACT 1 FROM WS-JSON-LEN
           .

      *----------------------------------------------------------------*
      * BUILD-NOTIFICATION: Build a JSON-RPC 2.0 notification (no id). *
      * Input:  WS-REQ-METHOD, WS-REQ-PARAMS                          *
      * Output: WS-JSON-BUFFER, WS-JSON-LEN                           *
      *----------------------------------------------------------------*
       BUILD-NOTIFICATION.
           MOVE SPACES TO WS-JSON-BUFFER
           MOVE 1 TO WS-JSON-LEN

           STRING
               '{"jsonrpc":"2.0","method":"'
               FUNCTION TRIM(WS-REQ-METHOD)
               '"'
               DELIMITED SIZE
               INTO WS-JSON-BUFFER
               WITH POINTER WS-JSON-LEN
           END-STRING

           IF WS-REQ-PARAMS NOT = SPACES
               STRING
                   ',"params":'
                   FUNCTION TRIM(WS-REQ-PARAMS)
                   DELIMITED SIZE
                   INTO WS-JSON-BUFFER
                   WITH POINTER WS-JSON-LEN
               END-STRING
           END-IF

           STRING
               '}'
               DELIMITED SIZE
               INTO WS-JSON-BUFFER
               WITH POINTER WS-JSON-LEN
           END-STRING

           SUBTRACT 1 FROM WS-JSON-LEN
           .

      *----------------------------------------------------------------*
      * BUILD-CONTENT-FRAME: Wrap JSON in Content-Length framing.       *
      * Input:  WS-JSON-BUFFER, WS-JSON-LEN                           *
      * Output: WS-FRAME-BUFFER, WS-FRAME-BUF-LEN                    *
      *----------------------------------------------------------------*
       BUILD-CONTENT-FRAME.
           MOVE SPACES TO WS-FRAME-BUFFER
           MOVE SPACES TO WS-TEMP-NUM
           MOVE WS-JSON-LEN TO WS-TEMP-NUM
           MOVE 1 TO WS-FRAME-BUF-LEN

           STRING
               'Content-Length: '
               FUNCTION TRIM(WS-TEMP-NUM LEADING)
               X'0D0A'
               X'0D0A'
               WS-JSON-BUFFER(1:WS-JSON-LEN)
               DELIMITED SIZE
               INTO WS-FRAME-BUFFER
               WITH POINTER WS-FRAME-BUF-LEN
           END-STRING

           SUBTRACT 1 FROM WS-FRAME-BUF-LEN
           .

      *----------------------------------------------------------------*
      * PARSE-RESPONSE: Parse a JSON-RPC 2.0 response.                 *
      * Input:  WS-JSON-BUFFER                                         *
      * Output: WS-JSONRPC-RESPONSE fields                             *
      *----------------------------------------------------------------*
       PARSE-RESPONSE.
           MOVE SPACES TO WS-RESP-JSONRPC
           MOVE 0      TO WS-RESP-ID
           MOVE 0      TO WS-RESP-HAS-ERROR
           MOVE 0      TO WS-RESP-ERROR-CODE
           MOVE SPACES TO WS-RESP-ERROR-MSG
           MOVE SPACES TO WS-RESP-RESULT
           MOVE 0      TO WS-RESP-RESULT-LEN

           PERFORM EXTRACT-JSONRPC-FIELD
           PERFORM EXTRACT-ID-FIELD
           PERFORM EXTRACT-ERROR-OR-RESULT
           .

      *----------------------------------------------------------------*
      * EXTRACT-JSONRPC-FIELD: Pull "jsonrpc" value from JSON.         *
      *----------------------------------------------------------------*
       EXTRACT-JSONRPC-FIELD.
           MOVE SPACES TO WS-PARSE-VALUE
           MOVE '"jsonrpc"' TO WS-PARSE-KEY
           PERFORM FIND-STRING-VALUE
           IF WS-PARSE-VALUE NOT = SPACES
               MOVE WS-PARSE-VALUE TO WS-RESP-JSONRPC
           END-IF
           .

      *----------------------------------------------------------------*
      * EXTRACT-ID-FIELD: Pull "id" value from JSON.                   *
      *----------------------------------------------------------------*
       EXTRACT-ID-FIELD.
           MOVE SPACES TO WS-PARSE-VALUE
           MOVE '"id"' TO WS-PARSE-KEY
           PERFORM FIND-NUMERIC-VALUE
           IF WS-PARSE-VALUE NOT = SPACES
               MOVE FUNCTION NUMVAL(WS-PARSE-VALUE) TO
                   WS-RESP-ID
           END-IF
           .

      *----------------------------------------------------------------*
      * EXTRACT-ERROR-OR-RESULT: Determine if response has error or    *
      * result, then extract the appropriate field.                    *
      *----------------------------------------------------------------*
       EXTRACT-ERROR-OR-RESULT.
           INSPECT WS-JSON-BUFFER TALLYING WS-TALLY-CTR
               FOR ALL '"error"'
           IF WS-TALLY-CTR > 0
               MOVE 1 TO WS-RESP-HAS-ERROR
               PERFORM EXTRACT-ERROR-FIELDS
           ELSE
               PERFORM EXTRACT-RESULT-FIELD
           END-IF
           MOVE 0 TO WS-TALLY-CTR
           .

      *----------------------------------------------------------------*
      * EXTRACT-ERROR-FIELDS: Pull error code and message.             *
      *----------------------------------------------------------------*
       EXTRACT-ERROR-FIELDS.
           MOVE '"message"' TO WS-PARSE-KEY
           PERFORM FIND-STRING-VALUE
           MOVE WS-PARSE-VALUE TO WS-RESP-ERROR-MSG
           .

      *----------------------------------------------------------------*
      * EXTRACT-RESULT-FIELD: Pull the result object as raw JSON.      *
      *----------------------------------------------------------------*
       EXTRACT-RESULT-FIELD.
           MOVE '"result"' TO WS-PARSE-KEY
           PERFORM FIND-OBJECT-VALUE
           MOVE WS-PARSE-VALUE TO WS-RESP-RESULT
           MOVE FUNCTION LENGTH(FUNCTION TRIM(
               WS-PARSE-VALUE TRAILING))
               TO WS-RESP-RESULT-LEN
           .

      *----------------------------------------------------------------*
      * FIND-STRING-VALUE: Locate a key and extract its string value.  *
      * Input:  WS-PARSE-KEY, WS-JSON-BUFFER                          *
      * Output: WS-PARSE-VALUE                                        *
      *----------------------------------------------------------------*
       FIND-STRING-VALUE.
           MOVE SPACES TO WS-PARSE-VALUE
           MOVE 0 TO WS-SCAN-IDX

           INSPECT WS-JSON-BUFFER TALLYING WS-SCAN-IDX
               FOR CHARACTERS BEFORE INITIAL WS-PARSE-KEY

           IF WS-SCAN-IDX > 0
               ADD FUNCTION LENGTH(FUNCTION TRIM(WS-PARSE-KEY))
                   TO WS-SCAN-IDX
               ADD 1 TO WS-SCAN-IDX

               PERFORM SKIP-TO-QUOTE
               ADD 1 TO WS-SCAN-IDX
               UNSTRING WS-JSON-BUFFER
                   DELIMITED '"'
                   INTO WS-PARSE-VALUE
                   WITH POINTER WS-SCAN-IDX
               END-UNSTRING
           END-IF
           MOVE 0 TO WS-SCAN-IDX
           .

      *----------------------------------------------------------------*
      * FIND-NUMERIC-VALUE: Locate a key and extract numeric value.    *
      *----------------------------------------------------------------*
       FIND-NUMERIC-VALUE.
           MOVE SPACES TO WS-PARSE-VALUE
           MOVE 0 TO WS-SCAN-IDX

           INSPECT WS-JSON-BUFFER TALLYING WS-SCAN-IDX
               FOR CHARACTERS BEFORE INITIAL WS-PARSE-KEY

           IF WS-SCAN-IDX > 0
               ADD FUNCTION LENGTH(FUNCTION TRIM(WS-PARSE-KEY))
                   TO WS-SCAN-IDX
               ADD 2 TO WS-SCAN-IDX
               UNSTRING WS-JSON-BUFFER
                   DELIMITED ',' OR '}'
                   INTO WS-PARSE-VALUE
                   WITH POINTER WS-SCAN-IDX
               END-UNSTRING
           END-IF
           MOVE 0 TO WS-SCAN-IDX
           .

      *----------------------------------------------------------------*
      * FIND-OBJECT-VALUE: Extract a JSON object value (brace match).  *
      *----------------------------------------------------------------*
       FIND-OBJECT-VALUE.
           MOVE SPACES TO WS-PARSE-VALUE
           MOVE 0 TO WS-SCAN-IDX
           MOVE 0 TO WS-BRACE-DEPTH

           INSPECT WS-JSON-BUFFER TALLYING WS-SCAN-IDX
               FOR CHARACTERS BEFORE INITIAL WS-PARSE-KEY

           IF WS-SCAN-IDX > 0
               ADD FUNCTION LENGTH(FUNCTION TRIM(WS-PARSE-KEY))
                   TO WS-SCAN-IDX
               ADD 2 TO WS-SCAN-IDX

               PERFORM SCAN-OBJECT-BRACES
           END-IF
           MOVE 0 TO WS-SCAN-IDX
           .

      *----------------------------------------------------------------*
      * SCAN-OBJECT-BRACES: Walk forward matching { } braces.         *
      *----------------------------------------------------------------*
       SCAN-OBJECT-BRACES.
           MOVE 0 TO WS-BRACE-DEPTH
           MOVE WS-SCAN-IDX TO WS-PARSE-POS

           PERFORM VARYING WS-SCAN-IDX
               FROM WS-PARSE-POS BY 1
               UNTIL WS-SCAN-IDX >
                   FUNCTION LENGTH(WS-JSON-BUFFER)

               MOVE WS-JSON-BUFFER(WS-SCAN-IDX:1)
                   TO WS-SCAN-CHAR

               EVALUATE WS-SCAN-CHAR
                   WHEN '{'
                       ADD 1 TO WS-BRACE-DEPTH
                   WHEN '}'
                       SUBTRACT 1 FROM WS-BRACE-DEPTH
                       IF WS-BRACE-DEPTH = 0
                           COMPUTE WS-TALLY-CTR =
                               WS-SCAN-IDX - WS-PARSE-POS + 1
                           MOVE WS-JSON-BUFFER(
                               WS-PARSE-POS:WS-TALLY-CTR)
                               TO WS-PARSE-VALUE
                           MOVE 0 TO WS-TALLY-CTR
                           EXIT PERFORM
                       END-IF
               END-EVALUATE
           END-PERFORM
           .

      *----------------------------------------------------------------*
      * SKIP-TO-QUOTE: Advance WS-SCAN-IDX to the next quote char.    *
      *----------------------------------------------------------------*
       SKIP-TO-QUOTE.
           PERFORM VARYING WS-SCAN-IDX
               FROM WS-SCAN-IDX BY 1
               UNTIL WS-SCAN-IDX >
                   FUNCTION LENGTH(WS-JSON-BUFFER)
               IF WS-JSON-BUFFER(WS-SCAN-IDX:1) = '"'
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

       STOP RUN.
