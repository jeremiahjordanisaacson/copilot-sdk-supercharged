      *================================================================*
      * COPILOT-CLIENT.cob                                             *
      * Main client for the GitHub Copilot SDK.                        *
      * Manages the CLI process lifecycle, I/O pipes, and dispatches   *
      * JSON-RPC calls to the session and JSONRPC modules.             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPILOT-CLIENT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLI-INPUT  ASSIGN TO WS-CLI-INPUT-FILE
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT CLI-OUTPUT ASSIGN TO WS-CLI-OUTPUT-FILE
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.
       FD  CLI-INPUT.
       01  CLI-INPUT-RECORD         PIC X(8192).

       FD  CLI-OUTPUT.
       01  CLI-OUTPUT-RECORD        PIC X(8192).

       WORKING-STORAGE SECTION.

       COPY "COPILOT-TYPES.cpy".
       COPY "COPILOT-VERSION.cpy".

       01  WS-FILE-STATUS           PIC XX      VALUE "00".
       01  WS-CLI-INPUT-FILE        PIC X(256)  VALUE SPACES.
       01  WS-CLI-OUTPUT-FILE       PIC X(256)  VALUE SPACES.
       01  WS-CLI-COMMAND           PIC X(512)  VALUE SPACES.
       01  WS-CMD-PTR               PIC 9(5)    VALUE 1.
       01  WS-PIPE-IN-NAME          PIC X(128)  VALUE SPACES.
       01  WS-PIPE-OUT-NAME         PIC X(128)  VALUE SPACES.
       01  WS-READ-BUFFER           PIC X(8192) VALUE SPACES.
       01  WS-READ-LEN              PIC 9(5)    VALUE 0.
       01  WS-WRITE-BUFFER          PIC X(8192) VALUE SPACES.
       01  WS-WRITE-LEN             PIC 9(5)    VALUE 0.
       01  WS-HEADER-LINE           PIC X(256)  VALUE SPACES.
       01  WS-HEADER-KEY            PIC X(64)   VALUE SPACES.
       01  WS-HEADER-VALUE          PIC X(64)   VALUE SPACES.
       01  WS-BODY-LENGTH           PIC 9(10)   VALUE 0.
       01  WS-TEMP-STRING           PIC X(256)  VALUE SPACES.
       01  WS-SYSTEM-RC             PIC S9(4)   VALUE 0.
       01  WS-IO-RETURN-CODE        PIC S9(4)   VALUE 0.
       01  WS-JSON-WORK             PIC X(8192) VALUE SPACES.
       01  WS-JSON-WORK-LEN         PIC 9(5)    VALUE 0.
       01  WS-FG-SESSION-ID         PIC X(256)  VALUE SPACES.

       LINKAGE SECTION.
       01  LS-REQUEST               PIC X(8192).
       01  LS-RESPONSE              PIC X(8192).
       01  LS-RETURN-CODE           PIC S9(4).

       PROCEDURE DIVISION.

      *================================================================*
      * PUBLIC ENTRY POINTS                                            *
      *================================================================*

      *----------------------------------------------------------------*
      * MAIN-ENTRY: Default entry when called without parameters.      *
      *----------------------------------------------------------------*
       MAIN-ENTRY.
           DISPLAY "COPILOT-CLIENT: Use CALL interface"
           STOP RUN
           .

      *----------------------------------------------------------------*
      * INITIALIZE-CLIENT: Set up the client with given options.       *
      * Input:  WS-CLIENT-OPTIONS                                      *
      * Output: WS-CLIENT-STATE, WS-RETURN-CODE                       *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-CLIENT-INIT" USING WS-CLIENT-OPTIONS
           WS-RETURN-CODE.
           PERFORM INIT-CLIENT
           GOBACK
           .

      *----------------------------------------------------------------*
      * START-CLIENT: Launch the CLI process and open pipes.           *
      * Output: WS-CLIENT-STATE, WS-RETURN-CODE                       *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-CLIENT-START" USING WS-RETURN-CODE.
           PERFORM START-CLI-PROCESS
           GOBACK
           .

      *----------------------------------------------------------------*
      * STOP-CLIENT: Shut down the CLI process and close pipes.        *
      * Output: WS-RETURN-CODE                                        *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-CLIENT-STOP" USING WS-RETURN-CODE.
           PERFORM STOP-CLI-PROCESS
           GOBACK
           .

      *----------------------------------------------------------------*
      * PING: Send a health-check ping to the CLI.                     *
      * Output: WS-RETURN-CODE (0 = healthy)                          *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-CLIENT-PING" USING WS-RETURN-CODE.
           PERFORM PING-CLI
           GOBACK
           .

      *----------------------------------------------------------------*
      * GET-FG-SESSION: Get the foreground session ID.                 *
      * Output: WS-FG-SESSION-ID, WS-RETURN-CODE                     *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-GET-FG-SESSION" USING WS-FG-SESSION-ID
           WS-RETURN-CODE.
           PERFORM GET-FOREGROUND-SESSION
           GOBACK
           .

      *----------------------------------------------------------------*
      * SET-FG-SESSION: Set the foreground session ID.                 *
      * Input:  WS-FG-SESSION-ID                                      *
      * Output: WS-RETURN-CODE                                        *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-SET-FG-SESSION" USING WS-FG-SESSION-ID
           WS-RETURN-CODE.
           PERFORM SET-FOREGROUND-SESSION
           GOBACK
           .

      *----------------------------------------------------------------*
      * SEND-RECEIVE: Send a JSON-RPC message and get response.        *
      * Input:  LS-REQUEST                                             *
      * Output: LS-RESPONSE, LS-RETURN-CODE                           *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-CLIENT" USING LS-REQUEST
           LS-RESPONSE LS-RETURN-CODE.
           PERFORM SEND-AND-RECEIVE
           GOBACK
           .

      *================================================================*
      * INTERNAL PARAGRAPHS                                            *
      *================================================================*

      *----------------------------------------------------------------*
      * INIT-CLIENT: Validate options and prepare state.               *
      *----------------------------------------------------------------*
       INIT-CLIENT.
           MOVE 0 TO WS-REQUEST-ID
           SET CLIENT-IS-STOPPED TO TRUE
           MOVE SPACES TO WS-LAST-ERROR

           IF WS-CLI-PATH = SPACES AND WS-CLI-URL = SPACES
               MOVE "copilot" TO WS-CLI-PATH
           END-IF

           IF WS-AUTO-START = 1
               PERFORM START-CLI-PROCESS
           END-IF

           MOVE 0 TO WS-RETURN-CODE
           .

      *----------------------------------------------------------------*
      * START-CLI-PROCESS: Launch the Copilot CLI in server mode.      *
      * Uses CALL "SYSTEM" to start the process and sets up named      *
      * pipes or temporary files for communication.                    *
      *----------------------------------------------------------------*
       START-CLI-PROCESS.
           IF CLIENT-IS-ACTIVE
               MOVE 0 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           IF WS-CLI-URL NOT = SPACES
               SET CLIENT-IS-ACTIVE TO TRUE
               MOVE 0 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           PERFORM SETUP-PIPE-NAMES

           MOVE SPACES TO WS-CLI-COMMAND
           MOVE 1 TO WS-CMD-PTR
           STRING
               FUNCTION TRIM(WS-CLI-PATH)
               " server --stdio"
               DELIMITED SIZE
               INTO WS-CLI-COMMAND
               WITH POINTER WS-CMD-PTR
           END-STRING

           CALL "SYSTEM" USING WS-CLI-COMMAND
               RETURNING WS-SYSTEM-RC

           IF WS-SYSTEM-RC NOT = 0
               MOVE -4 TO WS-RETURN-CODE
               STRING
                   "Failed to start CLI: rc="
                   WS-SYSTEM-RC
                   DELIMITED SIZE
                   INTO WS-LAST-ERROR
               END-STRING
               EXIT PARAGRAPH
           END-IF

           PERFORM OPEN-COMMUNICATION

           IF WS-IO-RETURN-CODE = 0
               SET CLIENT-IS-ACTIVE TO TRUE
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Failed to open communication"
                   TO WS-LAST-ERROR
           END-IF
           .

      *----------------------------------------------------------------*
      * SETUP-PIPE-NAMES: Generate pipe/file names for I/O.            *
      *----------------------------------------------------------------*
       SETUP-PIPE-NAMES.
           MOVE "copilot_sdk_in.pipe" TO WS-PIPE-IN-NAME
           MOVE "copilot_sdk_out.pipe" TO WS-PIPE-OUT-NAME
           MOVE WS-PIPE-IN-NAME TO WS-CLI-INPUT-FILE
           MOVE WS-PIPE-OUT-NAME TO WS-CLI-OUTPUT-FILE
           .

      *----------------------------------------------------------------*
      * OPEN-COMMUNICATION: Open the I/O files or pipes.               *
      *----------------------------------------------------------------*
       OPEN-COMMUNICATION.
           OPEN OUTPUT CLI-INPUT
           IF WS-FILE-STATUS NOT = "00"
               MOVE -1 TO WS-IO-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           OPEN INPUT CLI-OUTPUT
           IF WS-FILE-STATUS NOT = "00"
               CLOSE CLI-INPUT
               MOVE -1 TO WS-IO-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE 0 TO WS-IO-RETURN-CODE
           .

      *----------------------------------------------------------------*
      * STOP-CLI-PROCESS: Shut down the CLI and close files.           *
      *----------------------------------------------------------------*
       STOP-CLI-PROCESS.
           IF NOT CLIENT-IS-ACTIVE
               MOVE 0 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           CLOSE CLI-INPUT
           CLOSE CLI-OUTPUT

           SET CLIENT-IS-STOPPED TO TRUE
           MOVE 0 TO WS-RETURN-CODE
           .

      *----------------------------------------------------------------*
      * PING-CLI: Send a ping request to verify the CLI is alive.      *
      *----------------------------------------------------------------*
       PING-CLI.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE '{"jsonrpc":"2.0","method":"ping","id":0}'
               TO WS-WRITE-BUFFER
           MOVE 41 TO WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * GET-LAST-SESSION-ID: Get the last session ID.                  *
      * Output: WS-FG-SESSION-ID, WS-RETURN-CODE                     *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-GET-LAST-SESSION-ID" USING WS-FG-SESSION-ID
           WS-RETURN-CODE.
           PERFORM GET-LAST-SESSION-ID-PARA
           GOBACK
           .

      *----------------------------------------------------------------*
      * GET-SESSION-METADATA: Get metadata for a session.              *
      * Input:  WS-FG-SESSION-ID                                      *
      * Output: LS-RESPONSE, WS-RETURN-CODE                           *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-GET-SESSION-METADATA" USING WS-FG-SESSION-ID
           LS-RESPONSE WS-RETURN-CODE.
           PERFORM GET-SESSION-METADATA-PARA
           GOBACK
           .

      *----------------------------------------------------------------*
      * LIST-MODELS: List available models.                            *
      * Output: LS-RESPONSE, WS-RETURN-CODE                           *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-LIST-MODELS" USING LS-RESPONSE
           WS-RETURN-CODE.
           PERFORM LIST-MODELS-PARA
           GOBACK
           .

      *----------------------------------------------------------------*
      * GET-STATUS: Get the server status.                             *
      * Output: LS-RESPONSE, WS-RETURN-CODE                           *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-GET-STATUS" USING LS-RESPONSE
           WS-RETURN-CODE.
           PERFORM GET-STATUS-PARA
           GOBACK
           .

      *----------------------------------------------------------------*
      * GET-AUTH-STATUS: Get the authentication status.                *
      * Output: LS-RESPONSE, WS-RETURN-CODE                           *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-GET-AUTH-STATUS" USING LS-RESPONSE
           WS-RETURN-CODE.
           PERFORM GET-AUTH-STATUS-PARA
           GOBACK
           .

      *----------------------------------------------------------------*
      * CREATE-SESSION: Create a new conversation session.             *
      * Input:  LS-REQUEST (JSON params)                               *
      * Supports: authToken / auth_token, idleTimeout / idle_timeout,  *
      *   elicitation, skills, excludedTools / excluded_tools,         *
      *   requestHeaders / request_headers, modelCapabilities /        *
      *   model_capabilities, configDiscovery / config_discovery,      *
      *   subAgentStreaming / sub_agent_streaming, imageGeneration /    *
      *   image_generation / responseFormat / response_format           *
      * Output: LS-RESPONSE, WS-RETURN-CODE                           *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-CREATE-SESSION" USING LS-REQUEST
           LS-RESPONSE WS-RETURN-CODE.
           PERFORM CREATE-SESSION-PARA
           GOBACK
           .

      *----------------------------------------------------------------*
      * RESUME-SESSION: Resume an existing session by ID.              *
      * Input:  WS-FG-SESSION-ID, LS-REQUEST (JSON params)            *
      * Output: LS-RESPONSE, WS-RETURN-CODE                           *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-RESUME-SESSION" USING WS-FG-SESSION-ID
           LS-REQUEST LS-RESPONSE WS-RETURN-CODE.
           PERFORM RESUME-SESSION-PARA
           GOBACK
           .

      *----------------------------------------------------------------*
      * DELETE-SESSION: Delete a session permanently.                  *
      * Input:  WS-FG-SESSION-ID                                      *
      * Output: WS-RETURN-CODE                                        *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-DELETE-SESSION" USING WS-FG-SESSION-ID
           WS-RETURN-CODE.
           PERFORM DELETE-SESSION-PARA
           GOBACK
           .

      *----------------------------------------------------------------*
      * LIST-SESSIONS: List all sessions known to the server.          *
      * Output: LS-RESPONSE, WS-RETURN-CODE                           *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-LIST-SESSIONS" USING LS-RESPONSE
           WS-RETURN-CODE.
           PERFORM LIST-SESSIONS-PARA
           GOBACK
           .

      *----------------------------------------------------------------*
      * SEND-AND-RECEIVE: Write request, read response over pipes.     *
      *----------------------------------------------------------------*
       SEND-AND-RECEIVE.
           MOVE LS-REQUEST TO WS-WRITE-BUFFER
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(LS-REQUEST TRAILING))
               TO WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE NOT = 0
               MOVE -1 TO LS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO LS-RESPONSE
               MOVE 0 TO LS-RETURN-CODE
           ELSE
               MOVE -1 TO LS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * GET-FOREGROUND-SESSION: Get the current foreground session.     *
      *----------------------------------------------------------------*
       GET-FOREGROUND-SESSION.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"session.getForeground",'
               '"params":{},'
               '"id":1}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO WS-FG-SESSION-ID
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * SET-FOREGROUND-SESSION: Set the foreground session.             *
      *----------------------------------------------------------------*
       SET-FOREGROUND-SESSION.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"session.setForeground",'
               '"params":{"sessionId":"'
               FUNCTION TRIM(WS-FG-SESSION-ID)
               '"},"id":2}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Failed to set foreground session"
                   TO WS-LAST-ERROR
           END-IF
           .

      *----------------------------------------------------------------*
      * WRITE-FRAMED-MESSAGE: Write with Content-Length header.         *
      *----------------------------------------------------------------*
       WRITE-FRAMED-MESSAGE.
           MOVE 0 TO WS-IO-RETURN-CODE
           MOVE SPACES TO WS-TEMP-STRING
           MOVE WS-WRITE-LEN TO WS-TEMP-STRING

           MOVE SPACES TO WS-JSON-WORK
           MOVE 1 TO WS-JSON-WORK-LEN

           STRING
               "Content-Length: "
               FUNCTION TRIM(WS-TEMP-STRING LEADING)
               DELIMITED SIZE
               INTO WS-JSON-WORK
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING

           WRITE CLI-INPUT-RECORD FROM WS-JSON-WORK
           IF WS-FILE-STATUS NOT = "00"
               MOVE -1 TO WS-IO-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO CLI-INPUT-RECORD
           WRITE CLI-INPUT-RECORD
           IF WS-FILE-STATUS NOT = "00"
               MOVE -1 TO WS-IO-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           WRITE CLI-INPUT-RECORD
               FROM WS-WRITE-BUFFER(1:WS-WRITE-LEN)
           IF WS-FILE-STATUS NOT = "00"
               MOVE -1 TO WS-IO-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * READ-FRAMED-MESSAGE: Read Content-Length header then body.      *
      *----------------------------------------------------------------*
       READ-FRAMED-MESSAGE.
           MOVE 0 TO WS-IO-RETURN-CODE
           MOVE SPACES TO WS-READ-BUFFER
           MOVE 0 TO WS-BODY-LENGTH

           PERFORM READ-CONTENT-LENGTH-HEADER
           IF WS-IO-RETURN-CODE NOT = 0
               EXIT PARAGRAPH
           END-IF

           PERFORM READ-EMPTY-LINE
           PERFORM READ-BODY-DATA
           .

      *----------------------------------------------------------------*
      * READ-CONTENT-LENGTH-HEADER: Parse the Content-Length line.      *
      *----------------------------------------------------------------*
       READ-CONTENT-LENGTH-HEADER.
           READ CLI-OUTPUT INTO WS-HEADER-LINE
               AT END
                   MOVE -1 TO WS-IO-RETURN-CODE
                   EXIT PARAGRAPH
           END-READ

           IF WS-FILE-STATUS NOT = "00"
               MOVE -1 TO WS-IO-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           UNSTRING WS-HEADER-LINE
               DELIMITED ": "
               INTO WS-HEADER-KEY WS-HEADER-VALUE
           END-UNSTRING

           IF WS-HEADER-KEY = "Content-Length"
               MOVE FUNCTION NUMVAL(
                   FUNCTION TRIM(WS-HEADER-VALUE))
                   TO WS-BODY-LENGTH
           ELSE
               MOVE -3 TO WS-IO-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * READ-EMPTY-LINE: Consume the blank separator line.             *
      *----------------------------------------------------------------*
       READ-EMPTY-LINE.
           READ CLI-OUTPUT INTO WS-HEADER-LINE
               AT END
                   MOVE -1 TO WS-IO-RETURN-CODE
           END-READ
           .

      *----------------------------------------------------------------*
      * READ-BODY-DATA: Read the JSON body of the given length.        *
      *----------------------------------------------------------------*
       READ-BODY-DATA.
           READ CLI-OUTPUT INTO WS-READ-BUFFER
               AT END
                   MOVE -1 TO WS-IO-RETURN-CODE
                   EXIT PARAGRAPH
           END-READ

           IF WS-FILE-STATUS = "00"
               MOVE WS-BODY-LENGTH TO WS-READ-LEN
           ELSE
               MOVE -1 TO WS-IO-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * GET-LAST-SESSION-ID-PARA: Retrieve the last session ID.        *
      *----------------------------------------------------------------*
       GET-LAST-SESSION-ID-PARA.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"session.getLastId",'
               '"params":{},'
               '"id":3}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO WS-FG-SESSION-ID
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * GET-SESSION-METADATA-PARA: Get metadata for a given session.   *
      *----------------------------------------------------------------*
       GET-SESSION-METADATA-PARA.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"session.getMetadata",'
               '"params":{"sessionId":"'
               FUNCTION TRIM(WS-FG-SESSION-ID)
               '"},"id":4}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO LS-RESPONSE
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * LIST-MODELS-PARA: List available models from the server.       *
      *----------------------------------------------------------------*
       LIST-MODELS-PARA.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"models.list",'
               '"params":{},'
               '"id":5}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO LS-RESPONSE
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * GET-STATUS-PARA: Get the server status.                        *
      *----------------------------------------------------------------*
       GET-STATUS-PARA.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"status.get",'
               '"params":{},'
               '"id":6}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO LS-RESPONSE
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * GET-AUTH-STATUS-PARA: Get the authentication status.           *
      *----------------------------------------------------------------*
       GET-AUTH-STATUS-PARA.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"auth.getStatus",'
               '"params":{},'
               '"id":7}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO LS-RESPONSE
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * CREATE-SESSION-PARA: Create a new conversation session.        *
      *----------------------------------------------------------------*
       CREATE-SESSION-PARA.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"session.create",'
               '"params":'
               FUNCTION TRIM(LS-REQUEST TRAILING)
               ','
               '"id":8}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO LS-RESPONSE
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * RESUME-SESSION-PARA: Resume an existing session by ID.         *
      *----------------------------------------------------------------*
       RESUME-SESSION-PARA.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"session.resume",'
               '"params":{"sessionId":"'
               FUNCTION TRIM(WS-FG-SESSION-ID)
               '"},"id":9}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO LS-RESPONSE
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * DELETE-SESSION-PARA: Delete a session permanently.             *
      *----------------------------------------------------------------*
       DELETE-SESSION-PARA.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"session.delete",'
               '"params":{"sessionId":"'
               FUNCTION TRIM(WS-FG-SESSION-ID)
               '"},"id":10}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * LIST-SESSIONS-PARA: List all sessions from the server.         *
      *----------------------------------------------------------------*
       LIST-SESSIONS-PARA.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"session.list",'
               '"params":{},'
               '"id":11}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO LS-RESPONSE
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * SEND-RPC-REQUEST: Build and send a generic JSON-RPC request.    *
      * Uses WS-REQ-METHOD, WS-REQ-PARAMS, WS-REQ-PARAMS-LEN.         *
      *----------------------------------------------------------------*
       SEND-RPC-REQUEST.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO WS-REQUEST-ID

           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           STRING
               '{"jsonrpc":"2.0",'
               '"method":"'
               FUNCTION TRIM(WS-REQ-METHOD TRAILING)
               '",'
               '"params":'
               WS-REQ-PARAMS(1:WS-REQ-PARAMS-LEN)
               ','
               '"id":'
               FUNCTION TRIM(WS-REQUEST-ID)
               '}'
               DELIMITED SIZE
               INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN

           PERFORM WRITE-FRAMED-MESSAGE
           PERFORM READ-FRAMED-MESSAGE

           IF WS-IO-RETURN-CODE = 0
               MOVE 0 TO WS-RETURN-CODE
           ELSE
               MOVE -4 TO WS-RETURN-CODE
           END-IF
           .

      *----------------------------------------------------------------*
      * SET-SESSION-FS-PROVIDER                                        *
      * Register a session filesystem provider via                     *
      * sessionFs.setProvider                                          *
      *----------------------------------------------------------------*
       SET-SESSION-FS-PROVIDER.
           IF NOT SESSION-FS-ON
               SET RC-SUCCESS TO TRUE
               GOBACK
           END-IF

           MOVE SPACES TO WS-REQ-PARAMS
           STRING
               '{"initialCwd":"'
               FUNCTION TRIM(WS-SFS-INITIAL-CWD TRAILING)
               '","sessionStatePath":"'
               FUNCTION TRIM(WS-SFS-STATE-PATH TRAILING)
               '","conventions":"'
               FUNCTION TRIM(WS-SFS-CONVENTIONS TRAILING)
               '"}'
               DELIMITED BY SIZE
               INTO WS-REQ-PARAMS
           END-STRING

           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-REQ-PARAMS TRAILING))
               TO WS-REQ-PARAMS-LEN

           MOVE "sessionFs.setProvider" TO WS-REQ-METHOD
           PERFORM SEND-RPC-REQUEST

           .

      *----------------------------------------------------------------*
      * ADD-MCP-SERVER                                                 *
      * Register an MCP server with the CLI.                           *
      *----------------------------------------------------------------*
       ADD-MCP-SERVER.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-REQ-PARAMS
           STRING
               '{"name":"'
               FUNCTION TRIM(WS-MCP-SERVER-NAME TRAILING)
               '","type":"'
               FUNCTION TRIM(WS-MCP-SERVER-TYPE TRAILING)
               '","command":"'
               FUNCTION TRIM(WS-MCP-SERVER-CMD TRAILING)
               '","url":"'
               FUNCTION TRIM(WS-MCP-SERVER-URL TRAILING)
               '"}'
               DELIMITED BY SIZE
               INTO WS-REQ-PARAMS
           END-STRING

           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-REQ-PARAMS TRAILING))
               TO WS-REQ-PARAMS-LEN

           MOVE "mcp.addServer" TO WS-REQ-METHOD
           PERFORM SEND-RPC-REQUEST

           .

      *----------------------------------------------------------------*
      * REGISTER-COMMAND                                               *
      * Register a slash command with the CLI.                         *
      *----------------------------------------------------------------*
       REGISTER-COMMAND.
           IF NOT CLIENT-IS-ACTIVE
               MOVE -4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO WS-REQ-PARAMS
           STRING
               '{"name":"'
               FUNCTION TRIM(WS-CMD-NAME TRAILING)
               '","description":"'
               FUNCTION TRIM(WS-CMD-DESCRIPTION TRAILING)
               '"}'
               DELIMITED BY SIZE
               INTO WS-REQ-PARAMS
           END-STRING

           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-REQ-PARAMS TRAILING))
               TO WS-REQ-PARAMS-LEN

           MOVE "commands.register" TO WS-REQ-METHOD
           PERFORM SEND-RPC-REQUEST

           .

       STOP RUN.
