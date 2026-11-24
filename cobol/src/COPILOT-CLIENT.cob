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

      *    SDK protocol 4 handshake and RPC helpers
       01  WS-PATH-LEN              PIC 9(3)    VALUE 0.
       01  WS-SERVER-PROTOCOL       PIC 9(4)    VALUE 0.
       01  WS-SERVER-PROTO-ED       PIC Z(3)9.
       01  WS-SUPPORTED-PROTO-ED    PIC Z9.
       01  WS-PROTO-FOUND           PIC 9       VALUE 0.
       01  WS-DIGIT                 PIC 9       VALUE 0.
       01  WS-SCAN-COUNT            PIC 9(5)    VALUE 0.
       01  WS-SCAN-POS              PIC 9(5)    VALUE 0.
       01  WS-FIELD-COUNT           PIC 9(2)    VALUE 0.
       01  WS-ERROR-TEXT            PIC X(200)  VALUE SPACES.

      *    Server-scoped request builder (mcp.secrets.*,
      *    permissions.extensionApprovals.*)
       01  WS-JSON-KEY              PIC X(32)   VALUE SPACES.
       01  WS-JSON-TEXT             PIC X(2048) VALUE SPACES.
       01  WS-JSON-TEXT-LEN         PIC 9(5)    VALUE 0.
       01  WS-JSON-IDX              PIC 9(5)    VALUE 0.
       01  WS-JSON-CHAR             PIC X       VALUE SPACE.

       LINKAGE SECTION.
       01  LS-REQUEST               PIC X(8192).
       01  LS-RESPONSE              PIC X(8192).
       01  LS-RETURN-CODE           PIC S9(4).
      *    Caller records of the v3.1.0 server-scoped entries, each as
      *    long as its COPILOT-TYPES.cpy record (WS-MCP-SECRETS-REQ,
      *    WS-EXT-APPROVALS-REQ).
       01  LS-MCP-SECRETS-REQ       PIC X(3073).
       01  LS-EXT-APPROVALS-REQ     PIC X(898).

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

      *----------------------------------------------------------------*
      * WRITE: Write one JSON-RPC message (request, notification or    *
      * reply to a server request) without reading.                    *
      * Input:  LS-REQUEST                                             *
      * Output: LS-RETURN-CODE                                         *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-CLIENT-WRITE" USING LS-REQUEST LS-RETURN-CODE.
           PERFORM WRITE-ONLY-MESSAGE
           GOBACK
           .

      *----------------------------------------------------------------*
      * READ: Read the next JSON-RPC message (response, notification   *
      * or server-to-client request).                                  *
      * Output: LS-RESPONSE, LS-RETURN-CODE                            *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-CLIENT-READ" USING LS-RESPONSE LS-RETURN-CODE.
           PERFORM READ-ONLY-MESSAGE
           GOBACK
           .

      *----------------------------------------------------------------*
      * CLEAR-SETTINGS-CACHE: Clear the cached enterprise managed      *
      * settings (managedSettings.clearCache).                         *
      * Output: LS-RESPONSE, LS-RETURN-CODE                            *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-CLEAR-SETTINGS-CACHE" USING LS-RESPONSE
           LS-RETURN-CODE.
           PERFORM CLEAR-MANAGED-SETTINGS-CACHE
           MOVE WS-READ-BUFFER TO LS-RESPONSE
           MOVE WS-RETURN-CODE TO LS-RETURN-CODE
           GOBACK
           .

      *----------------------------------------------------------------*
      * MCP-SECRETS-SAVE: Save a legacy MCP credential in the runtime  *
      * secret store (mcp.secrets.save, Copilot CLI 1.0.116 or newer). *
      * Input:  WS-MCP-SECRETS-REQ (secret id, value, config dir)      *
      * Output: MSQ-USED-KEYCHAIN, LS-RESPONSE, LS-RETURN-CODE         *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-MCP-SECRETS-SAVE" USING LS-MCP-SECRETS-REQ
           LS-RESPONSE LS-RETURN-CODE.
           MOVE LS-MCP-SECRETS-REQ TO WS-MCP-SECRETS-REQ
           PERFORM MCP-SECRETS-SAVE
           MOVE WS-MCP-SECRETS-REQ TO LS-MCP-SECRETS-REQ
           MOVE WS-READ-BUFFER TO LS-RESPONSE
           MOVE WS-RETURN-CODE TO LS-RETURN-CODE
           GOBACK
           .

      *----------------------------------------------------------------*
      * MCP-SECRETS-DELETE: Delete a legacy MCP credential             *
      * (mcp.secrets.delete, Copilot CLI 1.0.116 or newer).            *
      * Input:  WS-MCP-SECRETS-REQ (secret id, config dir)             *
      * Output: LS-RESPONSE, LS-RETURN-CODE                            *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-MCP-SECRETS-DELETE" USING LS-MCP-SECRETS-REQ
           LS-RESPONSE LS-RETURN-CODE.
           MOVE LS-MCP-SECRETS-REQ TO WS-MCP-SECRETS-REQ
           PERFORM MCP-SECRETS-DELETE
           MOVE WS-READ-BUFFER TO LS-RESPONSE
           MOVE WS-RETURN-CODE TO LS-RETURN-CODE
           GOBACK
           .

      *----------------------------------------------------------------*
      * MCP-SECRETS-DEL-SERVER: Delete the legacy credentials indexed  *
      * for an MCP server (mcp.secrets.deleteServer, Copilot CLI       *
      * 1.0.116 or newer).                                             *
      * Input:  WS-MCP-SECRETS-REQ (server name, config dir)           *
      * Output: LS-RESPONSE, LS-RETURN-CODE                            *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-MCP-SECRETS-DEL-SERVER" USING LS-MCP-SECRETS-REQ
           LS-RESPONSE LS-RETURN-CODE.
           MOVE LS-MCP-SECRETS-REQ TO WS-MCP-SECRETS-REQ
           PERFORM MCP-SECRETS-DELETE-SERVER
           MOVE WS-READ-BUFFER TO LS-RESPONSE
           MOVE WS-RETURN-CODE TO LS-RETURN-CODE
           GOBACK
           .

      *----------------------------------------------------------------*
      * EXT-APPROVALS-GRANT: Persist a pending plugin extension        *
      * permission request as a user-scoped approval before answering  *
      * it (permissions.extensionApprovals.grant, Copilot CLI 1.0.116  *
      * or newer).                                                     *
      * Input:  WS-EXT-APPROVALS-REQ (session id, request id)          *
      * Output: EAQ-GRANTED, LS-RESPONSE (approval), LS-RETURN-CODE    *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-EXT-APPROVALS-GRANT" USING LS-EXT-APPROVALS-REQ
           LS-RESPONSE LS-RETURN-CODE.
           MOVE LS-EXT-APPROVALS-REQ TO WS-EXT-APPROVALS-REQ
           PERFORM EXT-APPROVALS-GRANT
           MOVE WS-EXT-APPROVALS-REQ TO LS-EXT-APPROVALS-REQ
           MOVE WS-READ-BUFFER TO LS-RESPONSE
           MOVE WS-RETURN-CODE TO LS-RETURN-CODE
           GOBACK
           .

      *----------------------------------------------------------------*
      * EXT-APPROVALS-LIST: List the user-scoped plugin extension      *
      * approvals (permissions.extensionApprovals.list, Copilot CLI    *
      * 1.0.116 or newer).                                             *
      * Input:  WS-EXT-APPROVALS-REQ (optional session id)             *
      * Output: LS-RESPONSE (approvals), LS-RETURN-CODE                *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-EXT-APPROVALS-LIST" USING LS-EXT-APPROVALS-REQ
           LS-RESPONSE LS-RETURN-CODE.
           MOVE LS-EXT-APPROVALS-REQ TO WS-EXT-APPROVALS-REQ
           PERFORM EXT-APPROVALS-LIST
           MOVE WS-READ-BUFFER TO LS-RESPONSE
           MOVE WS-RETURN-CODE TO LS-RETURN-CODE
           GOBACK
           .

      *----------------------------------------------------------------*
      * EXT-APPROVALS-REVOKE: Revoke one plugin extension approval     *
      * (permissions.extensionApprovals.revoke, Copilot CLI 1.0.116 or *
      * newer).                                                        *
      * Input:  WS-EXT-APPROVALS-REQ (extension name, installation     *
      *         key, optional session id)                              *
      * Output: EAQ-REVOKED, LS-RESPONSE, LS-RETURN-CODE               *
      *----------------------------------------------------------------*
       ENTRY "COPILOT-EXT-APPROVALS-REVOKE" USING LS-EXT-APPROVALS-REQ
           LS-RESPONSE LS-RETURN-CODE.
           MOVE LS-EXT-APPROVALS-REQ TO WS-EXT-APPROVALS-REQ
           PERFORM EXT-APPROVALS-REVOKE
           MOVE WS-EXT-APPROVALS-REQ TO LS-EXT-APPROVALS-REQ
           MOVE WS-READ-BUFFER TO LS-RESPONSE
           MOVE WS-RETURN-CODE TO LS-RETURN-CODE
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

      *    Client mode: "coding-agent" (default) or "empty";
      *    "copilot-cli" is accepted as an alias of "coding-agent".
           IF NOT CLIENT-MODE-CODING-AGENT AND NOT CLIENT-MODE-EMPTY
               MOVE -1 TO WS-RETURN-CODE
               MOVE "Unknown client mode (use coding-agent or empty)"
                   TO WS-LAST-ERROR
               EXIT PARAGRAPH
           END-IF

           IF WS-CLI-PATH = SPACES AND WS-CLI-URL = SPACES
               ACCEPT WS-CLI-PATH FROM ENVIRONMENT "COPILOT_CLI_PATH"
                   ON EXCEPTION
                       MOVE SPACES TO WS-CLI-PATH
               END-ACCEPT
               IF WS-CLI-PATH = SPACES
                   MOVE "copilot" TO WS-CLI-PATH
               END-IF
           END-IF

           IF WS-AUTO-START = 1
               PERFORM START-CLI-PROCESS
           END-IF

           MOVE 0 TO WS-RETURN-CODE
           .

      *----------------------------------------------------------------*
      * START-CLI-PROCESS: Launch the Copilot CLI in headless stdio    *
      * mode (--headless --no-auto-update --stdio), open the pipes and *
      * run the SDK protocol 4 handshake (CONNECT-HANDSHAKE).          *
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

      *    A .js entry point (for example npm-loader.js) runs through
      *    node; any other path (native binary or the .bin/copilot
      *    shim) runs directly.
           MOVE SPACES TO WS-CLI-COMMAND
           MOVE 1 TO WS-CMD-PTR
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-CLI-PATH TRAILING))
               TO WS-PATH-LEN
           IF WS-PATH-LEN > 3
               IF WS-CLI-PATH(WS-PATH-LEN - 2:3) = ".js"
                   STRING "node " DELIMITED SIZE
                       INTO WS-CLI-COMMAND
                       WITH POINTER WS-CMD-PTR
                   END-STRING
               END-IF
           END-IF
           STRING
               '"' FUNCTION TRIM(WS-CLI-PATH) '"'
               " --headless --no-auto-update --stdio"
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

           IF WS-IO-RETURN-CODE NOT = 0
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Failed to open communication"
                   TO WS-LAST-ERROR
               EXIT PARAGRAPH
           END-IF

           SET CLIENT-IS-ACTIVE TO TRUE
           MOVE 0 TO WS-RETURN-CODE
           PERFORM CONNECT-HANDSHAKE
           IF WS-RETURN-CODE = 0
               PERFORM REGISTER-NETWORK-PROVIDER
           END-IF
           IF WS-RETURN-CODE NOT = 0
               PERFORM STOP-CLI-PROCESS
               MOVE -4 TO WS-RETURN-CODE
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
      * Input:  LS-REQUEST (JSON params). SDK protocol 4 expects the   *
      *   grouped payload; COPILOT-SESSION BUILD-CREATE-PARAMS builds  *
      *   it from the flat session options.                            *
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
      * CONNECT-HANDSHAKE: SDK protocol 4 handshake. Sends connect     *
      * with clientInfo (and the connection token when set), falls     *
      * back to ping when the server does not know connect, and        *
      * accepts exactly protocol version 4 (WS-PROTO-MIN to            *
      * WS-PROTO-MAX).                                                 *
      *----------------------------------------------------------------*
       CONNECT-HANDSHAKE.
           PERFORM BUILD-CONNECT-REQUEST
           PERFORM WRITE-FRAMED-MESSAGE
           IF WS-IO-RETURN-CODE = 0
               PERFORM READ-FRAMED-MESSAGE
           END-IF
           IF WS-IO-RETURN-CODE NOT = 0
               MOVE -4 TO WS-RETURN-CODE
               MOVE "Connection handshake failed" TO WS-LAST-ERROR
               EXIT PARAGRAPH
           END-IF

      *    Servers without connect answer "Method not found".
           MOVE 0 TO WS-SCAN-COUNT
           INSPECT WS-READ-BUFFER TALLYING WS-SCAN-COUNT
               FOR ALL "-32601" ALL "Unhandled method connect"
           IF WS-SCAN-COUNT > 0
               MOVE SPACES TO WS-WRITE-BUFFER
               MOVE '{"jsonrpc":"2.0","method":"ping","id":0}'
                   TO WS-WRITE-BUFFER
               MOVE FUNCTION LENGTH(
                   FUNCTION TRIM(WS-WRITE-BUFFER TRAILING))
                   TO WS-WRITE-LEN
               PERFORM WRITE-FRAMED-MESSAGE
               IF WS-IO-RETURN-CODE = 0
                   PERFORM READ-FRAMED-MESSAGE
               END-IF
               IF WS-IO-RETURN-CODE NOT = 0
                   MOVE -4 TO WS-RETURN-CODE
                   MOVE "Ping handshake failed" TO WS-LAST-ERROR
                   EXIT PARAGRAPH
               END-IF
           END-IF

           PERFORM CHECK-PROTOCOL-VERSION
           .

      *----------------------------------------------------------------*
      * BUILD-CONNECT-REQUEST: connect params with the connection      *
      * token and clientInfo (ClientInfo). Blank clientInfo fields are *
      * dropped; clientInfo is omitted when every field is blank.      *
      *----------------------------------------------------------------*
       BUILD-CONNECT-REQUEST.
           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           MOVE 0 TO WS-FIELD-COUNT
           STRING '{"jsonrpc":"2.0","method":"connect","params":{'
               DELIMITED SIZE INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           IF WS-TCP-CONN-TOKEN NOT = SPACES
               STRING '"token":"' FUNCTION TRIM(WS-TCP-CONN-TOKEN) '"'
                   DELIMITED SIZE INTO WS-WRITE-BUFFER
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
               MOVE 1 TO WS-FIELD-COUNT
           END-IF
           IF WS-CLIENT-INFO NOT = SPACES
               IF WS-FIELD-COUNT > 0
                   STRING ',' DELIMITED SIZE INTO WS-WRITE-BUFFER
                       WITH POINTER WS-JSON-WORK-LEN
                   END-STRING
               END-IF
               STRING '"clientInfo":{' DELIMITED SIZE
                   INTO WS-WRITE-BUFFER
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
               MOVE 0 TO WS-FIELD-COUNT
               IF WS-CI-EDITOR-NAME NOT = SPACES
                   PERFORM CLIENT-INFO-SEPARATOR
                   STRING '"editorName":"'
                       FUNCTION TRIM(WS-CI-EDITOR-NAME) '"'
                       DELIMITED SIZE INTO WS-WRITE-BUFFER
                       WITH POINTER WS-JSON-WORK-LEN
                   END-STRING
               END-IF
               IF WS-CI-EDITOR-VERSION NOT = SPACES
                   PERFORM CLIENT-INFO-SEPARATOR
                   STRING '"editorVersion":"'
                       FUNCTION TRIM(WS-CI-EDITOR-VERSION) '"'
                       DELIMITED SIZE INTO WS-WRITE-BUFFER
                       WITH POINTER WS-JSON-WORK-LEN
                   END-STRING
               END-IF
               IF WS-CI-EXTENSION-NAME NOT = SPACES
                   PERFORM CLIENT-INFO-SEPARATOR
                   STRING '"extensionName":"'
                       FUNCTION TRIM(WS-CI-EXTENSION-NAME) '"'
                       DELIMITED SIZE INTO WS-WRITE-BUFFER
                       WITH POINTER WS-JSON-WORK-LEN
                   END-STRING
               END-IF
               IF WS-CI-EXTENSION-VER NOT = SPACES
                   PERFORM CLIENT-INFO-SEPARATOR
                   STRING '"extensionVersion":"'
                       FUNCTION TRIM(WS-CI-EXTENSION-VER) '"'
                       DELIMITED SIZE INTO WS-WRITE-BUFFER
                       WITH POINTER WS-JSON-WORK-LEN
                   END-STRING
               END-IF
               STRING '}' DELIMITED SIZE INTO WS-WRITE-BUFFER
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
           END-IF
           STRING '},"id":0}' DELIMITED SIZE INTO WS-WRITE-BUFFER
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-WRITE-LEN
           .

       CLIENT-INFO-SEPARATOR.
           IF WS-FIELD-COUNT > 0
               STRING ',' DELIMITED SIZE INTO WS-WRITE-BUFFER
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
           END-IF
           ADD 1 TO WS-FIELD-COUNT
           .

      *----------------------------------------------------------------*
      * CHECK-PROTOCOL-VERSION: Read protocolVersion from the connect  *
      * or ping result in WS-READ-BUFFER and accept exactly the        *
      * supported range; otherwise WS-RETURN-CODE -4 and the mismatch  *
      * message in WS-LAST-ERROR.                                      *
      *----------------------------------------------------------------*
       CHECK-PROTOCOL-VERSION.
           MOVE 0 TO WS-SERVER-PROTOCOL
           MOVE 0 TO WS-PROTO-FOUND
           MOVE 0 TO WS-SCAN-POS
           INSPECT WS-READ-BUFFER TALLYING WS-SCAN-POS
               FOR CHARACTERS BEFORE INITIAL '"protocolVersion":'
           IF WS-SCAN-POS < FUNCTION LENGTH(WS-READ-BUFFER) - 20
      *        Skip the 18-character key to the first value character.
               ADD 19 TO WS-SCAN-POS
               PERFORM UNTIL WS-SCAN-POS >= FUNCTION LENGTH(
                                               WS-READ-BUFFER)
                       OR WS-READ-BUFFER(WS-SCAN-POS:1) NOT = SPACE
                   ADD 1 TO WS-SCAN-POS
               END-PERFORM
               PERFORM UNTIL WS-SCAN-POS >= FUNCTION LENGTH(
                                               WS-READ-BUFFER)
                       OR WS-READ-BUFFER(WS-SCAN-POS:1) IS NOT NUMERIC
                       OR WS-SERVER-PROTOCOL > 999
                   MOVE WS-READ-BUFFER(WS-SCAN-POS:1) TO WS-DIGIT
                   COMPUTE WS-SERVER-PROTOCOL =
                       WS-SERVER-PROTOCOL * 10 + WS-DIGIT
                   MOVE 1 TO WS-PROTO-FOUND
                   ADD 1 TO WS-SCAN-POS
               END-PERFORM
           END-IF

           MOVE WS-PROTO-MAX TO WS-SUPPORTED-PROTO-ED
           MOVE SPACES TO WS-LAST-ERROR
           IF WS-PROTO-FOUND = 0
               MOVE -4 TO WS-RETURN-CODE
               STRING "SDK protocol version mismatch: SDK supports "
                   "version " FUNCTION TRIM(WS-SUPPORTED-PROTO-ED)
                   ", but server does not report a protocol "
                   "version. Please update your SDK or server to "
                   "ensure compatibility."
                   DELIMITED SIZE INTO WS-LAST-ERROR
               END-STRING
               EXIT PARAGRAPH
           END-IF
           IF WS-SERVER-PROTOCOL < WS-PROTO-MIN
              OR WS-SERVER-PROTOCOL > WS-PROTO-MAX
               MOVE -4 TO WS-RETURN-CODE
               MOVE WS-SERVER-PROTOCOL TO WS-SERVER-PROTO-ED
               STRING "SDK protocol version mismatch: SDK supports "
                   "version " FUNCTION TRIM(WS-SUPPORTED-PROTO-ED)
                   ", but server reports version "
                   FUNCTION TRIM(WS-SERVER-PROTO-ED)
                   ". Please update your SDK or server to ensure "
                   "compatibility."
                   DELIMITED SIZE INTO WS-LAST-ERROR
               END-STRING
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO WS-RETURN-CODE
           .

      *----------------------------------------------------------------*
      * REGISTER-NETWORK-PROVIDER: network.setProvider for the         *
      * network callback programs named in WS-NETWORK-OPTIONS (an      *
      * empty object marks each registered callback). Requests are     *
      * answered by COPILOT-SESSION (ANSWER-NETWORK).                  *
      *----------------------------------------------------------------*
       REGISTER-NETWORK-PROVIDER.
           IF WS-NET-PROXY-PGM = SPACES
              AND WS-NET-NEGOTIATE-PGM = SPACES
              AND WS-NET-BASIC-PGM = SPACES
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO WS-REQ-PARAMS
           MOVE 1 TO WS-JSON-WORK-LEN
           MOVE 0 TO WS-FIELD-COUNT
           STRING '{' DELIMITED SIZE INTO WS-REQ-PARAMS
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           IF WS-NET-SCOPE NOT = SPACES
               STRING '"scope":"' FUNCTION TRIM(WS-NET-SCOPE) '"'
                   DELIMITED SIZE INTO WS-REQ-PARAMS
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
               MOVE 1 TO WS-FIELD-COUNT
           END-IF
           IF WS-NET-PROXY-PGM NOT = SPACES
               PERFORM NETWORK-SEPARATOR
               STRING '"resolveProxy":{}' DELIMITED SIZE
                   INTO WS-REQ-PARAMS
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
           END-IF
           IF WS-NET-NEGOTIATE-PGM NOT = SPACES
               PERFORM NETWORK-SEPARATOR
               STRING '"authenticateNegotiate":{}' DELIMITED SIZE
                   INTO WS-REQ-PARAMS
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
           END-IF
           IF WS-NET-BASIC-PGM NOT = SPACES
               PERFORM NETWORK-SEPARATOR
               STRING '"authenticateBasic":{}' DELIMITED SIZE
                   INTO WS-REQ-PARAMS
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
           END-IF
           STRING '}' DELIMITED SIZE INTO WS-REQ-PARAMS
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-REQ-PARAMS-LEN
           MOVE "network.setProvider" TO WS-REQ-METHOD
           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RESPONSE-ERROR
           END-IF
           .

       NETWORK-SEPARATOR.
           IF WS-FIELD-COUNT > 0
               STRING ',' DELIMITED SIZE INTO WS-REQ-PARAMS
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
           END-IF
           ADD 1 TO WS-FIELD-COUNT
           .

      *----------------------------------------------------------------*
      * CLEAR-MANAGED-SETTINGS-CACHE: managedSettings.clearCache {}.   *
      *----------------------------------------------------------------*
       CLEAR-MANAGED-SETTINGS-CACHE.
           MOVE "managedSettings.clearCache" TO WS-REQ-METHOD
           MOVE "{}" TO WS-REQ-PARAMS
           MOVE 2 TO WS-REQ-PARAMS-LEN
           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RESPONSE-ERROR
           END-IF
           .

      *----------------------------------------------------------------*
      * MCP-SECRETS-SAVE: mcp.secrets.save {secretId, value,           *
      * configDir?}. The value is a credential and is never logged.    *
      * Sets MSQ-USED-KEYCHAIN when the OS keychain stored it          *
      * (usedKeychain true).                                           *
      *----------------------------------------------------------------*
       MCP-SECRETS-SAVE.
           SET MSQ-USED-FILE-BACKEND TO TRUE
           PERFORM BEGIN-JSON-PARAMS
           MOVE "secretId" TO WS-JSON-KEY
           MOVE WS-MSQ-SECRET-ID TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           MOVE "value" TO WS-JSON-KEY
           MOVE WS-MSQ-VALUE TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           MOVE "configDir" TO WS-JSON-KEY
           MOVE WS-MSQ-CONFIG-DIR TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           PERFORM END-JSON-PARAMS
           MOVE "mcp.secrets.save" TO WS-REQ-METHOD
           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RESPONSE-ERROR
           END-IF
           IF WS-RETURN-CODE = 0
               MOVE 0 TO WS-SCAN-COUNT
               INSPECT WS-READ-BUFFER TALLYING WS-SCAN-COUNT
                   FOR ALL '"usedKeychain":true'
               IF WS-SCAN-COUNT > 0
                   SET MSQ-USED-KEYCHAIN TO TRUE
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
      * MCP-SECRETS-DELETE: mcp.secrets.delete {secretId, configDir?}. *
      *----------------------------------------------------------------*
       MCP-SECRETS-DELETE.
           PERFORM BEGIN-JSON-PARAMS
           MOVE "secretId" TO WS-JSON-KEY
           MOVE WS-MSQ-SECRET-ID TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           MOVE "configDir" TO WS-JSON-KEY
           MOVE WS-MSQ-CONFIG-DIR TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           PERFORM END-JSON-PARAMS
           MOVE "mcp.secrets.delete" TO WS-REQ-METHOD
           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RESPONSE-ERROR
           END-IF
           .

      *----------------------------------------------------------------*
      * MCP-SECRETS-DELETE-SERVER: mcp.secrets.deleteServer            *
      * {serverName, configDir?}: every legacy credential indexed for  *
      * the MCP server.                                                *
      *----------------------------------------------------------------*
       MCP-SECRETS-DELETE-SERVER.
           PERFORM BEGIN-JSON-PARAMS
           MOVE "serverName" TO WS-JSON-KEY
           MOVE WS-MSQ-SERVER-NAME TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           MOVE "configDir" TO WS-JSON-KEY
           MOVE WS-MSQ-CONFIG-DIR TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           PERFORM END-JSON-PARAMS
           MOVE "mcp.secrets.deleteServer" TO WS-REQ-METHOD
           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RESPONSE-ERROR
           END-IF
           .

      *----------------------------------------------------------------*
      * EXT-APPROVALS-GRANT: permissions.extensionApprovals.grant      *
      * {sessionId, requestId}. Sets EAQ-GRANTED when the pending      *
      * request was eligible and persisted (granted true).             *
      *----------------------------------------------------------------*
       EXT-APPROVALS-GRANT.
           MOVE 0 TO WS-EAQ-GRANTED
           PERFORM BEGIN-JSON-PARAMS
           MOVE "sessionId" TO WS-JSON-KEY
           MOVE WS-EAQ-SESSION-ID TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           MOVE "requestId" TO WS-JSON-KEY
           MOVE WS-EAQ-REQUEST-ID TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           PERFORM END-JSON-PARAMS
           MOVE "permissions.extensionApprovals.grant" TO WS-REQ-METHOD
           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RESPONSE-ERROR
           END-IF
           IF WS-RETURN-CODE = 0
               MOVE 0 TO WS-SCAN-COUNT
               INSPECT WS-READ-BUFFER TALLYING WS-SCAN-COUNT
                   FOR ALL '"granted":true'
               IF WS-SCAN-COUNT > 0
                   SET EAQ-GRANTED TO TRUE
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
      * EXT-APPROVALS-LIST: permissions.extensionApprovals.list        *
      * {sessionId?}. The approvals are left in WS-READ-BUFFER.        *
      *----------------------------------------------------------------*
       EXT-APPROVALS-LIST.
           PERFORM BEGIN-JSON-PARAMS
           MOVE "sessionId" TO WS-JSON-KEY
           MOVE WS-EAQ-SESSION-ID TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           PERFORM END-JSON-PARAMS
           MOVE "permissions.extensionApprovals.list" TO WS-REQ-METHOD
           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RESPONSE-ERROR
           END-IF
           .

      *----------------------------------------------------------------*
      * EXT-APPROVALS-REVOKE: permissions.extensionApprovals.revoke    *
      * {extensionName, installationKey, sessionId?}. Sets EAQ-REVOKED *
      * when a matching approval was removed (revoked true).           *
      *----------------------------------------------------------------*
       EXT-APPROVALS-REVOKE.
           MOVE 0 TO WS-EAQ-REVOKED
           PERFORM BEGIN-JSON-PARAMS
           MOVE "extensionName" TO WS-JSON-KEY
           MOVE WS-EAQ-EXTENSION-NAME TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           MOVE "installationKey" TO WS-JSON-KEY
           MOVE WS-EAQ-INSTALL-KEY TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           MOVE "sessionId" TO WS-JSON-KEY
           MOVE WS-EAQ-SESSION-ID TO WS-JSON-TEXT
           PERFORM PUT-JSON-STRING-FIELD
           PERFORM END-JSON-PARAMS
           MOVE "permissions.extensionApprovals.revoke" TO WS-REQ-METHOD
           PERFORM SEND-RPC-REQUEST
           IF WS-RETURN-CODE = 0
               PERFORM CHECK-RESPONSE-ERROR
           END-IF
           IF WS-RETURN-CODE = 0
               MOVE 0 TO WS-SCAN-COUNT
               INSPECT WS-READ-BUFFER TALLYING WS-SCAN-COUNT
                   FOR ALL '"revoked":true'
               IF WS-SCAN-COUNT > 0
                   SET EAQ-REVOKED TO TRUE
               END-IF
           END-IF
           .

      *----------------------------------------------------------------*
      * BEGIN-JSON-PARAMS / END-JSON-PARAMS / PUT-JSON-STRING-FIELD:   *
      * Build the WS-REQ-PARAMS object of a server-scoped request.     *
      * PUT-JSON-STRING-FIELD appends "WS-JSON-KEY":"WS-JSON-TEXT"     *
      * (trailing blanks dropped; quotes, backslashes and control      *
      * characters escaped) and skips a blank WS-JSON-TEXT.            *
      *----------------------------------------------------------------*
       BEGIN-JSON-PARAMS.
           MOVE SPACES TO WS-REQ-PARAMS
           MOVE 1 TO WS-JSON-WORK-LEN
           MOVE 0 TO WS-FIELD-COUNT
           STRING '{' DELIMITED SIZE INTO WS-REQ-PARAMS
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           .

       END-JSON-PARAMS.
           STRING '}' DELIMITED SIZE INTO WS-REQ-PARAMS
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           SUBTRACT 1 FROM WS-JSON-WORK-LEN
               GIVING WS-REQ-PARAMS-LEN
           .

       PUT-JSON-STRING-FIELD.
           IF WS-JSON-TEXT = SPACES
               EXIT PARAGRAPH
           END-IF
           IF WS-FIELD-COUNT > 0
               STRING ',' DELIMITED SIZE INTO WS-REQ-PARAMS
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
           END-IF
           ADD 1 TO WS-FIELD-COUNT
           STRING '"' FUNCTION TRIM(WS-JSON-KEY) '":"'
               DELIMITED SIZE INTO WS-REQ-PARAMS
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-JSON-TEXT TRAILING))
               TO WS-JSON-TEXT-LEN
           PERFORM VARYING WS-JSON-IDX FROM 1 BY 1
                   UNTIL WS-JSON-IDX > WS-JSON-TEXT-LEN
               MOVE WS-JSON-TEXT(WS-JSON-IDX:1) TO WS-JSON-CHAR
               EVALUATE TRUE
                   WHEN WS-JSON-CHAR = '"' OR WS-JSON-CHAR = "\"
                       STRING "\" WS-JSON-CHAR DELIMITED SIZE
                           INTO WS-REQ-PARAMS
                           WITH POINTER WS-JSON-WORK-LEN
                       END-STRING
                   WHEN WS-JSON-CHAR = X"0A"
                       STRING "\n" DELIMITED SIZE INTO WS-REQ-PARAMS
                           WITH POINTER WS-JSON-WORK-LEN
                       END-STRING
                   WHEN WS-JSON-CHAR = X"0D"
                       STRING "\r" DELIMITED SIZE INTO WS-REQ-PARAMS
                           WITH POINTER WS-JSON-WORK-LEN
                       END-STRING
                   WHEN WS-JSON-CHAR = X"09"
                       STRING "\t" DELIMITED SIZE INTO WS-REQ-PARAMS
                           WITH POINTER WS-JSON-WORK-LEN
                       END-STRING
                   WHEN WS-JSON-CHAR < SPACE
                       CONTINUE
                   WHEN OTHER
                       STRING WS-JSON-CHAR DELIMITED SIZE
                           INTO WS-REQ-PARAMS
                           WITH POINTER WS-JSON-WORK-LEN
                       END-STRING
               END-EVALUATE
           END-PERFORM
           STRING '"' DELIMITED SIZE INTO WS-REQ-PARAMS
               WITH POINTER WS-JSON-WORK-LEN
           END-STRING
           .

      *----------------------------------------------------------------*
      * CHECK-RESPONSE-ERROR: Protocol 4 reports failures (including   *
      * session.delete and session.setForeground) as JSON-RPC errors.  *
      * An error in WS-READ-BUFFER sets WS-RETURN-CODE -1 and          *
      * WS-LAST-ERROR to "<WS-REQ-METHOD> failed: <message>".          *
      *----------------------------------------------------------------*
       CHECK-RESPONSE-ERROR.
           MOVE 0 TO WS-SCAN-COUNT
           INSPECT WS-READ-BUFFER TALLYING WS-SCAN-COUNT
               FOR ALL '"error":{'
           IF WS-SCAN-COUNT = 0
               EXIT PARAGRAPH
           END-IF
           MOVE -1 TO WS-RETURN-CODE
           MOVE SPACES TO WS-ERROR-TEXT
           MOVE 0 TO WS-SCAN-POS
           INSPECT WS-READ-BUFFER TALLYING WS-SCAN-POS
               FOR CHARACTERS BEFORE INITIAL '"message":"'
      *    Skip the 11-character key to the first message character.
           ADD 12 TO WS-SCAN-POS
           IF WS-SCAN-POS < FUNCTION LENGTH(WS-READ-BUFFER)
               UNSTRING WS-READ-BUFFER DELIMITED '"'
                   INTO WS-ERROR-TEXT
                   WITH POINTER WS-SCAN-POS
               END-UNSTRING
           END-IF
           MOVE SPACES TO WS-LAST-ERROR
           STRING FUNCTION TRIM(WS-REQ-METHOD) " failed: "
               FUNCTION TRIM(WS-ERROR-TEXT)
               DELIMITED SIZE INTO WS-LAST-ERROR
           END-STRING
           .

      *----------------------------------------------------------------*
      * WRITE-ONLY-MESSAGE / READ-ONLY-MESSAGE: Single-direction I/O   *
      * for the COPILOT-CLIENT-WRITE and COPILOT-CLIENT-READ entries.  *
      *----------------------------------------------------------------*
       WRITE-ONLY-MESSAGE.
           MOVE LS-REQUEST TO WS-WRITE-BUFFER
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(LS-REQUEST TRAILING))
               TO WS-WRITE-LEN
           PERFORM WRITE-FRAMED-MESSAGE
           IF WS-IO-RETURN-CODE = 0
               MOVE 0 TO LS-RETURN-CODE
           ELSE
               MOVE -1 TO LS-RETURN-CODE
           END-IF
           .

       READ-ONLY-MESSAGE.
           PERFORM READ-FRAMED-MESSAGE
           IF WS-IO-RETURN-CODE = 0
               MOVE WS-READ-BUFFER TO LS-RESPONSE
               MOVE 0 TO LS-RETURN-CODE
           ELSE
               MOVE -1 TO LS-RETURN-CODE
           END-IF
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
               MOVE "session.setForeground" TO WS-REQ-METHOD
               PERFORM CHECK-RESPONSE-ERROR
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

      *    Complete protocol 4 resume params (with sessionId, for
      *    example from BUILD-RESUME-PARAMS in COPILOT-SESSION) are
      *    sent as they are; otherwise only the session id is sent.
           MOVE 0 TO WS-SCAN-COUNT
           INSPECT LS-REQUEST TALLYING WS-SCAN-COUNT
               FOR ALL '"sessionId"'
           MOVE SPACES TO WS-WRITE-BUFFER
           MOVE 1 TO WS-JSON-WORK-LEN
           IF WS-SCAN-COUNT > 0
               STRING
                   '{"jsonrpc":"2.0",'
                   '"method":"session.resume",'
                   '"params":'
                   FUNCTION TRIM(LS-REQUEST TRAILING)
                   ',"id":9}'
                   DELIMITED SIZE
                   INTO WS-WRITE-BUFFER
                   WITH POINTER WS-JSON-WORK-LEN
               END-STRING
           ELSE
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
           END-IF
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
               MOVE "session.delete" TO WS-REQ-METHOD
               PERFORM CHECK-RESPONSE-ERROR
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
