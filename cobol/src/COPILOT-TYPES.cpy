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
