# Tools and Skills in COBOL

Patterns for defining custom tools and registering them with Copilot sessions.

## Scenario

Your application needs to give the AI assistant the ability to call custom COBOL functions (tools) so it can interact with your data, files, or business logic.

## Defining a Simple Tool

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOOL-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "COPILOT-TYPES.cpy".
       COPY "COPILOT-VERSION.cpy".

       01  WS-TOOL-JSON             PIC X(2048) VALUE SPACES.
       01  WS-TOOL-PTR              PIC 9(5) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PARA.
           CALL "COPILOT-CLIENT-INIT"
               USING WS-CLIENT-OPTIONS WS-RETURN-CODE

      *    Define a tool that returns current date
           MOVE "get_date" TO WS-TOOL-NAME
           MOVE "Returns the current system date"
               TO WS-TOOL-DESCRIPTION
           MOVE '{"type":"object","properties":{}}'
               TO WS-TOOL-SCHEMA

      *    Build tool registration JSON
           PERFORM BUILD-TOOL-JSON

      *    Create session with tools
           MOVE "tool-agent" TO WS-SESS-AGENT-NAME
           MOVE "1.0.0" TO WS-SESS-AGENT-VER
           CALL "COPILOT-SESSION" USING WS-SESSION-CONFIG
               WS-SESSION-STATE WS-RETURN-CODE

      *    Ask something that triggers the tool
           MOVE "What is today's date?" TO WS-SEND-MESSAGE
           CALL "COPILOT-SESSION" USING WS-SEND-OPTIONS
               WS-SEND-RESULT WS-RETURN-CODE
           DISPLAY "Response: "
               FUNCTION TRIM(WS-RESULT-MESSAGE)

           CALL "COPILOT-CLIENT-STOP" USING WS-RETURN-CODE
           STOP RUN
           .

       BUILD-TOOL-JSON.
           MOVE SPACES TO WS-TOOL-JSON
           MOVE 1 TO WS-TOOL-PTR
           STRING
               '{"name":"'
               FUNCTION TRIM(WS-TOOL-NAME)
               '","description":"'
               FUNCTION TRIM(WS-TOOL-DESCRIPTION)
               '","inputSchema":'
               FUNCTION TRIM(WS-TOOL-SCHEMA)
               '}'
               DELIMITED SIZE
               INTO WS-TOOL-JSON
               WITH POINTER WS-TOOL-PTR
           END-STRING
           .
```

## Handling Tool Invocations

When the assistant calls your tool, you receive a tool invocation event:

```cobol
       HANDLE-TOOL-CALL.
           EVALUATE WS-INVOKE-NAME
               WHEN "get_date"
                   PERFORM TOOL-GET-DATE
               WHEN "lookup_account"
                   PERFORM TOOL-LOOKUP-ACCOUNT
               WHEN OTHER
                   MOVE 0 TO WS-TRES-SUCCESS
                   MOVE "Unknown tool" TO WS-TRES-OUTPUT
           END-EVALUATE
           .

       TOOL-GET-DATE.
           MOVE 1 TO WS-TRES-SUCCESS
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-TRES-OUTPUT
           .

       TOOL-LOOKUP-ACCOUNT.
           MOVE 1 TO WS-TRES-SUCCESS
      *    Parse WS-INVOKE-ARGS for "account_id" and look up
           MOVE "Account: ACME Corp, Balance: $42,000"
               TO WS-TRES-OUTPUT
           .
```

## Tool with Parameters

```cobol
      * Tool with a required "query" parameter
       DEFINE-SEARCH-TOOL.
           MOVE "search_records" TO WS-TOOL-NAME
           MOVE "Search business records by keyword"
               TO WS-TOOL-DESCRIPTION
           STRING
               '{"type":"object",'
               '"properties":{"query":{"type":"string",'
               '"description":"Search keyword"}},'
               '"required":["query"]}'
               DELIMITED SIZE
               INTO WS-TOOL-SCHEMA
           END-STRING
           .
```
