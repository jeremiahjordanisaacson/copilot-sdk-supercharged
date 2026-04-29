# Advanced Features in COBOL

System prompts, streaming, agent configuration, and protocol options.

## System Prompts

Customize the assistant's behavior for your domain:

```cobol
       SETUP-BANKING-SESSION.
           MOVE "banking-assistant" TO WS-SESS-AGENT-NAME
           MOVE "2.0.0" TO WS-SESS-AGENT-VER

           STRING
               "You are a COBOL banking assistant. "
               "Help with CICS transactions, DB2 queries, "
               "and JCL job streams. Always validate "
               "account numbers before processing."
               DELIMITED SIZE
               INTO WS-SESS-SYS-PROMPT
           END-STRING

           CALL "COPILOT-SESSION" USING WS-SESSION-CONFIG
               WS-SESSION-STATE WS-RETURN-CODE
           .
```

## Streaming Responses

Enable streaming to receive partial responses as they arrive:

```cobol
       SETUP-STREAMING.
           SET STREAMING-ON TO TRUE

           CALL "COPILOT-SESSION" USING WS-SESSION-CONFIG
               WS-SESSION-STATE WS-RETURN-CODE

      *    When streaming, you receive delta events
      *    The session module accumulates them for you
           MOVE "Explain VSAM file organization"
               TO WS-SEND-MESSAGE
           CALL "COPILOT-SESSION" USING WS-SEND-OPTIONS
               WS-SEND-RESULT WS-RETURN-CODE

      *    WS-RESULT-MESSAGE contains the full accumulated text
           DISPLAY FUNCTION TRIM(WS-RESULT-MESSAGE)
           .
```

## Custom Agent Identity

Set a unique agent name and version for tracking:

```cobol
       SETUP-CUSTOM-AGENT.
           MOVE "mainframe-modernizer" TO WS-SESS-AGENT-NAME
           MOVE "3.1.0" TO WS-SESS-AGENT-VER

      *    The SDK automatically includes:
      *    - sdkName:         "copilot-sdk-cobol"
      *    - sdkVersion:      from COPILOT-VERSION.cpy
      *    - protocolVersion: from COPILOT-VERSION.cpy
           .
```

## Adjusting History Length

Control how many previous turns the session remembers:

```cobol
      * Keep a short history for quick lookups
       MOVE 10 TO WS-SESS-HISTORY-LEN

      * Or a longer history for complex multi-turn analysis
       MOVE 500 TO WS-SESS-HISTORY-LEN
```

## Connecting to an External CLI Server

Instead of launching a local CLI process, connect to a remote one:

```cobol
       CONNECT-TO-REMOTE.
           MOVE SPACES TO WS-CLI-PATH
           MOVE "http://copilot-server.internal:3000"
               TO WS-CLI-URL
           MOVE 0 TO WS-AUTO-START

           CALL "COPILOT-CLIENT-INIT"
               USING WS-CLIENT-OPTIONS WS-RETURN-CODE

           CALL "COPILOT-CLIENT-START" USING WS-RETURN-CODE

           IF NOT RC-SUCCESS
               DISPLAY "Failed to connect to remote CLI"
               STOP RUN WITH STATUS 1
           END-IF
           .
```

## Using GitHub Token Authentication

```cobol
       SETUP-AUTH.
           MOVE SPACES TO WS-CLI-PATH
           ACCEPT WS-GITHUB-TOKEN FROM ENVIRONMENT "GITHUB_TOKEN"

           IF WS-GITHUB-TOKEN = SPACES
               DISPLAY "WARNING: GITHUB_TOKEN not set"
               DISPLAY "Falling back to gh CLI auth"
           END-IF

           CALL "COPILOT-CLIENT-INIT"
               USING WS-CLIENT-OPTIONS WS-RETURN-CODE
           .
```
