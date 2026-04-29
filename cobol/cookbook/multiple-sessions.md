# Multiple Sessions in COBOL

Manage multiple independent Copilot sessions simultaneously.

## Scenario

Your application needs to maintain separate conversation contexts, for example one session for code review and another for documentation generation.

## Using Separate Session State Records

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MULTI-SESSION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "COPILOT-TYPES.cpy".
       COPY "COPILOT-VERSION.cpy".

      * Session state for each independent session
       01  WS-SESSION-A.
           05 SESS-A-ID             PIC X(64)  VALUE SPACES.
           05 SESS-A-ACTIVE         PIC 9      VALUE 0.
           05 SESS-A-TURN-COUNT     PIC 9(10)  VALUE 0.

       01  WS-SESSION-B.
           05 SESS-B-ID             PIC X(64)  VALUE SPACES.
           05 SESS-B-ACTIVE         PIC 9      VALUE 0.
           05 SESS-B-TURN-COUNT     PIC 9(10)  VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           CALL "COPILOT-CLIENT-INIT"
               USING WS-CLIENT-OPTIONS WS-RETURN-CODE

      *    Create first session for code review
           MOVE "code-reviewer" TO WS-SESS-AGENT-NAME
           MOVE "Review COBOL code for best practices."
               TO WS-SESS-SYS-PROMPT
           CALL "COPILOT-SESSION" USING WS-SESSION-CONFIG
               WS-SESSION-STATE WS-RETURN-CODE
           MOVE WS-SESSION-ID TO SESS-A-ID
           MOVE 1 TO SESS-A-ACTIVE

      *    Create second session for documentation
           MOVE "doc-writer" TO WS-SESS-AGENT-NAME
           MOVE "Generate documentation for COBOL programs."
               TO WS-SESS-SYS-PROMPT
           CALL "COPILOT-SESSION" USING WS-SESSION-CONFIG
               WS-SESSION-STATE WS-RETURN-CODE
           MOVE WS-SESSION-ID TO SESS-B-ID
           MOVE 1 TO SESS-B-ACTIVE

      *    Use session A
           MOVE SESS-A-ID TO WS-SESSION-ID
           MOVE "Review this PERFORM loop" TO WS-SEND-MESSAGE
           CALL "COPILOT-SESSION" USING WS-SEND-OPTIONS
               WS-SEND-RESULT WS-RETURN-CODE
           DISPLAY "Reviewer: " FUNCTION TRIM(WS-RESULT-MESSAGE)

      *    Use session B
           MOVE SESS-B-ID TO WS-SESSION-ID
           MOVE "Document the COPY statement" TO WS-SEND-MESSAGE
           CALL "COPILOT-SESSION" USING WS-SEND-OPTIONS
               WS-SEND-RESULT WS-RETURN-CODE
           DISPLAY "Doc Writer: " FUNCTION TRIM(WS-RESULT-MESSAGE)

      *    Destroy both sessions
           MOVE SESS-A-ID TO WS-SESSION-ID
           CALL "COPILOT-SESSION" USING WS-SESSION-STATE
               WS-RETURN-CODE
           MOVE SESS-B-ID TO WS-SESSION-ID
           CALL "COPILOT-SESSION" USING WS-SESSION-STATE
               WS-RETURN-CODE

           CALL "COPILOT-CLIENT-STOP" USING WS-RETURN-CODE
           STOP RUN
           .
```

## Session Table Pattern

For many sessions, use an OCCURS table:

```cobol
       01  WS-SESSION-TABLE.
           05 WS-SESS-ENTRY OCCURS 10 TIMES.
               10 SESS-TBL-ID      PIC X(64)  VALUE SPACES.
               10 SESS-TBL-ACTIVE  PIC 9      VALUE 0.
               10 SESS-TBL-PURPOSE PIC X(32)  VALUE SPACES.

       01  WS-SESS-IDX             PIC 9(2)   VALUE 0.

       USE-SESSION-TABLE.
           PERFORM VARYING WS-SESS-IDX FROM 1 BY 1
               UNTIL WS-SESS-IDX > 10
               IF SESS-TBL-ACTIVE(WS-SESS-IDX) = 1
                   MOVE SESS-TBL-ID(WS-SESS-IDX)
                       TO WS-SESSION-ID
                   DISPLAY "Active session: "
                       FUNCTION TRIM(
                           SESS-TBL-PURPOSE(WS-SESS-IDX))
               END-IF
           END-PERFORM
           .
```
