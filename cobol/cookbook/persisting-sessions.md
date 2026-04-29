# Persisting Sessions in COBOL

Save and resume Copilot sessions across program restarts.

## Scenario

Your batch job runs nightly. You want to continue a previous conversation without losing context.

## Saving Session ID to a File

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERSIST-SESSION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SESSION-FILE ASSIGN TO "copilot-session.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  SESSION-FILE.
       01  SESSION-RECORD           PIC X(64).

       WORKING-STORAGE SECTION.
       COPY "COPILOT-TYPES.cpy".
       COPY "COPILOT-VERSION.cpy".
       01  WS-FS                    PIC XX VALUE "00".
       01  WS-SAVED-SESSION-ID      PIC X(64) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PARA.
           CALL "COPILOT-CLIENT-INIT"
               USING WS-CLIENT-OPTIONS WS-RETURN-CODE

      *    Try to load a saved session
           PERFORM LOAD-SESSION-ID
           IF WS-SAVED-SESSION-ID NOT = SPACES
               DISPLAY "Resuming session: "
                   FUNCTION TRIM(WS-SAVED-SESSION-ID)
               MOVE WS-SAVED-SESSION-ID TO WS-SESSION-ID
               MOVE 1 TO WS-SESSION-ACTIVE
           ELSE
               DISPLAY "Creating new session..."
               MOVE "batch-agent" TO WS-SESS-AGENT-NAME
               MOVE "1.0.0" TO WS-SESS-AGENT-VER
               CALL "COPILOT-SESSION" USING WS-SESSION-CONFIG
                   WS-SESSION-STATE WS-RETURN-CODE
               PERFORM SAVE-SESSION-ID
           END-IF

      *    Use the session
           MOVE "Continue our analysis" TO WS-SEND-MESSAGE
           CALL "COPILOT-SESSION" USING WS-SEND-OPTIONS
               WS-SEND-RESULT WS-RETURN-CODE
           DISPLAY "Response: "
               FUNCTION TRIM(WS-RESULT-MESSAGE)

           CALL "COPILOT-CLIENT-STOP" USING WS-RETURN-CODE
           STOP RUN
           .

       SAVE-SESSION-ID.
           OPEN OUTPUT SESSION-FILE
           IF WS-FS = "00"
               WRITE SESSION-RECORD FROM WS-SESSION-ID
               CLOSE SESSION-FILE
               DISPLAY "Session saved."
           END-IF
           .

       LOAD-SESSION-ID.
           MOVE SPACES TO WS-SAVED-SESSION-ID
           OPEN INPUT SESSION-FILE
           IF WS-FS = "00"
               READ SESSION-FILE INTO WS-SAVED-SESSION-ID
                   AT END MOVE SPACES TO WS-SAVED-SESSION-ID
               END-READ
               CLOSE SESSION-FILE
           END-IF
           .
```

## Using VSAM for Enterprise Persistence

For mainframe environments, store session data in a VSAM KSDS:

```cobol
       SELECT SESSION-VSAM
           ASSIGN TO SESSVSAM
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS VSAM-SESSION-KEY
           FILE STATUS IS WS-FS.

       FD  SESSION-VSAM.
       01  VSAM-SESSION-RECORD.
           05 VSAM-SESSION-KEY      PIC X(32).
           05 VSAM-SESSION-ID       PIC X(64).
           05 VSAM-CREATED-DATE     PIC X(10).
           05 VSAM-TURN-COUNT       PIC 9(10).

       SAVE-TO-VSAM.
           MOVE "NIGHTLY-BATCH" TO VSAM-SESSION-KEY
           MOVE WS-SESSION-ID TO VSAM-SESSION-ID
           MOVE FUNCTION CURRENT-DATE(1:10)
               TO VSAM-CREATED-DATE
           MOVE WS-TURN-COUNT TO VSAM-TURN-COUNT
           WRITE VSAM-SESSION-RECORD
           .
```
