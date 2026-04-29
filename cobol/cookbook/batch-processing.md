# Batch Processing in COBOL

Process multiple prompts in sequence using PERFORM loops, perfect for report generation, code analysis pipelines, and data processing workflows.

## Scenario

Your nightly batch job needs to send multiple queries to Copilot and collect the results, for example analyzing a set of programs or generating summaries.

## Sequential Prompt Processing

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH-PROCESS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "COPILOT-TYPES.cpy".
       COPY "COPILOT-VERSION.cpy".

       01  WS-PROMPTS.
           05 WS-PROMPT-ENTRY OCCURS 5 TIMES.
               10 WS-PROMPT-TEXT    PIC X(256).
               10 WS-PROMPT-DONE   PIC 9 VALUE 0.

       01  WS-PROMPT-IDX           PIC 9(2) VALUE 0.
       01  WS-SUCCESS-COUNT        PIC 9(4) VALUE 0.
       01  WS-ERROR-COUNT          PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM LOAD-PROMPTS
           PERFORM INIT-CLIENT
           PERFORM CREATE-BATCH-SESSION

           PERFORM PROCESS-PROMPT
               VARYING WS-PROMPT-IDX FROM 1 BY 1
               UNTIL WS-PROMPT-IDX > 5

           DISPLAY "Batch complete: "
               WS-SUCCESS-COUNT " succeeded, "
               WS-ERROR-COUNT " failed"

           PERFORM CLEANUP
           STOP RUN
           .

       LOAD-PROMPTS.
           MOVE "Analyze the PERFORM statement"
               TO WS-PROMPT-TEXT(1)
           MOVE "Explain VSAM file types"
               TO WS-PROMPT-TEXT(2)
           MOVE "Review CICS SEND MAP usage"
               TO WS-PROMPT-TEXT(3)
           MOVE "Summarize DB2 BIND options"
               TO WS-PROMPT-TEXT(4)
           MOVE "List JCL DD statement parameters"
               TO WS-PROMPT-TEXT(5)
           .

       INIT-CLIENT.
           MOVE 1 TO WS-AUTO-START
           CALL "COPILOT-CLIENT-INIT"
               USING WS-CLIENT-OPTIONS WS-RETURN-CODE
           .

       CREATE-BATCH-SESSION.
           MOVE "batch-analyzer" TO WS-SESS-AGENT-NAME
           MOVE "1.0.0" TO WS-SESS-AGENT-VER
           MOVE "Answer concisely in 2-3 sentences."
               TO WS-SESS-SYS-PROMPT
           CALL "COPILOT-SESSION" USING WS-SESSION-CONFIG
               WS-SESSION-STATE WS-RETURN-CODE
           .

       PROCESS-PROMPT.
           DISPLAY "Processing prompt " WS-PROMPT-IDX "..."
           MOVE WS-PROMPT-TEXT(WS-PROMPT-IDX) TO WS-SEND-MESSAGE
           MOVE 60000 TO WS-SEND-TIMEOUT-MS

           CALL "COPILOT-SESSION" USING WS-SEND-OPTIONS
               WS-SEND-RESULT WS-RETURN-CODE

           EVALUATE TRUE
               WHEN RESULT-IS-OK
                   DISPLAY "  Result: "
                       FUNCTION TRIM(WS-RESULT-MESSAGE)
                   MOVE 1 TO WS-PROMPT-DONE(WS-PROMPT-IDX)
                   ADD 1 TO WS-SUCCESS-COUNT
               WHEN RESULT-HAS-ERROR
                   DISPLAY "  ERROR: "
                       FUNCTION TRIM(WS-RESULT-ERROR-MSG)
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           .

       CLEANUP.
           CALL "COPILOT-SESSION" USING WS-SESSION-STATE
               WS-RETURN-CODE
           CALL "COPILOT-CLIENT-STOP" USING WS-RETURN-CODE
           .
```

## Writing Results to a Report File

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE ASSIGN TO "batch-report.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE.
       01  REPORT-LINE              PIC X(132).

       WRITE-RESULT-TO-REPORT.
           OPEN EXTEND REPORT-FILE
           STRING
               "Prompt " WS-PROMPT-IDX ": "
               FUNCTION TRIM(WS-PROMPT-TEXT(WS-PROMPT-IDX))
               DELIMITED SIZE INTO REPORT-LINE
           END-STRING
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           STRING
               "  Answer: "
               FUNCTION TRIM(WS-RESULT-MESSAGE)
               DELIMITED SIZE INTO REPORT-LINE
           END-STRING
           WRITE REPORT-LINE
           CLOSE REPORT-FILE
           .
```

## Rate-Limiting with Delays

```cobol
       PROCESS-WITH-DELAY.
           PERFORM VARYING WS-PROMPT-IDX FROM 1 BY 1
               UNTIL WS-PROMPT-IDX > 5
               PERFORM PROCESS-PROMPT
               IF WS-PROMPT-IDX < 5
                   CALL "C$SLEEP" USING 2
               END-IF
           END-PERFORM
           .
```
