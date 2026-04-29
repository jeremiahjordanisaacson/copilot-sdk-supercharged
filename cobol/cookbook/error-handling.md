# Error Handling in COBOL

Patterns for handling errors gracefully in the Copilot SDK, including connection failures, timeouts, and resource cleanup.

## Scenario

Your COBOL application needs to handle CLI process failures, network errors, and timeouts without abnormal termination.

## Checking Connection Health

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECK-HEALTH.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "COPILOT-TYPES.cpy".

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE "copilot" TO WS-CLI-PATH
           MOVE 1 TO WS-AUTO-START

           CALL "COPILOT-CLIENT-INIT"
               USING WS-CLIENT-OPTIONS WS-RETURN-CODE

           CALL "COPILOT-CLIENT-PING" USING WS-RETURN-CODE

           EVALUATE TRUE
               WHEN RC-SUCCESS
                   DISPLAY "Connection is healthy"
               WHEN RC-CONNECTION-ERROR
                   DISPLAY "ERROR: CLI server not responding"
                   CALL "COPILOT-CLIENT-STOP"
                       USING WS-RETURN-CODE
                   STOP RUN WITH STATUS 1
               WHEN OTHER
                   DISPLAY "ERROR: Unexpected rc=" WS-RETURN-CODE
           END-EVALUATE

           CALL "COPILOT-CLIENT-STOP" USING WS-RETURN-CODE
           STOP RUN
           .
```

## Handling Session Errors

```cobol
           MOVE "Do something risky" TO WS-SEND-MESSAGE
           MOVE 30000 TO WS-SEND-TIMEOUT-MS

           CALL "COPILOT-SESSION" USING WS-SEND-OPTIONS
               WS-SEND-RESULT WS-RETURN-CODE

           EVALUATE TRUE
               WHEN RESULT-HAS-ERROR
                   DISPLAY "Session error: "
                       FUNCTION TRIM(WS-RESULT-ERROR-MSG)
               WHEN RESULT-IS-OK
                   DISPLAY "Success: "
                       FUNCTION TRIM(WS-RESULT-MESSAGE)
               WHEN OTHER
                   DISPLAY "Unknown result state"
           END-EVALUATE
```

## Cleanup on Failure

Always pair resource creation with destruction using structured flow:

```cobol
       MAIN-PARA.
           CALL "COPILOT-CLIENT-INIT"
               USING WS-CLIENT-OPTIONS WS-RETURN-CODE

           IF WS-RETURN-CODE NOT = 0
               DISPLAY "Client init failed"
               STOP RUN WITH STATUS 1
           END-IF

           PERFORM CREATE-AND-USE-SESSION
           PERFORM SHUTDOWN
           STOP RUN
           .

       CREATE-AND-USE-SESSION.
      *    Session creation and use goes here.
      *    Even if it fails, SHUTDOWN will always run.
           CONTINUE
           .

       SHUTDOWN.
           CALL "COPILOT-CLIENT-STOP" USING WS-RETURN-CODE
           .
```

## Retry Pattern

```cobol
       01  WS-ATTEMPT              PIC 9(2) VALUE 0.
       01  WS-MAX-ATTEMPTS         PIC 9(2) VALUE 3.

       RETRY-PING.
           PERFORM VARYING WS-ATTEMPT FROM 1 BY 1
               UNTIL WS-ATTEMPT > WS-MAX-ATTEMPTS

               CALL "COPILOT-CLIENT-PING" USING WS-RETURN-CODE

               IF RC-SUCCESS
                   EXIT PERFORM
               END-IF

               DISPLAY "Retry " WS-ATTEMPT " of " WS-MAX-ATTEMPTS
               CALL "C$SLEEP" USING 1
           END-PERFORM

           IF WS-RETURN-CODE NOT = 0
               DISPLAY "All retry attempts failed"
               STOP RUN WITH STATUS 1
           END-IF
           .
```
