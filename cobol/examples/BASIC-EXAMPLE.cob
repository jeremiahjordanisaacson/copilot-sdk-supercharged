      *================================================================*
      * BASIC-EXAMPLE.cob                                              *
      * Demonstrates basic usage of the Copilot SDK for COBOL.         *
      * Creates a client, opens a session, sends a message, and        *
      * prints the response.                                           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BASIC-EXAMPLE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY "COPILOT-TYPES.cpy".
       COPY "COPILOT-VERSION.cpy".

       01  WS-USER-INPUT            PIC X(256)  VALUE SPACES.
       01  WS-CONTINUE              PIC 9       VALUE 1.
           88 KEEP-GOING                        VALUE 1.
           88 USER-QUIT                         VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "=== GitHub Copilot SDK for COBOL ==="
           DISPLAY "SDK Version: " WS-SDK-VERSION
           DISPLAY " "

           PERFORM INITIALIZE-COPILOT

           IF WS-RETURN-CODE NOT = 0
               DISPLAY "ERROR: Failed to initialize client"
               DISPLAY "       " WS-LAST-ERROR
               STOP RUN
           END-IF

           PERFORM CREATE-COPILOT-SESSION

           IF WS-RETURN-CODE NOT = 0
               DISPLAY "ERROR: Failed to create session"
               DISPLAY "       " WS-LAST-ERROR
               PERFORM SHUTDOWN-CLIENT
               STOP RUN
           END-IF

           DISPLAY "Session created: " TRIM(WS-SESSION-ID)
           DISPLAY " "

           PERFORM CONVERSATION-LOOP
               UNTIL USER-QUIT

           PERFORM CLEANUP
           DISPLAY " "
           DISPLAY "=== Goodbye! ==="
           STOP RUN
           .

      *----------------------------------------------------------------*
      * INITIALIZE-COPILOT: Set up the client with default options.    *
      *----------------------------------------------------------------*
       INITIALIZE-COPILOT.
           MOVE SPACES TO WS-CLI-PATH
           MOVE SPACES TO WS-CLI-URL
           MOVE 1      TO WS-AUTO-START

           CALL "COPILOT-CLIENT-INIT"
               USING WS-CLIENT-OPTIONS WS-RETURN-CODE
           .

      *----------------------------------------------------------------*
      * CREATE-COPILOT-SESSION: Open a session with agent info.        *
      *----------------------------------------------------------------*
       CREATE-COPILOT-SESSION.
           MOVE "cobol-example"  TO WS-SESS-AGENT-NAME
           MOVE "1.0.0"          TO WS-SESS-AGENT-VER
           MOVE "You are a helpful assistant for COBOL developers."
               TO WS-SESS-SYS-PROMPT
           MOVE 0 TO WS-SESS-STREAMING

           CALL "COPILOT-SESSION" USING WS-SESSION-CONFIG
               WS-SESSION-STATE WS-RETURN-CODE
           .

      *----------------------------------------------------------------*
      * CONVERSATION-LOOP: Interactive prompt loop.                    *
      *----------------------------------------------------------------*
       CONVERSATION-LOOP.
           DISPLAY " "
           DISPLAY "You> " WITH NO ADVANCING
           ACCEPT WS-USER-INPUT FROM STANDARD-INPUT

           IF WS-USER-INPUT = "quit" OR "exit" OR "q"
               SET USER-QUIT TO TRUE
               EXIT PARAGRAPH
           END-IF

           MOVE WS-USER-INPUT TO WS-SEND-MESSAGE
           MOVE FUNCTION LENGTH(
               FUNCTION TRIM(WS-USER-INPUT TRAILING))
               TO WS-SEND-MSG-LEN
           MOVE 30000 TO WS-SEND-TIMEOUT-MS

           CALL "COPILOT-SESSION" USING WS-SEND-OPTIONS
               WS-SEND-RESULT WS-RETURN-CODE

           EVALUATE TRUE
               WHEN RESULT-HAS-ERROR
                   DISPLAY "ERROR: "
                       FUNCTION TRIM(WS-RESULT-ERROR-MSG)
               WHEN RESULT-IS-OK
                   DISPLAY " "
                   DISPLAY "Copilot> "
                       FUNCTION TRIM(WS-RESULT-MESSAGE)
               WHEN OTHER
                   DISPLAY "WARNING: Unexpected result state"
           END-EVALUATE
           .

      *----------------------------------------------------------------*
      * CLEANUP: Destroy session and stop the client.                  *
      *----------------------------------------------------------------*
       CLEANUP.
           CALL "COPILOT-SESSION" USING WS-SESSION-STATE
               WS-RETURN-CODE
           PERFORM SHUTDOWN-CLIENT
           .

      *----------------------------------------------------------------*
      * SHUTDOWN-CLIENT: Stop the Copilot CLI process.                 *
      *----------------------------------------------------------------*
       SHUTDOWN-CLIENT.
           CALL "COPILOT-CLIENT-STOP" USING WS-RETURN-CODE
           .
