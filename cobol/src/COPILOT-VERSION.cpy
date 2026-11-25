      *================================================================*
      * COPILOT-VERSION.cpy                                            *
      * Protocol version constants for the Copilot SDK.                *
      * SDK protocol 4: the handshake accepts exactly version 4        *
      * (WS-PROTO-MIN through WS-PROTO-MAX).                           *
      *================================================================*

       01  WS-PROTOCOL-VERSION.
           05 WS-PROTO-MAJOR        PIC 9(2)   VALUE 4.
           05 WS-PROTO-MINOR        PIC 9(2)   VALUE 0.
           05 WS-PROTO-STRING       PIC X(10)  VALUE "4".
           05 WS-SDK-NAME           PIC X(30)
              VALUE "copilot-sdk-cobol".
           05 WS-SDK-VERSION        PIC X(10)  VALUE "3.1.1".
           05 WS-PROTO-MIN          PIC 9(2)   VALUE 4.
           05 WS-PROTO-MAX          PIC 9(2)   VALUE 4.
