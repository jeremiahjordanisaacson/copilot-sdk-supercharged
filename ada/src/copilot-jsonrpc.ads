--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  Copilot.JSON_RPC -- JSON-RPC 2.0 transport with Content-Length framing.
--
--  The transport reads and writes messages using the LSP-style
--  Content-Length header protocol over a pair of file descriptors
--  (typically the stdin/stdout of the Copilot CLI process).
--

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with GNAT.OS_Lib;

package Copilot.JSON_RPC is

   --  A lightweight JSON-RPC connection that owns a pair of file descriptors
   --  (read_fd for incoming data, write_fd for outgoing data).
   type RPC_Connection is tagged private;

   --  Create a connection from two OS-level file descriptors.
   function Create
     (Read_FD  : GNAT.OS_Lib.File_Descriptor;
      Write_FD : GNAT.OS_Lib.File_Descriptor) return RPC_Connection;

   --  Send a JSON-RPC request and return the raw JSON response body.
   --  Raises Protocol_Error on framing or parse issues.
   function Send_Request
     (Self   : in out RPC_Connection;
      Method : String;
      Params : String := "{}") return String;

   --  Send a JSON-RPC notification (no response expected).
   procedure Send_Notification
     (Self   : in out RPC_Connection;
      Method : String;
      Params : String := "{}");

   --  Read one framed message from the connection and return it as a string.
   --  Blocks until a complete message is available.
   function Read_Message (Self : in out RPC_Connection) return String;

   --  Write a raw framed message (adds Content-Length header).
   procedure Write_Message
     (Self    : in out RPC_Connection;
      Payload : String);

   --  Close the underlying file descriptors.
   procedure Close (Self : in out RPC_Connection);

   --  Return True when the connection has been closed.
   function Is_Closed (Self : RPC_Connection) return Boolean;

private

   type RPC_Connection is tagged record
      Read_FD   : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Write_FD  : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Next_Id   : Positive := 1;
      Closed    : Boolean  := True;
   end record;

end Copilot.JSON_RPC;
