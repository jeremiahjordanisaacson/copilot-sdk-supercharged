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
with Ada.Containers.Indefinite_Vectors;
with GNAT.OS_Lib;

package Copilot.JSON_RPC is

   --  A lightweight JSON-RPC connection that owns a pair of file descriptors
   --  (read_fd for incoming data, write_fd for outgoing data).
   type RPC_Connection is tagged private;

   --  Handler for server-to-client requests (callbacks such as
   --  gitHubToken.getToken, hooks.invoke or memoryStore.store).  Set
   --  Handled to False for unknown methods; set Error to a non-empty message
   --  to answer with a JSON-RPC error.
   type Server_Request_Handler is access procedure
     (Method  : String;
      Params  : String;
      Result  : out Unbounded_String;
      Error   : out Unbounded_String;
      Handled : out Boolean);

   --  Create a connection from two OS-level file descriptors.
   function Create
     (Read_FD  : GNAT.OS_Lib.File_Descriptor;
      Write_FD : GNAT.OS_Lib.File_Descriptor) return RPC_Connection;

   --  Send a JSON-RPC request and return the raw JSON response body.
   --  Server-to-client requests that arrive meanwhile are answered through
   --  the registered handler; notifications are queued for
   --  Read_Notification.  Raises Protocol_Error on framing issues and when
   --  the server answers with a JSON-RPC error (see Last_Error).
   function Send_Request
     (Self   : in out RPC_Connection;
      Method : String;
      Params : String := "{}") return String;

   --  Send a JSON-RPC notification (no response expected).
   procedure Send_Notification
     (Self   : in out RPC_Connection;
      Method : String;
      Params : String := "{}");

   --  Install the handler used to answer server-to-client requests.
   procedure Set_Server_Request_Handler
     (Self    : in out RPC_Connection;
      Handler : Server_Request_Handler);

   --  Return the next server notification (queued or newly read), answering
   --  server-to-client requests while waiting.  Blocks until one arrives.
   function Read_Notification (Self : in out RPC_Connection) return String;

   --  The "error" object of the last JSON-RPC error response ("" if none).
   function Last_Error (Self : RPC_Connection) return String;

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

   package Message_Queues is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   type RPC_Connection is tagged record
      Read_FD   : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Write_FD  : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Next_Id   : Positive := 1;
      Closed    : Boolean  := True;
      Handler   : Server_Request_Handler := null;
      Queue     : Message_Queues.Vector;
      Last_Err  : Unbounded_String;
   end record;

end Copilot.JSON_RPC;
