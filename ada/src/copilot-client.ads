--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  Copilot.Client -- Main entry point for the Ada Copilot SDK.
--
--  The client manages the lifecycle of the Copilot CLI process, provides
--  JSON-RPC connectivity, and exposes session-management operations.
--
--  Typical usage:
--
--     declare
--        C : Copilot_Client := Create;
--     begin
--        Start (C);
--        declare
--           S : Copilot_Session := Create_Session (C);
--        begin
--           --  use session ...
--           Destroy (S);
--        end;
--        Stop (C);
--     end;
--

with Copilot.Types;    use Copilot.Types;
with Copilot.Session;  use Copilot.Session;
with Copilot.JSON_RPC; use Copilot.JSON_RPC;
with GNAT.OS_Lib;
with GNAT.Expect;

package Copilot.Client is

   type Copilot_Client is tagged private;

   --  Create a client with the given options (or sensible defaults).
   function Create
     (Options : Client_Options := Default_Options) return Copilot_Client;

   --  Start the CLI server and establish the JSON-RPC connection.  The
   --  handshake sends connect (with Options.Client_Info as clientInfo) and
   --  falls back to ping on runtimes without connect.  The server must
   --  speak SDK protocol 4 (Copilot CLI 1.0.113 or newer).
   --  Raises Connection_Error if the process cannot be spawned and
   --  Protocol_Error on a protocol version mismatch.
   procedure Start (Self : in out Copilot_Client);

   --  Gracefully stop the server and close all sessions.
   procedure Stop (Self : in out Copilot_Client);

   --  Force-stop the server without graceful cleanup.
   procedure Force_Stop (Self : in out Copilot_Client);

   --  Create a new conversational session.  The flat options are converted
   --  to the SDK protocol 4 grouped payload before session.create is sent.
   function Create_Session
     (Self   : in out Copilot_Client;
      Config : Session_Config := Default_Session_Config)
      return Copilot_Session;

   --  Resume a previously persisted session (session.resume).
   function Resume_Session
     (Self   : in out Copilot_Client;
      Config : Resume_Session_Config) return Copilot_Session;

   --  Delete a session by id.
   procedure Delete_Session
     (Self       : in out Copilot_Client;
      Session_Id : String);

   --  Retrieve metadata for a given session.
   function Get_Session_Metadata
     (Self       : in out Copilot_Client;
      Session_Id : String) return Session_Metadata;

   --  Ping the server; returns the echoed message.
   function Ping
     (Self    : in out Copilot_Client;
      Message : String := "ping") return String;

   --  Query the CLI version and protocol information.
   function Get_Status
     (Self : in out Copilot_Client) return Get_Status_Response;

   --  Query authentication status.
   function Get_Auth_Status
     (Self : in out Copilot_Client) return Auth_Status_Response;

   --  List available models (result may be cached by the CLI).
   function List_Models
     (Self : in out Copilot_Client) return Model_List;

   --  List all sessions. Returns the raw JSON response string.
   function List_Sessions
     (Self : in out Copilot_Client) return String;

   --  Get the last-used session ID (returns empty string if none).
   function Get_Last_Session_Id
     (Self : in out Copilot_Client) return String;

   --  Current connection state.
   function Get_State
     (Self : Copilot_Client) return Connection_State;

   --  Return True when the client has an active connection.
   function Is_Connected (Self : Copilot_Client) return Boolean;

   --  Get the foreground session ID. Returns empty string if none.
   function Get_Foreground_Session_Id
     (Self : in out Copilot_Client) return String;

   --  Set the foreground session ID.
   --  Raises Protocol_Error if the server reports failure.
   procedure Set_Foreground_Session_Id
     (Self       : in out Copilot_Client;
      Session_Id : String);

   --  Clear the runtime's cached enterprise managed settings
   --  (managedSettings.clearCache).
   procedure Clear_Managed_Settings_Cache (Self : in out Copilot_Client);

   --  Policy Helper failure attached to the last failed Create_Session or
   --  Resume_Session (error.data.policyHelperFailure).  Present is False
   --  when the last failure carried none.
   function Last_Policy_Helper_Failure
     (Self : Copilot_Client) return Policy_Helper_Failure;

private

   type Copilot_Client is tagged record
      Options      : Client_Options;
      Conn         : aliased RPC_Connection;
      State        : Connection_State := Disconnected;
      Process_Id   : GNAT.OS_Lib.Process_Id := GNAT.OS_Lib.Invalid_Pid;
      Process      : GNAT.Expect.Process_Descriptor;
      Last_Failure : Policy_Helper_Failure;
   end record;

end Copilot.Client;
