--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  Copilot.Session -- A single conversational session with the Copilot agent.
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Copilot.Types;         use Copilot.Types;
with Copilot.JSON_RPC;      use Copilot.JSON_RPC;

package Copilot.Session is

   --  Forward declaration: the client will allocate sessions via Create.
   type Copilot_Session is tagged private;

   --  Send a user message (non-blocking; events arrive asynchronously).
   procedure Send
     (Self    : in out Copilot_Session;
      Options : Message_Options);

   --  Send a message and block until the session becomes idle or the
   --  timeout (in seconds) expires.  Returns the final assistant event.
   function Send_And_Wait
     (Self    : in out Copilot_Session;
      Options : Message_Options;
      Timeout : Duration := 60.0) return Session_Event;

   --  Retrieve the full conversation history.
   function Get_Messages
     (Self : Copilot_Session) return Event_List;

   --  Abort any in-progress assistant turn.
   procedure Abort_Turn (Self : in out Copilot_Session);

   --  Permanently destroy this session on the server.
   procedure Destroy (Self : in out Copilot_Session);

   --  Register a tool handler by name.
   procedure Register_Tool
     (Self    : in out Copilot_Session;
      Name    : String;
      Handler : Tool_Handler);

   --  Register a permission handler.
   procedure Register_Permission_Handler
     (Self    : in out Copilot_Session;
      Handler : Permission_Handler);

   --  Register a user-input handler (enables the ask_user tool).
   procedure Register_User_Input_Handler
     (Self    : in out Copilot_Session;
      Handler : User_Input_Handler);

   --  Register an exit-plan-mode handler.
   procedure Register_Exit_Plan_Mode_Handler
     (Self    : in out Copilot_Session;
      Handler : Exit_Plan_Mode_Handler);

   --  Property accessors.
   function Session_Id      (Self : Copilot_Session) return String;
   function Workspace_Path  (Self : Copilot_Session) return String;
   function Is_Active       (Self : Copilot_Session) return Boolean;

   --  Internal: create a session object from an existing RPC connection.
   --  Not intended for direct use by application code.
   function Create_Internal
     (Conn         : access RPC_Connection;
      Sid          : String;
      Ws_Path      : String;
      Trace_Prov   : Trace_Context_Provider := null) return Copilot_Session;

private

   type Copilot_Session is tagged record
      Conn            : access RPC_Connection;
      Sid             : UString  := Null_UString;
      Ws_Path         : UString  := Null_UString;
      Active          : Boolean  := True;
      Perm_Handler    : Permission_Handler   := null;
      Input_Handler   : User_Input_Handler   := null;
      Exit_Plan_Handler : Exit_Plan_Mode_Handler := null;
      Trace_Provider  : Trace_Context_Provider := null;
   end record;

end Copilot.Session;
