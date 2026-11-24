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

   --  Abort any in-progress assistant turn (session.abort).
   procedure Abort_Turn (Self : in out Copilot_Session);

   --  Detach from this session on the server (session.detach) and release
   --  its callbacks.  The session can be resumed later.
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

   --  Register (or replace) the handler that answers this session's
   --  userInput.request (ask_user) requests.  The runtime offers the
   --  ask_user tool only when the session was created or resumed with
   --  Session_Config.On_User_Input_Request set.
   procedure Register_User_Input_Handler
     (Self    : in out Copilot_Session;
      Handler : User_Input_Handler);

   --  Register an exit-plan-mode handler.
   procedure Register_Exit_Plan_Mode_Handler
     (Self    : in out Copilot_Session;
      Handler : Exit_Plan_Mode_Handler);

   --  Request an Auto routing tier for future turns that use the "auto"
   --  model (session.model.switchAutoTier).  Tier is one of the Auto_Tier_*
   --  constants: "fast", "efficiency", "balance" or "intelligence".
   procedure Switch_Auto_Tier
     (Self : in out Copilot_Session;
      Tier : String);

   --  Detach this connection from the session (session.detach).  The
   --  session stays on disk and can be resumed later.
   procedure Detach (Self : in out Copilot_Session);

   --  Re-run the enterprise Policy Helper for this session
   --  (session.managedSettings.recheckPolicyHelper).  Returns the raw
   --  managed-settings snapshot JSON.
   function Recheck_Policy_Helper
     (Self : in out Copilot_Session) return String;

   --  Change the permission mode (session.permissions.setMode).  When the
   --  change is refused, Result.Reason explains why ("managed_policy").
   function Set_Permission_Mode
     (Self : in out Copilot_Session;
      Mode : String) return Permission_Mode_Result;

   --  Decode the data of a tool.shell_output event (Raw_Json holds the
   --  event object).
   function Shell_Output (Event : Session_Event) return Shell_Output_Event_Data;

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
      Trace_Prov   : Trace_Context_Provider := null;
      Token_Reg_Id : String := "") return Copilot_Session;

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
      --  GitHub token provider registration owned by this session.
      Token_Reg_Id    : UString  := Null_UString;
   end record;

end Copilot.Session;
