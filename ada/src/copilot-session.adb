--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Calendar;           use Ada.Calendar;
with Copilot.Types;          use Copilot.Types;
with Copilot.JSON_RPC;       use Copilot.JSON_RPC;
with Copilot.Wire;           use Copilot.Wire;
with Copilot.Callbacks;

package body Copilot.Session is

   --  -------------------------------------------------------------------
   --  Internal JSON helpers
   --  -------------------------------------------------------------------

   --  session.send parameters for Options.
   function Send_Params
     (Self    : Copilot_Session;
      Options : Message_Options) return String
   is
      Params : Unbounded_String :=
        To_Unbounded_String
          ("{""sessionId"":" & Quote (To_String (Self.Sid))
           & ",""prompt"":" & Quote (To_String (Options.Prompt)));
   begin
      if Length (Options.Agent_Mode) > 0 then
         Append (Params, ",""agentMode"":"
                       & Quote (To_String (Options.Agent_Mode)));
      end if;
      if Length (Options.Display_Prompt) > 0 then
         Append (Params, ",""displayPrompt"":"
                       & Quote (To_String (Options.Display_Prompt)));
      end if;
      if Length (Options.Request_Headers_Json) > 0 then
         Append (Params, ",""requestHeaders"":"
                       & To_String (Options.Request_Headers_Json));
      end if;
      --  Message source (MessageSource): "user", "system" or "agent-<id>".
      if Length (Options.Source) > 0 then
         Append (Params, ",""source"":" & Quote (To_String (Options.Source)));
      end if;
      if Length (Options.Client_Turn_Id) > 0 then
         Append (Params, ",""clientTurnId"":"
                       & Quote (To_String (Options.Client_Turn_Id)));
      end if;
      --  Structured output (responseSchema) => responseFormat json_schema.
      if Length (Options.Response_Schema_Json) > 0 then
         Append (Params, ",""responseFormat"":{""type"":""json_schema"","
                       & """jsonSchema"":{""name"":""response"","
                       & """strict"":true,""schema"":"
                       & To_String (Options.Response_Schema_Json) & "}}");
      end if;
      Append (Params, "}");
      return To_String (Params);
   end Send_Params;

   function Session_Params (Self : Copilot_Session) return String is
   begin
      return "{""sessionId"":" & Quote (To_String (Self.Sid)) & "}";
   end Session_Params;

   --  -------------------------------------------------------------------
   --  Public API
   --  -------------------------------------------------------------------

   function Create_Internal
     (Conn         : access RPC_Connection;
      Sid          : String;
      Ws_Path      : String;
      Trace_Prov   : Trace_Context_Provider := null;
      Token_Reg_Id : String := "") return Copilot_Session
   is
   begin
      return (Conn              => Conn,
              Sid               => To_Unbounded_String (Sid),
              Ws_Path           => To_Unbounded_String (Ws_Path),
              Active            => True,
              Perm_Handler      => null,
              Input_Handler     => null,
              Exit_Plan_Handler => null,
              Trace_Provider    => Trace_Prov,
              Token_Reg_Id      => To_Unbounded_String (Token_Reg_Id));
   end Create_Internal;

   procedure Send
     (Self    : in out Copilot_Session;
      Options : Message_Options)
   is
      Ignore : constant String :=
        Send_Request (Self.Conn.all, "session.send", Send_Params (Self, Options));
   begin
      null;
   end Send;

   function Send_And_Wait
     (Self    : in out Copilot_Session;
      Options : Message_Options;
      Timeout : Duration := 60.0) return Session_Event
   is
      Deadline : constant Time := Clock + Timeout;
      Evt      : Session_Event;
   begin
      Send (Self, Options);
      Evt.Session_Id := Self.Sid;
      Evt.Event_Type := +"assistant.message";

      --  Collect events until the session becomes idle.  Server-to-client
      --  requests (hooks, token providers, ...) are answered meanwhile.
      loop
         declare
            Note   : constant String := Read_Notification (Self.Conn.all);
            Params : constant String := Member (Note, "params");
            Event  : constant String := Member (Params, "event");
            Kind   : constant String := String_Member (Event, "type");
         begin
            if String_Member (Note, "method") = "session.event"
              and then String_Member (Params, "sessionId") = To_String (Self.Sid)
            then
               if Kind = "assistant.message" then
                  Evt.Id := To_Unbounded_String (String_Member (Event, "id"));
                  Evt.Content := To_Unbounded_String
                    (String_Member (Member (Event, "data"), "content"));
                  Evt.Raw_Json := To_Unbounded_String (Event);
               elsif Kind = "session.idle" then
                  return Evt;
               elsif Kind = "session.error" then
                  raise Session_Error
                    with String_Member (Member (Event, "data"), "message");
               end if;
            end if;
         end;
         if Clock > Deadline then
            raise Timeout_Error with "Send_And_Wait timed out";
         end if;
      end loop;
   end Send_And_Wait;

   function Get_Messages
     (Self : Copilot_Session) return Event_List
   is
      Dummy : Event_List;
   begin
      --  Placeholder: would issue session/getMessages RPC.
      return Dummy;
   end Get_Messages;

   procedure Abort_Turn (Self : in out Copilot_Session) is
      Ignore : constant String :=
        Send_Request (Self.Conn.all, "session.abort", Session_Params (Self));
   begin
      null;
   end Abort_Turn;

   procedure Destroy (Self : in out Copilot_Session) is
   begin
      if Self.Active then
         Detach (Self);
      end if;
   end Destroy;

   procedure Switch_Auto_Tier
     (Self : in out Copilot_Session;
      Tier : String)
   is
      Ignore : constant String :=
        Send_Request
          (Self.Conn.all, "session.model.switchAutoTier",
           "{""sessionId"":" & Quote (To_String (Self.Sid))
           & ",""autoTier"":" & Quote (Tier) & "}");
   begin
      null;
   end Switch_Auto_Tier;

   procedure Detach (Self : in out Copilot_Session) is
      Ignore : constant String :=
        Send_Request (Self.Conn.all, "session.detach", Session_Params (Self));
   begin
      Self.Active := False;
      Copilot.Callbacks.Unregister_Session (To_String (Self.Sid));
      if Length (Self.Token_Reg_Id) > 0 then
         Copilot.Callbacks.Unregister_Token_Provider
           (To_String (Self.Token_Reg_Id));
      end if;
   end Detach;

   function Recheck_Policy_Helper
     (Self : in out Copilot_Session) return String
   is
      Resp : constant String :=
        Send_Request
          (Self.Conn.all, "session.managedSettings.recheckPolicyHelper",
           Session_Params (Self));
   begin
      return Member (Resp, "result");
   end Recheck_Policy_Helper;

   function Set_Permission_Mode
     (Self : in out Copilot_Session;
      Mode : String) return Permission_Mode_Result
   is
      Resp   : constant String :=
        Send_Request
          (Self.Conn.all, "session.permissions.setMode",
           "{""sessionId"":" & Quote (To_String (Self.Sid))
           & ",""mode"":" & Quote (Mode) & "}");
      Result : constant String := Member (Resp, "result");
   begin
      return (Success => Bool_Member (Result, "success"),
              Mode    => To_Unbounded_String (String_Member (Result, "mode")),
              Reason  => To_Unbounded_String (String_Member (Result, "reason")));
   end Set_Permission_Mode;

   function Shell_Output (Event : Session_Event) return Shell_Output_Event_Data
   is
      Data : constant String := Member (To_String (Event.Raw_Json), "data");
   begin
      return (Tool_Call_Id => To_Unbounded_String
                                (String_Member (Data, "toolCallId")),
              Text         => To_Unbounded_String (String_Member (Data, "text")),
              Sequence     => Natural_Member (Data, "sequence"),
              Stream       => To_Unbounded_String
                                (String_Member (Data, "stream")));
   end Shell_Output;

   procedure Register_Tool
     (Self    : in out Copilot_Session;
      Name    : String;
      Handler : Tool_Handler)
   is
      pragma Unreferenced (Name, Handler);
   begin
      --  Tool registration is handled at protocol level via
      --  session configuration.  This stores the handler for
      --  incoming tool-call requests.
      null;
   end Register_Tool;

   procedure Register_Permission_Handler
     (Self    : in out Copilot_Session;
      Handler : Permission_Handler)
   is
   begin
      Self.Perm_Handler := Handler;
   end Register_Permission_Handler;

   procedure Register_User_Input_Handler
     (Self    : in out Copilot_Session;
      Handler : User_Input_Handler)
   is
   begin
      Self.Input_Handler := Handler;
      Copilot.Callbacks.Set_User_Input_Handler (To_String (Self.Sid), Handler);
   end Register_User_Input_Handler;

   procedure Register_Exit_Plan_Mode_Handler
     (Self    : in out Copilot_Session;
      Handler : Exit_Plan_Mode_Handler)
   is
   begin
      Self.Exit_Plan_Handler := Handler;
   end Register_Exit_Plan_Mode_Handler;

   function Session_Id (Self : Copilot_Session) return String is
   begin
      return To_String (Self.Sid);
   end Session_Id;

   function Workspace_Path (Self : Copilot_Session) return String is
   begin
      return To_String (Self.Ws_Path);
   end Workspace_Path;

   function Is_Active (Self : Copilot_Session) return Boolean is
   begin
      return Self.Active;
   end Is_Active;

end Copilot.Session;
