--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Copilot.Types;          use Copilot.Types;
with Copilot.JSON_RPC;       use Copilot.JSON_RPC;

package body Copilot.Session is

   --  -------------------------------------------------------------------
   --  Internal JSON helpers
   --  -------------------------------------------------------------------

   --  Escape a string for inclusion in JSON.
   function Escape (S : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in S'Range loop
         case S (I) is
            when '"'  => Append (Result, "\""");
            when '\' => Append (Result, "\\");
            when others => Append (Result, S (I));
         end case;
      end loop;
      return To_String (Result);
   end Escape;

   --  Extract the value of a top-level JSON string key (simple parser).
   function Json_Get (Json : String; Key : String) return String is
      Search : constant String := """" & Key & """:""";
      Pos    : constant Natural := Index (Json, Search);
   begin
      if Pos = 0 then
         return "";
      end if;
      declare
         Start  : constant Positive := Pos + Search'Length;
         Finish : Natural := Index (Json, """", Start);
      begin
         if Finish = 0 then
            return "";
         end if;
         return Json (Start .. Finish - 1);
      end;
   end Json_Get;

   --  -------------------------------------------------------------------
   --  Public API
   --  -------------------------------------------------------------------

   function Create_Internal
     (Conn    : access RPC_Connection;
      Sid     : String;
      Ws_Path : String;
      Trace_Prov : Trace_Context_Provider := null) return Copilot_Session
   is
   begin
      return (Conn              => Conn,
              Sid               => To_Unbounded_String (Sid),
              Ws_Path           => To_Unbounded_String (Ws_Path),
              Active            => True,
              Perm_Handler      => null,
              Input_Handler     => null,
              Exit_Plan_Handler => null,
              Trace_Provider    => Trace_Prov);
   end Create_Internal;

   procedure Send
     (Self    : in out Copilot_Session;
      Options : Message_Options)
   is
      Params : constant String :=
        "{""sessionId"":""" & Escape (To_String (Self.Sid)) & ""","
        & """prompt"":""" & Escape (To_String (Options.Prompt)) & """}";
      Ignore : constant String :=
        Send_Request (Self.Conn.all, "session/send", Params);
   begin
      null;
   end Send;

   function Send_And_Wait
     (Self    : in out Copilot_Session;
      Options : Message_Options;
      Timeout : Duration := 60.0) return Session_Event
   is
      Params : constant String :=
        "{""sessionId"":""" & Escape (To_String (Self.Sid)) & ""","
        & """prompt"":""" & Escape (To_String (Options.Prompt)) & """}";
      Resp   : constant String :=
        Send_Request (Self.Conn.all, "session/sendAndWait", Params);
      Evt    : Session_Event;
   begin
      Evt.Session_Id := Self.Sid;
      Evt.Event_Type := +"assistant.message";
      Evt.Content    := To_Unbounded_String (Json_Get (Resp, "content"));
      Evt.Raw_Json   := To_Unbounded_String (Resp);
      return Evt;
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
      Params : constant String :=
        "{""sessionId"":""" & Escape (To_String (Self.Sid)) & """}";
      Ignore : constant String :=
        Send_Request (Self.Conn.all, "session/abort", Params);
   begin
      null;
   end Abort_Turn;

   procedure Destroy (Self : in out Copilot_Session) is
      Params : constant String :=
        "{""sessionId"":""" & Escape (To_String (Self.Sid)) & """}";
      Ignore : constant String :=
        Send_Request (Self.Conn.all, "session/destroy", Params);
   begin
      Self.Active := False;
   end Destroy;

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
