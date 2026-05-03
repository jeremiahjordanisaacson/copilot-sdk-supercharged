--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  test_e2e -- Main E2E test program for the Ada Copilot SDK.
--
--  Spawns the shared replaying CAPI proxy, configures it with the
--  appropriate YAML snapshot for each test, exercises the SDK, and
--  reports pass/fail results.
--

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Environment_Variables;
with GNAT.OS_Lib;

with Test_Harness;           use Test_Harness;
with Copilot.Client;         use Copilot.Client;
with Copilot.Types;          use Copilot.Types;
with Copilot.Session;        use Copilot.Session;

procedure Test_E2E is

   Failures : Natural := 0;

   --  Helpers ---------------------------------------------------------------

   Snapshots_Dir : constant String :=
     Ada.Directories.Full_Name
       (".." & GNAT.OS_Lib.Directory_Separator &
        ".." & GNAT.OS_Lib.Directory_Separator &
        ".." & GNAT.OS_Lib.Directory_Separator &
        "test" & GNAT.OS_Lib.Directory_Separator &
        "snapshots");

   function Snapshot_Path (Category, Name : String) return String is
   begin
      return Snapshots_Dir &
             GNAT.OS_Lib.Directory_Separator &
             Category &
             GNAT.OS_Lib.Directory_Separator &
             Name & ".yaml";
   end Snapshot_Path;

   --  Work dir that the proxy uses for path substitution.
   Work_Dir : constant String := Ada.Directories.Current_Directory;

   --  Shared proxy instance.
   Proxy : Capi_Proxy;

   --  Utility: create a Client_Options with the proxy URL filled in.
   function Make_Options return Client_Options is
      Opts : Client_Options := Default_Options;
   begin
      Opts.CLI_Url := To_Unbounded_String (Proxy.Proxy_Url);
      return Opts;
   end Make_Options;

   --------------------------------------------------------------------------
   --  Test 1: Create a session and disconnect
   --------------------------------------------------------------------------

   procedure Test_Session_Create_And_Disconnect is
      Client  : Copilot_Client := Create (Make_Options);
      Session : Copilot_Session;
   begin
      Put ("  Test_Session_Create_And_Disconnect ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_create_and_disconnect_sessions"),
         Work_Dir);

      Start (Client);
      Session := Create_Session (Client);

      --  The session id must be a non-empty string.
      if Session_Id (Session) = "" then
         Put_Line ("FAIL (empty session id)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Session_Create_And_Disconnect;

   --------------------------------------------------------------------------
   --  Test 2: Send a message and receive a response
   --------------------------------------------------------------------------

   procedure Test_Send_Message is
      Client  : Copilot_Client := Create (Make_Options);
      Session : Copilot_Session;
      Opts    : Message_Options;
      Evt     : Session_Event;
   begin
      Put ("  Test_Send_Message ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);
      Session := Create_Session (Client);

      Opts.Prompt := To_Unbounded_String ("What is 1+1?");
      Evt := Send_And_Wait (Session, Opts);

      --  We expect the event to come back without raising an exception.
      --  The proxy replays a canned response so we just check non-empty.
      if To_String (Evt.Content) = "" then
         Put_Line ("FAIL (empty response content)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Send_Message;

   --------------------------------------------------------------------------
   --  Test 3: SessionFs configuration (setProvider on start)
   --------------------------------------------------------------------------

   procedure Test_Session_Fs_Config is
      Opts   : Client_Options := Make_Options;
      Client : Copilot_Client;
   begin
      Put ("  Test_Session_Fs_Config ... ");

      Proxy.Configure
        (Snapshot_Path ("session_fs",
                        "should_route_file_operations_through_the_session_fs_provider"),
         Work_Dir);

      --  Set SessionFs initial CWD so the client issues setProvider on start.
      Opts.Session_Fs_Initial_Cwd := To_Unbounded_String (Work_Dir);
      Client := Create (Opts);

      Start (Client);

      --  If setProvider were rejected we would get an exception from Start.
      Put_Line ("ok");

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Session_Fs_Config;

   --------------------------------------------------------------------------
   --  Test 4: Multi-turn conversation
   --------------------------------------------------------------------------

   procedure Test_Multi_Turn_Conversation is
      Client  : Copilot_Client := Create (Make_Options);
      Session : Copilot_Session;
      Opts1   : Message_Options;
      Opts2   : Message_Options;
      Evt1    : Session_Event;
      Evt2    : Session_Event;
   begin
      Put ("  Test_Multi_Turn_Conversation ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);
      Session := Create_Session (Client);

      Opts1.Prompt := To_Unbounded_String ("What is 1+1?");
      Evt1 := Send_And_Wait (Session, Opts1);

      if To_String (Evt1.Content) = "" then
         Put_Line ("FAIL (empty first response)");
         Failures := Failures + 1;
         Stop (Client);
         return;
      end if;

      Opts2.Prompt := To_Unbounded_String ("Now double that result");
      Evt2 := Send_And_Wait (Session, Opts2);

      if To_String (Evt2.Content) = "" then
         Put_Line ("FAIL (empty second response)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Multi_Turn_Conversation;

   --------------------------------------------------------------------------
   --  Test 5: Session resume
   --------------------------------------------------------------------------

   procedure Test_Session_Resume is
      Client1  : Copilot_Client := Create (Make_Options);
      Client2  : Copilot_Client := Create (Make_Options);
      Session  : Copilot_Session;
      Resumed  : Copilot_Session;
      Saved_Id : Unbounded_String;
      Config   : Session_Config;
   begin
      Put ("  Test_Session_Resume ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client1);
      Session := Create_Session (Client1);
      Saved_Id := To_Unbounded_String (Session_Id (Session));
      Stop (Client1);

      Start (Client2);
      Config.Session_Id := Saved_Id;
      Resumed := Create_Session (Client2, Config);

      if Session_Id (Resumed) = "" then
         Put_Line ("FAIL (empty resumed session id)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client2);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client1);
         exception
            when others => null;
         end;
         begin
            Stop (Client2);
         exception
            when others => null;
         end;
   end Test_Session_Resume;

   --------------------------------------------------------------------------
   --  Test 6: Session list
   --------------------------------------------------------------------------

   procedure Test_Session_List is
      Client   : Copilot_Client := Create (Make_Options);
      Session1 : Copilot_Session;
      Session2 : Copilot_Session;
      Sessions : Session_List;
   begin
      Put ("  Test_Session_List ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);
      Session1 := Create_Session (Client);
      Session2 := Create_Session (Client);

      Sessions := List_Sessions (Client);

      if Sessions'Length < 2 then
         Put_Line ("FAIL (expected >= 2 sessions, got" &
                   Natural'Image (Sessions'Length) & ")");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Session_List;

   --------------------------------------------------------------------------
   --  Test 7: Session metadata
   --------------------------------------------------------------------------

   procedure Test_Session_Metadata is
      Client   : Copilot_Client := Create (Make_Options);
      Session  : Copilot_Session;
      Metadata : Session_Metadata_Record;
   begin
      Put ("  Test_Session_Metadata ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);
      Session := Create_Session (Client);

      Metadata := Get_Session_Metadata (Client, Session_Id (Session));

      --  If we got here without exception, metadata was returned.
      Put_Line ("ok");

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Session_Metadata;

   --------------------------------------------------------------------------
   --  Test 8: Session delete
   --------------------------------------------------------------------------

   procedure Test_Session_Delete is
      Client   : Copilot_Client := Create (Make_Options);
      Session  : Copilot_Session;
      Id       : Unbounded_String;
      Sessions : Session_List;
      Found    : Boolean := False;
   begin
      Put ("  Test_Session_Delete ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);
      Session := Create_Session (Client);
      Id := To_Unbounded_String (Session_Id (Session));

      Delete_Session (Client, To_String (Id));

      Sessions := List_Sessions (Client);
      for I in Sessions'Range loop
         if To_Unbounded_String (Session_Id (Sessions (I))) = Id then
            Found := True;
         end if;
      end loop;

      if Found then
         Put_Line ("FAIL (deleted session still in list)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Session_Delete;

   --------------------------------------------------------------------------
   --  Test 9: Model list
   --------------------------------------------------------------------------

   procedure Test_Model_List is
      Client : Copilot_Client := Create (Make_Options);
      Models : Model_List;
   begin
      Put ("  Test_Model_List ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);
      Models := List_Models (Client);

      if Models'Length = 0 then
         Put_Line ("FAIL (empty models list)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Model_List;

   --------------------------------------------------------------------------
   --  Test 10: Ping
   --------------------------------------------------------------------------

   procedure Test_Ping is
      Client   : Copilot_Client := Create (Make_Options);
      Response : Ping_Response;
   begin
      Put ("  Test_Ping ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);
      Response := Ping (Client);

      if To_String (Response.Message) = "" then
         Put_Line ("FAIL (empty ping message)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Ping;

   --------------------------------------------------------------------------
   --  Test 11: Auth status
   --------------------------------------------------------------------------

   procedure Test_Auth_Status is
      Client : Copilot_Client := Create (Make_Options);
      Status : Auth_Status_Record;
   begin
      Put ("  Test_Auth_Status ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);
      Status := Get_Auth_Status (Client);

      --  If we got here without an exception the status was returned.
      Put_Line ("ok");

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Auth_Status;

   --------------------------------------------------------------------------
   --  Test 12: Client lifecycle
   --------------------------------------------------------------------------

   procedure Test_Client_Lifecycle is
      Client : Copilot_Client := Create (Make_Options);
   begin
      Put ("  Test_Client_Lifecycle ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);

      if not Is_Connected (Client) then
         Put_Line ("FAIL (not connected after start)");
         Failures := Failures + 1;
         Stop (Client);
         return;
      end if;

      Stop (Client);

      if Is_Connected (Client) then
         Put_Line ("FAIL (still connected after stop)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Client_Lifecycle;

   --------------------------------------------------------------------------
   --  Test 13: Foreground session
   --------------------------------------------------------------------------

   procedure Test_Foreground_Session is
      Client  : Copilot_Client := Create (Make_Options);
      Session : Copilot_Session;
      Id      : Unbounded_String;
      Fg_Id   : String (1 .. 256);
      Fg_Len  : Natural;
   begin
      Put ("  Test_Foreground_Session ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);
      Session := Create_Session (Client);
      Id := To_Unbounded_String (Session_Id (Session));

      Set_Foreground_Session (Client, To_String (Id));
      declare
         Fg : constant String := Get_Foreground_Session (Client);
      begin
         if Fg /= To_String (Id) then
            Put_Line ("FAIL (foreground id mismatch)");
            Failures := Failures + 1;
         else
            Put_Line ("ok");
         end if;
      end;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Foreground_Session;

   --------------------------------------------------------------------------
   --  Test 14: Tools
   --------------------------------------------------------------------------

   procedure Test_Tools is
      Client  : Copilot_Client := Create (Make_Options);
      Session : Copilot_Session;
      Config  : Session_Config;
      Tool    : Tool_Definition;
      Opts    : Message_Options;
      Evt     : Session_Event;
   begin
      Put ("  Test_Tools ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);

      Tool.Name := To_Unbounded_String ("test_tool");
      Tool.Description := To_Unbounded_String ("A test tool for E2E");
      Config.Tools := (1 => Tool);
      Session := Create_Session (Client, Config);

      Opts.Prompt := To_Unbounded_String ("Use the test_tool with input hello");
      Evt := Send_And_Wait (Session, Opts);

      if To_String (Evt.Content) = "" then
         Put_Line ("FAIL (empty response)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Tools;

   --------------------------------------------------------------------------
   --  Test 15: Streaming
   --------------------------------------------------------------------------

   procedure Test_Streaming is
      Client  : Copilot_Client := Create (Make_Options);
      Session : Copilot_Session;
      Config  : Session_Config;
      Opts    : Message_Options;
      Evt     : Session_Event;
   begin
      Put ("  Test_Streaming ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_receive_session_events"),
         Work_Dir);

      Start (Client);

      Config.Streaming := True;
      Session := Create_Session (Client, Config);

      Opts.Prompt := To_Unbounded_String ("Hello streaming");
      Evt := Send_And_Wait (Session, Opts);

      if To_String (Evt.Content) = "" then
         Put_Line ("FAIL (empty streaming response)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Streaming;

   --------------------------------------------------------------------------
   --  Test 16: System message customization
   --------------------------------------------------------------------------

   procedure Test_System_Message_Customization is
      Client  : Copilot_Client := Create (Make_Options);
      Session : Copilot_Session;
      Config  : Session_Config;
   begin
      Put ("  Test_System_Message_Customization ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);

      Config.System_Message_Content :=
        To_Unbounded_String ("You are a helpful test assistant.");
      Config.System_Message_Mode :=
        To_Unbounded_String ("append");
      Session := Create_Session (Client, Config);

      if Session_Id (Session) = "" then
         Put_Line ("FAIL (empty session id)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_System_Message_Customization;

   --------------------------------------------------------------------------
   --  Test 17: SessionFs provider
   --------------------------------------------------------------------------

   procedure Test_Session_Fs_Provider is
      Opts   : Client_Options := Make_Options;
      Client : Copilot_Client;
   begin
      Put ("  Test_Session_Fs_Provider ... ");

      Proxy.Configure
        (Snapshot_Path ("session_fs",
                        "should_route_file_operations_through_the_session_fs_provider"),
         Work_Dir);

      Opts.Session_Fs_Initial_Cwd := To_Unbounded_String (Work_Dir);
      Client := Create (Opts);
      Start (Client);

      if not Is_Connected (Client) then
         Put_Line ("FAIL (client not connected with sessionFs)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Session_Fs_Provider;

   --------------------------------------------------------------------------
   --  Test 18: MCP servers config
   --------------------------------------------------------------------------

   procedure Test_Mcp_Servers_Config is
      Client  : Copilot_Client := Create (Make_Options);
      Session : Copilot_Session;
      Config  : Session_Config;
   begin
      Put ("  Test_Mcp_Servers_Config ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);

      Config.Mcp_Servers := To_Unbounded_String
        ("{""testServer"":{""command"":""echo"",""args"":[""hello""]}}");
      Session := Create_Session (Client, Config);

      if Session_Id (Session) = "" then
         Put_Line ("FAIL (empty session id)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Mcp_Servers_Config;

   --------------------------------------------------------------------------
   --  Test 19: Skills config
   --------------------------------------------------------------------------

   procedure Test_Skills_Config is
      Client  : Copilot_Client := Create (Make_Options);
      Session : Copilot_Session;
      Config  : Session_Config;
   begin
      Put ("  Test_Skills_Config ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);

      Config.Skills_Directories := To_Unbounded_String
        ("[""" & Work_Dir & "/skills""]");
      Session := Create_Session (Client, Config);

      if Session_Id (Session) = "" then
         Put_Line ("FAIL (empty session id)");
         Failures := Failures + 1;
      else
         Put_Line ("ok");
      end if;

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Skills_Config;

   --------------------------------------------------------------------------
   --  Test 20: Compaction
   --------------------------------------------------------------------------

   procedure Test_Compaction is
      Client  : Copilot_Client := Create (Make_Options);
      Session : Copilot_Session;
      Opts    : Message_Options;
      Evt     : Session_Event;
   begin
      Put ("  Test_Compaction ... ");

      Proxy.Configure
        (Snapshot_Path ("session",
                        "should_have_stateful_conversation"),
         Work_Dir);

      Start (Client);
      Session := Create_Session (Client);

      --  Send multiple messages to trigger compaction.
      for I in 1 .. 5 loop
         Opts.Prompt := To_Unbounded_String
           ("Message" & Natural'Image (I) & ": Tell me something interesting.");
         Evt := Send_And_Wait (Session, Opts);

         if To_String (Evt.Content) = "" then
            Put_Line ("FAIL (empty response at message" &
                      Natural'Image (I) & ")");
            Failures := Failures + 1;
            Stop (Client);
            return;
         end if;
      end loop;

      Put_Line ("ok");

      Stop (Client);

   exception
      when E : others =>
         Put_Line ("FAIL (exception)");
         Failures := Failures + 1;
         begin
            Stop (Client);
         exception
            when others => null;
         end;
   end Test_Compaction;

   --------------------------------------------------------------------------
   --  Runner
   --------------------------------------------------------------------------

begin
   Put_Line ("Ada E2E Tests");
   Put_Line ("=============");
   New_Line;

   --  Start the shared proxy once.
   Put_Line ("Starting replaying CAPI proxy ...");
   Proxy.Start;
   Put_Line ("Proxy listening at " & Proxy.Proxy_Url);
   New_Line;

   --  Run tests.
   Test_Session_Create_And_Disconnect;
   Test_Send_Message;
   Test_Session_Fs_Config;
   Test_Multi_Turn_Conversation;
   Test_Session_Resume;
   Test_Session_List;
   Test_Session_Metadata;
   Test_Session_Delete;
   Test_Model_List;
   Test_Ping;
   Test_Auth_Status;
   Test_Client_Lifecycle;
   Test_Foreground_Session;
   Test_Tools;
   Test_Streaming;
   Test_System_Message_Customization;
   Test_Session_Fs_Provider;
   Test_Mcp_Servers_Config;
   Test_Skills_Config;
   Test_Compaction;

   New_Line;

   --  Tear down.
   Proxy.Stop (Skip_Cache => True);

   if Failures > 0 then
      Put_Line ("FAILED:" & Natural'Image (Failures) & " test(s)");
      GNAT.OS_Lib.OS_Exit (1);
   else
      Put_Line ("All E2E tests passed");
   end if;
end Test_E2E;
