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
