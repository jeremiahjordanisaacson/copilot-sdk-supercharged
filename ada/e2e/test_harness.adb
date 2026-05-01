--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------
--
--  Test_Harness body -- process management for the replaying CAPI proxy.
--
--  Strategy:
--    * Use GNAT.Expect to spawn the proxy and read its stdout for the URL.
--    * Use GNAT.OS_Lib.Spawn to call curl for HTTP POST requests
--      (configure / stop), keeping the implementation simple and portable.
--

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Directories;
with GNAT.OS_Lib;
with GNAT.Expect;

package body Test_Harness is

   Harness_Relative_Path : constant String :=
     ".." & GNAT.OS_Lib.Directory_Separator &
     ".." & GNAT.OS_Lib.Directory_Separator &
     ".." & GNAT.OS_Lib.Directory_Separator &
     "test" & GNAT.OS_Lib.Directory_Separator &
     "harness" & GNAT.OS_Lib.Directory_Separator &
     "server.ts";

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Capi_Proxy) is
      use GNAT.Expect;
      use Ada.Strings.Fixed;
      use Ada.Text_IO;

      Fd      : Process_Descriptor;
      Result  : Expect_Match;
      Timeout : constant Integer := 30_000;  --  30 seconds in ms

      --  Resolve absolute path to the harness server script.
      --  We are in ada/e2e/ so go up three levels to the repo root.
      Script : constant String :=
        Ada.Directories.Full_Name (Harness_Relative_Path);
   begin
      if Self.Running then
         return;
      end if;

      --  Spawn: npx tsx <script>
      --  The harness cwd must be the test/harness directory so that
      --  npx can find its node_modules.
      declare
         Harness_Dir : constant String :=
           Ada.Directories.Containing_Directory (Script);
      begin
         Non_Blocking_Spawn
           (Descriptor  => Fd,
            Command     => "npx",
            Args        => (new String'("tsx"),
                            new String'(Script)),
            Err_To_Out  => True);
      end;

      --  Wait for the "Listening: http://..." line.
      Expect (Fd, Result, "Listening: (http://[^ " & ASCII.LF & "]+)",
              Timeout => Timeout);

      if Result = Expect_Timeout then
         Close (Fd);
         raise Program_Error
           with "Proxy did not print Listening URL within timeout";
      end if;

      --  Extract URL from the matched group.
      declare
         Full_Match : constant String := Expect_Out_Match (Fd);
         Prefix     : constant String := "Listening: ";
         Idx        : constant Natural := Index (Full_Match, Prefix);
         Url_Start  : Natural;
      begin
         if Idx = 0 then
            Close (Fd);
            raise Program_Error
              with "Could not parse proxy URL from: " & Full_Match;
         end if;

         Url_Start := Idx + Prefix'Length;
         Self.Url := To_Unbounded_String
           (Trim (Full_Match (Url_Start .. Full_Match'Last),
                  Ada.Strings.Both));
      end;

      Self.Running := True;

      --  Stash the PID so we can clean up if needed.
      Self.Pid := Get_Pid (Fd);

      --  Set COPILOT_API_URL so the CLI process uses our proxy.
      Ada.Environment_Variables.Set
        ("COPILOT_API_URL", To_String (Self.Url));

      --  We intentionally do NOT close Fd here — the child process must
      --  keep running.  The descriptor will be cleaned up on Stop.

   exception
      when others =>
         Self.Running := False;
         raise;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (Self : in out Capi_Proxy; Skip_Cache : Boolean := False) is
      use GNAT.OS_Lib;

      Success  : Boolean;
      Stop_Url : constant String :=
        To_String (Self.Url) & "/stop" &
        (if Skip_Cache then "?skipWritingCache=true" else "");

      Args : constant Argument_List :=
        (new String'("-s"),
         new String'("-X"),
         new String'("POST"),
         new String'(Stop_Url));
   begin
      if not Self.Running then
         return;
      end if;

      --  POST /stop via curl (best-effort; ignore failures).
      declare
         Curl_Path : GNAT.OS_Lib.String_Access :=
           GNAT.OS_Lib.Locate_Exec_On_Path ("curl");
      begin
         if Curl_Path /= null then
            Spawn (Curl_Path.all, Args, Success);
            Free (Curl_Path);
         end if;
      end;

      Self.Running := False;
      Self.Url     := Null_Unbounded_String;
      Self.Pid     := -1;
   end Stop;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Self      : in out Capi_Proxy;
      File_Path : String;
      Work_Dir  : String)
   is
      use GNAT.OS_Lib;

      Success    : Boolean;
      Config_Url : constant String := To_String (Self.Url) & "/config";

      --  Build a minimal JSON body.
      Json_Body : constant String :=
        "{""filePath"":""" & File_Path &
        """,""workDir"":""" & Work_Dir & """}";

      Args : constant Argument_List :=
        (new String'("-s"),
         new String'("-X"),
         new String'("POST"),
         new String'("-H"),
         new String'("Content-Type: application/json"),
         new String'("-d"),
         new String'(Json_Body),
         new String'(Config_Url));
   begin
      if not Self.Running then
         raise Program_Error with "Proxy not started";
      end if;

      declare
         Curl_Path : GNAT.OS_Lib.String_Access :=
           GNAT.OS_Lib.Locate_Exec_On_Path ("curl");
      begin
         if Curl_Path = null then
            raise Program_Error with "curl not found on PATH";
         end if;

         Spawn (Curl_Path.all, Args, Success);
         Free (Curl_Path);

         if not Success then
            raise Program_Error with "curl POST /config failed";
         end if;
      end;
   end Configure;

   ---------------
   -- Proxy_Url --
   ---------------

   function Proxy_Url (Self : Capi_Proxy) return String is
   begin
      return To_String (Self.Url);
   end Proxy_Url;

end Test_Harness;
