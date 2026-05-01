--  ---------------------------------------------------------------------------
--  Copyright (c) Microsoft Corporation. All rights reserved.
--  ---------------------------------------------------------------------------

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Environment_Variables;
with GNAT.OS_Lib;            use GNAT.OS_Lib;

with Copilot.Types;          use Copilot.Types;
with Copilot.JSON_RPC;       use Copilot.JSON_RPC;
with Copilot.Session;        use Copilot.Session;
with Copilot.Version;

package body Copilot.Client is

   --  -------------------------------------------------------------------
   --  Internal helpers
   --  -------------------------------------------------------------------

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

   --  Simple extraction of a JSON string value by key.
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

   --  Resolve the path to the Copilot CLI binary.
   function Resolve_CLI_Path (Opts : Client_Options) return String is
   begin
      if Length (Opts.CLI_Path) > 0 then
         return To_String (Opts.CLI_Path);
      end if;
      --  Check COPILOT_CLI_PATH environment variable.
      if Ada.Environment_Variables.Exists ("COPILOT_CLI_PATH") then
         return Ada.Environment_Variables.Value ("COPILOT_CLI_PATH");
      end if;
      --  Fall back to bare name; rely on PATH.
      return "copilot-cli";
   end Resolve_CLI_Path;

   --  -------------------------------------------------------------------
   --  Public API
   --  -------------------------------------------------------------------

   function Create
     (Options : Client_Options := Default_Options) return Copilot_Client
   is
   begin
      return (Options    => Options,
              Conn       => <>,
              State      => Disconnected,
              Process_Id => Invalid_Pid);
   end Create;

   procedure Start (Self : in out Copilot_Client) is
      CLI_Bin : constant String := Resolve_CLI_Path (Self.Options);

      --  Pipe file descriptors for stdin/stdout of the child process.
      Read_From_Child  : File_Descriptor;
      Write_To_Parent  : File_Descriptor;
      Read_From_Parent : File_Descriptor;
      Write_To_Child   : File_Descriptor;
   begin
      if Self.State = Connected then
         return;  --  already started
      end if;

      Self.State := Connecting;

      --  If a remote CLI URL is specified, we would connect via TCP instead.
      if Length (Self.Options.CLI_Url) > 0 then
         raise Connection_Error
           with "TCP/remote CLI_Url connections are not yet implemented "
              & "in the Ada SDK.  Use stdio mode.";
      end if;

      --  Create two pipes: parent-to-child and child-to-parent.
      Create_Pipe (Read_From_Parent, Write_To_Child);
      Create_Pipe (Read_From_Child, Write_To_Parent);

      --  Spawn the CLI in server mode.
      declare
         Args : Argument_List_Access :=
           Argument_String_To_List ("--stdio");
      begin
         Self.Process_Id := Non_Blocking_Spawn
           (Program_Name => CLI_Bin,
            Args         => Args.all,
            Input_File   => Read_From_Parent,
            Output_File  => Write_To_Parent,
            Err_To_Out   => True);
         Free (Args);
      exception
         when others =>
            Free (Args);
            Self.State := Disconnected;
            raise Connection_Error
              with "Failed to spawn CLI process: " & CLI_Bin;
      end;

      if Self.Process_Id = Invalid_Pid then
         Self.State := Disconnected;
         raise Connection_Error
           with "CLI process returned invalid PID";
      end if;

      --  We read from the child's stdout and write to the child's stdin.
      Self.Conn := Copilot.JSON_RPC.Create
        (Read_FD  => Read_From_Child,
         Write_FD => Write_To_Child);

      Self.State := Connected;

      --  Validate protocol version.
      declare
         Status : constant Get_Status_Response := Get_Status (Self);
      begin
         if Status.Protocol_Version < Copilot.Version.Min_Protocol then
            Stop (Self);
            raise Protocol_Error
              with "CLI protocol version"
                 & Natural'Image (Status.Protocol_Version)
                 & " is below minimum"
                 & Natural'Image (Copilot.Version.Min_Protocol);
         end if;
      end;

      --  Set up session filesystem provider if configured.
      if Length (Self.Options.Session_Fs_Initial_Cwd) > 0 then
         declare
            Fs_Params : constant String :=
              "{""initialCwd"":""" & (-Self.Options.Session_Fs_Initial_Cwd) & """," &
              """sessionStatePath"":""" & (-Self.Options.Session_Fs_State_Path) & """," &
              """conventions"":""" & (-Self.Options.Session_Fs_Conventions) & """}";
            Fs_Response : constant String :=
              Send_Request (Self.Conn, "sessionFs.setProvider", Fs_Params);
         begin
            null;  --  Response is discarded; provider is now active.
         end;
      end if;
   end Start;

   procedure Stop (Self : in out Copilot_Client) is
   begin
      if Self.State /= Connected then
         return;
      end if;
      Self.State := Stopping;
      begin
         Send_Notification (Self.Conn, "shutdown");
      exception
         when others => null;
      end;
      Close (Self.Conn);
      if Self.Process_Id /= Invalid_Pid then
         --  Give the process a moment, then it will exit when its stdin closes.
         null;
      end if;
      Self.State := Stopped;
   end Stop;

   procedure Force_Stop (Self : in out Copilot_Client) is
   begin
      Close (Self.Conn);
      Self.State := Stopped;
   end Force_Stop;

   function Create_Session
     (Self   : in out Copilot_Client;
      Config : Session_Config := Default_Session_Config)
      return Copilot_Session
   is
      Params : Unbounded_String := To_Unbounded_String ("{");
   begin
      if Self.State /= Connected then
         if Self.Options.Auto_Start then
            Start (Self);
         else
            raise Connection_Error with "Client is not connected";
         end if;
      end if;

      --  Build JSON params.
      if Length (Config.System_Prompt) > 0 then
         Append (Params, """systemPrompt"":"""
                       & Escape (To_String (Config.System_Prompt)) & """,");
      end if;
      if Length (Config.Model) > 0 then
         Append (Params, """model"":"""
                       & Escape (To_String (Config.Model)) & """,");
      end if;
      if Config.Streaming then
         Append (Params, """streaming"":true,");
      else
         Append (Params, """streaming"":false,");
      end if;
      if Length (Config.Workspace_Path) > 0 then
         Append (Params, """workspacePath"":"""
                       & Escape (To_String (Config.Workspace_Path)) & """,");
      end if;
      if Length (Config.Github_Token) > 0 then
         Append (Params, """githubToken"":"""
                       & Escape (To_String (Config.Github_Token)) & """,");
      end if;
      if Config.Session_Idle_Timeout > 0 then
         declare
            Img : constant String :=
              Natural'Image (Config.Session_Idle_Timeout);
         begin
            Append (Params, """sessionIdleTimeoutSeconds"":"
                          & Img (Img'First + 1 .. Img'Last) & ",");
         end;
      end if;

      --  Strip trailing comma (if any) and close brace.
      --  Session config supports: authToken / auth_token, excludedTools / excluded_tools,
      --  requestHeaders / request_headers, modelCapabilities / model_capabilities,
      --  configDiscovery / config_discovery, subAgentStreaming / sub_agent_streaming,
      --  imageGeneration / image_generation / responseFormat / response_format
      declare
         S : constant String := To_String (Params);
      begin
         if S (S'Last) = ',' then
            Params := To_Unbounded_String (S (S'First .. S'Last - 1));
         end if;
      end;
      Append (Params, "}");

      declare
         Resp : constant String :=
           Send_Request (Self.Conn, "session.create", To_String (Params));
         Sid  : constant String := Json_Get (Resp, "sessionId");
         Ws   : constant String := Json_Get (Resp, "workspacePath");
      begin
         return Create_Internal (Self.Conn'Access, Sid, Ws);
      end;
   end Create_Session;

   function Resume_Session
     (Self   : in out Copilot_Client;
      Config : Resume_Session_Config) return Copilot_Session
   is
      Params : constant String :=
        "{""sessionId"":""" & Escape (To_String (Config.Session_Id)) & """}";
      Resp   : constant String :=
        Send_Request (Self.Conn, "session.resume", Params);
      Sid    : constant String := Json_Get (Resp, "sessionId");
      Ws     : constant String := Json_Get (Resp, "workspacePath");
   begin
      return Create_Internal (Self.Conn'Access, Sid, Ws);
   end Resume_Session;

   procedure Delete_Session
     (Self       : in out Copilot_Client;
      Session_Id : String)
   is
      Params : constant String :=
        "{""sessionId"":""" & Escape (Session_Id) & """}";
      Ignore : constant String :=
        Send_Request (Self.Conn, "session.delete", Params);
   begin
      null;
   end Delete_Session;

   function Get_Session_Metadata
     (Self       : Copilot_Client;
      Session_Id : String) return Session_Metadata
   is
      Params : constant String :=
        "{""sessionId"":""" & Escape (Session_Id) & """}";
      Resp   : constant String :=
        Send_Request (Self.Conn, "session.getMetadata", Params);
      Meta   : Session_Metadata;
   begin
      Meta.Session_Id := To_Unbounded_String (Session_Id);
      return Meta;
   end Get_Session_Metadata;

   function Ping
     (Self    : in out Copilot_Client;
      Message : String := "ping") return String
   is
      Params : constant String :=
        "{""message"":""" & Escape (Message) & """}";
      Resp   : constant String :=
        Send_Request (Self.Conn, "ping", Params);
   begin
      return Json_Get (Resp, "message");
   end Ping;

   function Get_Status
     (Self : in out Copilot_Client) return Get_Status_Response
   is
      Resp : constant String := Send_Request (Self.Conn, "status.get");
      Ver  : constant String := Json_Get (Resp, "version");
      PV   : constant String := Json_Get (Resp, "protocolVersion");
   begin
      return (Version          => To_Unbounded_String (Ver),
              Protocol_Version =>
                (if PV'Length > 0 then Natural'Value (PV) else 0));
   end Get_Status;

   function Get_Auth_Status
     (Self : in out Copilot_Client) return Auth_Status_Response
   is
      Resp : constant String := Send_Request (Self.Conn, "auth.getStatus");
      User : constant String := Json_Get (Resp, "user");
   begin
      return (Authenticated => User'Length > 0,
              User          => To_Unbounded_String (User));
   end Get_Auth_Status;

   function List_Models
     (Self : in out Copilot_Client) return Model_List
   is
      Resp   : constant String :=
        Send_Request (Self.Conn, "models.list", "{}");
      Models : Model_List;
   begin
      return Models;
   end List_Models;

   function List_Sessions
     (Self : in out Copilot_Client) return String
   is
      Resp : constant String :=
        Send_Request (Self.Conn, "session.list", "{}");
   begin
      return Resp;
   end List_Sessions;

   function Get_Last_Session_Id
     (Self : in out Copilot_Client) return String
   is
      Resp : constant String :=
        Send_Request (Self.Conn, "session.getLastId", "{}");
      Sid  : constant String := Json_Get (Resp, "sessionId");
   begin
      return Sid;
   end Get_Last_Session_Id;

   function Get_State
     (Self : Copilot_Client) return Connection_State
   is
   begin
      return Self.State;
   end Get_State;

   function Is_Connected (Self : Copilot_Client) return Boolean is
   begin
      return Self.State = Connected;
   end Is_Connected;

   function Get_Foreground_Session_Id
     (Self : in out Copilot_Client) return String
   is
      Resp : constant String :=
        Send_Request (Self.Conn, "session.getForeground", "{}");
      Sid  : constant String := Json_Get (Resp, "sessionId");
   begin
      return Sid;
   end Get_Foreground_Session_Id;

   procedure Set_Foreground_Session_Id
     (Self       : in out Copilot_Client;
      Session_Id : String)
   is
      Params : constant String :=
        "{""sessionId"":""" & Escape (Session_Id) & """}";
      Resp   : constant String :=
        Send_Request (Self.Conn, "session.setForeground", Params);
      Succ   : constant String := Json_Get (Resp, "success");
   begin
      if Succ /= "true" then
         declare
            Err : constant String := Json_Get (Resp, "error");
         begin
            raise Protocol_Error
              with "Failed to set foreground session: "
                 & (if Err'Length > 0 then Err else "Unknown error");
         end;
      end if;
   end Set_Foreground_Session_Id;

end Copilot.Client;
