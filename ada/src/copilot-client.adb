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
with Copilot.Wire;
with Copilot.Callbacks;

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
      --  Fall back to the npm bin name; rely on PATH.
      return "copilot";
   end Resolve_CLI_Path;

   function Image (N : Natural) return String is
      Img : constant String := Natural'Image (N);
   begin
      return Img (Img'First + 1 .. Img'Last);
   end Image;

   --  -------------------------------------------------------------------
   --  JSON building helpers (SDK protocol 4 options)
   --  -------------------------------------------------------------------

   --  Append "Key":Raw to an object body unless Raw is empty.
   procedure Add_Field
     (Fields : in out Unbounded_String;
      Key    : String;
      Raw    : String) is
   begin
      if Raw'Length = 0 then
         return;
      end if;
      if Length (Fields) > 0 then
         Append (Fields, ",");
      end if;
      Append (Fields, """" & Key & """:" & Raw);
   end Add_Field;

   function Text (Value : UString) return String is
     (if Length (Value) > 0 then Copilot.Wire.Quote (To_String (Value)) else "");

   function Count (N : Natural) return String is
     (if N > 0 then Image (N) else "");

   function Tri (Value : Tri_Bool) return String is
     (case Value is
         when Unset => "",
         when No    => "false",
         when Yes   => "true");

   function Object (Fields : Unbounded_String) return String is
     (if Length (Fields) > 0 then "{" & To_String (Fields) & "}" else "");

   function Client_Info_Json (Info : Client_Info_Config) return String is
      F : Unbounded_String;
   begin
      Add_Field (F, "editorName", Text (Info.Editor_Name));
      Add_Field (F, "editorVersion", Text (Info.Editor_Version));
      Add_Field (F, "extensionName", Text (Info.Extension_Name));
      Add_Field (F, "extensionVersion", Text (Info.Extension_Version));
      return Object (F);
   end Client_Info_Json;

   --  network.setProvider params: an empty object marks each callback.
   function Network_Provider_Params (N : Network_Options) return String is
      F : Unbounded_String;
   begin
      Add_Field (F, "scope", Text (N.Scope));
      if N.Resolve_Proxy /= null then
         Add_Field (F, "resolveProxy", "{}");
      end if;
      if N.Authenticate_Negotiate /= null then
         Add_Field (F, "authenticateNegotiate", "{}");
      end if;
      if N.Authenticate_Basic /= null then
         Add_Field (F, "authenticateBasic", "{}");
      end if;
      return "{" & To_String (F) & "}";
   end Network_Provider_Params;

   function Sandbox_Json (S : Sandbox_Config) return String is
      F : Unbounded_String;
   begin
      Add_Field (F, "enabled", (if S.Enabled then "true" else "false"));
      if S.Allow_Bypass then
         Add_Field (F, "allowBypass", "true");
      end if;
      Add_Field (F, "addCurrentWorkingDirectory",
                 Tri (S.Add_Current_Working_Directory));
      Add_Field (F, "userPolicy", To_String (S.User_Policy_Json));
      return Object (F);
   end Sandbox_Json;

   function Image_Generation_Json (C : Image_Generation_Config) return String
   is
      F : Unbounded_String;
   begin
      Add_Field (F, "enabled", Tri (C.Enabled));
      Add_Field (F, "watermark", Tri (C.Watermark));
      Add_Field (F, "saveToDisk", Tri (C.Save_To_Disk));
      return Object (F);
   end Image_Generation_Json;

   function Autopilot_Json (C : Autopilot_Config) return String is
      F : Unbounded_String;
   begin
      Add_Field (F, "continuationEnabled", Tri (C.Continuation_Enabled));
      Add_Field (F, "maxContinues", Count (C.Max_Continues));
      return Object (F);
   end Autopilot_Json;

   function Providers_Json (List : Provider_List) return String is
      R : Unbounded_String;
   begin
      for P of List loop
         declare
            F : Unbounded_String;
         begin
            Add_Field (F, "name", Text (P.Name));
            Add_Field (F, "type", Text (P.Provider_Type));
            Add_Field (F, "baseUrl", Text (P.Base_Url));
            Add_Field (F, "apiKey", Text (P.Api_Key));
            Add_Field (F, "apiKeyCommand", Text (P.Api_Key_Command));
            Add_Field (F, "bearerToken", Text (P.Bearer_Token));
            Add_Field (F, "wireApi", Text (P.Wire_Api));
            Add_Field (F, "transport", Text (P.Transport));
            Add_Field (F, "azure", To_String (P.Azure_Json));
            Add_Field (F, "headers", To_String (P.Headers_Json));
            Add_Field (F, "modelProvider", Text (P.Model_Provider));
            if Length (R) > 0 then
               Append (R, ",");
            end if;
            Append (R, "{" & To_String (F) & "}");
         end;
      end loop;
      return (if Length (R) > 0 then "[" & To_String (R) & "]" else "");
   end Providers_Json;

   function Models_Json (List : Provider_Model_List) return String is
      R : Unbounded_String;
   begin
      for M of List loop
         declare
            F : Unbounded_String;
         begin
            Add_Field (F, "provider", Text (M.Provider));
            Add_Field (F, "id", Text (M.Id));
            Add_Field (F, "modelId", Text (M.Model_Id));
            Add_Field (F, "wireModel", Text (M.Wire_Model));
            Add_Field (F, "name", Text (M.Name));
            Add_Field (F, "maxPromptTokens", Count (M.Max_Prompt_Tokens));
            Add_Field (F, "maxContextWindowTokens",
                       Count (M.Max_Context_Window_Tokens));
            Add_Field (F, "maxOutputTokens", Count (M.Max_Output_Tokens));
            Add_Field (F, "capabilities", To_String (M.Capabilities_Json));
            if Length (R) > 0 then
               Append (R, ",");
            end if;
            Append (R, "{" & To_String (F) & "}");
         end;
      end loop;
      return (if Length (R) > 0 then "[" & To_String (R) & "]" else "");
   end Models_Json;

   --  Build the flat (v1-style) session payload body, without braces.
   --  Copilot.Wire.Group_Session_Params converts it into the SDK protocol 4
   --  grouped payload right before session.create / session.resume.
   function Build_Flat_Params
     (Config : Session_Config;
      Reg_Id : String) return Unbounded_String
   is
      Params : Unbounded_String;

      procedure Add (Key : String; Raw : String) is
      begin
         Add_Field (Params, Key, Raw);
      end Add;

      procedure Add_Flag (Key : String; Flag : Boolean) is
      begin
         if Flag then
            Add (Key, "true");
         end if;
      end Add_Flag;

      Has_Hooks : constant Boolean :=
        Config.On_Post_Tool_Use /= null
        or else Config.On_Pre_Mcp_Tool_Call /= null
        or else Config.On_User_Prompt_Transformed /= null
        or else Config.On_Subagent_Start /= null
        or else Config.On_Subagent_Stop /= null;
   begin
      Add ("systemPrompt", Text (Config.System_Prompt));
      Add ("model", Text (Config.Model));
      Add ("streaming", (if Config.Streaming then "true" else "false"));
      Add ("workspacePath", Text (Config.Workspace_Path));
      Add ("gitHubToken", Text (Config.Github_Token));
      Add ("sessionIdleTimeoutSeconds", Count (Config.Session_Idle_Timeout));
      Add ("tools", To_String (Config.Tools_Json));
      Add ("availableTools", To_String (Config.Available_Tools_Json));
      Add ("mcpServers", To_String (Config.Mcp_Servers_Json));
      Add ("commands", To_String (Config.Commands_Json));
      Add ("instructionDirectories",
           To_String (Config.Instruction_Directories_Json));
      Add_Flag ("includeSubAgentStreamingEvents",
                Config.Include_Sub_Agent_Streaming);
      Add_Flag ("enableConfigDiscovery", Config.Enable_Config_Discovery);
      Add_Flag ("requestElicitation", Config.Elicitation_Handler_Set);

      --  --- Upstream-sync session config passthroughs (camelCase wire keys) ---
      Add_Flag ("enableCitations", Config.Enable_Citations);
      Add ("excludedBuiltinAgents",
           To_String (Config.Excluded_Builtin_Agents_Json));
      if Config.Session_Limits.Max_Ai_Credits > 0 then
         Add ("sessionLimits",
              "{""maxAiCredits"":"
              & Image (Config.Session_Limits.Max_Ai_Credits) & "}");
      end if;
      if Config.Memory.Enabled then
         Add ("memory", "{""enabled"":true}");
      end if;
      Add ("otlpProtocol", Text (Config.Otlp_Protocol));
      Add_Flag ("enableWebSocketResponses", Config.Enable_Web_Socket_Responses);
      Add ("expAssignments", To_String (Config.Exp_Assignments_Json));
      Add_Flag ("rewindEnabled", Config.Rewind_Enabled);
      Add ("additionalDirectories",
           To_String (Config.Additional_Directories_Json));
      Add ("disabledMcpServers", To_String (Config.Disabled_Mcp_Servers_Json));
      Add ("githubMcpToolConfig",
           To_String (Config.Github_Mcp_Tool_Config_Json));
      Add ("canvasProvider", To_String (Config.Canvas_Provider_Json));
      Add_Flag ("customAgentsLocalOnly", Config.Custom_Agents_Local_Only);
      Add ("decisionContext", To_String (Config.Decision_Context_Json));
      --  User-prompt-transformed hook: signal the runtime that a handler exists.
      Add_Flag ("userPromptTransformed",
                Config.On_User_Prompt_Transformed /= null);
      Add ("builtinPluginDirectories",
           To_String (Config.Builtin_Plugin_Directories_Json));
      Add ("argsSchema", To_String (Config.Args_Schema_Json));
      Add ("reasoningEffort", Text (Config.Reasoning_Effort));
      Add ("toolSearch", To_String (Config.Tool_Search_Json));
      Add_Flag ("inProcess", Config.In_Process);
      Add_Flag ("experimentalMode", Config.Experimental_Mode);
      Add_Flag ("contentExclusion", Config.Content_Exclusion);
      --  MCP OAuth host token handler: signal the runtime that a handler exists.
      Add_Flag ("mcpAuthHandler", Config.On_Mcp_Auth_Request /= null);

      --  --- SDK protocol 4 / v3.0.0 options ---
      Add ("workingDirectory", Text (Config.Working_Directory));
      Add ("askUserVariant", Text (Config.Ask_User_Variant));
      Add ("gitHubTokenProviderRegistrationId",
           (if Reg_Id'Length > 0 then Copilot.Wire.Quote (Reg_Id) else ""));
      if Config.Sandbox.Enabled then
         Add ("sandbox", Sandbox_Json (Config.Sandbox));
      end if;
      Add ("featureFlags", To_String (Config.Feature_Flags_Json));
      Add ("allowedModels", To_String (Config.Allowed_Models_Json));
      Add ("authClientIdMetadataUrl", Text (Config.Auth_Client_Id_Metadata_Url));
      if Length (Config.Model_Capabilities_Json) > 0 then
         Add ("modelCapabilities", To_String (Config.Model_Capabilities_Json));
      elsif Config.Max_Output_Tokens > 0 then
         --  The capabilities limits object uses snake_case wire keys.
         Add ("modelCapabilities",
              "{""limits"":{""max_output_tokens"":"
              & Image (Config.Max_Output_Tokens) & "}}");
      end if;
      Add ("agentDirectories", To_String (Config.Agent_Directories_Json));
      Add ("hookDirectories", To_String (Config.Hook_Directories_Json));
      Add ("trustWorkingDirectory", Text (Config.Trust_Working_Directory));
      Add ("imageGeneration", Image_Generation_Json (Config.Image_Generation));
      Add ("autopilot", Autopilot_Json (Config.Autopilot));
      Add_Flag ("hooks", Has_Hooks);
      Add ("provider", To_String (Config.Provider_Json));
      Add ("providers", Providers_Json (Config.Providers));
      Add ("models", Models_Json (Config.Models));
      Add ("defaultProvider", Text (Config.Default_Provider));
      if Config.Capi_Enabled /= Unset then
         Add ("capi", "{""enabled"":" & Tri (Config.Capi_Enabled) & "}");
      end if;
      Add_Flag ("requestShellAuthentication",
                Config.On_Shell_Authentication /= null);
      Add_Flag ("hasMemoryStore", Config.On_Store_Memory /= null);
      Add_Flag ("requestUserInput",
                Config.On_User_Input_Request /= null
                  or else Config.On_User_Input_Image_Request /= null);
      return Params;
   end Build_Flat_Params;

   function Is_Empty_Mode (Self : Copilot_Client) return Boolean is
     (To_String (Self.Options.Mode) = Client_Mode_Empty);

   procedure Validate_Session_Options
     (Self   : Copilot_Client;
      Config : Session_Config) is
   begin
      if Is_Empty_Mode (Self)
        and then Length (Config.Available_Tools_Json) = 0
      then
         raise Copilot_Error
           with "Client mode ""empty"" requires Available_Tools_Json on "
              & "every session create and resume.";
      end if;
      if Config.GitHub_Token_Provider /= null
        and then Length (Config.Github_Token) > 0
      then
         raise Copilot_Error
           with "Github_Token and GitHub_Token_Provider are mutually "
              & "exclusive.";
      end if;
   end Validate_Session_Options;

   procedure Register_Session_Callbacks
     (Session_Id : String;
      Config     : Session_Config) is
   begin
      Copilot.Callbacks.Register_Session
        (Session_Id        => Session_Id,
         Shell_Auth        => Config.On_Shell_Authentication,
         Store_Memory      => Config.On_Store_Memory,
         On_Subagent_Start => Config.On_Subagent_Start,
         On_Subagent_Stop  => Config.On_Subagent_Stop,
         User_Input        => Config.On_User_Input_Request,
         User_Input_Image  => Config.On_User_Input_Image_Request);
   end Register_Session_Callbacks;

   --  Decode error.data.policyHelperFailure from a JSON-RPC error object.
   function Parse_Policy_Helper_Failure
     (Error_Json : String) return Policy_Helper_Failure
   is
      F    : constant String :=
        Copilot.Wire.Member
          (Copilot.Wire.Member (Error_Json, "data"), "policyHelperFailure");
      Help : constant String := Copilot.Wire.Member (F, "helpLink");

      function S (Key : String) return UString is
        (To_Unbounded_String (Copilot.Wire.String_Member (F, Key)));
   begin
      if F'Length = 0 or else F (F'First) /= '{' then
         return (others => <>);
      end if;
      return (Present                  => True,
              Category                 => S ("category"),
              Failure_Mode             => S ("failureMode"),
              Message                  => S ("message"),
              Previous_Policy_Retained =>
                Copilot.Wire.Bool_Member (F, "previousPolicyRetained"),
              Reason                   => S ("reason"),
              Severity                 => S ("severity"),
              Authority                => S ("authority"),
              Code                     => S ("code"),
              Details_Json             =>
                To_Unbounded_String (Copilot.Wire.Member (F, "details")),
              Diagnostic               => S ("diagnostic"),
              Help_Link_Url            =>
                To_Unbounded_String (Copilot.Wire.String_Member (Help, "url")),
              Help_Link_Label          =>
                To_Unbounded_String
                  (Copilot.Wire.String_Member (Help, "label")),
              Remediation              => S ("remediation"));
   end Parse_Policy_Helper_Failure;

   --  Send session.create / session.resume; on failure remember the Policy
   --  Helper failure (if any) and release the token provider registration.
   function Send_Session_Request
     (Self   : in out Copilot_Client;
      Method : String;
      Params : String;
      Reg_Id : String) return String is
   begin
      Self.Last_Failure := (others => <>);
      return Send_Request (Self.Conn, Method, Params);
   exception
      when Protocol_Error =>
         Self.Last_Failure :=
           Parse_Policy_Helper_Failure (Last_Error (Self.Conn));
         if Reg_Id'Length > 0 then
            Copilot.Callbacks.Unregister_Token_Provider (Reg_Id);
         end if;
         raise;
   end Send_Session_Request;

   --  connect (with clientInfo), falling back to ping for runtimes without
   --  connect.  Returns the server protocol version, or -1 when the server
   --  does not report one.
   function Negotiate_Protocol (Self : in out Copilot_Client) return Integer
   is
      Info   : constant String := Client_Info_Json (Self.Options.Client_Info);
      Params : constant String :=
        (if Info'Length > 0 then "{""clientInfo"":" & Info & "}" else "{}");

      function Version_Of (Resp : String) return Integer is
         Raw : constant String :=
           Copilot.Wire.Member
             (Copilot.Wire.Member (Resp, "result"), "protocolVersion");
      begin
         if Raw'Length = 0 then
            return -1;
         end if;
         return Integer'Value (Raw);
      exception
         when Constraint_Error =>
            return -1;
      end Version_Of;
   begin
      return Version_Of (Send_Request (Self.Conn, "connect", Params));
   exception
      when Protocol_Error =>
         declare
            Err : constant String := Last_Error (Self.Conn);
         begin
            if Copilot.Wire.Member (Err, "code") = "-32601"
              or else Copilot.Wire.String_Member (Err, "message")
                      = "Unhandled method connect"
            then
               return Version_Of
                 (Send_Request (Self.Conn, "ping", "{""message"":""ping""}"));
            end if;
            raise;
         end;
   end Negotiate_Protocol;

   --  -------------------------------------------------------------------
   --  Public API
   --  -------------------------------------------------------------------

   function Create
     (Options : Client_Options := Default_Options) return Copilot_Client
   is
   begin
      return (Options      => Options,
              Conn         => <>,
              State        => Disconnected,
              Process_Id   => Invalid_Pid,
              Process      => <>,
              Last_Failure => <>);
   end Create;

   procedure Start (Self : in out Copilot_Client) is
      CLI_Bin : constant String := Resolve_CLI_Path (Self.Options);
      Mode    : constant String := To_String (Self.Options.Mode);
   begin
      if Self.State = Connected then
         return;  --  already started
      end if;

      if Mode /= ""
        and then Mode /= Client_Mode_Coding_Agent
        and then Mode /= Client_Mode_Copilot_Cli
        and then Mode /= Client_Mode_Empty
      then
         raise Copilot_Error with "Unknown client mode: " & Mode;
      end if;

      Self.State := Connecting;

      --  If a remote CLI URL is specified, we would connect via TCP instead.
      if Length (Self.Options.CLI_Url) > 0 then
         Self.State := Disconnected;
         raise Connection_Error
           with "TCP/remote CLI_Url connections are not yet implemented "
              & "in the Ada SDK.  Use stdio mode.";
      end if;

      --  Spawn the CLI in server mode.  A ".js" entry point (npm-loader.js)
      --  runs through node; any other path (native binary or the npm .bin
      --  shim) runs directly.
      declare
         Is_Script   : constant Boolean :=
           CLI_Bin'Length > 3
           and then CLI_Bin (CLI_Bin'Last - 2 .. CLI_Bin'Last) = ".js";
         Script_Arg  : Argument_List := (1 => new String'(CLI_Bin));
         Server_Args : Argument_List :=
           (new String'("--headless"),
            new String'("--no-auto-update"),
            new String'("--stdio"));
      begin
         begin
            if Is_Script then
               GNAT.Expect.Non_Blocking_Spawn
                 (Descriptor => Self.Process,
                  Command    => "node",
                  Args       => Script_Arg & Server_Args,
                  Err_To_Out => False);
            else
               GNAT.Expect.Non_Blocking_Spawn
                 (Descriptor => Self.Process,
                  Command    => CLI_Bin,
                  Args       => Server_Args,
                  Err_To_Out => False);
            end if;
         exception
            when others =>
               Free (Script_Arg (1));
               for Arg of Server_Args loop
                  Free (Arg);
               end loop;
               Self.State := Disconnected;
               raise Connection_Error
                 with "Failed to spawn CLI process: " & CLI_Bin;
         end;
         Free (Script_Arg (1));
         for Arg of Server_Args loop
            Free (Arg);
         end loop;
      end;

      --  We read from the child's stdout and write to the child's stdin.
      Self.Conn := Copilot.JSON_RPC.Create
        (Read_FD  => GNAT.Expect.Get_Output_Fd (Self.Process),
         Write_FD => GNAT.Expect.Get_Input_Fd (Self.Process));
      Set_Server_Request_Handler (Self.Conn, Copilot.Callbacks.Dispatch'Access);

      Self.State := Connected;

      --  Validate the SDK protocol version (exactly 4).
      declare
         Server_Version : constant Integer := Negotiate_Protocol (Self);
      begin
         if Server_Version < 0 then
            Stop (Self);
            raise Protocol_Error
              with "SDK protocol version mismatch: SDK supports version"
                 & Natural'Image (Copilot.Version.Protocol_Version)
                 & ", but server does not report a protocol version. "
                 & "Please update your SDK or server to ensure compatibility.";
         elsif Server_Version < Copilot.Version.Min_Protocol
           or else Server_Version > Copilot.Version.Max_Protocol
         then
            Stop (Self);
            raise Protocol_Error
              with "SDK protocol version mismatch: SDK supports version"
                 & Natural'Image (Copilot.Version.Protocol_Version)
                 & ", but server reports version"
                 & Integer'Image (Server_Version)
                 & ". Please update your SDK or server to ensure "
                 & "compatibility.";
         end if;
      end;

      --  Register network callbacks (network.setProvider).
      if Copilot.Callbacks.Has_Network_Callbacks (Self.Options.Network) then
         Copilot.Callbacks.Set_Network (Self.Options.Network);
         declare
            Ignore : constant String :=
              Send_Request (Self.Conn, "network.setProvider",
                            Network_Provider_Params (Self.Options.Network));
         begin
            null;
         end;
      end if;

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
      Reg_Id : constant String :=
        (if Config.GitHub_Token_Provider /= null
         then Copilot.Wire.New_Registration_Id else "");
   begin
      if Self.State /= Connected then
         if Self.Options.Auto_Start then
            Start (Self);
         else
            raise Connection_Error with "Client is not connected";
         end if;
      end if;

      Validate_Session_Options (Self, Config);
      Copilot.Callbacks.Register_Token_Provider
        (Reg_Id, Config.GitHub_Token_Provider);

      --  Build the flat payload, then convert it to the SDK protocol 4
      --  grouped shape (Copilot.Wire.Group_Session_Params).
      declare
         Flat   : constant String :=
           "{" & To_String (Build_Flat_Params (Config, Reg_Id)) & "}";
         Params : constant String :=
           Copilot.Wire.Group_Session_Params
             (Flat, Is_Resume => False, Empty_Mode => Is_Empty_Mode (Self));
         Resp   : constant String :=
           Send_Session_Request (Self, "session.create", Params, Reg_Id);
         Result : constant String := Copilot.Wire.Member (Resp, "result");
         Sid    : constant String :=
           Copilot.Wire.String_Member (Result, "sessionId");
         Ws     : constant String :=
           Copilot.Wire.String_Member (Result, "workspacePath");
      begin
         Register_Session_Callbacks (Sid, Config);
         return Create_Internal
           (Self.Conn'Access, Sid, Ws, Token_Reg_Id => Reg_Id);
      end;
   end Create_Session;

   function Resume_Session
     (Self   : in out Copilot_Client;
      Config : Resume_Session_Config) return Copilot_Session
   is
      Options : Session_Config := Config.Options;
      Reg_Id  : constant String :=
        (if Config.Options.GitHub_Token_Provider /= null
         then Copilot.Wire.New_Registration_Id else "");
      Flat    : Unbounded_String;
   begin
      if Self.State /= Connected then
         if Self.Options.Auto_Start then
            Start (Self);
         else
            raise Connection_Error with "Client is not connected";
         end if;
      end if;

      if Length (Config.Model) > 0 then
         Options.Model := Config.Model;
      end if;
      if Length (Config.Instruction_Directories_Json) > 0 then
         Options.Instruction_Directories_Json :=
           Config.Instruction_Directories_Json;
      end if;

      Validate_Session_Options (Self, Options);
      Copilot.Callbacks.Register_Token_Provider
        (Reg_Id, Options.GitHub_Token_Provider);

      Flat := To_Unbounded_String
        ("""sessionId"":" & Copilot.Wire.Quote (To_String (Config.Session_Id)));
      Add_Field (Flat, "disableResume",
                 (if Config.Disable_Resume then "true" else ""));
      Add_Field (Flat, "continuePendingWork",
                 (if Config.Continue_Pending_Work then "true" else ""));
      Add_Field (Flat, "allowTranscriptRecovery",
                 (if Config.Allow_Transcript_Recovery then "true" else ""));
      if Options.Clear_Default_Provider
        and then Length (Options.Default_Provider) = 0
      then
         --  Explicit null clears the default provider on resume.
         Add_Field (Flat, "defaultProvider", "null");
      end if;
      declare
         Shared : constant Unbounded_String :=
           Build_Flat_Params (Options, Reg_Id);
      begin
         if Length (Shared) > 0 then
            Append (Flat, ",");
            Append (Flat, Shared);
         end if;
      end;

      declare
         Params    : constant String :=
           Copilot.Wire.Group_Session_Params
             ("{" & To_String (Flat) & "}",
              Is_Resume  => True,
              Empty_Mode => Is_Empty_Mode (Self));
         Resp      : constant String :=
           Send_Session_Request (Self, "session.resume", Params, Reg_Id);
         Result    : constant String := Copilot.Wire.Member (Resp, "result");
         Sid       : constant String :=
           Copilot.Wire.String_Member (Result, "sessionId");
         Ws        : constant String :=
           Copilot.Wire.String_Member (Result, "workspacePath");
         Final_Sid : constant String :=
           (if Sid'Length > 0 then Sid else To_String (Config.Session_Id));
      begin
         Register_Session_Callbacks (Final_Sid, Options);
         return Create_Internal
           (Self.Conn'Access, Final_Sid, Ws, Token_Reg_Id => Reg_Id);
      end;
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
     (Self       : in out Copilot_Client;
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
      Result : constant String := Copilot.Wire.Member (Resp, "result");
   begin
      --  SDK protocol 4 reports failures as JSON-RPC errors, which
      --  Send_Request raises as Protocol_Error.  Older runtimes answer
      --  {"success":false,"error":...}.
      if Copilot.Wire.Member (Result, "success") = "false" then
         declare
            Err : constant String := Copilot.Wire.String_Member (Result, "error");
         begin
            raise Protocol_Error
              with "Failed to set foreground session: "
                 & (if Err'Length > 0 then Err else "Unknown error");
         end;
      end if;
   end Set_Foreground_Session_Id;

   procedure Clear_Managed_Settings_Cache (Self : in out Copilot_Client) is
      Ignore : constant String :=
        Send_Request (Self.Conn, "managedSettings.clearCache", "{}");
   begin
      null;
   end Clear_Managed_Settings_Cache;

   --  ,"configDir":... for the MCP secret store requests ("" = omitted).
   function Config_Dir_Field (Config_Dir : String) return String is
     (if Config_Dir'Length > 0
      then ",""configDir"":" & Copilot.Wire.Quote (Config_Dir)
      else "");

   --  Decode one PermissionsExtensionApproval object.
   function Parse_Extension_Approval
     (Json : String) return Permissions_Extension_Approval
   is
      function S (Key : String) return UString is
        (To_Unbounded_String (Copilot.Wire.String_Member (Json, Key)));
   begin
      return (Extension_Name     => S ("extensionName"),
              Plugin_Name        => S ("pluginName"),
              Capabilities_Json  =>
                To_Unbounded_String (Copilot.Wire.Member (Json, "capabilities")),
              Granted_At         => S ("grantedAt"),
              Installation_Key   => S ("installationKey"),
              Installation_Label => S ("installationLabel"));
   end Parse_Extension_Approval;

   function Mcp_Secrets_Save
     (Self       : in out Copilot_Client;
      Secret_Id  : String;
      Value      : String;
      Config_Dir : String := "") return Mcp_Secrets_Save_Result
   is
      Resp : constant String :=
        Send_Request
          (Self.Conn, "mcp.secrets.save",
           "{""secretId"":" & Copilot.Wire.Quote (Secret_Id)
           & ",""value"":" & Copilot.Wire.Quote (Value)
           & Config_Dir_Field (Config_Dir) & "}");
   begin
      return (Used_Keychain =>
                Copilot.Wire.Bool_Member
                  (Copilot.Wire.Member (Resp, "result"), "usedKeychain"));
   end Mcp_Secrets_Save;

   procedure Mcp_Secrets_Delete
     (Self       : in out Copilot_Client;
      Secret_Id  : String;
      Config_Dir : String := "")
   is
      Ignore : constant String :=
        Send_Request
          (Self.Conn, "mcp.secrets.delete",
           "{""secretId"":" & Copilot.Wire.Quote (Secret_Id)
           & Config_Dir_Field (Config_Dir) & "}");
   begin
      null;
   end Mcp_Secrets_Delete;

   procedure Mcp_Secrets_Delete_Server
     (Self        : in out Copilot_Client;
      Server_Name : String;
      Config_Dir  : String := "")
   is
      Ignore : constant String :=
        Send_Request
          (Self.Conn, "mcp.secrets.deleteServer",
           "{""serverName"":" & Copilot.Wire.Quote (Server_Name)
           & Config_Dir_Field (Config_Dir) & "}");
   begin
      null;
   end Mcp_Secrets_Delete_Server;

   function Extension_Approvals_Grant
     (Self       : in out Copilot_Client;
      Session_Id : String;
      Request_Id : String) return Permissions_Extension_Approvals_Grant_Result
   is
      Resp     : constant String :=
        Send_Request
          (Self.Conn, "permissions.extensionApprovals.grant",
           "{""sessionId"":" & Copilot.Wire.Quote (Session_Id)
           & ",""requestId"":" & Copilot.Wire.Quote (Request_Id) & "}");
      Result   : constant String := Copilot.Wire.Member (Resp, "result");
      Approval : constant String := Copilot.Wire.Member (Result, "approval");
   begin
      return (Granted  => Copilot.Wire.Bool_Member (Result, "granted"),
              Approval =>
                (if Approval'Length > 0 and then Approval (Approval'First) = '{'
                 then Parse_Extension_Approval (Approval)
                 else (others => <>)));
   end Extension_Approvals_Grant;

   function Extension_Approvals_List
     (Self       : in out Copilot_Client;
      Session_Id : String := "") return Permissions_Extension_Approval_List
   is
      Resp      : constant String :=
        Send_Request
          (Self.Conn, "permissions.extensionApprovals.list",
           (if Session_Id'Length > 0
            then "{""sessionId"":" & Copilot.Wire.Quote (Session_Id) & "}"
            else "{}"));
      Approvals : Permissions_Extension_Approval_List;

      procedure Add (Value : String) is
      begin
         if Value'Length > 0 and then Value (Value'First) = '{' then
            Approvals.Append (Parse_Extension_Approval (Value));
         end if;
      end Add;
   begin
      Copilot.Wire.For_Each_Element
        (Copilot.Wire.Member (Copilot.Wire.Member (Resp, "result"), "approvals"),
         Add'Access);
      return Approvals;
   end Extension_Approvals_List;

   function Extension_Approvals_Revoke
     (Self             : in out Copilot_Client;
      Extension_Name   : String;
      Installation_Key : String;
      Session_Id       : String := "")
      return Permissions_Extension_Approvals_Revoke_Result
   is
      Resp : constant String :=
        Send_Request
          (Self.Conn, "permissions.extensionApprovals.revoke",
           "{""extensionName"":" & Copilot.Wire.Quote (Extension_Name)
           & ",""installationKey"":" & Copilot.Wire.Quote (Installation_Key)
           & (if Session_Id'Length > 0
              then ",""sessionId"":" & Copilot.Wire.Quote (Session_Id)
              else "")
           & "}");
   begin
      return (Revoked =>
                Copilot.Wire.Bool_Member
                  (Copilot.Wire.Member (Resp, "result"), "revoked"));
   end Extension_Approvals_Revoke;

   function Last_Policy_Helper_Failure
     (Self : Copilot_Client) return Policy_Helper_Failure is
   begin
      return Self.Last_Failure;
   end Last_Policy_Helper_Failure;

end Copilot.Client;
