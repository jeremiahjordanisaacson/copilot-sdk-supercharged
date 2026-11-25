--  ---------------------------------------------------------------------------
--  test_client.adb -- Unit tests for the Copilot Ada SDK.
--
--  Run:
--    gnatmake test_client.adb -I../src -aI../src
--    ./test_client
--
--  The tests exercise public API surface without requiring a running CLI
--  (they use default constructors and verify initial state).
--  ---------------------------------------------------------------------------

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with Copilot.Types;          use Copilot.Types;
with Copilot.Client;         use Copilot.Client;
with Copilot.Session;        use Copilot.Session;
with Copilot.Tools;          use Copilot.Tools;
with Copilot.Version;
with Copilot.JSON_RPC;       use Copilot.JSON_RPC;
with Copilot.Wire;

procedure Test_Client is

   Pass_Count : Natural := 0;
   Fail_Count : Natural := 0;

   procedure Assert (Cond : Boolean; Label : String) is
   begin
      if Cond then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  PASS: " & Label);
      else
         Fail_Count := Fail_Count + 1;
         Put_Line ("  FAIL: " & Label);
      end if;
   end Assert;

   --  ---------------------------------------------------------------
   --  Test: default client options
   --  ---------------------------------------------------------------
   procedure Test_Default_Options is
      Opts : constant Client_Options := Default_Options;
   begin
      Put_Line ("[Test_Default_Options]");
      Assert (Opts.Use_Stdio, "Use_Stdio defaults to True");
      Assert (Opts.Auto_Start, "Auto_Start defaults to True");
      Assert (Opts.Auto_Restart, "Auto_Restart defaults to True");
      Assert (Opts.Port = 0, "Port defaults to 0");
      Assert (Opts.Log = Info, "Log defaults to Info");
      Assert (Opts.Session_Idle_Timeout_Seconds = 0,
              "Session idle timeout defaults to 0");
      Assert (Length (Opts.CLI_Path) = 0, "CLI_Path defaults to empty");
      Assert (Length (Opts.Github_Token) = 0,
              "Github_Token defaults to empty");
   end Test_Default_Options;

   --  ---------------------------------------------------------------
   --  Test: client creation and initial state
   --  ---------------------------------------------------------------
   procedure Test_Client_Create is
      C : constant Copilot_Client := Create;
   begin
      Put_Line ("[Test_Client_Create]");
      Assert (Get_State (C) = Disconnected,
              "New client is Disconnected");
      Assert (not Is_Connected (C),
              "New client Is_Connected returns False");
   end Test_Client_Create;

   --  ---------------------------------------------------------------
   --  Test: default session config
   --  ---------------------------------------------------------------
   procedure Test_Default_Session_Config is
      Cfg : constant Session_Config := Default_Session_Config;
   begin
      Put_Line ("[Test_Default_Session_Config]");
      Assert (Cfg.Streaming, "Streaming defaults to True");
      Assert (Length (Cfg.System_Prompt) = 0,
              "System_Prompt defaults to empty");
      Assert (Length (Cfg.Model) = 0, "Model defaults to empty");
      Assert (Cfg.Session_Idle_Timeout = 0,
              "Session_Idle_Timeout defaults to 0");
   end Test_Default_Session_Config;

   --  ---------------------------------------------------------------
   --  Test: version constants
   --  ---------------------------------------------------------------
   procedure Test_Version is
   begin
      Put_Line ("[Test_Version]");
      Assert (Copilot.Version.SDK_Version = "3.1.1",
              "SDK_Version is 3.1.1");
      Assert (Copilot.Version.Protocol_Version = 4,
              "Protocol_Version is 4");
      Assert (Copilot.Version.Min_Protocol = 4
                and then Copilot.Version.Max_Protocol = 4,
              "Handshake accepts exactly protocol 4");
   end Test_Version;

   --  ---------------------------------------------------------------
   --  Test: flat session payloads become SDK protocol 4 groups
   --  ---------------------------------------------------------------
   procedure Test_Group_Session_Params is
      use Copilot.Wire;
      Grouped : constant String :=
        Group_Session_Params
          ("{""model"":""m"",""streaming"":true,""requestPermission"":true,"
           & """tools"":[{""name"":""t""}],""hooks"":true,"
           & """disableResume"":true,""custom"":1}",
           Is_Resume => True);
      Legacy  : constant String :=
        Group_Session_Params
          ("{""model"":""m"",""provider"":{""type"":""openai"","
           & """baseUrl"":""http://x"",""maxOutputTokens"":100}}",
           Is_Resume => False);
   begin
      Put_Line ("[Test_Group_Session_Params]");
      Assert (String_Member (Grouped, "model") = "m", "model stays at the root");
      Assert (Bool_Member (Grouped, "suppressResumeEvent"),
              "disableResume becomes suppressResumeEvent");
      Assert (Bool_Member (Member (Grouped, "events"), "streaming"),
              "streaming moves to events.streaming");
      Assert (Bool_Member (Member (Grouped, "permissions"), "request"),
              "requestPermission moves to permissions.request");
      Assert (Member (Member (Grouped, "tools"), "definitions")
                = "[{""name"":""t""}]",
              "tools moves to tools.definitions");
      Assert (Bool_Member (Member (Grouped, "hooks"), "hasHandlers"),
              "hooks becomes hooks.hasHandlers");
      Assert (Member (Grouped, "custom") = "1", "unknown keys stay at the root");
      Assert (Member (Grouped, "streaming") = "", "flat keys are removed");
      Assert (String_Member (Member (Legacy, "inference"), "defaultProvider")
                = "default",
              "legacy provider becomes the default named provider");
      Assert (Member (Member (Legacy, "github"), "capi")
                = "{""enabled"":false}",
              "legacy provider disables CAPI models");
      Assert (Member (Member (Legacy, "inference"), "models")
                = "[{""provider"":""default"",""id"":""m"","
                  & """maxOutputTokens"":100}]",
              "legacy model limits move to inference.models");
   end Test_Group_Session_Params;

   --  ---------------------------------------------------------------
   --  Test: tool definition helpers
   --  ---------------------------------------------------------------
   procedure Test_Tool_Helpers is
      P  : Tool_Parameter;
      TD : Tool_Definition;
   begin
      Put_Line ("[Test_Tool_Helpers]");

      P := String_Param ("city", "City name");
      Assert (To_String (P.Name) = "city",
              "String_Param name is 'city'");
      Assert (P.Required, "String_Param is required");
      Assert (To_String (P.Param_Type) = "string",
              "String_Param type is 'string'");

      P := Integer_Param ("count", "Number of items");
      Assert (To_String (P.Param_Type) = "integer",
              "Integer_Param type is 'integer'");

      P := Boolean_Param ("verbose", "Enable verbose output");
      Assert (To_String (P.Param_Type) = "boolean",
              "Boolean_Param type is 'boolean'");

      P := Optional_String_Param ("units", "Temperature units");
      Assert (not P.Required,
              "Optional_String_Param is not required");

      TD := Define_Tool ("get_weather", "Get weather info");
      Assert (To_String (TD.Name) = "get_weather",
              "Define_Tool name is 'get_weather'");
      Assert (To_String (TD.Description) = "Get weather info",
              "Define_Tool description matches");
   end Test_Tool_Helpers;

   --  ---------------------------------------------------------------
   --  Test: tool JSON serialization
   --  ---------------------------------------------------------------
   procedure Test_Tools_To_Json is
      Defs : Tool_List;
      Params : Param_List;
      Json : Unbounded_String;
   begin
      Put_Line ("[Test_Tools_To_Json]");

      Params.Append (String_Param ("city", "City name"));
      Defs.Append (Define_Tool ("get_weather", "Get weather", Params));

      declare
         S : constant String := Tools_To_Json (Defs);
      begin
         Assert (S'Length > 0, "Tools_To_Json produces non-empty output");
         Assert (S (S'First) = '[', "JSON starts with '['");
         Assert (S (S'Last) = ']', "JSON ends with ']'");
      end;
   end Test_Tools_To_Json;

   --  ---------------------------------------------------------------
   --  Test: type conversions
   --  ---------------------------------------------------------------
   procedure Test_Type_Conversions is
   begin
      Put_Line ("[Test_Type_Conversions]");
      Assert ((-( +"hello")) = "hello",
              "UString round-trip works");
      Assert (To_String (Null_UString) = "",
              "Null_UString is empty string");
   end Test_Type_Conversions;

   --  ---------------------------------------------------------------
   --  Test: connection state enumeration
   --  ---------------------------------------------------------------
   procedure Test_Connection_States is
   begin
      Put_Line ("[Test_Connection_States]");
      Assert (Connection_State'First = Disconnected,
              "First state is Disconnected");
      Assert (Connection_State'Last = Stopped,
              "Last state is Stopped");
   end Test_Connection_States;

   --  ---------------------------------------------------------------
   --  Test: RPC connection initial state
   --  ---------------------------------------------------------------
   procedure Test_RPC_Initial is
      C : RPC_Connection;
   begin
      Put_Line ("[Test_RPC_Initial]");
      Assert (Is_Closed (C), "Default RPC_Connection is closed");
   end Test_RPC_Initial;

   --  ---------------------------------------------------------------
   --  Test: exception types exist
   --  ---------------------------------------------------------------
   procedure Test_Exceptions is
   begin
      Put_Line ("[Test_Exceptions]");
      --  Simply verify that the exceptions can be named.
      begin
         raise Session_Error;
      exception
         when Copilot_Error | Connection_Error | Protocol_Error
            | Timeout_Error | Tool_Error | Authentication_Error =>
            Assert (False, "All exception types are accessible");
         when Session_Error =>
            Assert (True, "All exception types are accessible");
      end;
   end Test_Exceptions;

begin
   Put_Line ("=== Copilot Ada SDK Unit Tests ===");
   New_Line;

   Test_Default_Options;
   Test_Client_Create;
   Test_Default_Session_Config;
   Test_Version;
   Test_Group_Session_Params;
   Test_Tool_Helpers;
   Test_Tools_To_Json;
   Test_Type_Conversions;
   Test_Connection_States;
   Test_RPC_Initial;
   Test_Exceptions;

   New_Line;
   Put_Line ("=== Results: "
           & Natural'Image (Pass_Count) & " passed,"
           & Natural'Image (Fail_Count) & " failed ===");

   if Fail_Count > 0 then
      Put_Line ("SOME TESTS FAILED");
   else
      Put_Line ("ALL TESTS PASSED");
   end if;
end Test_Client;
