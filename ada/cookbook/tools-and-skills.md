# Tools and Skills

Define custom tools, register skills, and orchestrate sub-agents using the GitHub Copilot SDK in Ada.

## Defining a Simple Tool

```ada
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Copilot.Client;         use Copilot.Client;
with Copilot.Session;        use Copilot.Session;
with Copilot.Types;          use Copilot.Types;
with Copilot.Tools;          use Copilot.Tools;

procedure Tool_Example is
   Client : Copilot_Client := Create;
   Sess   : Copilot_Session;

   --  Tool handler: receives JSON args, returns JSON result.
   function Weather_Handler (Args_Json : String) return String is
   begin
      return "{""city"":""Seattle"",""temp"":""72F"",""condition"":""sunny""}";
   end Weather_Handler;

begin
   Start (Client);

   --  Build tool definition.
   declare
      Params : Param_List;
      Def    : Tool_Definition;
      Cfg    : Session_Config := Default_Session_Config;
   begin
      Params.Append (String_Param ("city", "City name"));
      Def := Define_Tool ("get_weather", "Get weather for a city", Params);

      --  Create session, register handler.
      Sess := Create_Session (Client, Cfg);
      Register_Tool (Sess, "get_weather", Weather_Handler'Access);
   end;

   --  Ask the assistant to use the tool.
   declare
      Opts : Message_Options;
   begin
      Opts.Prompt := To_Unbounded_String ("What is the weather in Seattle?");
      declare
         Resp : constant Session_Event := Send_And_Wait (Sess, Opts);
      begin
         Put_Line (To_String (Resp.Content));
      end;
   end;

   Destroy (Sess);
   Stop (Client);
end Tool_Example;
```

## Tool with Multiple Parameters

```ada
declare
   Params : Param_List;
begin
   Params.Append (String_Param ("city", "City name"));
   Params.Append (Optional_String_Param ("units", "celsius or fahrenheit"));
   Params.Append (Boolean_Param ("detailed", "Include forecast"));

   declare
      Def : constant Tool_Definition :=
        Define_Tool ("get_forecast", "Get weather forecast", Params);
   begin
      --  Use Def in session config...
      null;
   end;
end;
```

## Multiple Tools

```ada
declare
   Defs : Tool_List;
   P1   : Param_List;
   P2   : Param_List;
begin
   P1.Append (String_Param ("query", "Search query"));
   Defs.Append (Define_Tool ("web_search", "Search the web", P1));

   P2.Append (String_Param ("path", "File path"));
   Defs.Append (Define_Tool ("read_file", "Read a file", P2));

   --  Serialize for protocol use.
   declare
      Json : constant String := Tools_To_Json (Defs);
   begin
      Put_Line ("Tools JSON: " & Json);
   end;
end;
```

## Permission Handler

Approve or deny permission requests from the agent.

```ada
function My_Permission_Handler
  (Req : Permission_Request) return Permission_Result
is
begin
   if Req.Kind = Read then
      return (Kind => Approved);
   else
      return (Kind => Denied_By_User);
   end if;
end My_Permission_Handler;

--  Register on a session.
Register_Permission_Handler (Sess, My_Permission_Handler'Access);
```

## User Input Handler

Enable the ask_user tool so the agent can prompt the user.

```ada
function My_Input_Handler
  (Req : User_Input_Request) return User_Input_Response
is
begin
   Put_Line ("Agent asks: " & To_String (Req.Question));
   return (Answer       => To_Unbounded_String ("Yes, proceed."),
           Was_Freeform => True);
end My_Input_Handler;

Register_User_Input_Handler (Sess, My_Input_Handler'Access);
```
