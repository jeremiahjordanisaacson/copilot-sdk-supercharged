# Copilot Supercharged SDK for Ada

A native Ada client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Installation

### Using Alire (recommended)

Add the dependency to your `alire.toml`:

```toml
[[depends-on]]
copilot_sdk_supercharged = ">=2.0.2"
```

Then run:

```sh
alr update
alr build
```

### Manual with GPR

1. Clone or copy the `ada/` directory into your project.
2. Reference the project file in your own `.gpr`:

```ada
with "path/to/ada/copilot_sdk.gpr";

project My_App is
   for Source_Dirs use ("src");
   for Object_Dir use "obj";
   for Main use ("my_app.adb");
end My_App;
```

3. Build:

```sh
gprbuild -P my_app.gpr
```

### Build from Source

```sh
cd ada
gprbuild -P copilot_sdk.gpr
```

## Quick Start

```ada
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Copilot.Client;         use Copilot.Client;
with Copilot.Session;        use Copilot.Session;
with Copilot.Types;          use Copilot.Types;

procedure Quick_Start is
   Client : Copilot_Client := Create;
   Sess   : Copilot_Session;
   Opts   : Message_Options;
begin
   --  Start the CLI server.
   Start (Client);

   --  Create a conversational session.
   Sess := Create_Session (Client);

   --  Send a message and wait for the response.
   Opts.Prompt := To_Unbounded_String ("What is 2 + 2?");
   declare
      Resp : constant Session_Event := Send_And_Wait (Sess, Opts);
   begin
      Put_Line ("Answer: " & To_String (Resp.Content));
   end;

   --  Cleanup.
   Destroy (Sess);
   Stop (Client);
end Quick_Start;
```

## Custom Tools

Define tools that the Copilot agent can call during a session:

```ada
with Copilot.Tools; use Copilot.Tools;

--  Define a tool handler.
function Get_Weather (Args_Json : String) return String is
begin
   return "{""city"":""Seattle"",""temp"":""72F"",""condition"":""sunny""}";
end Get_Weather;

--  Build the tool definition.
declare
   Params : Param_List;
   Def    : Tool_Definition;
begin
   Params.Append (String_Param ("city", "City name"));
   Def := Define_Tool ("get_weather", "Get weather for a city", Params);
end;

--  Register the handler on a session.
Register_Tool (Sess, "get_weather", Get_Weather'Access);
```

## Permission Handling

Register a handler to approve or deny permission requests from the agent:

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

Register_Permission_Handler (Sess, My_Permission_Handler'Access);
```

## User Input Handling

Enable the ask_user tool by registering a user input handler:

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

## Event Types

| Event Type | Description |
|---|---|
| `session.start` | Session started |
| `session.resume` | Session resumed |
| `session.idle` | Session finished processing |
| `session.error` | Error occurred |
| `session.info` | Informational message |
| `session.model_change` | Model changed |
| `session.truncation` | Context truncated |
| `session.compaction_start` | Context compaction started |
| `session.compaction_complete` | Context compaction finished |
| `user.message` | User message sent |
| `assistant.message` | Final assistant response |
| `assistant.message_delta` | Streaming response chunk |
| `assistant.reasoning` | Reasoning content |
| `assistant.reasoning_delta` | Streaming reasoning chunk |
| `assistant.turn_start` | Assistant turn started |
| `assistant.turn_end` | Assistant turn ended |
| `assistant.usage` | Token usage information |
| `tool.execution_start` | Tool execution started |
| `tool.execution_complete` | Tool execution finished |
| `tool.execution_progress` | Tool execution progress |
| `abort` | Session aborted |

## Session Idle Timeout

Configure automatic session cleanup after inactivity:

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Session_Idle_Timeout := 300;  --  5 minutes
   Sess := Create_Session (Client, Cfg);
end;
```

## Per-Session Authentication

Scope auth tokens per session for multi-tenant applications:

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Github_Token := To_Unbounded_String ("ghu_your_token_here");
   Sess := Create_Session (Client, Cfg);
end;
```

## Session Metadata

Retrieve metadata about a session:

```ada
declare
   Meta : constant Session_Metadata :=
     Get_Session_Metadata (Client, "session-123");
begin
   Put_Line ("Status: " & To_String (Meta.Status));
   Put_Line ("Turns:  " & Natural'Image (Meta.Turn_Count));
end;
```

## Resume Sessions

Save and resume sessions across restarts:

```ada
--  Save the session ID.
declare
   Id : constant String := Session_Id (Sess);
begin
   --  Persist Id to disk...
   null;
end;

--  Later, resume.
declare
   Cfg : Resume_Session_Config;
begin
   Cfg.Session_Id := To_Unbounded_String ("saved-session-id");
   Sess := Resume_Session (Client, Cfg);
end;
```

## API Reference

### Copilot_Client

| Method | Description |
|---|---|
| `Create (Options)` | Create a new client |
| `Start (Client)` | Start the CLI server and connect |
| `Stop (Client)` | Stop the server gracefully |
| `Force_Stop (Client)` | Force stop without cleanup |
| `Create_Session (Client, Config)` | Create a new session |
| `Resume_Session (Client, Config)` | Resume an existing session |
| `Delete_Session (Client, Id)` | Delete a session permanently |
| `Get_Session_Metadata (Client, Id)` | Get session metadata |
| `Ping (Client, Message)` | Ping the server |
| `Get_Status (Client)` | Get CLI version and protocol info |
| `Get_Auth_Status (Client)` | Get authentication status |
| `List_Models (Client)` | List available models |
| `Get_State (Client)` | Get current connection state |
| `Is_Connected (Client)` | Check if connected |

### Copilot_Session

| Method | Description |
|---|---|
| `Send (Session, Options)` | Send a message (non-blocking) |
| `Send_And_Wait (Session, Options, Timeout)` | Send and wait for idle |
| `Get_Messages (Session)` | Get session history |
| `Abort_Turn (Session)` | Abort current processing |
| `Destroy (Session)` | Destroy the session |
| `Register_Tool (Session, Name, Handler)` | Register a tool handler |
| `Register_Permission_Handler (Session, H)` | Register permission handler |
| `Register_User_Input_Handler (Session, H)` | Register user input handler |
| `Session_Id (Session)` | Get session ID |
| `Workspace_Path (Session)` | Get workspace path |
| `Is_Active (Session)` | Check if session is active |

### Client_Options

| Field | Type | Default | Description |
|---|---|---|---|
| `CLI_Path` | `UString` | `""` | Path to CLI executable |
| `CLI_Url` | `UString` | `""` | External server URL |
| `CLI_Args` | `UString` | `""` | Extra CLI arguments |
| `Cwd` | `UString` | `""` | Working directory |
| `Port` | `Natural` | `0` | TCP port (0 = auto) |
| `Use_Stdio` | `Boolean` | `True` | Use stdio transport |
| `Log` | `Log_Level` | `Info` | CLI log level |
| `Auto_Start` | `Boolean` | `True` | Auto-start on first use |
| `Auto_Restart` | `Boolean` | `True` | Auto-restart on crash |
| `Github_Token` | `UString` | `""` | GitHub auth token |
| `Use_Logged_In` | `Boolean` | `False` | Use stored OAuth |
| `Session_Idle_Timeout_Seconds` | `Natural` | `0` | Default idle timeout |

### Session_Config

| Field | Type | Default | Description |
|---|---|---|---|
| `System_Prompt` | `UString` | `""` | Custom system prompt |
| `Model` | `UString` | `""` | Model name |
| `Github_Token` | `UString` | `""` | Per-session auth token |
| `Streaming` | `Boolean` | `True` | Enable streaming |
| `Workspace_Path` | `UString` | `""` | Workspace directory |
| `Session_Idle_Timeout` | `Natural` | `0` | Idle timeout (seconds) |

## Connection Mode

### stdio (default)

The client spawns the CLI process and communicates via stdin/stdout pipes. This is the recommended mode.

```ada
declare
   Opts : Client_Options := Default_Options;
begin
   Opts.CLI_Path := To_Unbounded_String ("/path/to/copilot-cli");
   Opts.Use_Stdio := True;  --  default
   declare
      Client : Copilot_Client := Create (Opts);
   begin
      Start (Client);
      --  ...
      Stop (Client);
   end;
end;
```

### External CLI Server

Connect to an already-running CLI server:

```ada
declare
   Opts : Client_Options := Default_Options;
begin
   Opts.CLI_Url := To_Unbounded_String ("http://localhost:3000");
   declare
      Client : Copilot_Client := Create (Opts);
   begin
      Start (Client);
      --  ...
      Stop (Client);
   end;
end;
```

## Ada-Specific Patterns

### Controlled Types for RAII

Use Ada controlled types to ensure cleanup runs even on exceptions:

```ada
with Ada.Finalization; use Ada.Finalization;

type Managed_Client is new Limited_Controlled with record
   Inner : Copilot_Client;
end record;

overriding procedure Finalize (Self : in out Managed_Client) is
begin
   if Is_Connected (Self.Inner) then
      Stop (Self.Inner);
   end if;
end Finalize;
```

### Task-Based Concurrency

Ada tasks provide true concurrency for parallel sessions:

```ada
task type Worker is
   entry Run (Prompt : String);
end Worker;

task body Worker is
   Client : Copilot_Client := Create;
   Sess   : Copilot_Session;
begin
   Start (Client);
   Sess := Create_Session (Client);
   accept Run (Prompt : String) do
      declare
         Opts : Message_Options;
      begin
         Opts.Prompt := To_Unbounded_String (Prompt);
         declare
            R : constant Session_Event := Send_And_Wait (Sess, Opts);
         begin
            Put_Line (To_String (R.Content));
         end;
      end;
   end Run;
   Destroy (Sess);
   Stop (Client);
end Worker;
```

### Exception Handling

All SDK errors are represented as named exceptions:

| Exception | When raised |
|---|---|
| `Copilot_Error` | General SDK error |
| `Connection_Error` | Cannot connect to CLI |
| `Protocol_Error` | JSON-RPC or framing error |
| `Session_Error` | Session operation failed |
| `Timeout_Error` | Operation timed out |
| `Tool_Error` | Tool execution failed |
| `Authentication_Error` | Auth token invalid |

```ada
begin
   Start (Client);
exception
   when Connection_Error =>
      Put_Line ("Install the Copilot CLI or set COPILOT_CLI_PATH.");
   when Protocol_Error =>
      Put_Line ("Update your CLI to a compatible version.");
end;
```

## Cookbook

See the [cookbook/](cookbook/) directory for detailed recipes:

- [Error Handling](cookbook/error-handling.md)
- [Multiple Sessions](cookbook/multiple-sessions.md)
- [Persisting Sessions](cookbook/persisting-sessions.md)
- [Tools and Skills](cookbook/tools-and-skills.md)
- [Advanced v2.0 Features](cookbook/advanced-features.md)

## Requirements

- GNAT (GCC Ada) or any Ada 2012-compliant compiler
- Copilot CLI installed or reachable via `COPILOT_CLI_PATH`
- GNAT.OS_Lib (included with GNAT)
- Optional: Alire package manager

## Running Tests

```sh
cd ada/tests
gnatmake test_client.adb -I../src -aI../src
./test_client
```

## License

MIT License. See [LICENSE](../LICENSE) for details.
