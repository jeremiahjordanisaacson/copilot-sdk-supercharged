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

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new client calls
below are server-scoped requests that need **Copilot CLI 1.0.116 or newer**;
older CLIs answer method-not-found, raised as `Protocol_Error` like any other
JSON-RPC error. The examples assume `use Copilot.Types; use Copilot.Client;
use Copilot.Session;` and a running `Client`.

### MCP secret store

Save and delete legacy MCP configuration credentials in the runtime-owned
secret store (`mcp.secrets.save`, `mcp.secrets.delete`,
`mcp.secrets.deleteServer`). Each call also takes an optional `Config_Dir`.
The secret value is never logged.

```ada
declare
   Saved : constant Mcp_Secrets_Save_Result :=
     Mcp_Secrets_Save (Client, "my-server-token",
                       Ada.Environment_Variables.Value ("MY_SERVER_TOKEN"));
begin
   Put_Line (if Saved.Used_Keychain then "Stored in the OS keychain"
             else "Stored in the file backend");
end;
Mcp_Secrets_Delete (Client, "my-server-token");
Mcp_Secrets_Delete_Server (Client, "my-server");  --  every credential of the server
```

### Plugin extension approvals

Persist, list and revoke user-scoped approvals for installed plugin extensions
(`permissions.extensionApprovals.grant`, `.list`, `.revoke`).
`Extension_Approvals_List` and `Extension_Approvals_Revoke` take an optional
`Session_Id` to use an attached session's configuration profile.

```ada
declare
   --  Persist a pending extension permission request before answering it.
   Grant : constant Permissions_Extension_Approvals_Grant_Result :=
     Extension_Approvals_Grant (Client, Session_Id (Sess), Pending_Request_Id);
begin
   if Grant.Granted then
      Put_Line ("Approved " & To_String (Grant.Approval.Extension_Name));
   end if;
end;

for Approval of Extension_Approvals_List (Client) loop
   --  Extension_Name, Plugin_Name, Capabilities_Json, Granted_At,
   --  Installation_Key, Installation_Label
   if Extension_Approvals_Revoke
        (Client, To_String (Approval.Extension_Name),
         To_String (Approval.Installation_Key)).Revoked
   then
      Put_Line ("Revoked " & To_String (Approval.Extension_Name));
   end if;
end loop;
```

### Ask-user image attachments

The client now answers the runtime's `userInput.request` (the ask_user tool).
Setting `Session_Config.On_User_Input_Request` (a text handler, see
[User Input Handling](#user-input-handling)) or the new
`On_User_Input_Image_Request` sends `requestUserInput`, so the runtime offers
the tool. `User_Input_Request` now also carries `Request_Id`, `Choices_Json`,
`Allow_Freeform`, `Meta_Json` (the opaque `_meta` object) and
`Image_Attachment_Constraints` (`Present`, `Allowed_Mime_Types_Json`,
`Max_Bytes_Per_Image`, `Max_Count`); `Present` is False when the runtime does
not accept images. `User_Input_Response` and `User_Input_Handler` are
unchanged, so existing handlers keep working.

To answer with images, set `Session_Config.On_User_Input_Image_Request` to a
`User_Input_Image_Handler`. It returns a `User_Input_Image_Response`: `Answer`,
`Was_Freeform` and `Attachments`, a `User_Input_Image_Attachment_List` of
`User_Input_Image_Attachment` (base64 `Data`, `Mime_Type`, optional
`Description` and `Display_Name`; the wire `type` is always `"image"`).
`attachments` is sent only when the list is not empty. Image-only answers use
an empty `Answer` and `Was_Freeform => True`; the runtime enforces the limits.
When both handlers are set, the image handler answers.

```ada
function Answer_With_Screenshot
  (Req : User_Input_Request) return User_Input_Image_Response
is
   Images : User_Input_Image_Attachment_List;
begin
   if Req.Image_Attachment_Constraints.Present
     and then Ada.Strings.Fixed.Index
       (To_String (Req.Image_Attachment_Constraints.Allowed_Mime_Types_Json),
        """image/png""") > 0
   then
      Images.Append ((Data         => +Screenshot_Base64,  --  base64 PNG bytes
                      Mime_Type    => +"image/png",
                      Display_Name => +"screenshot.png",
                      others       => <>));
      return (Answer => Null_UString, Was_Freeform => True,
              Attachments => Images);
   end if;
   return (Answer => +"No screenshot available", Was_Freeform => True,
           Attachments => <>);  --  no images
end Answer_With_Screenshot;

--  ...
Cfg.On_User_Input_Image_Request := Answer_With_Screenshot'Access;
```

## What's New in v3.0.0

The Ada SDK now speaks **SDK protocol 4** (the Microsoft Copilot SDK 2.0
protocol). It requires **Copilot CLI 1.0.113 or newer**: `Start` rejects older
CLIs with `SDK protocol version mismatch: SDK supports version 4, but server
reports version N. Please update your SDK or server to ensure compatibility.`

Existing options keep working. `Create_Session` and `Resume_Session` take the
same `Session_Config` fields as before; `Copilot.Wire.Group_Session_Params`
converts the flat payload into the protocol 4 grouped shape (`inference`,
`tools`, `permissions`, `events`, ...) right before `session.create` and
`session.resume` are sent. Server-to-client callbacks (token providers, shell
authentication, memory store, hooks, network) are answered while the SDK reads
from the CLI (inside requests and `Send_And_Wait`).

The examples below assume `use Copilot.Types; use Copilot.Client;
use Copilot.Session;` and a running `Client`.

### Client options

```ada
declare
   Opts : Client_Options := Default_Options;
begin
   --  Client info handshake: sent as clientInfo on connect.
   Opts.Client_Info := (Editor_Name       => +"MyEditor",
                        Editor_Version    => +"1.2.0",
                        Extension_Name    => +"my-extension",
                        Extension_Version => +"0.1.0");
   --  Client mode: "coding-agent" (default) or "empty".  Empty mode needs
   --  Available_Tools_Json on every session and turns off ambient features.
   Opts.Mode := +Client_Mode_Empty;
   --  Network callbacks (network.setProvider); see My_Proxy below.
   Opts.Network := (Scope         => +"connection",
                    Resolve_Proxy => My_Proxy'Access,
                    others        => <>);
   Client := Create (Opts);
end;
```

```ada
function My_Proxy (Url : String) return String is
begin
   return (if Url'Length > 0 then "http://proxy.local:8080" else "");
end My_Proxy;
```

### Session options

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Available_Tools_Json := +"[""view"",""grep""]";
   Cfg.Ask_User_Variant     := +Ask_User_Variant_Elicitation;
   Cfg.Sandbox              := (Enabled => True, Allow_Bypass => True,
                                others  => <>);
   Cfg.Feature_Flags_Json   := +"{""my-flag"":true}";
   Cfg.Allowed_Models_Json  := +"[""claude-sonnet-4.5""]";
   Cfg.Auth_Client_Id_Metadata_Url := +"https://example.com/client.json";
   Cfg.Max_Output_Tokens    := 4096;  --  limits.max_output_tokens
   Cfg.Agent_Directories_Json := +"[""/repo/.agents""]";
   Cfg.Hook_Directories_Json  := +"[""/repo/.hooks""]";
   Cfg.Working_Directory    := +"/repo";
   Cfg.Trust_Working_Directory := +Working_Directory_Trust_Session;  --  create only
   Cfg.Image_Generation := (Enabled => Yes, Watermark => No,
                            Save_To_Disk => Yes);
   Cfg.Autopilot := (Continuation_Enabled => Yes, Max_Continues => 5);
   Sess := Create_Session (Client, Cfg);
end;
```

### Named BYOK providers

`Api_Key_Command` runs a command whose output is the API key. Select a
registered model with `"provider/id"`.

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Providers.Append
     ((Name            => +"local",
       Provider_Type   => +"openai",
       Base_Url        => +"http://localhost:11434/v1",
       Api_Key_Command => +"cat ~/.keys/local",
       others          => <>));
   Cfg.Models.Append
     ((Provider => +"local", Id => +"llama3", Max_Output_Tokens => 2048,
       others   => <>));
   Cfg.Default_Provider := +"local";
   Cfg.Model            := +"local/llama3";
   Cfg.Capi_Enabled     := No;  --  BYOK only
   Sess := Create_Session (Client, Cfg);
end;
```

The legacy single provider still works: set `Cfg.Provider_Json` to the v1
`provider` object and the SDK sends it as the `"default"` named provider.

### Idempotent tools

```ada
declare
   Defs : Tool_List;
   Def  : Tool_Definition := Define_Tool ("lookup", "Read-only lookup");
   Cfg  : Session_Config := Default_Session_Config;
begin
   Def.Idempotent := True;  --  safe to retry
   Defs.Append (Def);
   Cfg.Tools_Json := +Tools_To_Json (Defs);
   Sess := Create_Session (Client, Cfg);
end;
```

### Message options

```ada
declare
   Opts : Message_Options;
begin
   Opts.Prompt         := +"Summarize the diff";
   Opts.Source         := +Agent_Message_Source ("reviewer");  --  MessageSource
   Opts.Client_Turn_Id := +"turn-42";
   --  Structured output: sent as responseFormat json_schema.
   Opts.Response_Schema_Json :=
     +"{""type"":""object"",""properties"":{""summary"":{""type"":""string""}}}";
   Send (Sess, Opts);
end;
```

### Session methods

```ada
Switch_Auto_Tier (Sess, Auto_Tier_Balance);           --  session.model.switchAutoTier
declare
   Snapshot : constant String := Recheck_Policy_Helper (Sess);
   Result   : constant Permission_Mode_Result :=
     Set_Permission_Mode (Sess, "allow-all");
begin
   if To_String (Result.Reason) = Permission_Mode_Reason_Managed_Policy then
      Put_Line ("Blocked by policy; snapshot: " & Snapshot);
   end if;
end;
Clear_Managed_Settings_Cache (Client);                --  managedSettings.clearCache
Detach (Sess);                                        --  session.detach
```

### Resume options

```ada
declare
   RCfg : Resume_Session_Config;
begin
   RCfg.Session_Id                := +"saved-session-id";
   RCfg.Disable_Resume            := True;  --  suppressResumeEvent
   RCfg.Allow_Transcript_Recovery := True;
   RCfg.Options.Available_Tools_Json := +"[""view""]";
   Sess := Resume_Session (Client, RCfg);
end;
```

### Callbacks

```ada
function Mint_Token (Request : GitHub_Token_Request) return GitHub_Token_Result is
begin
   return (Access_Token => +"ghu_example", Token_Type => +"Bearer",
           Expires_In => 3600, others => <>);
end Mint_Token;

function Ask_Password (Request : Shell_Authentication_Request)
  return Shell_Authentication_Response is
begin
   return (Accepted => To_String (Request.Input_Kind) = "secret",
           Value    => +"s3cret");
end Ask_Password;

procedure Remember (Memory : Stored_Memory) is
begin
   Put_Line ("Remember: " & To_String (Memory.Fact));
end Remember;

function On_Start (Input : Subagent_Hook_Input) return String is
begin
   return "You are helping " & To_String (Input.Agent_Name);
end On_Start;

function On_Stop (Input : Subagent_Hook_Input) return Subagent_Stop_Decision is
begin
   return (Block => False, Reason => Null_UString,
           Modified_Response => Input.Response);
end On_Stop;

--  ...
Cfg.GitHub_Token_Provider   := Mint_Token'Access;   --  gitHubToken.getToken
Cfg.On_Shell_Authentication := Ask_Password'Access; --  shellAuthentication.request
Cfg.On_Store_Memory         := Remember'Access;     --  memoryStore.store
Cfg.On_Subagent_Start       := On_Start'Access;     --  hooks: subagentStart
Cfg.On_Subagent_Stop        := On_Stop'Access;      --  hooks: subagentStop
```

`GitHub_Token_Provider` and `Github_Token` are mutually exclusive; the
registration is released when the session is detached.

### Events and errors

```ada
--  Shell output streams arrive as tool.shell_output events.
if To_String (Evt.Event_Type) = Event_Tool_Shell_Output then
   Put_Line (To_String (Shell_Output (Evt).Text));
end if;

--  Policy Helper failures attached to a failed create/resume.
begin
   Sess := Create_Session (Client, Cfg);
exception
   when Protocol_Error =>
      declare
         Failure : constant Policy_Helper_Failure :=
           Last_Policy_Helper_Failure (Client);
      begin
         if Failure.Present then
            Put_Line ("Policy helper: " & To_String (Failure.Message));
         end if;
      end;
end;
```

JSON-RPC errors (including `session.abort`, `session.delete` and
`session.setForeground` failures) are raised as `Protocol_Error`.

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*`) is not
  available: it is an experimental upstream surface that depends on the
  runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection: the Ada SDK
  spawns the Copilot CLI over stdio (`CLI_Url` TCP connections are still not
  implemented).
- Server-to-client callbacks are answered only while the SDK is reading from
  the CLI (`Send_Request`, `Send_And_Wait`, `Read_Notification`); there is no
  background reader task. Callback registrations are process-wide, so use one
  `Copilot_Client` per process when you rely on network callbacks.
- Shell authentication prompts are answered synchronously, so
  `shellAuthentication.cancel` is acknowledged without further action.
- The CLI's stderr is a separate pipe that the SDK does not drain; keep the
  CLI log level low for long sessions.

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

Enable the ask_user tool by setting `Session_Config.On_User_Input_Request`
before creating (or resuming) the session; the client then answers the
runtime's `userInput.request`. `Register_User_Input_Handler` replaces the
handler of a live session. To answer with images, set
`On_User_Input_Image_Request` instead (see
[Ask-user image attachments](#ask-user-image-attachments)).

```ada
function My_Input_Handler
  (Req : User_Input_Request) return User_Input_Response
is
begin
   Put_Line ("Agent asks: " & To_String (Req.Question));
   return (Answer       => To_Unbounded_String ("Yes, proceed."),
           Was_Freeform => True);
end My_Input_Handler;

Cfg.On_User_Input_Request := My_Input_Handler'Access;
Sess := Create_Session (Client, Cfg);
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
| `tool.shell_output` | Streamed shell command output (protocol 4) |
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
| `Clear_Managed_Settings_Cache (Client)` | Clear cached managed settings |
| `Mcp_Secrets_Save (Client, Secret_Id, Value, Config_Dir)` | Save an MCP credential (`mcp.secrets.save`) |
| `Mcp_Secrets_Delete (Client, Secret_Id, Config_Dir)` | Delete an MCP credential (`mcp.secrets.delete`) |
| `Mcp_Secrets_Delete_Server (Client, Server_Name, Config_Dir)` | Delete a server's MCP credentials (`mcp.secrets.deleteServer`) |
| `Extension_Approvals_Grant (Client, Session_Id, Request_Id)` | Persist a pending plugin extension approval |
| `Extension_Approvals_List (Client, Session_Id)` | List plugin extension approvals |
| `Extension_Approvals_Revoke (Client, Extension_Name, Installation_Key, Session_Id)` | Revoke a plugin extension approval |
| `Last_Policy_Helper_Failure (Client)` | Policy Helper failure of the last failed create/resume |

### Copilot_Session

| Method | Description |
|---|---|
| `Send (Session, Options)` | Send a message (non-blocking) |
| `Send_And_Wait (Session, Options, Timeout)` | Send and wait for idle |
| `Get_Messages (Session)` | Get session history |
| `Abort_Turn (Session)` | Abort current processing |
| `Destroy (Session)` | Detach from the session and release its callbacks |
| `Detach (Session)` | Detach from the session (`session.detach`) |
| `Switch_Auto_Tier (Session, Tier)` | Request an Auto routing tier |
| `Recheck_Policy_Helper (Session)` | Re-run the enterprise Policy Helper |
| `Set_Permission_Mode (Session, Mode)` | Change the permission mode |
| `Shell_Output (Event)` | Decode a `tool.shell_output` event |
| `Register_Tool (Session, Name, Handler)` | Register a tool handler |
| `Register_Permission_Handler (Session, H)` | Register permission handler |
| `Register_User_Input_Handler (Session, H)` | Replace the user input (ask_user) handler |
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

## Recent Features (v2.4–v2.5)

Fields added in the v2.4 and v2.5 upstream syncs. Unless noted, they are fields of
`Session_Config` (start from `Default_Session_Config`); a few live on `Client_Options`
or `Message_Options`. Fields with a `_Json` suffix take a JSON string.

### v2.5 session options

- **Reasoning effort** — `Cfg.Reasoning_Effort` (`"low"`/`"medium"`/`"high"`).
- **Tool search** — `Cfg.Tool_Search_Json`.
- **Session rewind** — `Cfg.Rewind_Enabled`.
- **Content exclusion** — `Cfg.Content_Exclusion`.
- **Additional directories** — `Cfg.Additional_Directories_Json`.
- **Disabled MCP servers** — `Cfg.Disabled_Mcp_Servers_Json`.
- **GitHub MCP tool config** — `Cfg.Github_Mcp_Tool_Config_Json`.
- **Canvas provider** — `Cfg.Canvas_Provider_Json`.
- **Custom agents local-only** — `Cfg.Custom_Agents_Local_Only`.
- **Permission decision context** — `Cfg.Decision_Context_Json`.
- **User-prompt-transformed hook** — `Cfg.On_User_Prompt_Transformed`.
- **Built-in plugin directories** — `Cfg.Builtin_Plugin_Directories_Json`.
- **Agent-factory args schema** — `Cfg.Args_Schema_Json`.
- **In-process FFI transport** — `Cfg.In_Process`.
- **Experimental mode** — `Cfg.Experimental_Mode`.

### v2.4 session options

- **BYOK bearer token provider** — `Opts.Bearer_Token_Provider` (`Client_Options`).
- **HTTP request handler** — `Opts.Request_Handler` (`Client_Options`).
- **MCP OAuth token handler** — `Cfg.On_Mcp_Auth_Request`.
- **Session citations** — `Cfg.Enable_Citations`.
- **Excluded built-in agents** — `Cfg.Excluded_Builtin_Agents_Json`.
- **Session spending limits** — `Cfg.Session_Limits.Max_Ai_Credits`.
- **Session memory** — `Cfg.Memory.Enabled`.
- **OTLP protocol** — `Cfg.Otlp_Protocol`.
- **WebSocket transport** — `Cfg.Enable_Web_Socket_Responses`.
- **Experiment assignments** — `Cfg.Exp_Assignments_Json`.
- **Post/pre-tool hooks** — `Cfg.On_Post_Tool_Use`, `Cfg.On_Pre_Mcp_Tool_Call`.
- **Per-message overrides** — `Opts.Agent_Mode`, `Opts.Display_Prompt` (`Message_Options`).
- **Constants** — `Tool_Defer_Auto`/`Tool_Defer_Never`, `Section_Preamble`/`Section_Preserve`, `GitHub_Commit`/`GitHub_Repository`.

### Reasoning effort and tool search

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Reasoning_Effort := To_Unbounded_String ("high");
   Cfg.Tool_Search_Json := To_Unbounded_String ("{""enabled"":true}");
   Sess := Create_Session (Client, Cfg);
end;
```

### Session rewind and content exclusion

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Rewind_Enabled    := True;
   Cfg.Content_Exclusion := True;
   Sess := Create_Session (Client, Cfg);
end;
```

### Additional workspace directories

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Additional_Directories_Json :=
     To_Unbounded_String ("[""/data/shared"",""/data/reference""]");
   Sess := Create_Session (Client, Cfg);
end;
```

### Session spending limits

```ada
declare
   Cfg : Session_Config := Default_Session_Config;
begin
   Cfg.Session_Limits.Max_Ai_Credits := 500;
   Sess := Create_Session (Client, Cfg);
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
