# Copilot Supercharged SDK for OCaml

An OCaml client library for programmatic control of GitHub Copilot CLI via JSON-RPC 2.0.

This SDK communicates with the Copilot CLI server using JSON-RPC 2.0 over stdio or TCP, with Content-Length header framing (LSP protocol style). It follows the same architecture as all other SDKs in this repository (see the [full list of supported languages](../README.md#available-sdks)).

## Installation

### With opam

```bash
opam install copilot-sdk-supercharged
```

### From source (development)

```bash
cd ocaml
opam install . --deps-only --with-test
dune build
```

### Dependencies

| Package | Min version | Purpose |
|---------|-------------|---------|
| `ocaml` | 4.14 | Language |
| `lwt` | 5.6 | Async / concurrency |
| `lwt_ppx` | 2.1 | Lwt syntax (`let*`) |
| `yojson` | 2.0 | JSON parsing / generation |
| `alcotest` | 1.7 | Test framework (dev only) |
| `alcotest-lwt` | 1.7 | Lwt test helpers (dev only) |

## Quick Start

```ocaml
open Copilot_sdk

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let client = Client.create () in
     let* () = Client.start client in

     let* session = Client.create_session client in
     Session.register_permission_handler session Tools.approve_all;

     Session.on session (fun event ->
       (match event.event_type with
        | Types.AssistantMessage ->
          let open Yojson.Safe.Util in
          let content =
            try event.data |> member "content" |> to_string
            with _ -> ""
          in
          Printf.printf "Assistant: %s\n" content
        | _ -> ());
       Lwt.return_unit);

     let msg = Types.make_message "What is 2 + 2?" in
     let* response = Session.send_and_wait session msg in
     (match response with
      | Some ev ->
        let open Yojson.Safe.Util in
        Printf.printf "Answer: %s\n"
          (try ev.data |> member "content" |> to_string with _ -> "")
      | None -> Printf.printf "Timed out\n");

     let* () = Session.destroy session in
     Client.stop client)
```

## Custom Tools

Register tools that the Copilot agent can invoke during a session:

```ocaml
open Copilot_sdk

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let client = Client.create () in
     let* () = Client.start client in

     (* Define a tool *)
     let params =
       Tools.make_object_schema
         ~properties:
           [ Tools.make_string_param ~name:"location" ~description:"City" () ]
         ~required:["location"]
         ()
     in
     let defn, handler =
       Tools.define_tool
         ~name:"get_weather"
         ~description:"Get weather for a city"
         ~parameters:params
         (fun invocation ->
           let open Yojson.Safe.Util in
           let loc =
             try invocation.arguments |> member "location" |> to_string
             with _ -> "Unknown"
           in
           Lwt.return
             (Types.make_tool_result
                (Printf.sprintf "{\"city\": \"%s\", \"temp\": 22}" loc)))
     in

     (* Create session with the tool *)
     let config =
       { (Types.default_session_config ()) with tools = [ defn ] }
     in
     let* session = Client.create_session ~config client in
     Session.register_tool session "get_weather" handler;
     Session.register_permission_handler session Tools.approve_all;

     let msg = Types.make_message "What is the weather in Paris?" in
     let* _ = Session.send_and_wait session msg in

     let* () = Session.destroy session in
     Client.stop client)
```

## Permission Handling

Control which operations the agent is allowed to perform:

```ocaml
(* Approve everything (development) *)
Session.register_permission_handler session Tools.approve_all;

(* Selective approval *)
let my_handler : Session.permission_handler =
  fun request _session_id ->
    let open Yojson.Safe.Util in
    let kind =
      try request |> member "kind" |> to_string with _ -> "unknown"
    in
    if kind = "read" then
      Lwt.return Types.{ decision = Approved; rules = None }
    else
      Lwt.return Types.{ decision = DeniedByUser; rules = None }
in
Session.register_permission_handler session my_handler;
```

## Streaming

Handle real-time streaming of assistant responses:

```ocaml
Session.on session (fun event ->
  (match event.event_type with
   | Types.AssistantMessageDelta ->
     let open Yojson.Safe.Util in
     let delta =
       try event.data |> member "content" |> to_string with _ -> ""
     in
     print_string delta; flush stdout
   | Types.AssistantMessage ->
     print_newline ()
   | _ -> ());
  Lwt.return_unit);
```

## Connecting to an External Server

Instead of spawning a CLI subprocess, connect to an already-running server:

```ocaml
let opts =
  { (Types.default_client_options ()) with
    cli_url = Some "127.0.0.1:3000"
  }
in
let client = Client.create ~options:opts () in
```

## Recent Features (v2.4–v2.5)

Fields added in the v2.4 and v2.5 upstream syncs. Session options are record
fields on `Types.session_config` (build with
`{ (Types.default_session_config ()) with ... }`); transport options are fields
on `Types.client_options`.

**v2.5.0**

- Reasoning effort — `reasoning_effort : reasoning_effort option` (`Low | Medium | High | Xhigh`)
- Tool search — `tool_search : (string * Yojson.Safe.t) list`
- Session rewind — `rewind_enabled : bool option`
- Additional directories — `additional_directories : string list option`
- Disabled MCP servers — `disabled_mcp_servers : string list option`
- GitHub MCP tool config — `github_mcp_tool_config`
- Canvas provider — `canvas_provider`
- Custom agents local-only — `custom_agents_local_only : bool option`
- Experimental mode — `experimental_mode : bool option`
- Content exclusion — `content_exclusion : bool option`
- Permission decision context — `permission_result_to_yojson_with_context`
- Agent-factory args schema — `args_schema`
- Built-in plugin directories — `builtin_plugin_directories : string list option`
- In-process FFI transport — `in_process : bool option`

**v2.4.0**

- BYOK bearer-token provider — `bearer_token_provider : bearer_token_provider option`
- MCP OAuth handler flag — `on_mcp_auth_request : bool`
- HTTP request handler — `request_handler : copilot_request_handler option`
- Session citations — `enable_citations : bool`
- Excluded built-in agents — `excluded_builtin_agents : string list`
- Session spending limits — `session_limits : session_limits_config option` (`max_ai_credits`)
- Session memory — `memory : memory_configuration option`
- OTLP protocol — `otlp_protocol : string option`
- WebSocket responses — `enable_web_socket_responses : bool`
- Experiment assignments — `exp_assignments`
- Tool defer loading — `tool_defer` (`DeferAuto | DeferNever`)
- System-message sections — `system_message_section_preamble`, `system_message_section_preserve`
- Hook name constants — `hook_on_post_tool_use`, `hook_on_pre_mcp_tool_call`, `hook_on_user_prompt_transformed`
- Message agent mode / display prompt — `Types.make_message ?agent_mode ?display_prompt`
- GitHub attachment variants — `attachment_github_commit`, `attachment_github_repository`

Reasoning effort, content exclusion, extra directories, and rewind:

```ocaml
let config =
  { (Types.default_session_config ()) with
    reasoning_effort       = Some Types.High   (* Low | Medium | High | Xhigh *)
  ; content_exclusion      = Some true
  ; additional_directories = Some [ "../shared"; "../docs" ]
  ; rewind_enabled         = Some true
  ; disabled_mcp_servers   = Some [ "playwright" ]
  }
in
let* session = Client.create_session ~config client in
```

Tool search and experimental mode:

```ocaml
let config =
  { (Types.default_session_config ()) with
    tool_search       = [ ("enabled", `Bool true) ]
  ; experimental_mode = Some true
  }
in
```

Session spending limits, memory, and citations:

```ocaml
let config =
  { (Types.default_session_config ()) with
    session_limits   = Some { Types.max_ai_credits = Some 5.0 }
  ; memory           = Some { Types.memory_enabled = true }
  ; enable_citations = true
  ; otlp_protocol    = Some "http/protobuf"
  }
in
```

BYOK bearer-token provider and in-process transport (client options):

```ocaml
let opts =
  { (Types.default_client_options ()) with
    bearer_token_provider      = Some (fun _args -> "ghs_ephemeral_token")
  ; builtin_plugin_directories = Some [ "./plugins" ]
  ; in_process                 = Some false
  }
in
let client = Client.create ~options:opts () in
```

## Architecture

```
Your App
  |
  v
Client (client.ml)     -- lifecycle, session factory
  |
  v
Session (session.ml)   -- events, tools, permissions
  |
  v
Jsonrpc (jsonrpc.ml)   -- Content-Length framing, request/response
  |
  v
Copilot CLI             -- via stdio or TCP
```

### Module overview

| Module | Purpose |
|--------|---------|
| `Client` | Connection lifecycle, session creation, server queries |
| `Session` | Event handling, tool dispatch, send/receive messages |
| `Jsonrpc` | JSON-RPC 2.0 transport with LSP-style framing |
| `Types` | Records, variants, JSON conversion helpers |
| `Tools` | Tool definition helpers, parameter schema builders |
| `Version` | Protocol version constants |

## Building and Testing

```bash
# Build
cd ocaml && dune build

# Run tests
cd ocaml && dune runtest

# Format (if ocamlformat is installed)
cd ocaml && dune fmt
```

## Cookbook

See the `cookbook/` directory for focused recipes:

| Recipe | Description |
|--------|-------------|
| [`custom_tools.ml`](cookbook/custom_tools.ml) | Defining and registering custom tools |
| [`streaming.ml`](cookbook/streaming.ml) | Handling streaming delta events |
| [`permission_handling.ml`](cookbook/permission_handling.ml) | Selective permission approval |
| [`multi_turn.ml`](cookbook/multi_turn.ml) | Multi-turn conversations |
| [`error_handling.ml`](cookbook/error_handling.ml) | Robust error handling patterns |
| [`model_selection.ml`](cookbook/model_selection.ml) | Listing and choosing models |

## License

MIT - See [LICENSE](../LICENSE) for details.
