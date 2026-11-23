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

## What's New in v3.0.0

This release speaks **SDK protocol 4** (the protocol of the Microsoft Copilot SDK 2.0 runtime) and
requires **Copilot CLI 1.0.113 or newer**. `Client.start` sends `connect` (falling back to `ping` on
older servers) and rejects any server that does not report protocol version 4 with an
`SDK protocol version mismatch: SDK supports version 4, but server reports version N` error.

Existing options keep working. `Types.session_config` keeps its flat fields; right before sending
`session.create` and `session.resume`, the client translates the flat payload into the grouped v4
shape (`inference`, `tools`, `permissions`, `interaction`, `events`, `agents`, `mcp`, `github`,
`state`, ...) with `Session_wire.group_session_params`. Only members the runtime accepts are sent:
unknown flat keys stay at the root, create-only members (`trust_working_directory`,
`refresh_custom_instructions`) are dropped on resume, and the legacy single `provider` is sent as a
named provider called `default` (with `github.capi.enabled = false`, as v1 BYOK behaved) unless you
also pass `providers` / `default_provider` / `capi_enabled`. `Session.send` now calls `session.send`
with the message fields next to the session id, and `Client.delete_session` raises when the server
reports a failed delete.

| Feature | OCaml API |
|---|---|
| Client info handshake | `client_options.client_info` (`Types.client_info`), sent with `connect` |
| Client mode | `client_options.client_mode = Some Types.client_mode_empty` (default `"coding-agent"`, legacy `"copilot-cli"` accepted); empty mode requires `available_tools` on every create/resume and applies locked-down defaults under your values |
| Network callbacks | `client_options.network` (`Types.network_options`: `resolve_proxy`, `authenticate_negotiate`, `authenticate_basic`, `network_scope`), registered with `network.setProvider` |
| Auto routing tier | `session_config.auto_tier` (sent as `github.capi.autoTier`); `Session.switch_auto_tier session (Some Types.auto_tier_fast)` |
| Ask-user variant | `ask_user_variant = Some Types.ask_user_variant_elicitation` |
| GitHub token provider | `github_token_provider` answers `gitHubToken.getToken` with `Types.Github_token {...}` or `Types.Github_token_cancelled`; mutually exclusive with `github_token` |
| Sandbox bypass | `sandbox_config = Some { (Types.default_sandbox_config ()) with allow_bypass = Some true }` |
| Feature flags | `feature_flags = Some [ ("my-flag", true) ]` |
| Session detach | `Client.disconnect_session client session` / `Session.detach session` (`session.detach`) |
| Message source | `Types.make_message ~source:(Types.message_source_agent "reviewer")` |
| Managed settings | `Client.clear_managed_settings_cache`, `Session.get_managed_settings`, `Session.recheck_policy_helper` |
| Model allowlist | `allowed_models` |
| OAuth client metadata URL | `auth_client_id_metadata_url` |
| Max output tokens | `model_capabilities = Types.model_limits_to_capabilities { ... max_output_tokens = Some 16384 ... }` (`limits.max_output_tokens`) |
| Structured output | `Types.make_message ~response_schema:schema` (sent as a `json_schema` `responseFormat`) |
| Agent / hook directories | `agent_directories`, `hook_directories` |
| Working directory trust | `trust_working_directory = Some Types.working_directory_trust_session` (create only) |
| Transcript recovery | `allow_transcript_recovery` (resume only) |
| Image generation | `image_generation = Some { Types.image_enabled = Some true; image_watermark = None; save_to_disk = Some true }` |
| Autopilot | `autopilot = Some { Types.continuation_enabled = Some true; max_continues = Some 5 }` |
| Sub-agent hooks | `on_subagent_start` / `on_subagent_stop` (dispatched from `hooks.invoke`) |
| BYOK API key command | `Types.make_provider_config ~api_key_command:...`, `Types.make_named_provider ~api_key_command:...` |
| Named BYOK providers | `providers` (`Types.make_named_provider`), `models` (`Types.make_provider_model`), `default_provider`, `capi_enabled`; `clear_default_provider = true` on resume sends `defaultProvider: null` |
| Idempotent tools | `Tools.define_tool ~idempotent:true` (sent in `tools.definitions`) |
| Client turn ID | `Types.make_message ~client_turn_id:"turn-1"` |
| Shell output events | `Types.ToolShellOutput` event type; parse the data with `Types.tool_shell_output_of_yojson` |
| Shell authentication | `on_shell_authentication_request` returns `Types.Shell_auth_accept value` or `Types.Shell_auth_cancel`; a `shellAuthentication.cancel` cancels the pending answer |
| Memory store | `on_store_memory` receives `Types.stored_memory` (sends `state.hasMemoryStore`) |
| Policy helper failure | `Client.policy_helper_failure exn` reads `error.data.policyHelperFailure` |
| Permission mode refusal reason | `Session.set_permission_mode session Types.permission_mode_allow_all` returns `pms_reason = Some Types.permission_mode_refusal_managed_policy` (`"managed_policy"`) when refused |
| Resume event suppression | `suppress_resume_event = Some true` (resume only, wire `suppressResumeEvent`) |

```ocaml
open Copilot_sdk

(* load_token, ask_user and save_memory are your own functions. *)
let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let options =
       { (Types.default_client_options ()) with
         client_info =
           Some
             { Types.editor_name = Some "my-ide"
             ; editor_version = Some "1.4.0"
             ; extension_name = None
             ; extension_version = None
             }
       ; client_mode = Some Types.client_mode_empty (* sessions must set available_tools *)
       ; network =
           Some
             { (Types.default_network_options ()) with
               network_scope = Some Types.network_scope_connection
             ; resolve_proxy =
                 Some (fun _url -> Lwt.return_some "PROXY proxy.contoso.com:8080; DIRECT")
             }
       }
     in
     let client = Client.create ~options () in
     let* () = Client.start client in
     let lookup_order, lookup_handler =
       Tools.define_tool ~name:"lookup_order" ~description:"Look up an order" ~idempotent:true
         (fun _invocation -> Lwt.return (Types.make_tool_result "shipped"))
     in
     let config =
       { (Types.default_session_config ()) with
         model = Some "ollama/llama3"
       ; session_name = Some "release-triage"
       ; tools = [ lookup_order ]
       ; available_tools = Some [ "lookup_order"; "view" ]
       ; auto_tier = Some Types.auto_tier_intelligence
       ; ask_user_variant = Some Types.ask_user_variant_elicitation
       ; sandbox_config =
           Some { (Types.default_sandbox_config ()) with allow_bypass = Some true }
       ; feature_flags = Some [ ("my-flag", true) ]
       ; trust_working_directory = Some Types.working_directory_trust_session
       ; autopilot = Some { Types.continuation_enabled = Some true; max_continues = Some 5 }
       ; model_capabilities =
           Types.model_limits_to_capabilities
             { Types.max_prompt_tokens = None
             ; max_output_tokens = Some 16384
             ; max_context_window_tokens = None
             }
       ; providers =
           Some
             [ Types.make_named_provider ~provider_type:"openai"
                 ~api_key_command:"op read op://dev/ollama/key" ~name:"ollama"
                 "http://localhost:11434/v1"
             ]
       ; models = Some [ Types.make_provider_model ~max_output_tokens:4096 ~provider:"ollama" "llama3" ]
       ; default_provider = Some "ollama"
       ; capi_enabled = Some false
       ; github_token_provider =
           Some
             (fun request ->
               Lwt.return
                 (Types.Github_token
                    { access_token = load_token request.Types.gtr_host
                    ; token_type = None
                    ; expires_in = 28800
                    }))
       ; on_subagent_start =
           Some (fun _input -> Lwt.return_some (`Assoc [ ("additionalContext", `String "Be concise.") ]))
       ; on_shell_authentication_request =
           Some
             (fun request ->
               if request.Types.sar_input_kind = Types.shell_input_kind_confirm then
                 Lwt.return Types.Shell_auth_cancel
               else Lwt.return (Types.Shell_auth_accept (ask_user request.Types.sar_prompt)))
       ; on_store_memory =
           Some (fun memory -> save_memory memory.Types.sm_subject memory.Types.sm_fact)
       }
     in
     let* session =
       Lwt.catch
         (fun () -> Client.create_session ~config client)
         (fun exn ->
           (match Client.policy_helper_failure exn with
            | Some failure ->
              Printf.printf "Blocked by the policy helper (%s): %s\n"
                failure.Types.phf_category failure.Types.phf_message
            | None -> ());
           Lwt.fail exn)
     in
     Session.register_tool session "lookup_order" lookup_handler;
     Session.on session (fun event ->
       (match event.Types.event_type with
        | Types.ToolShellOutput ->
          (match Types.tool_shell_output_of_yojson event.Types.data with
           | Some output -> print_string output.Types.so_text
           | None -> ())
        | _ -> ());
       Lwt.return_unit);
     let* () =
       Session.send session
         (Types.make_message ~source:(Types.message_source_agent "reviewer")
            ~client_turn_id:"turn-1"
            ~response_schema:(`Assoc [ ("type", `String "object") ])
            "Where is order 42? Answer as JSON.")
     in
     let* _ack = Session.switch_auto_tier session (Some Types.auto_tier_fast) in
     let* _settings = Session.recheck_policy_helper session in
     let* () = Client.clear_managed_settings_cache client in
     let* result = Session.set_permission_mode session Types.permission_mode_allow_all in
     if (not result.Types.pms_success)
        && result.Types.pms_reason = Some Types.permission_mode_refusal_managed_policy
     then Printf.printf "Managed policy keeps the permission mode at %s\n" result.Types.pms_mode;
     let* () = Client.disconnect_session client session in
     let* _resumed =
       Client.resume_session client (Session.session_id session)
         ~config:
           { config with
             allow_transcript_recovery = Some true
           ; suppress_resume_event = Some true
           ; clear_default_provider = true
           }
         ()
     in
     Client.stop client)
```

### Known limitations

- The runtime-supervised AHP host (`startAhpHost`, `ahp.*` / `host.*` RPC families) is not available:
  it is an experimental upstream surface that depends on the runtime's in-process hosting library.
- In-process native runtime hosting is not the default connection: this SDK keeps spawning the CLI
  (stdio) or connecting over TCP. The existing `in_process` option is unchanged.
- Sub-agent hooks receive and return raw JSON (`Yojson.Safe.t`) rather than typed records.

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
| `Session_wire` | Flat to grouped SDK protocol 4 session payload translation |
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
