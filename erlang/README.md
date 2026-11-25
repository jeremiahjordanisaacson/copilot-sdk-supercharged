# Copilot Supercharged SDK for Erlang

An Erlang/OTP SDK for interacting with the GitHub Copilot CLI server. Communicates via JSON-RPC 2.0 over stdio using Content-Length header framing. Built with OTP gen_server behaviours and idiomatic Erlang patterns.

## Requirements

- Erlang/OTP 25+
- [rebar3](https://rebar3.org/) build tool (3.14 or newer for the `git_subdir` dependency below)
- [jsx](https://hex.pm/packages/jsx) for JSON encoding/decoding
- Copilot CLI 1.0.113 or newer: `copilot` on `PATH`, the `COPILOT_CLI_PATH` environment variable, or the `cli_path` client option

## Installation

### rebar3 dependency (git_subdir)

The Erlang SDK is not published to Hex (the `copilot_sdk_supercharged` package on Hex is the Elixir SDK). Add it to your `rebar.config` as a `git_subdir` dependency, which fetches the `erlang/` directory of this repository at a release tag:

```erlang
{deps, [
    {copilot_sdk, {git_subdir, "https://github.com/jeremiahjordanisaacson/copilot-sdk-supercharged.git",
                   {tag, "v3.1.1"}, "erlang"}}
]}.
```

Then run:

```bash
rebar3 compile
```

The OTP application is `copilot_sdk`: list it in the `applications` of your `.app.src`. Its only dependency, `jsx`, is fetched from Hex.

### Manual

Copy the `src/` directory into your project and add `jsx` as a dependency.

## Quick Start

```erlang
%% Start the client
{ok, Client} = copilot_client:start_link(#{log_level => <<"info">>}),

%% Create a session
{ok, Session} = copilot_client:create_session(Client, #{
    model => <<"gpt-4">>
}),

%% Subscribe to events
Ref = copilot_session:subscribe(Session, self()),

%% Send a message and wait for the response
{ok, Response} = copilot_session:send_and_wait(Session, #{
    prompt => <<"What is 2+2?">>
}),
io:format("Answer: ~p~n", [Response]),

%% Clean up
copilot_session:unsubscribe(Session, Ref),
copilot_session:stop(Session),
copilot_client:stop(Client).
```

## What's New in v3.1.1

Fixes for the CLI connection, permission requests, and plan approvals. Earlier 3.x releases could not exchange messages with a real CLI.

* **Working stdio connection**: `copilot_jsonrpc` now opens and owns the CLI port, so the CLI's replies reach it. The port runs in stream mode with the Content-Length framing of vscode-jsonrpc (a frame split across reads, or several frames in one read, are both handled), and the CLI's stderr stays out of the JSON-RPC stream (it goes to the VM's stderr). The CLI is started with `spawn_executable` and one argument per item, so paths with spaces work. As in the official SDKs, `github_token` reaches the CLI through the `COPILOT_SDK_AUTH_TOKEN` environment variable (`--auth-token-env`), never the command line, and `--no-auto-login` is passed unless `use_logged_in` is true. New client options: `env` (environment for the CLI process) and `cwd` (its working directory). A request on a closed connection returns `{error, {connection_closed, Reason}}`, and a timeout returns `{error, timeout}`.
* **Permissions (SDK protocol 4)**: an `on_permission_request` handler now sends `permissions.request` and answers the `permission.requested` session events with `session.permissions.handlePendingPermissionRequest`, in its own process. `copilot_types:permission_result/1,2` returns the protocol 4 kinds (`approve_once`, `approve_for_session`, `reject`, `{reject, Feedback}`, `user_not_available`, `no_result`); `approved` and `denied` remain as aliases of `approve_once` and `reject` (`denied` used to send the rejected kind `"denied"`). See [Permission Handling](#permission-handling).
* **Exit plan mode**: the new `on_exit_plan_mode_request` session option sends `interaction.requestExitPlanMode` and answers `exitPlanMode.request` for its own session only. Before this release the request crashed the handler. See [Exit Plan Mode](#exit-plan-mode).
* **Real CLI tests**: `test/copilot_cli_e2e_SUITE.erl` drives the real CLI against the shared replaying proxy in CI (see [Running Tests](#running-tests)).

## What's New in v3.1.0

Parity with the Microsoft Copilot SDK 2.0.3-preview.0. The new `copilot_client` functions below call server-scoped RPCs that require **Copilot CLI 1.0.116 or newer**; older CLIs answer with a JSON-RPC method-not-found error, returned as `{error, Error}` like any other RPC error.

### MCP secret store and plugin extension approvals

```erlang
%% MCP secret store. The value is a credential: it is sent to the runtime and never logged.
{ok, #{used_keychain := InKeychain}} =
    copilot_client:mcp_secrets_save(Client, <<"my-server/api-key">>,
                                    list_to_binary(os:getenv("MY_API_KEY", "")),
                                    #{config_dir => <<"/path/to/config">>}),
io:format("stored in the ~s~n", [case InKeychain of true -> "OS keychain"; false -> "file backend" end]),
ok = copilot_client:mcp_secrets_delete(Client, <<"my-server/api-key">>),
ok = copilot_client:mcp_secrets_delete_server(Client, <<"my-server">>),

%% Plugin extension approvals (user scoped). Grant persists a pending extension
%% permission request before you answer it.
{ok, #{granted := true, approval := #{extension_name := Name}}} =
    copilot_client:extension_approvals_grant(Client, SessionId, RequestId),
io:format("approved ~s~n", [Name]),
{ok, #{approvals := Approvals}} = copilot_client:extension_approvals_list(Client),
lists:foreach(fun(#{extension_name := ExtensionName, installation_key := Key}) ->
    {ok, #{revoked := true}} =
        copilot_client:extension_approvals_revoke(Client, ExtensionName, Key)
end, Approvals).
```

| Function | Wire method | Returns |
|---|---|---|
| `mcp_secrets_save(Pid, SecretId, Value)`, `mcp_secrets_save(Pid, SecretId, Value, #{config_dir => Dir})` | `mcp.secrets.save` | `{ok, #{used_keychain}}` (`false` means the file backend stored it) |
| `mcp_secrets_delete(Pid, SecretId)`, `mcp_secrets_delete(Pid, SecretId, #{config_dir => Dir})` | `mcp.secrets.delete` | `ok` |
| `mcp_secrets_delete_server(Pid, ServerName)`, `mcp_secrets_delete_server(Pid, ServerName, #{config_dir => Dir})` | `mcp.secrets.deleteServer` | `ok` |
| `extension_approvals_grant(Pid, SessionId, RequestId)` | `permissions.extensionApprovals.grant` | `{ok, #{granted, approval}}` |
| `extension_approvals_list(Pid)`, `extension_approvals_list(Pid, #{session_id => Id})` | `permissions.extensionApprovals.list` | `{ok, #{approvals}}` |
| `extension_approvals_revoke(Pid, ExtensionName, InstallationKey)`, `extension_approvals_revoke(Pid, ExtensionName, InstallationKey, #{session_id => Id})` | `permissions.extensionApprovals.revoke` | `{ok, #{revoked}}` |

Each approval (`copilot_types:extension_approval/1`) is `#{extension_name, plugin_name, capabilities, granted_at, installation_key, installation_label}`: `extension_name` is the stable identity (for example `<<"plugin:my-plugin:my-extension">>`), `granted_at` an RFC 3339 timestamp, and `installation_key` an opaque key to pass back unchanged to revoke.

### Ask-user image attachments

The `on_user_input_request` handler receives the request map unchanged. When the runtime accepts images it carries `<<"imageAttachmentConstraints">>` (`allowedMimeTypes`, `maxBytesPerImage`, `maxCount`), which `copilot_types:image_attachment_constraints/1` decodes into `#{allowed_mime_types, max_bytes_per_image, max_count}` (`undefined` when images are not accepted); `<<"requestId">>` and `<<"_meta">>` are present when the runtime sends them. Answer with `copilot_types:user_input_response/3`, whose third argument is a list of attachments built with `copilot_types:user_input_image_attachment/1,2` (base64 `data`, `mime_type`, optional `description` and `display_name`; the wire `type` is always `<<"image">>`). Attachments are sent only when set and are only valid when the request advertised constraints; the runtime enforces the limits.

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    on_user_input_request => fun(Request) ->
        Constraints = copilot_types:image_attachment_constraints(Request),
        case Constraints =/= undefined andalso
             lists:member(<<"image/png">>, maps:get(allowed_mime_types, Constraints)) of
            true ->
                {ok, Png} = file:read_file("screenshot.png"),
                Image = copilot_types:user_input_image_attachment(#{
                    data         => base64:encode(Png),
                    mime_type    => <<"image/png">>,
                    display_name => <<"screenshot.png">>
                }),
                %% Image-only answer: empty answer and WasFreeform = true
                copilot_types:user_input_response(<<>>, true, [Image]);
            false ->
                copilot_types:user_input_response(<<"No screenshot available">>, true)
        end
    end
}).
```

The `on_elicitation_request` context also carries `request_id` and `meta` (the request's opaque `_meta`); return `copilot_types:elicitation_result(Action, Content, Meta)` to send a `_meta` back unchanged.

## What's New in v3.0.0

This release speaks **SDK protocol 4**, the contract of the Microsoft Copilot SDK 2.0 runtime, and requires **Copilot CLI 1.0.113 or newer**: older CLIs fail startup with `{error, {protocol_mismatch, Message}}`. Existing options keep working. You pass the same atom-keyed config maps as before, and `copilot_client` converts the flat payload into the grouped v4 `session.create` / `session.resume` payload right before sending (for example `streaming` becomes `events.streaming`, a hooks map becomes `hooks.hasHandlers`, and `allow_transcript_recovery` stays at the root). The legacy single `provider` map becomes a named `"default"` connection with Copilot API (CAPI) models disabled, which preserves v1 BYOK behavior. A `cli_path` ending in `.js` (such as the CLI's `npm-loader.js`) now runs under `node`.

Protocol 4 also brings these behavior fixes: tools are declared in `tools.definitions` and answered through `tool.call`; hooks run through `hooks.invoke`; session events are read from the `event` field of `session.event` notifications; and a `success: false` reply from `session.delete` or `session.setForeground` is returned as `{error, ...}`.

New in this release:

* **Named BYOK providers**: `providers`, `models`, `default_provider`, and `capi` session keys (sent as `inference.providers`, `inference.models`, `inference.defaultProvider`, and `github.capi`). Select a registered model as `<<"Provider/Id">>`; `default_provider => null` clears the default on resume. Wire helpers: `copilot_types:named_provider_config/1` and `copilot_types:provider_model_config/1`.
* **Idempotent tools**: `copilot_tools:define_tool/5` with `#{idempotent => true}` (or `<<"idempotent">> => true` in the tool map) marks a tool as safe to retry.
* **Client turn ID**: the `client_turn_id` message option is sent as `clientTurnId`.
* **Client mode**: client option `mode => coding_agent` (the default, unchanged behavior) or `mode => empty`; the binaries `<<"coding-agent">>` and `<<"empty">>` work too, and the legacy `<<"copilot-cli">>` is an alias of `coding-agent`. In empty mode every create and resume must set `available_tools` (otherwise `{error, {invalid_config, _}}`), and safe defaults are applied under your values: experimental mode, telemetry, cross-session store, memory, on-demand instructions, repository hooks, and host Git operations off; in-memory MCP OAuth token and embedding cache storage; embedding retrieval skipped.
* **Shell output events**: `copilot_types:session_event_type(tool_shell_output)` (`<<"tool.shell_output">>`) and `copilot_types:shell_output/1`, which decodes `#{tool_call_id, text, sequence, stream}`.
* **Shell authentication**: `on_shell_authentication_request => fun(Request) -> {accept, Value} | cancel end` answers `sudo`, `ssh`, and `git` prompts (`shellAuthentication.request`). When the runtime sends `shellAuthentication.cancel`, the pending prompt is answered with `cancel`.
* **Memory store**: `store_memory => fun(Memory) -> ok | {error, Reason} end` receives each approved memory (`#{session_id, subject, fact, citations, reason}`) through `memoryStore.store`.
* **Network callbacks**: client option `network => #{scope, resolve_proxy, authenticate_negotiate, authenticate_basic}`, registered with `network.setProvider` after connecting. Tokens and passwords are never logged.
* **Policy helper failure**: `copilot_types:policy_helper_failure/1` reads `policyHelperFailure` from a failed `create_session` / `resume_session` result or a managed settings snapshot.
* **Permission mode refusal reason**: `copilot_session:set_permission_mode/2` returns `#{success, mode, reason}`; `copilot_types:permission_mode_refusal(managed_policy)` is `<<"managed_policy">>`.

Run a shared service in empty mode with proxy callbacks:

```erlang
{ok, Client} = copilot_client:start_link(#{
    mode    => empty,
    network => #{
        scope         => connection,
        resolve_proxy => fun(#{url := Url}) ->
            case binary:match(Url, <<".internal.example">>) of
                nomatch -> <<"PROXY proxy.example:8080">>;
                _       -> <<"DIRECT">>
            end
        end,
        authenticate_basic => fun
            (#{previous_rejected := true}) -> undefined;
            (#{proxy := _Proxy}) ->
                #{username => <<"svc-proxy">>,
                  password => list_to_binary(os:getenv("PROXY_PASSWORD", ""))}
        end
    }
}).
```

Use named providers, an idempotent tool, shell authentication, and an application memory store:

```erlang
LookupOrder = copilot_tools:define_tool(
    <<"lookup_order">>,
    <<"Look up an order by ID">>,
    #{<<"type">> => <<"object">>,
      <<"properties">> => #{<<"id">> => #{<<"type">> => <<"string">>}}},
    fun(#{<<"id">> := Id}, _Invocation) -> <<"Order ", Id/binary, " shipped">> end,
    #{idempotent => true}
),
{ok, Session} = copilot_client:create_session(Client, #{
    model           => <<"gateway/chat">>,
    available_tools => [<<"view">>, <<"lookup_order">>],
    tools           => [LookupOrder],
    providers       => [#{name            => <<"gateway">>,
                          type            => <<"openai">>,
                          base_url        => <<"https://models.example.com/v1">>,
                          api_key_command => <<"print-gateway-key">>}],
    models          => [#{provider          => <<"gateway">>,
                          id                => <<"chat">>,
                          wire_model        => <<"deployment-1">>,
                          max_output_tokens => 8192}],
    capi            => #{enabled => false},
    on_shell_authentication_request => fun
        (#{input_kind := <<"confirm">>}) -> cancel;
        (#{program := <<"sudo">>}) -> {accept, list_to_binary(os:getenv("SUDO_PASSWORD", ""))};
        (_Other) -> cancel
    end,
    store_memory => fun(#{subject := Subject, fact := Fact}) ->
        io:format("remember [~s]: ~s~n", [Subject, Fact]),
        ok
    end
}).
```

Stream shell output, tag a turn, and change the permission mode:

```erlang
Ref = copilot_session:subscribe(Session, self()),
{ok, _MessageId} = copilot_session:send(Session, #{
    prompt         => <<"Run the test suite">>,
    client_turn_id => <<"turn-42">>
}),
receive
    {copilot_session_event, _SessionId, #{type := <<"tool.shell_output">>, data := Data}} ->
        #{text := Text} = copilot_types:shell_output(Data),
        io:format("~s", [Text])
after 5000 ->
    ok
end,
ManagedPolicy = copilot_types:permission_mode_refusal(managed_policy),
case copilot_session:set_permission_mode(Session, <<"allow-all">>) of
    {ok, #{success := false, reason := ManagedPolicy, mode := Mode}} ->
        io:format("Managed policy keeps the session in ~s mode~n", [Mode]);
    {ok, _Applied} ->
        ok
end.
```

Inspect a policy helper failure, and clear the default provider on resume:

```erlang
case copilot_client:create_session(Client, #{available_tools => [<<"view">>]}) of
    {ok, _NewSession} ->
        ok;
    {error, _} = Error ->
        case copilot_types:policy_helper_failure(Error) of
            #{category := Category, message := Message} ->
                io:format("Policy helper failed (~s): ~s~n", [Category, Message]);
            undefined ->
                Error
        end
end,
{ok, Resumed} = copilot_client:resume_session(Client, SessionId, #{
    available_tools  => [<<"view">>],
    default_provider => null   %% sends inference.defaultProvider: null
}).
```

### Known limitations

* The runtime-supervised AHP host (`startAhpHost` and the `ahp.*` / `host.*` RPC families) is not available. It is an experimental upstream surface that depends on the runtime's in-process hosting library.
* In-process native runtime hosting is not the default connection: this SDK keeps spawning the CLI over stdio. The existing `in_process` helper option is unchanged.
* Callbacks that the runtime raises while `create_session/2` is still in flight (for example a `sessionStart` hook) are answered with no output, because the session ID is not known until `session.create` returns. `resume_session/3` registers its callbacks before sending the request.
* Option keys this SDK sent before v3.0.0 that have no protocol 4 equivalent (`rewindEnabled`, `otlpProtocol`, `contentExclusion`, `mcpAuthHandler`) are kept unchanged at the top level of the payload for forward compatibility.

## Architecture

The SDK follows OTP principles with gen_server behaviours:

```
copilot_client (gen_server)
    |
    |-- starts copilot_jsonrpc and answers client-level server requests
    |   (tool.call, hooks.invoke, userInput.request, exitPlanMode.request, ...)
    |   routed by sessionId
    |-- dispatches session.event notifications to sessions
    |
    +-- copilot_jsonrpc (gen_server)
    |       |-- spawns the CLI with --headless --no-auto-update --stdio and owns its port
    |       |-- reads stdout in stream mode (Content-Length framing); stderr is not read
    |
    +-- copilot_session (gen_server)
            |-- event subscriptions (subscribe / unsubscribe)
            |-- send / send_and_wait
            |-- permission handler (permission.requested events)
            |-- elicitation and command events
```

Communication uses **JSON-RPC 2.0** with **Content-Length header framing**:

```
Content-Length: 123\r\n
\r\n
{"jsonrpc":"2.0","id":"...","method":"...","params":{...}}
```

## Modules

| Module | Description |
|--------|-------------|
| `copilot_client` | Client gen_server - manages CLI process and sessions |
| `copilot_session` | Session gen_server - conversation with event handling |
| `copilot_jsonrpc` | JSON-RPC 2.0 gen_server that owns the CLI port (stdio transport) |
| `copilot_types` | Type specs, records, and constant helpers |
| `copilot_tools` | Tool definition helpers with result normalization |

## Custom Tools

Define tools that the Copilot agent can invoke:

```erlang
WeatherTool = copilot_tools:define_tool(
    <<"get_weather">>,
    <<"Get current weather for a city">>,
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"city">> => #{<<"type">> => <<"string">>, <<"description">> => <<"City name">>},
            <<"unit">> => #{<<"type">> => <<"string">>, <<"description">> => <<"celsius or fahrenheit">>}
        },
        <<"required">> => [<<"city">>]
    },
    fun(Args, _Invocation) ->
        City = maps:get(<<"city">>, Args, <<"unknown">>),
        Unit = maps:get(<<"unit">>, Args, <<"celsius">>),
        iolist_to_binary(io_lib:format("Weather in ~s: 22 degrees ~s", [City, Unit]))
    end
),

{ok, Session} = copilot_client:create_session(Client, #{
    tools => [WeatherTool]
}).
```

The handler can return:

- A **binary** - becomes `textResultForLlm` with `resultType = "success"`
- A **map with resultType** - passed through directly
- Any **other map/list** - JSON-serialized to `textResultForLlm`
- **undefined** - empty success result

If the handler throws, a failure `ToolResult` is returned automatically.

## Permission Handling

Register a handler to approve or deny tool permission requests. With a handler, `create_session/2` and `resume_session/3` send `permissions.request`; the runtime then emits a `permission.requested` session event for each request, and the session runs the handler in its own process (a slow prompt never blocks the session) and sends the decision with `session.permissions.handlePendingPermissionRequest`. Requests already resolved by a `permissionRequest` hook are skipped. The runtime only asks about operations that its rules and the session's permission mode do not already allow: read-only shell commands such as `echo`, for example, can run without a request.

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    on_permission_request => fun(Request) ->
        io:format("Permission requested: ~p~n", [maps:get(<<"kind">>, Request)]),
        copilot_types:permission_result(approve_once)
    end
}).
```

A `fun/1` handler receives the permission request map (binary keys such as `<<"kind">>`: `<<"shell">>`, `<<"write">>`, `<<"read">>`, `<<"mcp">>`, `<<"url">>`, and `<<"toolCallId">>`). A `fun/2` handler also receives an invocation map `#{session_id, request_id}` with `prompt_request`, `assisted_approval_assessment`, and `program_tool_call_id` when the runtime sends them:

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    on_permission_request => fun
        (#{<<"kind">> := <<"shell">>}, _Invocation) ->
            copilot_types:permission_result({reject, <<"Shell commands are not allowed.">>});
        (_Request, #{request_id := _RequestId}) ->
            copilot_types:permission_result(approve_for_session)
    end
}).
```

`copilot_types:permission_result/1` builds the protocol 4 decisions used by the Node and Python SDKs:

| Argument | Wire `kind` | Meaning |
|---|---|---|
| `approve_once` (alias `approved`) | `approve-once` | Allow this request |
| `approve_for_session` | `approve-for-session` | Allow it and remember the approval for the session |
| `reject` (alias `denied`), `{reject, Feedback}` | `reject` | Deny it; the optional feedback is shown to the agent |
| `user_not_available` | `user-not-available` | Deny it because no user can confirm it |
| `no_result` | `no-result` | Send nothing, so another client can answer the pending request |

A handler may also return one of these atoms directly, or any other `PermissionDecision` wire map (for example `#{<<"kind">> => <<"approve-for-location">>, <<"approval">> => Approval, <<"locationKey">> => Key}`). `copilot_types:permission_result/2` adds a decision context, sent as `decisionContext` next to the result. If the handler crashes or returns an invalid decision, or the runtime rejects the answer, the request is answered with `user-not-available`.

## Exit Plan Mode

Register `on_exit_plan_mode_request` to review a plan before the agent leaves plan mode. The handler makes `create_session/2` and `resume_session/3` send `interaction.requestExitPlanMode`. Each `exitPlanMode.request` is routed by `sessionId`, so a session's handler only answers its own requests; a session without a handler approves, as in the Node SDK.

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    on_exit_plan_mode_request => fun(Request) ->
        io:format("Plan: ~s~n", [maps:get(<<"summary">>, Request)]),
        %% <<"actions">> lists the choices; <<"recommendedAction">> is the runtime's pick
        copilot_types:exit_plan_mode_result(true, #{
            selected_action => maps:get(<<"recommendedAction">>, Request),
            feedback        => <<"Approved by the reviewer">>
        })
    end
}).
```

The handler receives the request map unchanged (`<<"summary">>`, `<<"planContent">>` when available, `<<"actions">>`, `<<"recommendedAction">>`, and `<<"requestId">>` on newer runtimes). It returns a boolean, `copilot_types:exit_plan_mode_result/1,2` (`#{selected_action, feedback, decision_context}`, each optional), or a wire map with `<<"approved">>`.

## User Input Handling

Register a handler to respond to user input requests:

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    on_user_input_request => fun(Request) ->
        io:format("Question: ~p~n", [maps:get(<<"question">>, Request)]),
        copilot_types:user_input_response(<<"Yes, proceed.">>, true)
    end
}).
```

A `fun/1` handler makes `create_session/2` and `resume_session/3` send `interaction.requestUserInput`, so the runtime asks ask_user questions through `userInput.request`. Requests are routed by `sessionId`: each session's handler only answers its own questions, and a request for a session without a handler gets a JSON-RPC error.

## Hooks

Register lifecycle hooks. The runtime calls them through `hooks.invoke` requests
with the hook input (a map with binary keys). Return a map to supply the hook
output (for example `#{<<"permissionDecision">> => <<"deny">>}` from
`on_pre_tool_use`); any other return value (such as `ok`) leaves the runtime's
behavior unchanged:

```erlang
Hooks = #{
    on_pre_tool_use => fun(Data) ->
        io:format("About to use tool: ~p~n", [maps:get(<<"toolName">>, Data, <<>>)])
    end,
    on_post_tool_use => fun(Data) ->
        io:format("Tool completed: ~p~n", [maps:get(<<"toolName">>, Data, <<>>)])
    end,
    on_session_start => fun(Data) ->
        io:format("Session started~n")
    end,
    on_session_end => fun(Data) ->
        io:format("Session ended~n")
    end,
    on_error_occurred => fun(Data) ->
        io:format("Error: ~p~n", [Data])
    end
},

{ok, Session} = copilot_client:create_session(Client, #{hooks => Hooks}).
```

## Client Options

```erlang
{ok, Client} = copilot_client:start_link(#{
    cli_path      => <<"/usr/local/bin/copilot">>,
    cwd           => <<"/path/to/project">>,
    env           => #{<<"HTTPS_PROXY">> => <<"http://proxy.example:8080">>},
    log_level     => <<"debug">>,
    github_token  => <<"ghp_...">>,
    use_logged_in => false,
    auto_start    => true
}).
```

The client spawns the CLI with `--headless --no-auto-update --log-level Level --stdio` and talks JSON-RPC over its stdin and stdout; the CLI's stderr goes to the VM's stderr.

* `cli_path`: the CLI to run (default: `COPILOT_CLI_PATH`, then `copilot` on `PATH`). A path ending in `.js` runs under `node`; any other path, such as `node_modules/.bin/copilot`, runs directly.
* `cwd`: the CLI's working directory (default: the VM's).
* `env`: a map or list of `{Name, Value}` merged into the VM environment for the CLI; `false` removes a variable.
* `github_token`: passed as the `COPILOT_SDK_AUTH_TOKEN` environment variable with `--auth-token-env`, never on the command line.
* `use_logged_in`: whether the CLI may use the stored login; `false` adds `--no-auto-login`. Defaults to `true` without `github_token` and `false` with it.
* `auto_start`: start the CLI right after `start_link/1` (default `true`); otherwise call `copilot_client:start_cli/1`, which returns the startup error, if any.

## Session Configuration

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    model               => <<"gpt-4">>,
    reasoning_effort    => <<"medium">>,
    tools               => [MyTool],
    system_message      => #{<<"mode">> => <<"append">>, <<"content">> => <<"Extra instructions">>},
    available_tools     => [<<"Read">>, <<"Write">>],
    excluded_tools      => [<<"Bash">>],
    working_directory   => <<"/path/to/project">>,
    streaming           => true,
    on_permission_request => fun(_) -> copilot_types:permission_result(approve_once) end,
    on_user_input_request => fun(_) -> copilot_types:user_input_response(<<"ok">>, true) end,
    on_exit_plan_mode_request => fun(_) -> copilot_types:exit_plan_mode_result(true) end,
    hooks               => #{},
    provider            => #{
        <<"type">>    => <<"openai">>,
        <<"baseUrl">> => <<"https://api.example.com/v1">>,
        <<"apiKey">>  => <<"sk-...">>
    },
    mcp_servers => #{
        <<"myServer">> => #{
            <<"type">>    => <<"local">>,
            <<"command">> => <<"node">>,
            <<"args">>    => [<<"server.js">>],
            <<"tools">>   => [<<"tool1">>, <<"tool2">>]
        }
    },
    infinite_sessions => #{
        <<"enabled">> => true,
        <<"backgroundCompactionThreshold">> => 0.80,
        <<"bufferExhaustionThreshold">>     => 0.95
    },
    skill_directories => [<<"./skills">>],
    disabled_skills   => [<<"test-skill">>],
    include_sub_agent_streaming_events => true
}).
```

## What's New in v2.6.0

Parity with upstream `@github/copilot-sdk` v2.6.0. Session keys go in the
`create_session/2` / `resume_session/3` config map (atom keys, serialized to the
camelCase wire keys). Object options (`sandbox_config`, `image_generation`,
`autopilot`, `provider`, `model_capabilities`) accept atom-keyed maps, which are
converted to wire keys, or wire-shaped binary-keyed maps, which pass through.

**Client info** — identify the integrating application. Sent as a `connect`
request (`clientInfo` → `editorName`/`editorVersion`/`extensionName`/`extensionVersion`)
before the ping handshake; skipped when unset, and ignored by older CLIs:

```erlang
{ok, Client} = copilot_client:start_link(#{
    client_info => #{
        application_name    => <<"acme-portal">>,
        application_version => <<"2.4.0">>,
        integration_name    => <<"acme-copilot-plugin">>,
        integration_version => <<"0.9.1">>
    }
}).
```

**Auto routing tier (experimental)** — `auto_tier` and `enable_web_socket_responses`
are sent inside the `capi` object; switch the tier later with `set_auto_tier/2`
(`session.model.switchAutoTier`, pass `null` to return to default routing):

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    model     => <<"auto">>,
    auto_tier => copilot_types:auto_tier(intelligence)
}),
{ok, #{<<"status">> := _}} = copilot_session:set_auto_tier(Session, <<"balance">>),
{ok, _} = copilot_session:set_auto_tier(Session, null).
```

**GitHub token provider** — supply GitHub tokens on demand (mutually exclusive
with `github_token`). The client answers `gitHubToken.getToken` requests from a
client-level registry; the registration is dropped when the session ends:

```erlang
Provider = fun(#{host := Host, session_id := _SessionId, reason := _Reason}) ->
    #{kind => token, access_token => my_vault:github_token(Host), expires_in => 28800}
    %% or #{kind => cancelled}
end,
{ok, Session} = copilot_client:create_session(Client, #{github_token_provider => Provider}).
```

**Session options** — sandboxing, feature flags, model allowlist, OAuth client
metadata, name and directories, runtime toggles, image generation, autopilot,
and BYOK/model-limit additions:

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    name                           => <<"release-triage">>,          %% create only
    ask_user_variant               => copilot_types:ask_user_variant(elicitation),
    sandbox_config                 => #{enabled => true, allow_bypass => true},
    feature_flags                  => #{<<"some_flag">> => true},   %% sent even when #{}
    allowed_models                 => [<<"gpt-5">>, <<"claude-sonnet-4.6">>],
    auth_client_id_metadata_url    => <<"https://example.com/oauth/client.json">>,
    agent_directories              => [<<"./agents">>],
    hook_directories               => [<<"./hooks">>],
    interactive_user               => true,
    excluded_managed_settings      => [<<"model">>],
    enforce_managed_model_defaults => true,
    trust_working_directory        => <<"session">>,                %% create only
    refresh_custom_instructions    => true,                         %% create only
    image_generation               => #{enabled => true, watermark => true, save_to_disk => false},
    autopilot                      => #{continuation_enabled => true, max_continues => 3},
    provider                       => #{base_url        => <<"http://localhost:11434/v1">>,
                                        model_provider  => <<"ollama">>,
                                        api_key_command => <<"op read op://dev/ollama/key">>},
    model_capabilities             => #{limits => #{max_output_tokens => 8192}}
}),
{ok, Resumed} = copilot_client:resume_session(Client, SessionId, #{
    allow_transcript_recovery => true                               %% resume only
}).
```

Wire helpers are also exported: `copilot_types:sandbox_config/1`,
`image_generation/1`, `autopilot_options/1`, `provider_config/1`,
`model_capabilities/1`, `model_limits/1`, and `client_info/1`.

**Message source and structured output** — `source` accepts `user`, `system`,
`{agent, Id}` or a binary (`copilot_types:message_source/1`); `response_schema`
sends a `json_schema` `responseFormat`, and `send_and_wait_structured/3,4` returns
the final assistant content decoded as JSON:

```erlang
{ok, _} = copilot_session:send(Session, #{prompt => <<"Build is green">>, source => system}),
{ok, _} = copilot_session:send(Session, #{
    prompt => <<"Review the plan">>,
    source => copilot_types:message_source({agent, <<"planner">>})
}),
Schema = #{
    <<"type">>       => <<"object">>,
    <<"properties">> => #{<<"answer">> => #{<<"type">> => <<"integer">>}},
    <<"required">>   => [<<"answer">>]
},
{ok, #{<<"answer">> := 42}} =
    copilot_session:send_and_wait_structured(Session, <<"What is 6 * 7?">>, Schema).
```

**Managed settings and detach**:

```erlang
{ok, _} = copilot_client:clear_managed_settings_cache(Client),   %% managedSettings.clearCache
{ok, Settings} = copilot_session:get_managed_settings(Session),  %% session.managedSettings.get
{ok, _} = copilot_session:recheck_policy_helper(Session),        %% session.managedSettings.recheckPolicyHelper
ok = copilot_session:disconnect(Session).  %% session.detach (falls back to session.destroy)
```

**Sub-agent lifecycle hooks** — `on_subagent_start` / `on_subagent_stop`
(`copilot_types:hook_type(subagent_start)` → `<<"subagentStart">>`):

```erlang
Hooks = #{
    on_subagent_start => fun(Data) ->
        io:format("sub-agent ~s started~n", [maps:get(<<"agentName">>, Data, <<>>)])
    end,
    on_subagent_stop => fun(Data) ->
        io:format("sub-agent ~s finished~n", [maps:get(<<"agentName">>, Data, <<>>)])
    end
},
{ok, Session} = copilot_client:create_session(Client, #{hooks => Hooks}).
```

**Parity notes**

- External-tool cancellation is N/A: this SDK runs tools on the `tool.call` request path.
- Experimental host-integration callbacks (skill provider, installation confirmation,
  extension launch provider, Entra/Kerberos auth callbacks) are not yet available in this SDK.
- Since v3.0.0, hooks (including the sub-agent hooks) run through `hooks.invoke`
  requests: a hook that returns a map supplies the hook output, and any other
  return value leaves the runtime's behavior unchanged.

## Recent Features (v2.4–v2.5)

Keys added in the v2.4 and v2.5 upstream syncs. Session keys go in the
`copilot_client:create_session/2` config map (atom keys, serialized to the
camelCase wire keys shown); transport/BYOK options live in `copilot_types`.

**v2.5.0**

- Reasoning effort — `reasoning_effort` → `reasoningEffort`
- Tool search — `tool_search` → `toolSearch`
- Session rewind — `rewind_enabled` → `rewindEnabled`
- Additional directories — `additional_directories` → `additionalDirectories`
- Disabled MCP servers — `disabled_mcp_servers` → `disabledMcpServers`
- GitHub MCP tool config — `github_mcp_tool_config` → `githubMcpToolConfig`
- Canvas provider — `canvas_provider` → `canvasProvider`
- Custom agents local-only — `custom_agents_local_only` → `customAgentsLocalOnly`
- Experimental mode — `experimental_mode` → `experimentalMode`
- Content exclusion — `content_exclusion` → `contentExclusion`
- Permission decision context — `copilot_types:permission_result/2`
- Agent-factory args schema — `copilot_types:agent_definition/1` (`argsSchema`)
- Built-in plugin dirs / in-process — `copilot_types:client_options_params/1`

**v2.4.0**

- BYOK bearer-token provider — `bearer_token_provider` (`copilot_types` client-options record)
- MCP OAuth handler flag — `on_mcp_auth_request` → `mcpAuthHandler`
- HTTP request handler — `request_handler` (`copilot_types` client-options record)
- Session citations — `enable_citations` → `enableCitations`
- Excluded built-in agents — `excluded_builtin_agents` → `excludedBuiltinAgents`
- Session spending limits — `copilot_types:session_limits/1` (`maxAiCredits`)
- Session memory — `copilot_types:memory_configuration/1`
- OTLP protocol — `otlp_protocol` → `otlpProtocol`
- WebSocket responses — `enable_web_socket_responses` → `capi.enableWebSocketResponses`
- Experiment assignments — `exp_assignments` → `expAssignments`
- Tool defer loading — `copilot_types:tool_defer/1` (`auto` | `never`)
- System-message sections — `copilot_types:system_message_section/1` (`preamble` | `preserve`)
- Hook name constants — `copilot_types:hook_type/1` (`post_tool_use`, `pre_mcp_tool_call`, `user_prompt_transformed`)
- Message agent mode / display prompt — `agent_mode`, `display_prompt`
- GitHub attachment variants — `copilot_types:github_attachment/1` (`github_commit` | `github_repository`)

Reasoning effort, content exclusion, extra directories, and rewind:

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    reasoning_effort       => <<"high">>,
    content_exclusion      => true,
    additional_directories => [<<"../shared">>, <<"../docs">>],
    rewind_enabled         => true,
    disabled_mcp_servers   => [<<"playwright">>]
}).
```

Tool search, spending limits, memory, and citations:

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    tool_search       => #{<<"enabled">> => true},
    experimental_mode => true,
    session_limits    => copilot_types:session_limits(#{max_ai_credits => 5.0}),
    memory_config     => copilot_types:memory_configuration(#{enabled => true}),
    enable_citations  => true
}).
```

Transport-level client options (built-in plugin dirs, in-process FFI):

```erlang
Params = copilot_types:client_options_params(#{
    builtin_plugin_directories => [<<"./plugins">>],
    in_process                 => false
}).
%% => #{<<"builtinPluginDirectories">> => [<<"./plugins">>], <<"inProcess">> => false}
```

Permission decision context and wire-constant helpers:

```erlang
Reply = copilot_types:permission_result(approve_once, #{
    <<"outcome">> => <<"auto_approved">>,
    <<"source">>  => <<"host_policy">>,
    <<"surface">> => <<"sdk">>
}),
copilot_types:tool_defer(auto),                  %% <<"auto">>
copilot_types:system_message_section(preamble),  %% <<"preamble">>
copilot_types:github_attachment(github_commit).  %% <<"GitHubCommit">>
```

## Event Handling

Subscribe to session events using message passing:

```erlang
Ref = copilot_session:subscribe(Session, self()),

receive_loop() ->
    receive
        {copilot_session_event, _SessionId, #{type := <<"assistant.message">>, data := Data}} ->
            io:format("Assistant: ~p~n", [maps:get(<<"content">>, Data, <<>>)]),
            receive_loop();
        {copilot_session_event, _SessionId, #{type := <<"session.idle">>}} ->
            io:format("Session is idle~n"),
            ok;
        {copilot_session_event, _SessionId, Event} ->
            io:format("Event: ~p~n", [Event]),
            receive_loop()
    after 30000 ->
        io:format("Timeout waiting for events~n")
    end.
```

## Session Event Types

All event type constants are available via `copilot_types:session_event_type/1`:

```erlang
copilot_types:session_event_type(assistant_message).        %% <<"assistant.message">>
copilot_types:session_event_type(session_idle).             %% <<"session.idle">>
copilot_types:session_event_type(tool_execution_complete).  %% <<"tool.execution_complete">>
copilot_types:session_event_type(session_compaction_start). %% <<"session.compaction_start">>
```

## Protocol Version

The SDK speaks SDK protocol **4**, which requires Copilot CLI 1.0.113 or newer.

The client verifies protocol compatibility on startup via the `ping` response. A server that reports another version (or none) fails startup with `{error, {protocol_mismatch, Message}}`, where `Message` reads `SDK protocol version mismatch: SDK supports version 4, but server reports version N. Please update your SDK or server to ensure compatibility.`

## Running Tests

```bash
cd erlang
rebar3 ct
```

`test/copilot_cli_e2e_SUITE.erl` runs the real Copilot CLI through `copilot_client` against the shared replaying CAPI proxy (`test/harness/server.ts`) and the snapshots in `test/snapshots/`: it pings (SDK protocol 4), creates a session, sends a message, and waits for the reply and `session.idle`; it also approves a file edit and refuses a shell command through the permission flow. It needs Node.js and `npm install` in `nodejs/` and `test/harness/`; set `COPILOT_CLI_PATH` to use another CLI build. Without these prerequisites the suite is skipped locally, and it fails on GitHub Actions.

## API Reference

### copilot_client

| Function | Description |
|----------|-------------|
| `start_link()` | Start client with defaults |
| `start_link(Options)` | Start client with options |
| `stop(Pid)` | Stop client and clean up |
| `start_cli(Pid)` | Manually start CLI server |
| `ping(Pid)` | Ping server |
| `get_status(Pid)` | Get server status |
| `get_auth_status(Pid)` | Get authentication status |
| `list_models(Pid)` | List available models (cached) |
| `create_session(Pid, Config)` | Create a new session |
| `resume_session(Pid, Id, Config)` | Resume an existing session |
| `list_sessions(Pid)` | List all sessions |
| `delete_session(Pid, Id)` | Delete a session |
| `get_session_metadata(Pid, Id)` | Get session metadata |
| `clear_managed_settings_cache(Pid)` | Clear the managed-settings cache |
| `mcp_secrets_save(Pid, SecretId, Value[, Opts])` | Save an MCP secret; returns `#{used_keychain}` |
| `mcp_secrets_delete(Pid, SecretId[, Opts])` | Delete an MCP secret |
| `mcp_secrets_delete_server(Pid, ServerName[, Opts])` | Delete every stored secret of an MCP server |
| `extension_approvals_grant(Pid, SessionId, RequestId)` | Persist a pending plugin extension approval |
| `extension_approvals_list(Pid[, Opts])` | List plugin extension approvals |
| `extension_approvals_revoke(Pid, ExtensionName, InstallationKey[, Opts])` | Revoke a plugin extension approval |
| `state(Pid)` | Get connection state |

### copilot_session

| Function | Description |
|----------|-------------|
| `send(Pid, Options)` | Send a message |
| `send_and_wait(Pid, Options)` | Send and wait for idle |
| `send_and_wait(Pid, Options, Timeout)` | Send and wait with timeout |
| `send_and_wait_structured(Pid, PromptOrOptions, Schema)` | Send with a JSON Schema and decode the JSON reply |
| `subscribe(Pid, Subscriber)` | Subscribe to events (returns ref) |
| `unsubscribe(Pid, Ref)` | Unsubscribe from events |
| `get_messages(Pid)` | Get conversation history |
| `abort(Pid)` | Abort current processing |
| `set_auto_tier(Pid, Tier)` | Switch the Auto routing tier (`null` resets) |
| `get_managed_settings(Pid)` | Get effective managed settings |
| `recheck_policy_helper(Pid)` | Re-run the managed policy helper |
| `set_permission_mode(Pid, Mode)` | Change the permission mode; returns `#{success, mode, reason}` |
| `disconnect(Pid)` | Detach from the server session and stop the process |
| `session_id(Pid)` | Get session ID |

### copilot_tools

| Function | Description |
|----------|-------------|
| `define_tool(Name, Desc, Handler)` | Define a tool without params |
| `define_tool(Name, Desc, Params, Handler)` | Define a tool with JSON Schema |
| `define_tool(Name, Desc, Params, Handler, Options)` | Define a tool with declaration options such as `#{idempotent => true}` |
| `normalize_result(Value)` | Normalize a return value |

## License

MIT
