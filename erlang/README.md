# Copilot Supercharged SDK for Erlang

An Erlang/OTP SDK for interacting with the GitHub Copilot CLI server. Communicates via JSON-RPC 2.0 over stdio using Content-Length header framing. Built with OTP gen_server behaviours and idiomatic Erlang patterns.

## Requirements

- Erlang/OTP 25+
- [rebar3](https://rebar3.org/) build tool
- [jsx](https://hex.pm/packages/jsx) for JSON encoding/decoding
- The `copilot` CLI installed and available on `PATH` (or specify the path via `cli_path`)

## Installation

### Via Hex

Add to your `rebar.config` deps:

```erlang
{deps, [
    {copilot_sdk, "2.0.0"}
]}.
```

Then run:

```bash
rebar3 compile
```

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

## Architecture

The SDK follows OTP principles with gen_server behaviours:

```
copilot_client (gen_server)
    |
    |-- spawns CLI process with --headless --no-auto-update --stdio
    |-- manages copilot_jsonrpc for communication
    |-- dispatches incoming server requests to sessions
    |
    +-- copilot_session (gen_server)
            |-- event subscriptions (subscribe / unsubscribe)
            |-- send / send_and_wait
            |-- tool handlers
            |-- permission handler
            |-- user input handler
            |-- hooks (pre/post tool use, session start/end, etc.)
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
| `copilot_jsonrpc` | JSON-RPC 2.0 gen_server over stdio transport |
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

Register a handler to approve or deny tool permission requests:

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    on_permission_request => fun(Request) ->
        io:format("Permission requested: ~p~n", [maps:get(<<"kind">>, Request)]),
        copilot_types:permission_result(approved)
    end
}).
```

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

## Hooks

Register lifecycle hooks for observability:

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
    cli_path     => <<"/usr/local/bin/copilot">>,
    cwd          => <<"/path/to/project">>,
    log_level    => <<"debug">>,
    github_token => <<"ghp_...">>,
    use_logged_in => true,
    auto_start   => true,
    auto_restart => true,
    session_idle_timeout_seconds => 300
}).
```

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
    on_permission_request => fun(_) -> copilot_types:permission_result(approved) end,
    on_user_input_request => fun(_) -> copilot_types:user_input_response(<<"ok">>, true) end,
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
- Like the other Erlang hooks, the sub-agent hooks are observers dispatched from the
  `subagent.started` / `subagent.completed` events; their return values are ignored.

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
Reply = copilot_types:permission_result(approved, #{<<"source">> => <<"policy">>}),
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

The SDK protocol version must match the server. The current version is **3**.

The client automatically verifies protocol compatibility on startup via the `ping` response.

## Running Tests

```bash
cd erlang
rebar3 ct
```

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
| `disconnect(Pid)` | Detach from the server session and stop the process |
| `session_id(Pid)` | Get session ID |

### copilot_tools

| Function | Description |
|----------|-------------|
| `define_tool(Name, Desc, Handler)` | Define a tool without params |
| `define_tool(Name, Desc, Params, Handler)` | Define a tool with JSON Schema |
| `normalize_result(Value)` | Normalize a return value |

## License

MIT
