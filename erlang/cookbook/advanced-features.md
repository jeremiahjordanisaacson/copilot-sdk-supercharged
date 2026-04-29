# Advanced Features - Erlang

Patterns for streaming events, lifecycle hooks, BYOK providers, and image generation with the Copilot SDK in Erlang.

## Streaming Events

**Scenario:** Enable streaming to receive incremental response deltas.

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    streaming => true
}),

Ref = copilot_session:subscribe(Session, self()),

copilot_session:send(Session, #{prompt => <<"Write a haiku about Erlang.">>}),

stream_loop(<<>>) ->
    receive
        {copilot_session_event, _Id, #{type := <<"assistant.message_delta">>, data := Data}} ->
            Delta = maps:get(<<"content">>, Data, <<>>),
            io:format("~s", [Delta]),
            stream_loop(<<>>);
        {copilot_session_event, _Id, #{type := <<"assistant.message">>, data := Data}} ->
            FullContent = maps:get(<<"content">>, Data, <<>>),
            io:format("~nFull message: ~s~n", [FullContent]);
        {copilot_session_event, _Id, #{type := <<"session.idle">>}} ->
            io:format("~nStream complete.~n")
    after 30000 ->
        io:format("~nStream timeout.~n")
    end.
```

## Lifecycle Hooks

**Scenario:** Register hooks for observability and control flow.

```erlang
Hooks = #{
    on_pre_tool_use => fun(Data) ->
        ToolName = maps:get(<<"toolName">>, Data, <<>>),
        io:format("[HOOK] Pre-tool: ~s~n", [ToolName]),
        %% Return value is ignored for event-driven hooks
        ok
    end,
    on_post_tool_use => fun(Data) ->
        ToolName = maps:get(<<"toolName">>, Data, <<>>),
        io:format("[HOOK] Post-tool: ~s~n", [ToolName])
    end,
    on_session_start => fun(_Data) ->
        io:format("[HOOK] Session started~n")
    end,
    on_session_end => fun(_Data) ->
        io:format("[HOOK] Session ended~n")
    end,
    on_error_occurred => fun(Data) ->
        io:format("[HOOK] Error: ~p~n", [Data])
    end
},

{ok, Session} = copilot_client:create_session(Client, #{hooks => Hooks}).
```

## BYOK (Bring Your Own Key) Provider

**Scenario:** Use your own OpenAI-compatible API endpoint.

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    provider => #{
        <<"type">>    => <<"openai">>,
        <<"baseUrl">> => <<"https://api.example.com/v1">>,
        <<"apiKey">>  => <<"sk-your-key-here">>
    },
    model => <<"gpt-4">>
}).
```

## Image Generation

**Scenario:** Request image responses from the model.

```erlang
{ok, Response} = copilot_session:send_and_wait(Session, #{
    prompt          => <<"Generate a sunset over mountains">>,
    response_format => <<"image">>,
    image_options   => copilot_types:image_options(#{
        size    => <<"1024x1024">>,
        quality => <<"hd">>,
        style   => <<"natural">>
    })
}).
```

## MCP Server Integration

**Scenario:** Connect external MCP tool servers to your session.

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    mcp_servers => #{
        <<"file_server">> => #{
            <<"type">>    => <<"local">>,
            <<"command">> => <<"node">>,
            <<"args">>    => [<<"file-server.js">>],
            <<"tools">>   => [<<"read_file">>, <<"write_file">>]
        },
        <<"db_server">> => #{
            <<"type">>    => <<"local">>,
            <<"command">> => <<"python">>,
            <<"args">>    => [<<"db-server.py">>]
        }
    }
}).
```

## System Message Customization

**Scenario:** Append custom instructions to the system prompt.

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    system_message => #{
        <<"mode">>    => <<"append">>,
        <<"content">> => <<"Always respond in formal English. "
                           "Include code examples when relevant.">>
    }
}).
```

## Model Selection and Reasoning

**Scenario:** Choose a specific model and control reasoning effort.

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    model            => <<"gpt-4">>,
    reasoning_effort => <<"high">>
}).
```
