# Tools and Skills - Erlang

Patterns for defining custom tools, registering skills, and orchestrating sub-agents with the Copilot SDK in Erlang.

## Defining a Simple Tool

**Scenario:** Expose an Erlang function as a tool that the Copilot model can call during a conversation.

```erlang
{ok, Client} = copilot_client:start_link(#{}),

%% Define a tool that returns the current timestamp
TimeTool = copilot_tools:define_tool(
    <<"get_current_time">>,
    <<"Returns the current date and time">>,
    fun(_Args, _Invocation) ->
        {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
        iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                        [Y, M, D, H, Mi, S]))
    end
),

{ok, Session} = copilot_client:create_session(Client, #{
    tools => [TimeTool]
}),

{ok, Response} = copilot_session:send_and_wait(Session, #{
    prompt => <<"What is the current date and time?">>
}),
io:format("Response: ~p~n", [Response]).
```

## Tool with Parameters

**Scenario:** Define a tool that accepts structured input parameters from the model.

```erlang
CalcTool = copilot_tools:define_tool(
    <<"calculate">>,
    <<"Performs basic arithmetic operations">>,
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"a">>  => #{<<"type">> => <<"number">>, <<"description">> => <<"First operand">>},
            <<"b">>  => #{<<"type">> => <<"number">>, <<"description">> => <<"Second operand">>},
            <<"op">> => #{<<"type">> => <<"string">>,
                          <<"enum">> => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>]}
        },
        <<"required">> => [<<"a">>, <<"b">>, <<"op">>]
    },
    fun(Args, _Invocation) ->
        A  = maps:get(<<"a">>, Args),
        B  = maps:get(<<"b">>, Args),
        Op = maps:get(<<"op">>, Args),
        Result = case Op of
            <<"add">>      -> A + B;
            <<"subtract">> -> A - B;
            <<"multiply">> -> A * B;
            <<"divide">>   when B =/= 0 -> A / B;
            <<"divide">>   -> error(division_by_zero)
        end,
        iolist_to_binary(io_lib:format("~p", [Result]))
    end
).
```

## Multiple Tools

**Scenario:** Register several tools for a single session.

```erlang
ReadTool = copilot_tools:define_tool(
    <<"read_file">>,
    <<"Read contents of a file">>,
    #{<<"type">> => <<"object">>,
      <<"properties">> => #{
          <<"path">> => #{<<"type">> => <<"string">>}
      },
      <<"required">> => [<<"path">>]
    },
    fun(Args, _Inv) ->
        Path = binary_to_list(maps:get(<<"path">>, Args)),
        case file:read_file(Path) of
            {ok, Content} -> Content;
            {error, Reason} ->
                copilot_types:tool_result(
                    iolist_to_binary(io_lib:format("Error: ~p", [Reason])),
                    failure
                )
        end
    end
),

ListTool = copilot_tools:define_tool(
    <<"list_files">>,
    <<"List files in a directory">>,
    #{<<"type">> => <<"object">>,
      <<"properties">> => #{
          <<"dir">> => #{<<"type">> => <<"string">>}
      },
      <<"required">> => [<<"dir">>]
    },
    fun(Args, _Inv) ->
        Dir = binary_to_list(maps:get(<<"dir">>, Args)),
        case file:list_dir(Dir) of
            {ok, Files} -> jsx:encode(Files);
            {error, Reason} ->
                copilot_types:tool_result(
                    iolist_to_binary(io_lib:format("Error: ~p", [Reason])),
                    failure
                )
        end
    end
),

{ok, Session} = copilot_client:create_session(Client, #{
    tools => [ReadTool, ListTool]
}).
```

## Registering Skills

**Scenario:** Point the session at directories containing skill definitions.

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    skill_directories => [<<"./skills">>, <<"/opt/shared-skills">>],
    disabled_skills   => [<<"experimental-skill">>],
    include_sub_agent_streaming_events => true
}).
```
