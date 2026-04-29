%%%-------------------------------------------------------------------
%%% @doc Basic example demonstrating the Copilot SDK for Erlang.
%%%
%%% Run with:
%%%   cd erlang
%%%   rebar3 shell
%%%   basic_example:run().
%%% @end
%%%-------------------------------------------------------------------
-module(basic_example).

-export([run/0]).

run() ->
    io:format("Starting Copilot client...~n"),

    %% Start the client with default options
    {ok, Client} = copilot_client:start_link(#{
        log_level => <<"info">>
    }),

    %% Check connection state
    State = copilot_client:state(Client),
    io:format("Connection state: ~p~n", [State]),

    %% Create a session with a custom tool
    WeatherTool = copilot_tools:define_tool(
        <<"get_weather">>,
        <<"Get current weather for a city">>,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"city">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"City name">>
                }
            },
            <<"required">> => [<<"city">>]
        },
        fun(Args, _Invocation) ->
            City = maps:get(<<"city">>, Args, <<"unknown">>),
            iolist_to_binary(io_lib:format("Sunny, 22C in ~s", [City]))
        end
    ),

    {ok, Session} = copilot_client:create_session(Client, #{
        model => <<"gpt-4">>,
        tools => [WeatherTool],
        streaming => true
    }),

    %% Subscribe to events
    Ref = copilot_session:subscribe(Session, self()),

    %% Send a message and wait for the response
    io:format("Sending message...~n"),
    case copilot_session:send_and_wait(Session, #{
        prompt => <<"What is the weather in San Francisco?">>
    }) of
        {ok, #{data := Data}} ->
            Content = maps:get(<<"content">>, Data, <<"no content">>),
            io:format("Response: ~s~n", [Content]);
        {ok, Response} ->
            io:format("Response: ~p~n", [Response]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,

    %% Listen for streaming events
    listen_events(3000),

    %% Clean up
    copilot_session:unsubscribe(Session, Ref),
    copilot_session:stop(Session),
    copilot_client:stop(Client),

    io:format("Done.~n").

listen_events(Timeout) ->
    receive
        {copilot_session_event, SessionId, #{type := Type, data := Data}} ->
            io:format("[~s] ~s: ~p~n", [SessionId, Type, Data]),
            listen_events(Timeout)
    after Timeout ->
        ok
    end.
