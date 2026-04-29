# Error Handling - Erlang

Patterns for handling errors gracefully in the Copilot SDK for Erlang, using OTP idioms like supervisors, try/catch, and {ok, Result}/{error, Reason} tuples.

## Basic Error Handling with Pattern Matching

**Scenario:** Handle SDK errors using Erlang's standard result tuples.

```erlang
{ok, Client} = copilot_client:start_link(#{auto_start => false}),

%% Pattern match on start result
case copilot_client:start_cli(Client) of
    ok ->
        io:format("CLI started successfully~n");
    {error, {start_failed, Reason}} ->
        io:format("Failed to start CLI: ~p~n", [Reason]),
        copilot_client:stop(Client),
        erlang:halt(1);
    {error, {protocol_mismatch, Got, Expected}} ->
        io:format("Protocol mismatch: got ~p, expected ~p~n", [Got, Expected]),
        copilot_client:stop(Client),
        erlang:halt(1)
end.
```

## Session Error Recovery

**Scenario:** Handle session creation and message sending failures.

```erlang
create_session_safe(Client) ->
    case copilot_client:create_session(Client, #{model => <<"gpt-4">>}) of
        {ok, Session} ->
            {ok, Session};
        {error, Reason} ->
            io:format("Session creation failed: ~p, retrying...~n", [Reason]),
            timer:sleep(2000),
            copilot_client:create_session(Client, #{model => <<"gpt-4">>})
    end.

send_with_retry(Session, Prompt, MaxRetries) ->
    send_with_retry(Session, Prompt, MaxRetries, 0).

send_with_retry(_Session, _Prompt, MaxRetries, Attempt) when Attempt >= MaxRetries ->
    {error, max_retries_exceeded};
send_with_retry(Session, Prompt, MaxRetries, Attempt) ->
    case copilot_session:send_and_wait(Session, #{prompt => Prompt}) of
        {ok, Response} ->
            {ok, Response};
        {error, timeout} ->
            io:format("Attempt ~p timed out, retrying...~n", [Attempt + 1]),
            timer:sleep(1000 * (Attempt + 1)),
            send_with_retry(Session, Prompt, MaxRetries, Attempt + 1);
        {error, Reason} ->
            {error, Reason}
    end.
```

## Using a Supervisor

**Scenario:** Wrap the client in an OTP supervisor for automatic restart.

```erlang
-module(copilot_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpec = #{
        id       => copilot_client,
        start    => {copilot_client, start_link, [#{log_level => <<"info">>}]},
        restart  => permanent,
        shutdown => 5000,
        type     => worker,
        modules  => [copilot_client]
    },
    {ok, {#{strategy => one_for_one, intensity => 3, period => 60}, [ChildSpec]}}.
```

## Handling Port Crashes

**Scenario:** Detect and handle CLI process crashes gracefully.

```erlang
%% The client receives {copilot_jsonrpc, port_closed} when the CLI exits.
%% Subscribe to check connection state periodically:

monitor_connection(Client) ->
    spawn_link(fun() -> monitor_loop(Client) end).

monitor_loop(Client) ->
    timer:sleep(5000),
    case copilot_client:state(Client) of
        connected ->
            monitor_loop(Client);
        disconnected ->
            io:format("Connection lost, attempting restart...~n"),
            case copilot_client:start_cli(Client) of
                ok -> io:format("Reconnected~n");
                {error, R} -> io:format("Reconnect failed: ~p~n", [R])
            end,
            monitor_loop(Client);
        error ->
            io:format("Client in error state~n"),
            monitor_loop(Client)
    end.
```

## Tool Error Handling

**Scenario:** Handle errors within tool handlers so failures are reported cleanly.

```erlang
SafeTool = copilot_tools:define_tool(
    <<"safe_operation">>,
    <<"An operation that handles its own errors">>,
    fun(Args, _Inv) ->
        try
            Value = maps:get(<<"input">>, Args),
            %% Might throw if input is invalid
            Result = process_value(Value),
            iolist_to_binary(io_lib:format("~p", [Result]))
        catch
            error:badarg ->
                copilot_types:tool_result(<<"Invalid input">>, failure);
            _:Reason ->
                copilot_types:tool_result(
                    iolist_to_binary(io_lib:format("Unexpected error: ~p", [Reason])),
                    failure
                )
        end
    end
).
```
