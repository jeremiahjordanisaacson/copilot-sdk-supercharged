# Multiple Sessions - Erlang

Patterns for running concurrent sessions with the Copilot SDK in Erlang. OTP's lightweight processes make parallel sessions natural.

## Running Two Sessions in Parallel

**Scenario:** Create two sessions that process different prompts concurrently.

```erlang
{ok, Client} = copilot_client:start_link(#{}),

%% Create two sessions
{ok, Session1} = copilot_client:create_session(Client, #{model => <<"gpt-4">>}),
{ok, Session2} = copilot_client:create_session(Client, #{model => <<"gpt-4">>}),

%% Send messages concurrently using spawned processes
Self = self(),

spawn(fun() ->
    Result = copilot_session:send_and_wait(Session1, #{
        prompt => <<"Explain pattern matching in Erlang.">>
    }),
    Self ! {session1_result, Result}
end),

spawn(fun() ->
    Result = copilot_session:send_and_wait(Session2, #{
        prompt => <<"Explain OTP supervisors.">>
    }),
    Self ! {session2_result, Result}
end),

%% Collect results
receive {session1_result, R1} -> io:format("Session 1: ~p~n", [R1]) end,
receive {session2_result, R2} -> io:format("Session 2: ~p~n", [R2]) end,

%% Clean up
copilot_session:stop(Session1),
copilot_session:stop(Session2),
copilot_client:stop(Client).
```

## Session Pool with Worker Processes

**Scenario:** Manage a pool of sessions for handling multiple requests.

```erlang
-module(session_pool).
-export([start/2, send_to_pool/2, stop/1]).

start(Client, PoolSize) ->
    Sessions = lists:map(fun(_) ->
        {ok, Session} = copilot_client:create_session(Client, #{model => <<"gpt-4">>}),
        Session
    end, lists:seq(1, PoolSize)),
    {ok, Sessions}.

send_to_pool(Sessions, Prompts) ->
    Self = self(),
    %% Zip sessions with prompts (round-robin if more prompts than sessions)
    Pairs = lists:zip(
        lists:sublist(cycle(Sessions, length(Prompts)), length(Prompts)),
        Prompts
    ),
    lists:foreach(fun({Session, Prompt}) ->
        spawn(fun() ->
            Result = copilot_session:send_and_wait(Session, #{prompt => Prompt}),
            Self ! {pool_result, Prompt, Result}
        end)
    end, Pairs),
    %% Collect all results
    [receive {pool_result, P, R} -> {P, R} end || _ <- Prompts].

stop(Sessions) ->
    lists:foreach(fun(S) -> copilot_session:stop(S) end, Sessions).

cycle(List, N) ->
    lists:flatten(lists:duplicate(1 + (N div length(List)), List)).
```

## Per-Session Event Routing

**Scenario:** Subscribe to events from multiple sessions and route them by session ID.

```erlang
{ok, Session1} = copilot_client:create_session(Client, #{}),
{ok, Session2} = copilot_client:create_session(Client, #{}),

_Ref1 = copilot_session:subscribe(Session1, self()),
_Ref2 = copilot_session:subscribe(Session2, self()),

Id1 = copilot_session:session_id(Session1),
Id2 = copilot_session:session_id(Session2),

event_router() ->
    receive
        {copilot_session_event, SessionId, Event} when SessionId =:= Id1 ->
            io:format("[Session A] ~p~n", [Event]),
            event_router();
        {copilot_session_event, SessionId, Event} when SessionId =:= Id2 ->
            io:format("[Session B] ~p~n", [Event]),
            event_router();
        stop ->
            ok
    end.
```

## Listing and Cleaning Up Sessions

**Scenario:** List all active sessions and clean up stale ones.

```erlang
{ok, Sessions} = copilot_client:list_sessions(Client),
lists:foreach(fun(#{<<"sessionId">> := Id, <<"status">> := Status}) ->
    io:format("Session ~s: ~s~n", [Id, Status]),
    case Status of
        <<"idle">> ->
            ok;
        <<"error">> ->
            io:format("  Deleting errored session ~s~n", [Id]),
            copilot_client:delete_session(Client, Id);
        _ ->
            ok
    end
end, Sessions).
```
