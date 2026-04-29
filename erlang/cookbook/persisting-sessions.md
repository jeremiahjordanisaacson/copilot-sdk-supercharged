# Persisting Sessions - Erlang

Patterns for using infinite sessions and session persistence with the Copilot SDK in Erlang.

## Enabling Infinite Sessions

**Scenario:** Keep conversation context across compaction boundaries so sessions can run indefinitely.

```erlang
{ok, Client} = copilot_client:start_link(#{}),

{ok, Session} = copilot_client:create_session(Client, #{
    infinite_sessions => #{
        <<"enabled">> => true,
        <<"backgroundCompactionThreshold">> => 0.80,
        <<"bufferExhaustionThreshold">>     => 0.95
    }
}).
```

## Monitoring Compaction Events

**Scenario:** Subscribe to compaction events to track when the context window is compacted.

```erlang
Ref = copilot_session:subscribe(Session, self()),

compaction_monitor() ->
    receive
        {copilot_session_event, _Id, #{type := <<"session.compaction_start">>}} ->
            io:format("Compaction started - context is being summarized~n"),
            compaction_monitor();
        {copilot_session_event, _Id, #{type := <<"session.compaction_complete">>}} ->
            io:format("Compaction complete - context preserved~n"),
            compaction_monitor();
        {copilot_session_event, _Id, _Event} ->
            compaction_monitor();
        stop ->
            ok
    end.
```

## Resuming a Previous Session

**Scenario:** Resume a previously created session by its ID.

```erlang
%% Save the session ID somewhere persistent (e.g., a file or database)
SessionId = copilot_session:session_id(Session),
file:write_file("/var/data/last_session.txt", SessionId),

%% Later, resume the session
{ok, SavedId} = file:read_file("/var/data/last_session.txt"),
{ok, ResumedSession} = copilot_client:resume_session(Client, SavedId, #{
    model => <<"gpt-4">>,
    infinite_sessions => #{<<"enabled">> => true}
}),

{ok, Response} = copilot_session:send_and_wait(ResumedSession, #{
    prompt => <<"Continue where we left off.">>
}).
```

## Session Metadata

**Scenario:** Retrieve metadata about a session to check its state.

```erlang
SessionId = copilot_session:session_id(Session),
{ok, Metadata} = copilot_client:get_session_metadata(Client, SessionId),
io:format("Model: ~p~n", [maps:get(<<"model">>, Metadata, <<"unknown">>)]),
io:format("Created: ~p~n", [maps:get(<<"createdAt">>, Metadata, <<"unknown">>)]).
```

## Session Filesystem (SessionFs)

**Scenario:** Configure persistent session filesystem for state across compaction.

```erlang
{ok, Session} = copilot_client:create_session(Client, #{
    working_directory => <<"/repo">>,
    infinite_sessions => #{
        <<"enabled">> => true,
        <<"backgroundCompactionThreshold">> => 0.80
    }
}),

%% The workspace path is available on the session
WorkspacePath = copilot_session:workspace_path(Session),
io:format("Session state stored at: ~s~n", [WorkspacePath]).
```

## Idle Timeout

**Scenario:** Automatically clean up sessions after a period of inactivity.

```erlang
{ok, Client} = copilot_client:start_link(#{
    session_idle_timeout_seconds => 300
}).
```
