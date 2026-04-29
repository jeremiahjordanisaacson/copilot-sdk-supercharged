# OTP Patterns - Erlang

Patterns for integrating the Copilot SDK into OTP applications with proper supervision, application structure, and distributed Erlang features.

## Application Module

**Scenario:** Package the Copilot client as a proper OTP application.

```erlang
-module(my_copilot_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    my_copilot_sup:start_link().

stop(_State) ->
    ok.
```

## Supervision Tree

**Scenario:** Build a supervision tree that manages the client and session workers.

```erlang
-module(my_copilot_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ClientSpec = #{
        id       => copilot_client,
        start    => {copilot_client, start_link, [#{
            log_level  => <<"info">>,
            auto_start => true
        }]},
        restart  => permanent,
        shutdown => 10000,
        type     => worker,
        modules  => [copilot_client]
    },
    SessionSupSpec = #{
        id       => session_sup,
        start    => {session_supervisor, start_link, []},
        restart  => permanent,
        shutdown => infinity,
        type     => supervisor,
        modules  => [session_supervisor]
    },
    Strategy = #{
        strategy  => rest_for_one,
        intensity => 5,
        period    => 60
    },
    {ok, {Strategy, [ClientSpec, SessionSupSpec]}}.
```

## Dynamic Session Supervisor

**Scenario:** Use a simple_one_for_one supervisor to manage sessions dynamically.

```erlang
-module(session_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_session/2, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpec = #{
        id       => session_worker,
        start    => {session_worker, start_link, []},
        restart  => temporary,
        shutdown => 5000,
        type     => worker,
        modules  => [session_worker]
    },
    {ok, {#{strategy => simple_one_for_one}, [ChildSpec]}}.

start_session(ClientPid, Config) ->
    supervisor:start_child(?MODULE, [ClientPid, Config]).
```

## Session Worker with gen_server

**Scenario:** Wrap session interactions in a gen_server for clean lifecycle management.

```erlang
-module(session_worker).
-behaviour(gen_server).

-export([start_link/2, ask/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(ClientPid, Config) ->
    gen_server:start_link(?MODULE, {ClientPid, Config}, []).

ask(Pid, Prompt) ->
    gen_server:call(Pid, {ask, Prompt}, 60000).

init({ClientPid, Config}) ->
    case copilot_client:create_session(ClientPid, Config) of
        {ok, Session} ->
            _Ref = copilot_session:subscribe(Session, self()),
            {ok, #{client => ClientPid, session => Session}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({ask, Prompt}, _From, #{session := Session} = State) ->
    Result = copilot_session:send_and_wait(Session, #{prompt => Prompt}),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({copilot_session_event, _Id, Event}, State) ->
    io:format("Session event: ~p~n", [maps:get(type, Event, unknown)]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{session := Session}) ->
    copilot_session:stop(Session),
    ok.
```

## ETS-Based Session Registry

**Scenario:** Track active sessions using ETS for fast lookup.

```erlang
-module(session_registry).
-export([init/0, register/2, lookup/1, unregister/1, all/0]).

init() ->
    ets:new(copilot_sessions, [named_table, public, set, {keypos, 1}]).

register(SessionId, Pid) ->
    ets:insert(copilot_sessions, {SessionId, Pid, erlang:system_time(second)}).

lookup(SessionId) ->
    case ets:lookup(copilot_sessions, SessionId) of
        [{_, Pid, _}] -> {ok, Pid};
        []            -> {error, not_found}
    end.

unregister(SessionId) ->
    ets:delete(copilot_sessions, SessionId).

all() ->
    ets:tab2list(copilot_sessions).
```

## Distributed Session Management

**Scenario:** Run sessions across multiple Erlang nodes.

```erlang
%% On node 'a@host':
{ok, Client} = copilot_client:start_link(#{}),
{ok, Session} = copilot_client:create_session(Client, #{}),
global:register_name(copilot_session_1, Session),

%% On node 'b@host':
SessionPid = global:whereis_name(copilot_session_1),
{ok, Response} = copilot_session:send_and_wait(SessionPid, #{
    prompt => <<"Hello from another node!">>
}).
```
