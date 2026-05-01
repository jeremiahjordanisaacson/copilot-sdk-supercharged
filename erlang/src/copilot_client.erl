%%%-------------------------------------------------------------------
%%% @doc Copilot CLI client gen_server.
%%%
%%% Manages the lifecycle of the Copilot CLI process, communicates
%%% over JSON-RPC 2.0 via stdio, and provides session management.
%%%
%%% Usage:
%%%   {ok, Client} = copilot_client:start_link(#{}),
%%%   {ok, Session} = copilot_client:create_session(Client, #{}),
%%%   copilot_client:stop(Client).
%%% @end
%%%-------------------------------------------------------------------
-module(copilot_client).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    start_cli/1,
    ping/1,
    ping/2,
    get_status/1,
    get_auth_status/1,
    list_models/1,
    create_session/2,
    resume_session/3,
    list_sessions/1,
    delete_session/2,
    get_session_metadata/2,
    get_last_session_id/1,
    get_foreground_session_id/1,
    set_foreground_session_id/2,
    state/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SDK_PROTOCOL_VERSION, 3).
-define(DEFAULT_TIMEOUT, 30000).

-record(state, {
    options       :: map(),
    cli_path      :: string(),
    cli_url       :: binary() | undefined,
    port          :: port() | undefined,
    rpc           :: pid() | undefined,
    conn_state    :: disconnected | connecting | connected | error,
    sessions      :: #{binary() => pid()},
    models_cache  :: [map()] | undefined,
    session_monitors :: #{reference() => binary()}
}).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid, normal, 10000).

-spec start_cli(pid()) -> ok | {error, term()}.
start_cli(Pid) ->
    gen_server:call(Pid, start_cli, ?DEFAULT_TIMEOUT).

-spec ping(pid()) -> {ok, map()} | {error, term()}.
ping(Pid) ->
    ping(Pid, <<"ping">>).

-spec ping(pid(), binary()) -> {ok, map()} | {error, term()}.
ping(Pid, Message) ->
    gen_server:call(Pid, {ping, Message}, ?DEFAULT_TIMEOUT).

-spec get_status(pid()) -> {ok, map()} | {error, term()}.
get_status(Pid) ->
    gen_server:call(Pid, get_status, ?DEFAULT_TIMEOUT).

-spec get_auth_status(pid()) -> {ok, map()} | {error, term()}.
get_auth_status(Pid) ->
    gen_server:call(Pid, get_auth_status, ?DEFAULT_TIMEOUT).

-spec list_models(pid()) -> {ok, [map()]} | {error, term()}.
list_models(Pid) ->
    gen_server:call(Pid, list_models, ?DEFAULT_TIMEOUT).

-spec create_session(pid(), map()) -> {ok, pid()} | {error, term()}.
create_session(Pid, Config) ->
    gen_server:call(Pid, {create_session, Config}, ?DEFAULT_TIMEOUT).

-spec resume_session(pid(), binary(), map()) -> {ok, pid()} | {error, term()}.
resume_session(Pid, SessionId, Config) ->
    gen_server:call(Pid, {resume_session, SessionId, Config}, ?DEFAULT_TIMEOUT).

-spec list_sessions(pid()) -> {ok, [map()]} | {error, term()}.
list_sessions(Pid) ->
    gen_server:call(Pid, list_sessions, ?DEFAULT_TIMEOUT).

-spec delete_session(pid(), binary()) -> ok | {error, term()}.
delete_session(Pid, SessionId) ->
    gen_server:call(Pid, {delete_session, SessionId}, ?DEFAULT_TIMEOUT).

-spec get_session_metadata(pid(), binary()) -> {ok, map()} | {error, term()}.
get_session_metadata(Pid, SessionId) ->
    gen_server:call(Pid, {get_session_metadata, SessionId}, ?DEFAULT_TIMEOUT).

-spec get_last_session_id(pid()) -> {ok, binary() | undefined} | {error, term()}.
get_last_session_id(Pid) ->
    gen_server:call(Pid, get_last_session_id, ?DEFAULT_TIMEOUT).

-spec get_foreground_session_id(pid()) -> {ok, binary()} | {error, term()}.
get_foreground_session_id(Pid) ->
    gen_server:call(Pid, get_foreground_session_id, ?DEFAULT_TIMEOUT).

-spec set_foreground_session_id(pid(), binary()) -> ok | {error, term()}.
set_foreground_session_id(Pid, SessionId) ->
    gen_server:call(Pid, {set_foreground_session_id, SessionId}, ?DEFAULT_TIMEOUT).

-spec state(pid()) -> disconnected | connecting | connected | error.
state(Pid) ->
    gen_server:call(Pid, get_state).

%% ---------------------------------------------------------------------------
%% gen_server callbacks
%% ---------------------------------------------------------------------------

init(Options) ->
    CliPath = case maps:get(cli_path, Options, undefined) of
        undefined ->
            case os:getenv("COPILOT_CLI_PATH") of
                false -> "copilot";
                Path  -> Path
            end;
        P -> binary_to_list(P)
    end,
    CliUrl = maps:get(cli_url, Options, undefined),
    AutoStart = maps:get(auto_start, Options, true),
    State = #state{
        options = Options,
        cli_path = CliPath,
        cli_url = CliUrl,
        conn_state = disconnected,
        sessions = #{},
        session_monitors = #{}
    },
    case AutoStart of
        true ->
            self() ! auto_start,
            {ok, State};
        false ->
            {ok, State}
    end.

handle_call(start_cli, _From, State) ->
    case do_start_cli(State) of
        {ok, NewState}    -> {reply, ok, NewState};
        {error, _} = Err  -> {reply, Err, State}
    end;

handle_call({ping, Message}, _From, #state{rpc = Rpc} = State) when Rpc =/= undefined ->
    Result = copilot_jsonrpc:request(Rpc, <<"ping">>, #{<<"message">> => Message}),
    {reply, Result, State};

handle_call(get_status, _From, #state{rpc = Rpc} = State) when Rpc =/= undefined ->
    Result = copilot_jsonrpc:request(Rpc, <<"getStatus">>, #{}),
    {reply, Result, State};

handle_call(get_auth_status, _From, #state{rpc = Rpc} = State) when Rpc =/= undefined ->
    Result = copilot_jsonrpc:request(Rpc, <<"getAuthStatus">>, #{}),
    {reply, Result, State};

handle_call(list_models, _From, #state{rpc = Rpc, models_cache = Cache} = State)
  when Rpc =/= undefined ->
    case Cache of
        undefined ->
            case copilot_jsonrpc:request(Rpc, <<"listModels">>, #{}) of
                {ok, Models} ->
                    {reply, {ok, Models}, State#state{models_cache = Models}};
                Error ->
                    {reply, Error, State}
            end;
        _ ->
            {reply, {ok, Cache}, State}
    end;

handle_call({create_session, Config}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    RpcParams = build_session_params(Config, State),
    case copilot_jsonrpc:request(Rpc, <<"session.create">>, RpcParams) of
        {ok, #{<<"sessionId">> := SessionId} = Result} ->
            WorkspacePath = maps:get(<<"workspacePath">>, Result, <<>>),
            {ok, SessionPid} = copilot_session:start_link(SessionId, Rpc, WorkspacePath, Config),
            MonRef = monitor(process, SessionPid),
            NewSessions = maps:put(SessionId, SessionPid, State#state.sessions),
            NewMonitors = maps:put(MonRef, SessionId, State#state.session_monitors),
            register_session_handlers(Rpc, SessionId, SessionPid, Config),
            {reply, {ok, SessionPid}, State#state{
                sessions = NewSessions,
                session_monitors = NewMonitors
            }};
        {error, _} = Err ->
            {reply, Err, State}
    end;

handle_call({resume_session, SessionId, Config}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Params = build_session_params(Config, State),
    ResumeParams = Params#{<<"sessionId">> => SessionId},
    case copilot_jsonrpc:request(Rpc, <<"session.resume">>, ResumeParams) of
        {ok, Result} ->
            WorkspacePath = maps:get(<<"workspacePath">>, Result, <<>>),
            {ok, SessionPid} = copilot_session:start_link(SessionId, Rpc, WorkspacePath, Config),
            MonRef = monitor(process, SessionPid),
            NewSessions = maps:put(SessionId, SessionPid, State#state.sessions),
            NewMonitors = maps:put(MonRef, SessionId, State#state.session_monitors),
            register_session_handlers(Rpc, SessionId, SessionPid, Config),
            {reply, {ok, SessionPid}, State#state{
                sessions = NewSessions,
                session_monitors = NewMonitors
            }};
        {error, _} = Err ->
            {reply, Err, State}
    end;

handle_call(list_sessions, _From, #state{rpc = Rpc} = State) when Rpc =/= undefined ->
    Result = copilot_jsonrpc:request(Rpc, <<"session.list">>, #{}),
    {reply, Result, State};

handle_call({delete_session, SessionId}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    case copilot_jsonrpc:request(Rpc, <<"session.delete">>, #{<<"sessionId">> => SessionId}) of
        {ok, _} ->
            case maps:get(SessionId, State#state.sessions, undefined) of
                undefined -> ok;
                SessionPid -> copilot_session:stop(SessionPid)
            end,
            NewSessions = maps:remove(SessionId, State#state.sessions),
            {reply, ok, State#state{sessions = NewSessions}};
        {error, _} = Err ->
            {reply, Err, State}
    end;

handle_call({get_session_metadata, SessionId}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Result = copilot_jsonrpc:request(Rpc, <<"session.getMetadata">>,
                                     #{<<"sessionId">> => SessionId}),
    {reply, Result, State};

handle_call(get_last_session_id, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    case copilot_jsonrpc:request(Rpc, <<"session.getLastId">>, #{}) of
        {ok, #{<<"sessionId">> := SessionId}} ->
            {reply, {ok, SessionId}, State};
        {ok, _} ->
            {reply, {ok, undefined}, State};
        {error, _} = Err ->
            {reply, Err, State}
    end;

handle_call(get_foreground_session_id, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Result = copilot_jsonrpc:request(Rpc, <<"session.getForegroundSessionId">>, #{}),
    {reply, Result, State};

handle_call({set_foreground_session_id, SessionId}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    case copilot_jsonrpc:request(Rpc, <<"session.setForegroundSessionId">>,
                                 #{<<"sessionId">> => SessionId}) of
        {ok, _} -> {reply, ok, State};
        Err     -> {reply, Err, State}
    end;

handle_call(get_state, _From, #state{conn_state = ConnState} = State) ->
    {reply, ConnState, State};

handle_call(_Request, _From, #state{rpc = undefined} = State) ->
    {reply, {error, not_connected}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auto_start, State) ->
    case do_start_cli(State) of
        {ok, NewState}   -> {noreply, NewState};
        {error, _Reason} -> {noreply, State#state{conn_state = error}}
    end;

handle_info({copilot_jsonrpc, notification, Method, Params}, State) ->
    NewState = dispatch_notification(Method, Params, State),
    {noreply, NewState};

handle_info({copilot_jsonrpc, port_closed}, State) ->
    {noreply, State#state{conn_state = disconnected, rpc = undefined, port = undefined}};

handle_info({'DOWN', MonRef, process, _Pid, _Reason}, State) ->
    case maps:take(MonRef, State#state.session_monitors) of
        {SessionId, NewMonitors} ->
            NewSessions = maps:remove(SessionId, State#state.sessions),
            {noreply, State#state{
                sessions = NewSessions,
                session_monitors = NewMonitors
            }};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port, sessions = Sessions}) ->
    maps:foreach(fun(_Id, Pid) ->
        catch copilot_session:stop(Pid)
    end, Sessions),
    case Port of
        undefined -> ok;
        _         -> catch port_close(Port)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

do_start_cli(#state{conn_state = connected} = State) ->
    {ok, State};
do_start_cli(#state{cli_path = CliPath, options = Options} = State) ->
    LogLevel = maps:get(log_level, Options, <<"info">>),
    Args = build_cli_args(CliPath, LogLevel, Options),
    Command = string:join(Args, " "),
    try
        Port = open_port({spawn, Command}, [
            binary,
            {line, 65536},
            use_stdio,
            exit_status,
            stderr_to_stdout
        ]),
        {ok, RpcPid} = copilot_jsonrpc:start_link(Port, self()),
        NewState = State#state{
            port = Port,
            rpc = RpcPid,
            conn_state = connecting
        },
        case verify_connection(RpcPid) of
            ok ->
                maybe_set_session_fs(RpcPid, Options),
                {ok, NewState#state{conn_state = connected}};
            {error, Reason} ->
                catch port_close(Port),
                {error, Reason}
        end
    catch
        _:Reason ->
            {error, {start_failed, Reason}}
    end.

build_cli_args(CliPath, LogLevel, Options) ->
    Base = [
        CliPath,
        "--headless",
        "--no-auto-update",
        "--log-level", binary_to_list(ensure_binary(LogLevel)),
        "--stdio"
    ],
    GithubToken = maps:get(github_token, Options, undefined),
    UseLoggedIn = maps:get(use_logged_in, Options, undefined),
    WithToken = case GithubToken of
        undefined -> Base;
        Token     -> Base ++ ["--github-token", binary_to_list(ensure_binary(Token))]
    end,
    case UseLoggedIn of
        true -> WithToken ++ ["--use-logged-in-user"];
        _    -> WithToken
    end.

verify_connection(RpcPid) ->
    case copilot_jsonrpc:request(RpcPid, <<"ping">>, #{<<"message">> => <<"init">>}) of
        {ok, #{<<"sdkProtocolVersion">> := Version}} ->
            case Version of
                ?SDK_PROTOCOL_VERSION -> ok;
                _ -> {error, {protocol_mismatch, Version, ?SDK_PROTOCOL_VERSION}}
            end;
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

build_session_params(Config, _State) ->
    Params = #{},
    Fields = [
        {model, <<"model">>},
        {reasoning_effort, <<"reasoningEffort">>},
        {system_message, <<"systemMessage">>},
        {available_tools, <<"availableTools">>},
        {excluded_tools, <<"excludedTools">>},
        {working_directory, <<"workingDirectory">>},
        {streaming, <<"streaming">>},
        {provider, <<"provider">>},
        {mcp_servers, <<"mcpServers">>},
        {infinite_sessions, <<"infiniteSessions">>},
        {skill_directories, <<"skillDirectories">>},
        {disabled_skills, <<"disabledSkills">>},
        {include_sub_agent_streaming_events, <<"includeSubAgentStreamingEvents">>},
        {model_capabilities, <<"modelCapabilities">>},
        {enable_config_discovery, <<"enableConfigDiscovery">>},
        {github_token, <<"gitHubToken">>}
    ],
    Params1 = lists:foldl(fun({Key, JsonKey}, Acc) ->
        case maps:get(Key, Config, undefined) of
            undefined -> Acc;
            Value     -> Acc#{JsonKey => Value}
        end
    end, Params, Fields),
    %% Wire commands (name + description only, handler stays client-side)
    Params2 = case maps:get(commands, Config, undefined) of
        undefined -> Params1;
        Commands ->
            CmdList = lists:map(fun(Cmd) ->
                Base = #{<<"name">> => maps:get(name, Cmd)},
                case maps:get(description, Cmd, undefined) of
                    undefined -> Base;
                    Desc      -> Base#{<<"description">> => Desc}
                end
            end, Commands),
            Params1#{<<"commands">> => CmdList}
    end,
    %% Wire requestElicitation flag
    case maps:get(on_elicitation_request, Config, undefined) of
        undefined -> Params2;
        _         -> Params2#{<<"requestElicitation">> => true}
    end.

register_session_handlers(Rpc, SessionId, SessionPid, Config) ->
    %% Register tool handlers
    Tools = maps:get(tools, Config, []),
    lists:foreach(fun(Tool) ->
        ToolName = maps:get(<<"name">>, Tool),
        Handler = maps:get(<<"handler">>, Tool),
        MethodName = <<"tools/call.", ToolName/binary>>,
        copilot_jsonrpc:set_request_handler(Rpc, MethodName, fun(Params) ->
            TargetSession = maps:get(<<"sessionId">>, Params, <<>>),
            case TargetSession of
                SessionId ->
                    Invocation = #{
                        <<"sessionId">>   => SessionId,
                        <<"toolCallId">>  => maps:get(<<"toolCallId">>, Params, <<>>),
                        <<"toolName">>    => ToolName,
                        <<"arguments">>   => maps:get(<<"arguments">>, Params, #{})
                    },
                    Handler(Invocation);
                _ ->
                    copilot_types:tool_result(<<"Session mismatch">>, failure)
            end
        end)
    end, Tools),
    %% Register permission handler
    case maps:get(on_permission_request, Config, undefined) of
        undefined -> ok;
        PermHandler ->
            copilot_jsonrpc:set_request_handler(Rpc, <<"permissionRequest">>,
                fun(Params) ->
                    case maps:get(<<"sessionId">>, Params, <<>>) of
                        SessionId -> PermHandler(Params);
                        _         -> copilot_types:permission_result(denied)
                    end
                end)
    end,
    %% Register user input handler
    case maps:get(on_user_input_request, Config, undefined) of
        undefined -> ok;
        InputHandler ->
            copilot_jsonrpc:set_request_handler(Rpc, <<"userInputRequest">>,
                fun(Params) ->
                    case maps:get(<<"sessionId">>, Params, <<>>) of
                        SessionId -> InputHandler(Params);
                        _         -> copilot_types:user_input_response(<<>>, false)
                    end
                end)
    end,
    %% Notify session process it is ready
    copilot_session:set_ready(SessionPid),
    ok.

dispatch_notification(<<"session.event">>, Params, State) ->
    SessionId = maps:get(<<"sessionId">>, Params, <<>>),
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined -> State;
        SessionPid ->
            EventType = maps:get(<<"type">>, Params, <<>>),
            EventData = maps:get(<<"data">>, Params, #{}),
            copilot_session:handle_event(SessionPid, EventType, EventData),
            State
    end;
dispatch_notification(_Method, _Params, State) ->
    State.

maybe_set_session_fs(RpcPid, Options) ->
    case maps:get(session_fs, Options, undefined) of
        undefined -> ok;
        FsConfig ->
            FsParams = maps:filter(fun(_K, V) -> V =/= undefined end, #{
                <<"initialCwd">>       => maps:get(initial_cwd, FsConfig),
                <<"sessionStatePath">> => maps:get(session_state_path, FsConfig),
                <<"conventions">>      => maps:get(conventions, FsConfig, <<>>)
            }),
            copilot_jsonrpc:request(RpcPid, <<"sessionFs.setProvider">>, FsParams),
            ok
    end.

ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_list(V)   -> list_to_binary(V);
ensure_binary(V) when is_atom(V)   -> atom_to_binary(V, utf8).
