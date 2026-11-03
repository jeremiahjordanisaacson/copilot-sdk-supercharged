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
    clear_managed_settings_cache/1,
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
    session_monitors :: #{reference() => binary()},
    %% Client-level GitHub token provider registry:
    %% {RegistrationId, Provider, SessionId | undefined}
    github_token_providers :: ets:tid(),
    %% Session pid => GitHub token provider registration id
    token_registrations :: #{pid() => binary()}
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

%% @doc Clear the runtime's managed-settings cache (`managedSettings.clearCache`).
-spec clear_managed_settings_cache(pid()) -> {ok, term()} | {error, term()}.
clear_managed_settings_cache(Pid) ->
    gen_server:call(Pid, clear_managed_settings_cache, ?DEFAULT_TIMEOUT).

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
        session_monitors = #{},
        github_token_providers = ets:new(copilot_github_token_providers, [set, protected]),
        token_registrations = #{}
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
    Result = copilot_jsonrpc:request(Rpc, <<"status.get">>, #{}),
    {reply, Result, State};

handle_call(get_auth_status, _From, #state{rpc = Rpc} = State) when Rpc =/= undefined ->
    Result = copilot_jsonrpc:request(Rpc, <<"auth.getStatus">>, #{}),
    {reply, Result, State};

handle_call(list_models, _From, #state{rpc = Rpc, models_cache = Cache} = State)
  when Rpc =/= undefined ->
    case Cache of
        undefined ->
            case copilot_jsonrpc:request(Rpc, <<"models.list">>, #{}) of
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
    case validate_session_config(Config) of
        {error, _} = Invalid ->
            {reply, Invalid, State};
        ok ->
            RegId = register_github_token_provider(Config, undefined, State),
            RpcParams = with_registration_id(RegId, build_session_params(create, Config, State)),
            case copilot_jsonrpc:request(Rpc, <<"session.create">>, RpcParams) of
                {ok, #{<<"sessionId">> := SessionId} = Result} ->
                    WorkspacePath = maps:get(<<"workspacePath">>, Result, <<>>),
                    {ok, SessionPid} = copilot_session:start_link(SessionId, Rpc, WorkspacePath, Config),
                    MonRef = monitor(process, SessionPid),
                    NewSessions = maps:put(SessionId, SessionPid, State#state.sessions),
                    NewMonitors = maps:put(MonRef, SessionId, State#state.session_monitors),
                    register_session_handlers(Rpc, SessionId, SessionPid, Config),
                    assign_github_token_provider(RegId, SessionId, State),
                    {reply, {ok, SessionPid}, State#state{
                        sessions = NewSessions,
                        session_monitors = NewMonitors,
                        token_registrations = track_registration(RegId, SessionPid, State)
                    }};
                {error, _} = Err ->
                    unregister_github_token_provider(RegId, State),
                    {reply, Err, State}
            end
    end;

handle_call({resume_session, SessionId, Config}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    case validate_session_config(Config) of
        {error, _} = Invalid ->
            {reply, Invalid, State};
        ok ->
            RegId = register_github_token_provider(Config, SessionId, State),
            Params = with_registration_id(RegId, build_session_params(resume, Config, State)),
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
                        session_monitors = NewMonitors,
                        token_registrations = track_registration(RegId, SessionPid, State)
                    }};
                {error, _} = Err ->
                    unregister_github_token_provider(RegId, State),
                    {reply, Err, State}
            end
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
    Result = copilot_jsonrpc:request(Rpc, <<"session.getForeground">>, #{}),
    {reply, Result, State};

handle_call({set_foreground_session_id, SessionId}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    case copilot_jsonrpc:request(Rpc, <<"session.setForeground">>,
                                 #{<<"sessionId">> => SessionId}) of
        {ok, _} -> {reply, ok, State};
        Err     -> {reply, Err, State}
    end;

handle_call(clear_managed_settings_cache, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Result = copilot_jsonrpc:request(Rpc, <<"managedSettings.clearCache">>, #{}),
    {reply, Result, State};

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
    clear_github_token_providers(State),
    {noreply, State#state{conn_state = disconnected, rpc = undefined, port = undefined,
                          token_registrations = #{}}};

handle_info({'DOWN', MonRef, process, Pid, _Reason}, State0) ->
    State = release_token_registration(Pid, State0),
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

terminate(_Reason, #state{port = Port, sessions = Sessions} = State) ->
    maps:fold(fun(_Id, Pid, _) ->
        catch copilot_session:stop(Pid),
        ok
    end, ok, Sessions),
    clear_github_token_providers(State),
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
        register_client_handlers(RpcPid, State),
        NewState = State#state{
            port = Port,
            rpc = RpcPid,
            conn_state = connecting
        },
        case connect_and_verify(RpcPid, Options) of
            ok ->
                maybe_set_session_fs(RpcPid, Options),
                {ok, NewState#state{conn_state = connected}};
            {error, Reason} ->
                catch port_close(Port),
                {error, Reason}
        end
    catch
        _:CrashReason ->
            {error, {start_failed, CrashReason}}
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
    WithToken2 = case UseLoggedIn of
        true -> WithToken ++ ["--use-logged-in-user"];
        _    -> WithToken
    end,
    case maps:get(remote, Options, undefined) of
        true -> WithToken2 ++ ["--remote"];
        _    -> WithToken2
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

%% Send the optional `connect` handshake (client_info), then run the existing
%% ping verification unchanged.
connect_and_verify(RpcPid, Options) ->
    case maybe_send_connect(RpcPid, Options) of
        ok    -> verify_connection(RpcPid);
        Error -> Error
    end.

%% Declare the integrating application's identity before the ping handshake.
%% Skipped when client_info is unset or all-empty; older CLIs that do not know
%% `connect` (-32601 / "Unhandled method connect") are tolerated.
maybe_send_connect(RpcPid, Options) ->
    case copilot_types:client_info(maps:get(client_info, Options, undefined)) of
        ClientInfo when map_size(ClientInfo) =:= 0 ->
            ok;
        ClientInfo ->
            case copilot_jsonrpc:request(RpcPid, <<"connect">>,
                                         #{<<"clientInfo">> => ClientInfo}) of
                {ok, _} ->
                    ok;
                {error, Error} ->
                    case is_unhandled_connect(Error) of
                        true  -> ok;
                        false -> {error, Error}
                    end
            end
    end.

is_unhandled_connect(#{<<"code">> := -32601}) ->
    true;
is_unhandled_connect(#{<<"message">> := Message}) when is_binary(Message) ->
    binary:match(Message, <<"Unhandled method connect">>) =/= nomatch;
is_unhandled_connect(_) ->
    false.

build_session_params(Mode, Config, _State) ->
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
        {github_token, <<"gitHubToken">>},
        {enable_citations, <<"enableCitations">>},
        {excluded_builtin_agents, <<"excludedBuiltinAgents">>},
        {session_limits, <<"sessionLimits">>},
        {memory_config, <<"memory">>},
        {otlp_protocol, <<"otlpProtocol">>},
        {exp_assignments, <<"expAssignments">>},
        {rewind_enabled, <<"rewindEnabled">>},
        {additional_directories, <<"additionalDirectories">>},
        {disabled_mcp_servers, <<"disabledMcpServers">>},
        {github_mcp_tool_config, <<"githubMcpToolConfig">>},
        {canvas_provider, <<"canvasProvider">>},
        {custom_agents_local_only, <<"customAgentsLocalOnly">>},
        {tool_search, <<"toolSearch">>},
        {experimental_mode, <<"experimentalMode">>},
        {content_exclusion, <<"contentExclusion">>},
        %% v2.6.0 options sent on both create and resume
        {ask_user_variant, <<"askUserVariant">>},
        {sandbox_config, <<"sandboxConfig">>},
        {feature_flags, <<"featureFlags">>},
        {allowed_models, <<"allowedModels">>},
        {auth_client_id_metadata_url, <<"authClientIdMetadataUrl">>},
        {agent_directories, <<"agentDirectories">>},
        {hook_directories, <<"hookDirectories">>},
        {interactive_user, <<"interactiveUser">>},
        {excluded_managed_settings, <<"excludedManagedSettings">>},
        {enforce_managed_model_defaults, <<"enforceManagedModelDefaults">>},
        {image_generation, <<"imageGeneration">>},
        {autopilot, <<"autopilot">>}
    ] ++ mode_fields(Mode),
    ParamsFolded = lists:foldl(fun({Key, JsonKey}, Acc) ->
        case maps:get(Key, Config, undefined) of
            undefined -> Acc;
            Value     -> Acc#{JsonKey => Value}
        end
    end, Params, Fields),
    %% Object options accept atom-keyed maps (converted to wire keys) or
    %% wire-shaped binary-keyed maps (passed through).
    Converters = [
        {<<"provider">>,          fun copilot_types:provider_config/1},
        {<<"modelCapabilities">>, fun copilot_types:model_capabilities/1},
        {<<"sandboxConfig">>,     fun copilot_types:sandbox_config/1},
        {<<"imageGeneration">>,   fun copilot_types:image_generation/1},
        {<<"autopilot">>,         fun copilot_types:autopilot_options/1}
    ],
    ParamsWire = lists:foldl(fun({JsonKey, Convert}, Acc) ->
        case maps:find(JsonKey, Acc) of
            {ok, Value} -> Acc#{JsonKey => Convert(Value)};
            error       -> Acc
        end
    end, ParamsFolded, Converters),
    %% CAPI provider options: auto routing tier and WebSocket Responses
    %% transport (enableWebSocketResponses is only accepted inside capi).
    Capi = maps:filter(fun(_K, V) -> V =/= undefined end, #{
        <<"autoTier">>                 => maps:get(auto_tier, Config, undefined),
        <<"enableWebSocketResponses">> => maps:get(enable_web_socket_responses, Config, undefined)
    }),
    Params1 = case map_size(Capi) of
        0 -> ParamsWire;
        _ -> ParamsWire#{<<"capi">> => Capi}
    end,
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
    Params3 = case maps:get(on_elicitation_request, Config, undefined) of
        undefined -> Params2;
        _         -> Params2#{<<"requestElicitation">> => true}
    end,
    %% Wire mcpAuthHandler flag (MCP OAuth host token handler registered)
    case maps:get(on_mcp_auth_request, Config, undefined) of
        undefined -> Params3;
        _         -> Params3#{<<"mcpAuthHandler">> => true}
    end.

%% Options only accepted on session.create (name, trust_working_directory,
%% refresh_custom_instructions) or session.resume (allow_transcript_recovery).
mode_fields(create) ->
    [
        {name, <<"name">>},
        {trust_working_directory, <<"trustWorkingDirectory">>},
        {refresh_custom_instructions, <<"refreshCustomInstructions">>}
    ];
mode_fields(resume) ->
    [
        {allow_transcript_recovery, <<"allowTranscriptRecovery">>}
    ].

%% ---------------------------------------------------------------------------
%% GitHub token provider (gitHubToken.getToken) registry
%% ---------------------------------------------------------------------------

%% Client-level server->client request handlers, registered once per connection.
register_client_handlers(RpcPid, #state{github_token_providers = Tab}) ->
    copilot_jsonrpc:set_request_handler(RpcPid, <<"gitHubToken.getToken">>,
                                        github_token_handler(Tab)).

%% The handler runs outside the client process, so the registry lives in an
%% ETS table (no gen_server round-trip while session.create is in flight).
github_token_handler(Tab) ->
    fun(Params) ->
        RegId = maps:get(<<"registrationId">>, Params, undefined),
        case ets:lookup(Tab, RegId) of
            [{RegId, Provider, RegisteredSessionId}] ->
                SessionId = case maps:get(<<"sessionId">>, Params, null) of
                    null -> RegisteredSessionId;
                    Sid  -> Sid
                end,
                Args = #{
                    host       => maps:get(<<"host">>, Params, <<>>),
                    session_id => SessionId,
                    reason     => maps:get(<<"reason">>, Params, <<"initial">>)
                },
                copilot_types:github_token_result(Provider(Args));
            [] ->
                error({no_github_token_provider_registered, RegId})
        end
    end.

validate_session_config(Config) ->
    Token = maps:get(github_token, Config, undefined),
    Provider = maps:get(github_token_provider, Config, undefined),
    if
        Token =/= undefined, Provider =/= undefined ->
            {error, {invalid_config,
                     <<"github_token and github_token_provider are mutually exclusive">>}};
        Provider =/= undefined, not is_function(Provider, 1) ->
            {error, {invalid_config, <<"github_token_provider must be a fun/1">>}};
        true ->
            ok
    end.

register_github_token_provider(Config, SessionId, #state{github_token_providers = Tab}) ->
    case maps:get(github_token_provider, Config, undefined) of
        undefined ->
            undefined;
        Provider ->
            RegId = new_registration_id(),
            true = ets:insert(Tab, {RegId, Provider, SessionId}),
            RegId
    end.

assign_github_token_provider(undefined, _SessionId, _State) ->
    ok;
assign_github_token_provider(RegId, SessionId, #state{github_token_providers = Tab}) ->
    ets:update_element(Tab, RegId, {3, SessionId}),
    ok.

unregister_github_token_provider(undefined, _State) ->
    ok;
unregister_github_token_provider(RegId, #state{github_token_providers = Tab}) ->
    ets:delete(Tab, RegId),
    ok.

with_registration_id(undefined, Params) ->
    Params;
with_registration_id(RegId, Params) ->
    Params#{<<"gitHubTokenProviderRegistrationId">> => RegId}.

track_registration(undefined, _SessionPid, #state{token_registrations = Regs}) ->
    Regs;
track_registration(RegId, SessionPid, #state{token_registrations = Regs}) ->
    Regs#{SessionPid => RegId}.

%% Drop the registration owned by a session process that went down
%% (disconnect/stop/crash).
release_token_registration(Pid, #state{token_registrations = Regs} = State) ->
    case maps:take(Pid, Regs) of
        {RegId, Rest} ->
            unregister_github_token_provider(RegId, State),
            State#state{token_registrations = Rest};
        error ->
            State
    end.

clear_github_token_providers(#state{github_token_providers = Tab}) ->
    catch ets:delete_all_objects(Tab),
    ok.

%% Random (version 4) UUID used as the provider registration id.
new_registration_id() ->
    <<A:32, B:16, _:4, C:12, _:2, D:14, E:48>> = rand:bytes(16),
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
                                   [A, B, C, D bor 16#8000, E])).

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
    %% Register exit plan mode handler
    copilot_jsonrpc:set_request_handler(Rpc, <<"exitPlanMode.request">>, fun(Params) ->
        TargetSession = maps:get(<<"sessionId">>, Params, <<>>),
        case TargetSession of
            SessionId ->
                Result = copilot_session:handle_exit_plan_mode(SessionPid, Params),
                #{<<"result">> => Result};
            _ ->
                #{<<"result">> => #{<<"approved">> => true}}
        end
    end),
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
