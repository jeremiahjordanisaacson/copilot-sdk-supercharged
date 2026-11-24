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
    mcp_secrets_save/3,
    mcp_secrets_save/4,
    mcp_secrets_delete/2,
    mcp_secrets_delete/3,
    mcp_secrets_delete_server/2,
    mcp_secrets_delete_server/3,
    extension_approvals_grant/3,
    extension_approvals_list/1,
    extension_approvals_list/2,
    extension_approvals_revoke/3,
    extension_approvals_revoke/4,
    state/1
]).

%% Internal: SDK protocol 4 payload translation (exported for unit tests).
-export([
    group_session_params/2,
    apply_mode_defaults/2
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

-define(SDK_PROTOCOL_VERSION, 4).
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
    token_registrations :: #{pid() => binary()},
    %% Client mode: coding_agent (default) or empty
    mode          :: coding_agent | empty,
    %% Per-session callbacks answered by client-level request handlers
    %% (tool.call, hooks.invoke, shellAuthentication.*, memoryStore.store):
    %% {SessionId, #{tools, hooks, on_shell_authentication_request, store_memory}}
    session_callbacks :: ets:tid(),
    %% Pending shell authentication prompts: {{SessionId, RequestId}, Waiter, Ref}
    shell_auth_requests :: ets:tid()
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

%% @doc Save an MCP server secret in the runtime-owned secret store
%% (`mcp.secrets.save`). `Value` is a credential: it is sent to the runtime and
%% never logged. Returns {ok, #{used_keychain => boolean()}}: `true` when the
%% OS keychain stored the secret, `false` for the file backend. Requires
%% Copilot CLI 1.0.116 or newer; older runtimes answer with a JSON-RPC
%% method-not-found error, returned as {error, Error}.
-spec mcp_secrets_save(pid(), binary(), binary()) -> {ok, map()} | {error, term()}.
mcp_secrets_save(Pid, SecretId, Value) ->
    mcp_secrets_save(Pid, SecretId, Value, #{}).

%% @doc Like mcp_secrets_save/3; `Opts` may set `config_dir` (the configuration
%% directory, the runtime's default otherwise).
-spec mcp_secrets_save(pid(), binary(), binary(), map()) -> {ok, map()} | {error, term()}.
mcp_secrets_save(Pid, SecretId, Value, Opts) ->
    gen_server:call(Pid, {mcp_secrets_save, SecretId, Value, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Delete one secret from the runtime-owned MCP secret store
%% (`mcp.secrets.delete`). Requires Copilot CLI 1.0.116 or newer.
-spec mcp_secrets_delete(pid(), binary()) -> ok | {error, term()}.
mcp_secrets_delete(Pid, SecretId) ->
    mcp_secrets_delete(Pid, SecretId, #{}).

%% @doc Like mcp_secrets_delete/2; `Opts` may set `config_dir`.
-spec mcp_secrets_delete(pid(), binary(), map()) -> ok | {error, term()}.
mcp_secrets_delete(Pid, SecretId, Opts) ->
    gen_server:call(Pid, {mcp_secrets_delete, SecretId, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Delete every stored secret of one MCP server
%% (`mcp.secrets.deleteServer`). Requires Copilot CLI 1.0.116 or newer.
-spec mcp_secrets_delete_server(pid(), binary()) -> ok | {error, term()}.
mcp_secrets_delete_server(Pid, ServerName) ->
    mcp_secrets_delete_server(Pid, ServerName, #{}).

%% @doc Like mcp_secrets_delete_server/2; `Opts` may set `config_dir`.
-spec mcp_secrets_delete_server(pid(), binary(), map()) -> ok | {error, term()}.
mcp_secrets_delete_server(Pid, ServerName, Opts) ->
    gen_server:call(Pid, {mcp_secrets_delete_server, ServerName, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Persist a pending plugin extension permission request as a user-scoped
%% approval, before answering that request normally
%% (`permissions.extensionApprovals.grant`). Returns {ok, #{granted, approval}}
%% (see copilot_types:extension_approval/1). Requires Copilot CLI 1.0.116 or
%% newer.
-spec extension_approvals_grant(pid(), binary(), binary()) -> {ok, map()} | {error, term()}.
extension_approvals_grant(Pid, SessionId, RequestId) ->
    gen_server:call(Pid, {extension_approvals_grant, SessionId, RequestId}, ?DEFAULT_TIMEOUT).

%% @doc List the persisted plugin extension approvals
%% (`permissions.extensionApprovals.list`). Returns {ok, #{approvals => [Approval]}}
%% (see copilot_types:extension_approval/1). Requires Copilot CLI 1.0.116 or
%% newer.
-spec extension_approvals_list(pid()) -> {ok, map()} | {error, term()}.
extension_approvals_list(Pid) ->
    extension_approvals_list(Pid, #{}).

%% @doc Like extension_approvals_list/1; `Opts` may set `session_id` to list
%% the configuration profile of that attached session.
-spec extension_approvals_list(pid(), map()) -> {ok, map()} | {error, term()}.
extension_approvals_list(Pid, Opts) ->
    gen_server:call(Pid, {extension_approvals_list, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Revoke a persisted plugin extension approval
%% (`permissions.extensionApprovals.revoke`). Pass the approval's
%% `extension_name` and `installation_key` unchanged. Returns
%% {ok, #{revoked => boolean()}}. Requires Copilot CLI 1.0.116 or newer.
-spec extension_approvals_revoke(pid(), binary(), binary()) -> {ok, map()} | {error, term()}.
extension_approvals_revoke(Pid, ExtensionName, InstallationKey) ->
    extension_approvals_revoke(Pid, ExtensionName, InstallationKey, #{}).

%% @doc Like extension_approvals_revoke/3; `Opts` may set `session_id` to use
%% the attached session's configuration profile.
-spec extension_approvals_revoke(pid(), binary(), binary(), map()) ->
    {ok, map()} | {error, term()}.
extension_approvals_revoke(Pid, ExtensionName, InstallationKey, Opts) ->
    gen_server:call(Pid, {extension_approvals_revoke, ExtensionName, InstallationKey, Opts},
                    ?DEFAULT_TIMEOUT).

-spec state(pid()) -> disconnected | connecting | connected | error.
state(Pid) ->
    gen_server:call(Pid, get_state).

%% ---------------------------------------------------------------------------
%% gen_server callbacks
%% ---------------------------------------------------------------------------

init(Options) ->
    case copilot_types:parse_client_mode(maps:get(mode, Options, undefined)) of
        {ok, Mode} ->
            init_state(Options#{mode => Mode}, Mode);
        {error, Reason} ->
            {stop, {invalid_config, Reason}}
    end.

init_state(Options, Mode) ->
    CliPath = case maps:get(cli_path, Options, undefined) of
        undefined ->
            case os:getenv("COPILOT_CLI_PATH") of
                false -> "copilot";
                Path  -> Path
            end;
        P when is_binary(P) -> binary_to_list(P);
        P -> P
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
        token_registrations = #{},
        mode = Mode,
        session_callbacks = ets:new(copilot_session_callbacks, [set, protected]),
        shell_auth_requests = ets:new(copilot_shell_auth_requests, [set, public])
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
    case validate_session_config(Config, State) of
        {error, _} = Invalid ->
            {reply, Invalid, State};
        ok ->
            RegId = register_github_token_provider(Config, undefined, State),
            FlatParams = with_registration_id(RegId, build_session_params(create, Config, State)),
            RpcParams = apply_mode_defaults(group_session_params(FlatParams, create),
                                            State#state.mode),
            case copilot_jsonrpc:request(Rpc, <<"session.create">>, RpcParams) of
                {ok, #{<<"sessionId">> := SessionId} = Result} ->
                    WorkspacePath = maps:get(<<"workspacePath">>, Result, <<>>),
                    register_session_callbacks(SessionId, Config, State),
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
                    %% A refusing managed-settings policy helper is available through
                    %% copilot_types:policy_helper_failure(Err).
                    unregister_github_token_provider(RegId, State),
                    {reply, Err, State}
            end
    end;

handle_call({resume_session, SessionId, Config}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    case validate_session_config(Config, State) of
        {error, _} = Invalid ->
            {reply, Invalid, State};
        ok ->
            RegId = register_github_token_provider(Config, SessionId, State),
            Params = with_registration_id(RegId, build_session_params(resume, Config, State)),
            FlatParams = Params#{<<"sessionId">> => SessionId},
            ResumeParams = apply_mode_defaults(group_session_params(FlatParams, resume),
                                               State#state.mode),
            %% Registered before the request so callbacks raised while the
            %% session resumes are routed to it.
            register_session_callbacks(SessionId, Config, State),
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
                    unregister_session_callbacks(SessionId, State),
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
        {ok, #{<<"success">> := false} = Resp} ->
            {reply, {error, {delete_failed, maps:get(<<"error">>, Resp, <<"Unknown error">>)}},
             State};
        {ok, _} ->
            case maps:get(SessionId, State#state.sessions, undefined) of
                undefined -> ok;
                SessionPid -> copilot_session:stop(SessionPid)
            end,
            unregister_session_callbacks(SessionId, State),
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
        {ok, #{<<"success">> := false} = Resp} ->
            {reply, {error, {set_foreground_failed,
                             maps:get(<<"error">>, Resp, <<"Failed to set foreground session">>)}},
             State};
        {ok, _} -> {reply, ok, State};
        Err     -> {reply, Err, State}
    end;

handle_call(clear_managed_settings_cache, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Result = copilot_jsonrpc:request(Rpc, <<"managedSettings.clearCache">>, #{}),
    {reply, Result, State};

handle_call({mcp_secrets_save, SecretId, Value, Opts}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Params = with_config_dir(Opts, #{<<"secretId">> => SecretId, <<"value">> => Value}),
    Result = copilot_jsonrpc:request(Rpc, <<"mcp.secrets.save">>, Params),
    {reply, decode_reply(Result, fun copilot_types:mcp_secrets_save_result/1), State};

handle_call({mcp_secrets_delete, SecretId, Opts}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Params = with_config_dir(Opts, #{<<"secretId">> => SecretId}),
    Result = copilot_jsonrpc:request(Rpc, <<"mcp.secrets.delete">>, Params),
    {reply, ok_reply(Result), State};

handle_call({mcp_secrets_delete_server, ServerName, Opts}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Params = with_config_dir(Opts, #{<<"serverName">> => ServerName}),
    Result = copilot_jsonrpc:request(Rpc, <<"mcp.secrets.deleteServer">>, Params),
    {reply, ok_reply(Result), State};

handle_call({extension_approvals_grant, SessionId, RequestId}, _From,
            #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Params = #{<<"sessionId">> => SessionId, <<"requestId">> => RequestId},
    Result = copilot_jsonrpc:request(Rpc, <<"permissions.extensionApprovals.grant">>, Params),
    {reply, decode_reply(Result, fun copilot_types:extension_approvals_grant_result/1), State};

handle_call({extension_approvals_list, Opts}, _From, #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Params = with_session_id(Opts, #{}),
    Result = copilot_jsonrpc:request(Rpc, <<"permissions.extensionApprovals.list">>, Params),
    {reply, decode_reply(Result, fun copilot_types:extension_approvals_list_result/1), State};

handle_call({extension_approvals_revoke, ExtensionName, InstallationKey, Opts}, _From,
            #state{rpc = Rpc} = State)
  when Rpc =/= undefined ->
    Params = with_session_id(Opts, #{<<"extensionName">>   => ExtensionName,
                                     <<"installationKey">> => InstallationKey}),
    Result = copilot_jsonrpc:request(Rpc, <<"permissions.extensionApprovals.revoke">>, Params),
    {reply, decode_reply(Result, fun copilot_types:extension_approvals_revoke_result/1), State};

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
    catch ets:delete_all_objects(State#state.session_callbacks),
    catch ets:delete_all_objects(State#state.shell_auth_requests),
    {noreply, State#state{conn_state = disconnected, rpc = undefined, port = undefined,
                          token_registrations = #{}}};

handle_info({'DOWN', MonRef, process, Pid, _Reason}, State0) ->
    State = release_token_registration(Pid, State0),
    case maps:take(MonRef, State#state.session_monitors) of
        {SessionId, NewMonitors} ->
            %% A later resume may have replaced this session process; only drop
            %% the callbacks that still belong to the process that went down.
            case maps:get(SessionId, State#state.sessions, undefined) of
                Pid -> unregister_session_callbacks(SessionId, State);
                _   -> ok
            end,
            NewSessions = case maps:get(SessionId, State#state.sessions, undefined) of
                Pid -> maps:remove(SessionId, State#state.sessions);
                _   -> State#state.sessions
            end,
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
                case maybe_set_network_provider(RpcPid, Options) of
                    ok ->
                        maybe_set_session_fs(RpcPid, Options),
                        {ok, NewState#state{conn_state = connected}};
                    {error, NetworkReason} ->
                        catch port_close(Port),
                        {error, NetworkReason}
                end;
            {error, Reason} ->
                catch port_close(Port),
                {error, Reason}
        end
    catch
        _:CrashReason ->
            {error, {start_failed, CrashReason}}
    end.

build_cli_args(CliPath, LogLevel, Options) ->
    %% JavaScript entry points (npm-loader.js, legacy index.js) run under node;
    %% any other path (a native binary or the node_modules/.bin/copilot shim)
    %% is executed directly.
    Launcher = case lists:member(string:lowercase(filename:extension(CliPath)),
                                 [".js", ".mjs", ".cjs"]) of
        true  -> ["node", CliPath];
        false -> [CliPath]
    end,
    Base = Launcher ++ [
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
        {ok, Result} when is_map(Result) ->
            Version = maps:get(<<"protocolVersion">>, Result,
                               maps:get(<<"sdkProtocolVersion">>, Result, undefined)),
            check_protocol_version(Version);
        {ok, _} ->
            check_protocol_version(undefined);
        {error, Reason} ->
            {error, Reason}
    end.

%% The server must report exactly SDK protocol version 4.
check_protocol_version(?SDK_PROTOCOL_VERSION) ->
    ok;
check_protocol_version(Version) ->
    {error, {protocol_mismatch, protocol_mismatch_message(Version)}}.

protocol_mismatch_message(Version) when is_integer(Version) ->
    iolist_to_binary(io_lib:format(
        "SDK protocol version mismatch: SDK supports version ~b, but server reports version ~b. "
        "Please update your SDK or server to ensure compatibility.",
        [?SDK_PROTOCOL_VERSION, Version]));
protocol_mismatch_message(_Missing) ->
    iolist_to_binary(io_lib:format(
        "SDK protocol version mismatch: SDK supports version ~b, but server does not report "
        "a protocol version. Please update your server to ensure compatibility.",
        [?SDK_PROTOCOL_VERSION])).

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
        {autopilot, <<"autopilot">>},
        %% v3.0.0 named BYOK providers (inference.providers / models /
        %% defaultProvider); default_provider => null clears it on resume.
        {providers, <<"providers">>},
        {models, <<"models">>},
        {default_provider, <<"defaultProvider">>}
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
        {<<"autopilot">>,         fun copilot_types:autopilot_options/1},
        {<<"providers">>,         fun copilot_types:named_provider_config/1},
        {<<"models">>,            fun copilot_types:provider_model_config/1}
    ],
    ParamsWire = lists:foldl(fun({JsonKey, Convert}, Acc) ->
        case maps:find(JsonKey, Acc) of
            {ok, Value} -> Acc#{JsonKey => Convert(Value)};
            error       -> Acc
        end
    end, ParamsFolded, Converters),
    %% CAPI provider options: auto routing tier, WebSocket Responses transport
    %% and the CAPI catalog switch (`capi => #{enabled => false}`), grouped as
    %% github.capi.
    Capi = maps:filter(fun(_K, V) -> V =/= undefined end, #{
        <<"enabled">>                  => capi_enabled(maps:get(capi, Config, undefined)),
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
    Params4 = case maps:get(on_mcp_auth_request, Config, undefined) of
        undefined -> Params3;
        _         -> Params3#{<<"mcpAuthHandler">> => true}
    end,
    %% Tool declarations (tools.definitions); handlers stay client-side and are
    %% invoked through tool.call.
    Params5 = case maps:get(tools, Config, undefined) of
        Tools when is_list(Tools), Tools =/= [] ->
            Params4#{<<"tools">> => [copilot_types:tool_definition(T) || T <- Tools, is_map(T)]};
        _ ->
            Params4
    end,
    %% Capability flags for client-side callbacks: hooks.hasHandlers,
    %% interaction.requestShellAuthentication and state.hasMemoryStore.
    Flags = [
        {<<"hooks">>, has_hook_handlers(maps:get(hooks, Config, undefined))},
        {<<"requestShellAuthentication">>,
         is_function(maps:get(on_shell_authentication_request, Config, undefined), 1)},
        {<<"hasMemoryStore">>, is_function(maps:get(store_memory, Config, undefined), 1)}
    ],
    lists:foldl(fun
        ({Flag, true}, Acc)  -> Acc#{Flag => true};
        ({_Flag, _}, Acc)    -> Acc
    end, Params5, Flags).

%% `capi => #{enabled => Bool}` (or the wire-shaped `#{<<"enabled">> => Bool}`).
capi_enabled(#{enabled := Enabled}) -> Enabled;
capi_enabled(#{<<"enabled">> := Enabled}) -> Enabled;
capi_enabled(_) -> undefined.

has_hook_handlers(Hooks) when is_map(Hooks) ->
    lists:any(fun(Hook) -> is_function(Hook, 1) end, maps:values(Hooks));
has_hook_handlers(_) ->
    false.

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
%% SDK protocol 4: grouped session payloads
%% ---------------------------------------------------------------------------

%% @doc Convert the flat session payload built by build_session_params/3 into
%% the SDK protocol 4 grouped payload sent with session.create (`create`) and
%% session.resume (`resume`).
%%
%% Protocol 4 runtimes reject the flat keys and unknown group members, while
%% unknown root keys are tolerated. Known flat keys therefore move to their
%% exact `group.member` (for example <<"streaming">> to events.streaming and
%% <<"hooks">> => true to hooks.hasHandlers); root keys and unrecognized keys
%% stay at the top level; `null`/`undefined` values are skipped (except an
%% explicit defaultProvider => null on resume, which clears the default
%% provider); members invalid for the method are dropped; empty groups are not
%% emitted. Already-grouped objects are merged and their members win. The
%% legacy single <<"provider">> becomes a named "default" connection unless
%% providers, defaultProvider or capi.enabled are given explicitly.
-spec group_session_params(map(), create | resume) -> map().
group_session_params(Flat, Mode) ->
    Paths = session_option_paths(),
    {Root, Grouped, Explicit, Provider} = maps:fold(fun(Key, Value, Acc) ->
        group_session_param(Key, Value, Mode, Paths, Acc)
    end, {#{}, #{}, #{}, undefined}, Flat),
    Merged = maps:fold(fun(Group, Members, Acc) ->
        Kept = maps:filter(fun(Member, Value) ->
            Value =/= null orelse is_default_provider_clear(Mode, Group, Member)
        end, Members),
        Acc#{Group => maps:merge(maps:get(Group, Acc, #{}), Kept)}
    end, Grouped, Explicit),
    WithProvider = translate_legacy_provider(Provider, maps:get(<<"model">>, Root, undefined),
                                             Merged),
    WithEnvMode = ensure_mcp_env_value_mode(WithProvider),
    %% Protocol 4 runtimes reject members that are invalid for the lifecycle
    %% method, so drop them even when they arrive in an already-grouped object.
    Valid = lists:foldl(fun({Group, Member}, Acc) ->
        case maps:get(Group, Acc, undefined) of
            Members when is_map(Members) -> Acc#{Group => maps:remove(Member, Members)};
            _                            -> Acc
        end
    end, WithEnvMode, excluded_group_members(Mode)),
    NonEmpty = maps:filter(fun(_Group, Members) ->
        is_map(Members) andalso map_size(Members) > 0
    end, Valid),
    maps:merge(Root, NonEmpty).

group_session_param(Key, Value, Mode, Paths, {Root, Grouped, Explicit, Provider} = Acc) ->
    Absent = Value =:= undefined orelse
             (Value =:= null andalso not (Mode =:= resume andalso Key =:= <<"defaultProvider">>)),
    case Absent orelse lists:member(Key, excluded_session_keys(Mode)) of
        true ->
            Acc;
        false ->
            case Key of
                <<"provider">> ->
                    {Root, Grouped, Explicit, Value};
                <<"configDir">> ->
                    {maps:merge(#{<<"configDirectory">> => Value}, Root), Grouped, Explicit, Provider};
                <<"disableResume">> when Mode =:= resume ->
                    {maps:merge(#{<<"suppressResumeEvent">> => Value}, Root), Grouped, Explicit,
                     Provider};
                <<"disableResume">> ->
                    Acc;
                _ ->
                    IsGroupObject = is_map(Value) andalso
                                    lists:member(Key, session_option_groups()),
                    case {IsGroupObject, maps:find(Key, Paths)} of
                        {true, _} ->
                            {Root, Grouped, Explicit#{Key => Value}, Provider};
                        {false, {ok, {Group, Member}}} ->
                            {Root, put_group_member(Group, Member, Value, Grouped), Explicit,
                             Provider};
                        {false, error} ->
                            {Root#{Key => Value}, Grouped, Explicit, Provider}
                    end
            end
    end.

is_default_provider_clear(resume, <<"inference">>, <<"defaultProvider">>) -> true;
is_default_provider_clear(_Mode, _Group, _Member) -> false.

put_group_member(Group, Member, Value, Groups) ->
    Members = maps:get(Group, Groups, #{}),
    Groups#{Group => Members#{Member => Value}}.

%% Keys valid only on the other lifecycle method.
excluded_session_keys(create) ->
    [<<"suppressResumeEvent">>, <<"continuePendingWork">>, <<"allowTranscriptRecovery">>,
     <<"requestedEnvironmentVariables">>, <<"openCanvases">>];
excluded_session_keys(resume) ->
    [<<"name">>, <<"cloud">>, <<"trustWorkingDirectory">>, <<"refreshCustomInstructions">>].

%% Group members valid only on the other lifecycle method.
excluded_group_members(create) ->
    [{<<"canvases">>, <<"open">>}];
excluded_group_members(resume) ->
    [{<<"filesystem">>, <<"trustWorkingDirectory">>},
     {<<"instructions">>, <<"refreshCustomInstructions">>}].

session_option_groups() ->
    [<<"inference">>, <<"tools">>, <<"permissions">>, <<"interaction">>, <<"events">>,
     <<"agents">>, <<"skills">>, <<"mcp">>, <<"github">>, <<"filesystem">>, <<"execution">>,
     <<"instructions">>, <<"hooks">>, <<"git">>, <<"state">>, <<"plugins">>, <<"canvases">>,
     <<"extensions">>, <<"experiments">>, <<"policy">>].

%% Flat session option key => {Group, Member}. Keys not listed here (sessionId,
%% model, name, clientName, workingDirectory, configDirectory,
%% enableConfigDiscovery, systemMessage, allowTranscriptRecovery, ...) stay at
%% the root.
session_option_paths() ->
    #{
        <<"reasoningEffort">>                    => {<<"inference">>, <<"reasoningEffort">>},
        <<"reasoningSummary">>                   => {<<"inference">>, <<"reasoningSummary">>},
        <<"contextTier">>                        => {<<"inference">>, <<"contextTier">>},
        <<"allowMultipleToolCalls">>             => {<<"inference">>, <<"allowMultipleToolCalls">>},
        <<"enableCitations">>                    => {<<"inference">>, <<"enableCitations">>},
        <<"modelCapabilities">>                  => {<<"inference">>, <<"capabilities">>},
        <<"verbosity">>                          => {<<"inference">>, <<"verbosity">>},
        <<"providers">>                          => {<<"inference">>, <<"providers">>},
        <<"models">>                             => {<<"inference">>, <<"models">>},
        <<"defaultProvider">>                    => {<<"inference">>, <<"defaultProvider">>},
        <<"tools">>                              => {<<"tools">>, <<"definitions">>},
        <<"availableTools">>                     => {<<"tools">>, <<"availableTools">>},
        <<"excludedTools">>                      => {<<"tools">>, <<"excludedTools">>},
        <<"toolSearch">>                         => {<<"tools">>, <<"search">>},
        <<"largeOutput">>                        => {<<"tools">>, <<"largeOutput">>},
        <<"imageGeneration">>                    => {<<"tools">>, <<"imageGeneration">>},
        <<"manageScheduleEnabled">>              => {<<"tools">>, <<"manageScheduleEnabled">>},
        <<"webSearch">>                          => {<<"tools">>, <<"webSearch">>},
        <<"shell">>                              => {<<"tools">>, <<"shell">>},
        <<"maxInlineBinaryBytes">>               => {<<"tools">>, <<"maxInlineBinaryBytes">>},
        <<"requestPermission">>                  => {<<"permissions">>, <<"request">>},
        <<"sandboxConfig">>                      => {<<"permissions">>, <<"sandbox">>},
        <<"sandbox">>                            => {<<"permissions">>, <<"sandbox">>},
        <<"permissionMode">>                     => {<<"permissions">>, <<"mode">>},
        <<"sandboxConfigSource">>                => {<<"permissions">>, <<"sandboxSource">>},
        <<"commands">>                           => {<<"interaction">>, <<"commands">>},
        <<"askUserVariant">>                     => {<<"interaction">>, <<"askUserVariant">>},
        <<"requestUserInput">>                   => {<<"interaction">>, <<"requestUserInput">>},
        <<"requestElicitation">>                 => {<<"interaction">>, <<"requestElicitation">>},
        <<"requestShellAuthentication">>         => {<<"interaction">>, <<"requestShellAuthentication">>},
        <<"requestExitPlanMode">>                => {<<"interaction">>, <<"requestExitPlanMode">>},
        <<"requestAutoModeSwitch">>              => {<<"interaction">>, <<"requestAutoModeSwitch">>},
        <<"requestAutoModeRecovery">>            => {<<"interaction">>, <<"requestAutoModeRecovery">>},
        <<"streaming">>                          => {<<"events">>, <<"streaming">>},
        <<"includeSubAgentStreamingEvents">>     => {<<"events">>, <<"includeSubAgentStreamingEvents">>},
        <<"customAgents">>                       => {<<"agents">>, <<"definitions">>},
        <<"defaultAgent">>                       => {<<"agents">>, <<"default">>},
        <<"agent">>                              => {<<"agents">>, <<"selected">>},
        <<"includedBuiltinAgents">>              => {<<"agents">>, <<"includedBuiltinAgents">>},
        <<"excludedBuiltinAgents">>              => {<<"agents">>, <<"excludedBuiltinAgents">>},
        <<"customAgentsLocalOnly">>              => {<<"agents">>, <<"localDiscoveryOnly">>},
        <<"agentDirectories">>                   => {<<"agents">>, <<"directories">>},
        <<"skillDirectories">>                   => {<<"skills">>, <<"directories">>},
        <<"enableSkills">>                       => {<<"skills">>, <<"enabled">>},
        <<"includedBuiltinSkills">>              => {<<"skills">>, <<"includedBuiltinSkills">>},
        <<"disabledSkills">>                     => {<<"skills">>, <<"disabledSkills">>},
        <<"hasSkillProvider">>                   => {<<"skills">>, <<"hasProvider">>},
        <<"ignoredSkillsLocations">>             => {<<"skills">>, <<"ignoredLocations">>},
        <<"mcpServers">>                         => {<<"mcp">>, <<"servers">>},
        <<"disabledMcpServers">>                 => {<<"mcp">>, <<"disabledServers">>},
        <<"requestMcpApps">>                     => {<<"mcp">>, <<"enableApps">>},
        <<"enableMcpApps">>                      => {<<"mcp">>, <<"enableApps">>},
        <<"mcpOAuthTokenStorage">>               => {<<"mcp">>, <<"oauthTokenStorage">>},
        <<"authClientIdMetadataUrl">>            => {<<"mcp">>, <<"authClientIdMetadataUrl">>},
        <<"envValueMode">>                       => {<<"mcp">>, <<"envValueMode">>},
        <<"managedMcpServers">>                  => {<<"mcp">>, <<"managedServers">>},
        <<"allowAllMcpServerInstructions">>      => {<<"mcp">>, <<"allowAllServerInstructions">>},
        <<"gitHubToken">>                        => {<<"github">>, <<"token">>},
        <<"gitHubTokenProviderRegistrationId">>  => {<<"github">>, <<"tokenProviderRegistrationId">>},
        <<"githubMcpToolConfig">>                => {<<"github">>, <<"mcp">>},
        <<"capi">>                               => {<<"github">>, <<"capi">>},
        <<"enableSessionTelemetry">>             => {<<"github">>, <<"telemetryEnabled">>},
        <<"enableManagedSettings">>              => {<<"github">>, <<"fetchManagedSettings">>},
        <<"interactiveUser">>                    => {<<"github">>, <<"interactiveUser">>},
        <<"enableGitHubTelemetryForwarding">>    => {<<"github">>, <<"telemetryForwarding">>},
        <<"authInfo">>                           => {<<"github">>, <<"authInfo">>},
        <<"copilotUrl">>                         => {<<"github">>, <<"copilotUrl">>},
        <<"additionalDirectories">>              => {<<"filesystem">>, <<"additionalDirectories">>},
        <<"enableFileChangeTracking">>           => {<<"filesystem">>, <<"trackFileChanges">>},
        <<"trustWorkingDirectory">>              => {<<"filesystem">>, <<"trustWorkingDirectory">>},
        <<"sessionLimits">>                      => {<<"execution">>, <<"limits">>},
        <<"autopilot">>                          => {<<"execution">>, <<"autopilot">>},
        <<"autopilotContinuation">>              => {<<"execution">>, <<"autopilotContinuation">>},
        <<"instructionDirectories">>             => {<<"instructions">>, <<"directories">>},
        <<"organizationCustomInstructions">>     => {<<"instructions">>, <<"organizationText">>},
        <<"enableOnDemandInstructionDiscovery">> => {<<"instructions">>, <<"onDemandDiscovery">>},
        <<"skipCustomInstructions">>             => {<<"instructions">>, <<"skipCustomInstructions">>},
        <<"refreshCustomInstructions">>          => {<<"instructions">>, <<"refreshCustomInstructions">>},
        <<"hooks">>                              => {<<"hooks">>, <<"hasHandlers">>},
        <<"hookDirectories">>                    => {<<"hooks">>, <<"directories">>},
        <<"enableFileHooks">>                    => {<<"hooks">>, <<"enableRepositoryHooks">>},
        <<"coauthorEnabled">>                    => {<<"git">>, <<"coauthorEnabled">>},
        <<"enableHostGitOperations">>            => {<<"git">>, <<"enableHostGitOperations">>},
        <<"infiniteSessions">>                   => {<<"state">>, <<"infiniteSessions">>},
        <<"memory">>                             => {<<"state">>, <<"memory">>},
        <<"hasMemoryStore">>                     => {<<"state">>, <<"hasMemoryStore">>},
        <<"enableSessionStore">>                 => {<<"state">>, <<"enableCrossSessionStore">>},
        <<"skipEmbeddingRetrieval">>             => {<<"state">>, <<"skipEmbeddingRetrieval">>},
        <<"embeddingCacheStorage">>              => {<<"state">>, <<"embeddingCacheStorage">>},
        <<"pluginDirectories">>                  => {<<"plugins">>, <<"directories">>},
        <<"installedPlugins">>                   => {<<"plugins">>, <<"installed">>},
        <<"canvases">>                           => {<<"canvases">>, <<"definitions">>},
        <<"canvasProvider">>                     => {<<"canvases">>, <<"provider">>},
        <<"requestCanvasRenderer">>              => {<<"canvases">>, <<"requestRenderer">>},
        <<"openCanvases">>                       => {<<"canvases">>, <<"open">>},
        <<"requestExtensions">>                  => {<<"extensions">>, <<"enabled">>},
        <<"extensionSdkPath">>                   => {<<"extensions">>, <<"sdkPath">>},
        <<"extensionInfo">>                      => {<<"extensions">>, <<"identity">>},
        <<"enableExperimentalMode">>             => {<<"experiments">>, <<"enableExperimentalMode">>},
        <<"isExperimentalMode">>                 => {<<"experiments">>, <<"enableExperimentalMode">>},
        %% This SDK historically sent the experimental-mode flag as experimentalMode.
        <<"experimentalMode">>                   => {<<"experiments">>, <<"enableExperimentalMode">>},
        <<"featureFlags">>                       => {<<"experiments">>, <<"flags">>},
        <<"expAssignments">>                     => {<<"experiments">>, <<"assignments">>},
        <<"featureFlagConfiguration">>           => {<<"experiments">>, <<"configuration">>},
        <<"allowedModels">>                      => {<<"policy">>, <<"allowedModels">>},
        <<"managedSettings">>                    => {<<"policy">>, <<"managedSettings">>},
        <<"excludedManagedSettings">>            => {<<"policy">>, <<"excludedManagedSettings">>},
        <<"enforceManagedModelDefaults">>        => {<<"policy">>, <<"enforceManagedModelDefaults">>}
    }.

%% Legacy BYOK provider fields that describe the model, not the connection.
legacy_provider_model_fields() ->
    [<<"modelId">>, <<"wireModel">>, <<"maxPromptTokens">>, <<"maxContextWindowTokens">>,
     <<"maxOutputTokens">>, <<"capabilities">>].

%% The legacy single `provider` becomes the named "default" connection
%% (inference.providers), selected by inference.defaultProvider, with CAPI
%% disabled (github.capi.enabled = false) as v1 BYOK did. Model-specific fields
%% move to an inference.models entry for the session model.
translate_legacy_provider(Provider, Model, Groups) when is_map(Provider) ->
    Inference = maps:get(<<"inference">>, Groups, #{}),
    Github = maps:get(<<"github">>, Groups, #{}),
    ExplicitCapi = case maps:get(<<"capi">>, Github, undefined) of
        Capi when is_map(Capi) -> maps:is_key(<<"enabled">>, Capi);
        _                      -> false
    end,
    Explicit = maps:is_key(<<"providers">>, Inference) orelse
               maps:is_key(<<"defaultProvider">>, Inference) orelse
               ExplicitCapi,
    case Explicit of
        true ->
            Groups;
        false ->
            ModelKeys = legacy_provider_model_fields(),
            Present = maps:filter(fun(Key, Value) ->
                Value =/= undefined andalso Value =/= null andalso
                Key =/= <<"name">> andalso Key =/= name
            end, Provider),
            Connection = maps:without(ModelKeys, Present),
            ModelFields = maps:with(ModelKeys, Present),
            Inference1 = Inference#{
                <<"providers">>       => [Connection#{<<"name">> => <<"default">>}],
                <<"defaultProvider">> => <<"default">>
            },
            Inference2 = case {map_size(ModelFields), Model} of
                {0, _} ->
                    Inference1;
                {_, ModelId} when is_binary(ModelId) ->
                    Definition = ModelFields#{<<"provider">> => <<"default">>,
                                              <<"id">>       => ModelId},
                    Inference1#{<<"models">> => [Definition]};
                _ ->
                    Inference1
            end,
            Capi1 = case maps:get(<<"capi">>, Github, #{}) of
                CapiMap when is_map(CapiMap) -> CapiMap#{<<"enabled">> => false};
                _                            -> #{<<"enabled">> => false}
            end,
            Groups#{<<"inference">> => Inference2, <<"github">> => Github#{<<"capi">> => Capi1}}
    end;
translate_legacy_provider(_Provider, _Model, Groups) ->
    Groups.

%% mcp.envValueMode is "direct" whenever MCP servers are sent.
ensure_mcp_env_value_mode(Groups) ->
    case maps:get(<<"mcp">>, Groups, undefined) of
        #{<<"servers">> := _} = Mcp ->
            Groups#{<<"mcp">> => maps:merge(#{<<"envValueMode">> => <<"direct">>}, Mcp)};
        _ ->
            Groups
    end.

%% @doc Apply the client mode defaults under the caller's grouped values. In
%% `empty` mode ambient capabilities default off; `coding_agent` mode leaves
%% the payload unchanged.
-spec apply_mode_defaults(map(), coding_agent | empty) -> map().
apply_mode_defaults(Params, empty) ->
    Defaults = [
        {<<"experiments">>,  <<"enableExperimentalMode">>,  false},
        {<<"github">>,       <<"telemetryEnabled">>,        false},
        {<<"mcp">>,          <<"oauthTokenStorage">>,       <<"in-memory">>},
        {<<"state">>,        <<"skipEmbeddingRetrieval">>,  true},
        {<<"state">>,        <<"embeddingCacheStorage">>,   <<"in-memory">>},
        {<<"state">>,        <<"enableCrossSessionStore">>, false},
        {<<"state">>,        <<"memory">>,                  #{<<"enabled">> => false}},
        {<<"instructions">>, <<"onDemandDiscovery">>,       false},
        {<<"hooks">>,        <<"enableRepositoryHooks">>,   false},
        {<<"git">>,          <<"enableHostGitOperations">>, false}
    ],
    lists:foldl(fun({Group, Member, Default}, Acc) ->
        Members = maps:get(Group, Acc, #{}),
        case maps:get(Member, Members, null) of
            null -> Acc#{Group => Members#{Member => Default}};
            _    -> Acc
        end
    end, Params, Defaults);
apply_mode_defaults(Params, _CodingAgent) ->
    Params.

%% ---------------------------------------------------------------------------
%% GitHub token provider (gitHubToken.getToken) registry
%% ---------------------------------------------------------------------------

%% Client-level server->client request handlers, registered once per connection.
register_client_handlers(RpcPid, #state{github_token_providers = Tab,
                                        session_callbacks = Callbacks,
                                        shell_auth_requests = Pending,
                                        options = Options}) ->
    Handlers = [
        {<<"gitHubToken.getToken">>,         github_token_handler(Tab)},
        {<<"tool.call">>,                    tool_call_handler(Callbacks)},
        {<<"hooks.invoke">>,                 hooks_invoke_handler(Callbacks)},
        {<<"shellAuthentication.request">>,  shell_authentication_handler(Callbacks, Pending)},
        {<<"shellAuthentication.cancel">>,   shell_authentication_cancel_handler(Pending)},
        {<<"memoryStore.store">>,            memory_store_handler(Callbacks)}
    ] ++ network_handlers(network_options(Options)),
    lists:foreach(fun({Method, Handler}) ->
        copilot_jsonrpc:set_request_handler(RpcPid, Method, Handler)
    end, Handlers).

%% ---------------------------------------------------------------------------
%% Per-session callbacks (tool.call, hooks.invoke, shellAuthentication.*,
%% memoryStore.store)
%% ---------------------------------------------------------------------------

%% The request handlers run outside the client process, so the per-session
%% callbacks live in an ETS table (no gen_server round-trip, which could
%% deadlock while session.create / session.resume is in flight).
register_session_callbacks(SessionId, Config, #state{session_callbacks = Tab}) ->
    Hooks = case maps:get(hooks, Config, undefined) of
        H when is_map(H) -> H;
        _                -> #{}
    end,
    Callbacks = #{
        tools                           => tool_handlers(maps:get(tools, Config, [])),
        hooks                           => Hooks,
        on_shell_authentication_request => maps:get(on_shell_authentication_request, Config, undefined),
        store_memory                    => maps:get(store_memory, Config, undefined)
    },
    true = ets:insert(Tab, {SessionId, Callbacks}),
    ok.

unregister_session_callbacks(SessionId, #state{session_callbacks = Tab}) ->
    catch ets:delete(Tab, SessionId),
    ok.

session_callback(Tab, SessionId, Key, Default) ->
    case ets:lookup(Tab, SessionId) of
        [{SessionId, Callbacks}] -> maps:get(Key, Callbacks, Default);
        []                       -> Default
    end.

tool_handlers(Tools) when is_list(Tools) ->
    lists:foldl(fun
        (Tool, Acc) when is_map(Tool) ->
            Name = maps:get(<<"name">>, Tool, maps:get(name, Tool, undefined)),
            Handler = maps:get(<<"handler">>, Tool, maps:get(handler, Tool, undefined)),
            case is_function(Handler, 1) andalso Name =/= undefined of
                true  -> Acc#{Name => Handler};
                false -> Acc
            end;
        (_Other, Acc) ->
            Acc
    end, #{}, Tools);
tool_handlers(_) ->
    #{}.

%% tool.call {sessionId, toolCallId, toolName, arguments} => {result: ToolResult}
tool_call_handler(Tab) ->
    fun(Params) ->
        SessionId = maps:get(<<"sessionId">>, Params, <<>>),
        ToolName = maps:get(<<"toolName">>, Params, <<>>),
        Tools = session_callback(Tab, SessionId, tools, #{}),
        Result = case maps:get(ToolName, Tools, undefined) of
            undefined ->
                copilot_types:tool_result(
                    <<"Tool '", ToolName/binary, "' is not supported by this client instance.">>,
                    failure,
                    <<"tool '", ToolName/binary, "' not supported">>);
            Handler ->
                Invocation = #{
                    <<"sessionId">>  => SessionId,
                    <<"toolCallId">> => maps:get(<<"toolCallId">>, Params, <<>>),
                    <<"toolName">>   => ToolName,
                    <<"arguments">>  => maps:get(<<"arguments">>, Params, #{})
                },
                try Handler(Invocation) of
                    Value -> copilot_tools:normalize_result(Value)
                catch
                    _:Reason ->
                        copilot_types:tool_result(
                            <<"Invoking this tool produced an error. "
                              "Detailed information is not available.">>,
                            failure,
                            iolist_to_binary(io_lib:format("~p", [Reason])))
                end
        end,
        #{<<"result">> => Result}
    end.

%% hooks.invoke {sessionId, hookType, input} => {output}. A hook returning a
%% map supplies the hook output; any other value (or a crash) leaves the
%% runtime's behavior unchanged.
hooks_invoke_handler(Tab) ->
    fun(Params) ->
        SessionId = maps:get(<<"sessionId">>, Params, <<>>),
        HookType = maps:get(<<"hookType">>, Params, <<>>),
        Input = maps:get(<<"input">>, Params, #{}),
        Hooks = session_callback(Tab, SessionId, hooks, #{}),
        case maps:get(hook_key(HookType), Hooks, undefined) of
            Hook when is_function(Hook, 1) ->
                try Hook(Input) of
                    Output when is_map(Output) -> #{<<"output">> => Output};
                    _                          -> #{}
                catch
                    _:_ -> #{}
                end;
            _ ->
                #{}
        end
    end.

hook_key(<<"preToolUse">>)            -> on_pre_tool_use;
hook_key(<<"postToolUse">>)           -> on_post_tool_use;
hook_key(<<"postToolUseFailure">>)    -> on_post_tool_use_failure;
hook_key(<<"preMcpToolCall">>)        -> on_pre_mcp_tool_call;
hook_key(<<"userPromptSubmitted">>)   -> on_user_prompt_submitted;
hook_key(<<"userPromptTransformed">>) -> on_user_prompt_transformed;
hook_key(<<"sessionStart">>)          -> on_session_start;
hook_key(<<"sessionEnd">>)            -> on_session_end;
hook_key(<<"errorOccurred">>)         -> on_error_occurred;
hook_key(<<"agentStop">>)             -> on_agent_stop;
hook_key(<<"subagentStart">>)         -> on_subagent_start;
hook_key(<<"subagentStop">>)          -> on_subagent_stop;
hook_key(_Unknown)                    -> undefined.

%% shellAuthentication.request {sessionId, requestId, program, prompt,
%% inputKind, command, cwd} => {action: accept, value} | {action: cancel}.
shell_authentication_handler(Tab, Pending) ->
    fun(Params) ->
        SessionId = maps:get(<<"sessionId">>, Params, <<>>),
        RequestId = maps:get(<<"requestId">>, Params, <<>>),
        case session_callback(Tab, SessionId, on_shell_authentication_request, undefined) of
            Handler when is_function(Handler, 1) ->
                Request = copilot_types:shell_authentication_request(Params),
                await_shell_authentication(Pending, {SessionId, RequestId},
                                           fun() -> Handler(Request) end);
            _ ->
                error({no_shell_authentication_handler, SessionId})
        end
    end.

%% Runs the handler in a worker so a shellAuthentication.cancel can answer the
%% pending request with `cancel` while the handler is still waiting for input.
await_shell_authentication(Pending, Key, Answer) ->
    Ref = make_ref(),
    Waiter = self(),
    true = ets:insert(Pending, {Key, Waiter, Ref}),
    {Worker, MonRef} = spawn_monitor(fun() -> Waiter ! {Ref, {answer, Answer()}} end),
    Reply = receive
        {Ref, {answer, Response}} ->
            erlang:demonitor(MonRef, [flush]),
            {ok, copilot_types:shell_authentication_response(Response)};
        {Ref, cancel} ->
            erlang:demonitor(MonRef, [flush]),
            exit(Worker, kill),
            {ok, copilot_types:shell_authentication_response(cancel)};
        {'DOWN', MonRef, process, Worker, Reason} ->
            {failed, Reason}
    end,
    ets:delete(Pending, Key),
    case Reply of
        {ok, Wire}     -> Wire;
        {failed, Why}  -> error({shell_authentication_handler_failed, Why})
    end.

%% shellAuthentication.cancel {sessionId, requestId} => {}
shell_authentication_cancel_handler(Pending) ->
    fun(Params) ->
        Key = {maps:get(<<"sessionId">>, Params, <<>>), maps:get(<<"requestId">>, Params, <<>>)},
        case ets:take(Pending, Key) of
            [{_Key, Waiter, Ref}] -> Waiter ! {Ref, cancel};
            []                    -> ok
        end,
        #{}
    end.

%% memoryStore.store {sessionId, subject, fact, citations, reason} => null.
%% The `store_memory` callback returns ok, or {error, Reason} to fail the
%% agent's store_memory tool call.
memory_store_handler(Tab) ->
    fun(Params) ->
        SessionId = maps:get(<<"sessionId">>, Params, <<>>),
        case session_callback(Tab, SessionId, store_memory, undefined) of
            Store when is_function(Store, 1) ->
                case Store(copilot_types:stored_memory(Params)) of
                    {error, Reason} -> error({memory_store_failed, Reason});
                    _               -> null
                end;
            _ ->
                error({no_memory_store_registered, SessionId})
        end
    end.

%% ---------------------------------------------------------------------------
%% Network callbacks (network.setProvider)
%% ---------------------------------------------------------------------------

network_options(Options) ->
    case maps:get(network, Options, undefined) of
        Network when is_map(Network) -> Network;
        _                            -> #{}
    end.

%% Register the configured callbacks with the runtime after the protocol check.
maybe_set_network_provider(RpcPid, Options) ->
    Params = copilot_types:network_set_provider_params(network_options(Options)),
    HasCallbacks = lists:any(fun(Key) -> maps:is_key(Key, Params) end,
                             [<<"resolveProxy">>, <<"authenticateNegotiate">>,
                              <<"authenticateBasic">>]),
    case HasCallbacks of
        false ->
            ok;
        true ->
            case copilot_jsonrpc:request(RpcPid, <<"network.setProvider">>, Params) of
                {ok, _}          -> ok;
                {error, _} = Err -> Err
            end
    end.

network_handlers(Network) ->
    Specs = [
        {resolve_proxy,          <<"network.resolveProxy">>,          fun resolve_proxy_handler/1},
        {authenticate_negotiate, <<"network.authenticateNegotiate">>, fun negotiate_handler/1},
        {authenticate_basic,     <<"network.authenticateBasic">>,     fun basic_auth_handler/1}
    ],
    [{Method, Build(Callback)} || {Key, Method, Build} <- Specs,
                                  Callback <- [maps:get(Key, Network, undefined)],
                                  is_function(Callback, 1)].

%% network.resolveProxy {url} => {proxy?}; `undefined` leaves the request to
%% the runtime's own proxy resolution.
resolve_proxy_handler(Callback) ->
    fun(Params) ->
        case Callback(#{url => maps:get(<<"url">>, Params, <<>>)}) of
            Proxy when is_binary(Proxy), Proxy =/= <<>> -> #{<<"proxy">> => Proxy};
            Proxy when is_list(Proxy), Proxy =/= []     -> #{<<"proxy">> => unicode:characters_to_binary(Proxy)};
            _                                          -> #{}
        end
    end.

%% network.authenticateNegotiate {spn} => {token?}. Tokens are never logged.
negotiate_handler(Callback) ->
    fun(Params) ->
        case Callback(#{spn => maps:get(<<"spn">>, Params, <<>>)}) of
            Token when is_binary(Token), Token =/= <<>> -> #{<<"token">> => Token};
            _                                          -> #{}
        end
    end.

%% network.authenticateBasic {proxy, realm?, previousRejected} =>
%% {username?, password?}. Credentials are never logged.
basic_auth_handler(Callback) ->
    fun(Params) ->
        Request = #{
            proxy             => maps:get(<<"proxy">>, Params, <<>>),
            realm             => maps:get(<<"realm">>, Params, undefined),
            previous_rejected => maps:get(<<"previousRejected">>, Params, false)
        },
        case Callback(Request) of
            #{username := Username} = Credentials ->
                basic_credentials(Username, maps:get(password, Credentials, undefined));
            {Username, Password} ->
                basic_credentials(Username, Password);
            _ ->
                #{}
        end
    end.

basic_credentials(Username, Password) ->
    Base = #{<<"username">> => to_bin(Username)},
    case Password of
        undefined -> Base;
        null      -> Base;
        _         -> Base#{<<"password">> => to_bin(Password)}
    end.

to_bin(Value) when is_binary(Value) -> Value;
to_bin(Value) when is_list(Value)   -> unicode:characters_to_binary(Value);
to_bin(Value) when is_atom(Value)   -> atom_to_binary(Value, utf8).

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

validate_session_config(Config, #state{mode = Mode}) ->
    Token = maps:get(github_token, Config, undefined),
    Provider = maps:get(github_token_provider, Config, undefined),
    AvailableTools = maps:get(available_tools, Config, undefined),
    if
        Token =/= undefined, Provider =/= undefined ->
            {error, {invalid_config,
                     <<"github_token and github_token_provider are mutually exclusive">>}};
        Provider =/= undefined, not is_function(Provider, 1) ->
            {error, {invalid_config, <<"github_token_provider must be a fun/1">>}};
        Mode =:= empty, AvailableTools =:= undefined ->
            {error, {invalid_config,
                     <<"CopilotClient is in mode \"empty\" but the session config did not "
                       "specify available_tools. Empty mode requires every session to "
                       "explicitly opt into the tools it wants.">>}};
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

%% Optional `config_dir` / `session_id` of the server-scoped mcp.secrets.* and
%% permissions.extensionApprovals.* requests.
with_config_dir(Opts, Params) ->
    put_optional(<<"configDir">>, maps:get(config_dir, Opts, undefined), Params).

with_session_id(Opts, Params) ->
    put_optional(<<"sessionId">>, maps:get(session_id, Opts, undefined), Params).

put_optional(_Key, undefined, Params) -> Params;
put_optional(Key, Value, Params)      -> Params#{Key => Value}.

decode_reply({ok, Result}, Decode)     -> {ok, Decode(Result)};
decode_reply({error, _} = Err, _Decode) -> Err.

ok_reply({ok, _})          -> ok;
ok_reply({error, _} = Err) -> Err.

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
    %% Tool calls arrive as `tool.call` requests and are routed by the
    %% client-level handler (see tool_call_handler/1).
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
            %% Notifications carry #{sessionId, event => #{type, data, ...}};
            %% the flat #{type, data} shape is still accepted. Every event type,
            %% including unknown ones and tool.shell_output, reaches subscribers.
            Event = case maps:get(<<"event">>, Params, undefined) of
                Nested when is_map(Nested) -> Nested;
                _                          -> Params
            end,
            EventType = maps:get(<<"type">>, Event, <<>>),
            EventData = maps:get(<<"data">>, Event, #{}),
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
