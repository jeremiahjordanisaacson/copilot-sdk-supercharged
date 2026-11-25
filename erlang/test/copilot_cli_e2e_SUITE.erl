%%%-------------------------------------------------------------------
%%% @doc End-to-end tests against the real Copilot CLI.
%%%
%%% The Erlang client spawns the real CLI (COPILOT_CLI_PATH, or the CLI
%%% installed in nodejs/node_modules) over stdio, and the CLI talks to the
%%% shared replaying CAPI proxy (test/harness/server.ts) with the snapshots in
%%% test/snapshots/. The proxy setup mirrors python/e2e/testharness (proxy.py
%%% and context.py): the per-token /copilot_internal/user response, a
%%% `/config` POST of {filePath, workDir} per test, the CONNECT proxy and CA
%%% environment, and an isolated COPILOT_HOME.
%%%
%%% Prerequisites: Node.js with `npm install` run in nodejs/ and
%%% test/harness/. Without them the suite is skipped, except on GitHub
%%% Actions, where it fails instead.
%%%
%%% Run with:
%%%   cd erlang
%%%   rebar3 ct --suite test/copilot_cli_e2e_SUITE
%%% @end
%%%-------------------------------------------------------------------
-module(copilot_cli_e2e_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    real_cli_send_message/1,
    real_cli_permission_request/1,
    diag_permissions_with_connect/1
]).

-define(TOKEN, <<"fake-token-for-e2e-tests">>).
-define(MODEL, <<"claude-sonnet-4.5">>).
-define(PROXY_START_TIMEOUT, 120000).
-define(REPLY_TIMEOUT, 120000).

suite() ->
    [{timetrap, {minutes, 10}}].

all() ->
    [real_cli_send_message, real_cli_permission_request, diag_permissions_with_connect].

%% ---------------------------------------------------------------------------
%% Setup
%% ---------------------------------------------------------------------------

init_per_suite(Config) ->
    case prerequisites() of
        {ok, Env} ->
            start_harness(Env, Config);
        {missing, Why} ->
            Reason = "real CLI E2E prerequisites missing: " ++ Why,
            case os:getenv("GITHUB_ACTIONS") of
                "true" -> ct:fail(Reason);
                _      -> {skip, Reason}
            end
    end.

end_per_suite(Config) ->
    stop_proxy(?config(proxy, Config)),
    case ?config(temp_root, Config) of
        TempRoot when is_list(TempRoot) -> _ = file:del_dir_r(TempRoot);
        _                               -> ok
    end,
    ok.

init_per_testcase(TestCase, Config) ->
    #{root := Root} = ?config(e2e, Config),
    {Dir, Name} = snapshot(TestCase),
    Snapshot = filename:join([Root, "test", "snapshots", Dir, Name ++ ".yaml"]),
    true = filelib:is_regular(Snapshot),
    Home = ?config(home, Config),
    Work = ?config(work, Config),
    ok = reset_dir(Home),
    ok = reset_dir(Work),
    #{url := Url} = ?config(proxy, Config),
    {ok, 200, _} = post_json(<<Url/binary, "/config">>,
                             #{<<"filePath">> => unicode:characters_to_binary(Snapshot),
                               <<"workDir">>  => unicode:characters_to_binary(Work)}),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

snapshot(real_cli_send_message) ->
    {"session", "sendandwait_blocks_until_session_idle_and_returns_final_assistant_message"};
snapshot(_PermissionTest) ->
    {"permissions", "should_receive_toolcallid_in_permission_requests"}.

%% ---------------------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------------------

%% Start and ping (SDK protocol 4), create a session, send one message, and
%% receive the assistant reply and the idle event.
real_cli_send_message(Config) ->
    Client = start_client(Config),
    try
        {ok, Pong} = copilot_client:ping(Client, <<"erlang-e2e">>),
        4 = maps:get(<<"protocolVersion">>, Pong,
                     maps:get(<<"sdkProtocolVersion">>, Pong, undefined)),
        {ok, Session} = copilot_client:create_session(Client, #{
            model                 => ?MODEL,
            on_permission_request => fun(_Request) -> copilot_types:permission_result(approved) end
        }),
        SessionId = copilot_session:session_id(Session),
        true = is_binary(SessionId),
        _ = copilot_session:subscribe(Session, self()),
        {ok, _MessageId} = copilot_session:send(Session, #{prompt => <<"What is 2+2?">>}),
        Content = await_reply(SessionId),
        ct:pal("Assistant reply: ~ts", [Content]),
        true = binary:match(Content, <<"4">>) =/= nomatch,
        ok
    after
        stop_client(Client)
    end.

%% The runtime asks for permission to run a shell command through a
%% permission.requested event; the handler approves it through
%% session.permissions.handlePendingPermissionRequest, so the command runs and
%% the turn completes.
real_cli_permission_request(Config) ->
    diag_permissions(Config, false).

diag_permissions_with_connect(Config) ->
    diag_permissions(Config, true).

diag_permissions(Config, Connect) ->
    Self = self(),
    Client = start_client(Config),
    try
        ClientRpc = element(5, sys:get_state(Client)),
        case Connect of
            true ->
                ct:pal("DIAG connect: ~tp", [copilot_jsonrpc:request(ClientRpc, <<"connect">>,
                    #{<<"supportedTaskKinds">> => [<<"agent">>, <<"client">>, <<"shell">>]})]);
            false ->
                ok
        end,
        Handler = fun(Request, Invocation) ->
            Self ! {permission_request, Request, Invocation},
            copilot_types:permission_result(approve_once)
        end,
        {ok, Session} = copilot_client:create_session(Client, #{
            model                 => ?MODEL,
            on_permission_request => Handler
        }),
        SessionId = copilot_session:session_id(Session),
        lists:foreach(fun(Method) ->
            ct:pal("DIAG ~ts: ~tp", [Method, copilot_jsonrpc:request(ClientRpc, Method,
                                                                     #{<<"sessionId">> => SessionId})])
        end, [<<"session.permissions.getMode">>, <<"session.permissions.getAllowAll">>,
              <<"session.permissions.getRequired">>, <<"session.permissions.getCapabilities">>]),
        _ = copilot_session:subscribe(Session, self()),
        {ok, _MessageId} = copilot_session:send(Session, #{prompt => <<"Run 'echo test'">>}),
        Content = await_reply(SessionId),
        ct:pal("Assistant reply: ~ts", [Content]),
        Requests = permission_requests(),
        ct:pal("Permission requests: ~p", [Requests]),
        [{ShellRequest, Invocation} | _] =
            [Entry || {#{<<"kind">> := <<"shell">>}, _} = Entry <- Requests],
        true = is_binary(maps:get(<<"toolCallId">>, ShellRequest)),
        #{session_id := SessionId, request_id := RequestId} = Invocation,
        true = is_binary(RequestId),
        true = binary:match(Content, <<"test">>) =/= nomatch,
        ok
    after
        stop_client(Client)
    end.

%% ---------------------------------------------------------------------------
%% Client helpers
%% ---------------------------------------------------------------------------

start_client(Config) ->
    #{cli := Cli} = ?config(e2e, Config),
    {ok, Client} = copilot_client:start_link(#{
        cli_path     => Cli,
        cwd          => ?config(work, Config),
        env          => cli_env(Config),
        github_token => ?TOKEN,
        auto_start   => false
    }),
    case copilot_client:start_cli(Client) of
        ok ->
            connected = copilot_client:state(Client),
            Client;
        {error, Reason} ->
            ct:fail({cli_start_failed, Reason})
    end.

stop_client(Client) ->
    catch copilot_client:stop(Client),
    ok.

%% Environment of the CLI process, as in python/e2e/testharness (proxy.py
%% get_proxy_env and context.py get_env). The token reaches the CLI as
%% COPILOT_SDK_AUTH_TOKEN through the client's github_token option.
cli_env(Config) ->
    #{url := Url, connect_proxy_url := ConnectProxy, ca_file := CaFile} = ?config(proxy, Config),
    Home = ?config(home, Config),
    NoProxy = "127.0.0.1,localhost,::1",
    [
        {"HTTP_PROXY", ConnectProxy},
        {"HTTPS_PROXY", ConnectProxy},
        {"http_proxy", ConnectProxy},
        {"https_proxy", ConnectProxy},
        {"NO_PROXY", NoProxy},
        {"no_proxy", NoProxy},
        {"NODE_EXTRA_CA_CERTS", CaFile},
        {"SSL_CERT_FILE", CaFile},
        {"REQUESTS_CA_BUNDLE", CaFile},
        {"CURL_CA_BUNDLE", CaFile},
        {"GIT_SSL_CAINFO", CaFile},
        {"GH_ENTERPRISE_TOKEN", false},
        {"GITHUB_ENTERPRISE_TOKEN", false},
        {"COPILOT_API_URL", Url},
        {"COPILOT_DEBUG_GITHUB_API_URL", Url},
        {"COPILOT_HOME", Home},
        {"COPILOT_CACHE_HOME", filename:join(Home, "cache")},
        {"COPILOT_HMAC_KEY", false},
        {"CAPI_HMAC_KEY", false},
        {"GH_CONFIG_DIR", Home},
        {"XDG_CONFIG_HOME", Home},
        {"XDG_STATE_HOME", Home},
        {"GH_TOKEN", ?TOKEN},
        {"GITHUB_TOKEN", ?TOKEN},
        {"COPILOT_MCP_APPS", "true"},
        {"MCP_APPS", "true"}
    ].

%% Wait for the turn to end: the last assistant.message content, returned
%% when session.idle follows it.
await_reply(SessionId) ->
    await_reply(SessionId, erlang:monotonic_time(millisecond) + ?REPLY_TIMEOUT, undefined).

await_reply(SessionId, Deadline, Content) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {copilot_session_event, SessionId, #{type := <<"assistant.message">>, data := Data}} ->
            await_reply(SessionId, Deadline, maps:get(<<"content">>, Data, <<>>));
        {copilot_session_event, SessionId, #{type := <<"session.idle">>}}
          when Content =/= undefined ->
            Content;
        {copilot_session_event, SessionId, #{type := <<"session.error">>, data := Data}} ->
            ct:fail({session_error, Data});
        {copilot_session_event, SessionId, #{type := Type, data := Data}} ->
            case binary:match(Type, [<<"permission">>, <<"tool.">>, <<"sandbox">>,
                                     <<"session.start">>, <<"managed_settings">>]) of
                nomatch -> ct:pal("Session event ~ts", [Type]);
                _       -> ct:pal("Session event ~ts: ~tp", [Type, Data])
            end,
            await_reply(SessionId, Deadline, Content)
    after Remaining ->
        ct:fail({no_reply_before_timeout, Content})
    end.

permission_requests() ->
    receive
        {permission_request, Request, Invocation} ->
            [{Request, Invocation} | permission_requests()]
    after 0 ->
        []
    end.

%% ---------------------------------------------------------------------------
%% Prerequisites
%% ---------------------------------------------------------------------------

prerequisites() ->
    case repo_root() of
        undefined ->
            {missing, "test/harness/server.ts was not found above this build"};
        Root ->
            Harness = filename:join([Root, "test", "harness"]),
            Npx = os:find_executable("npx"),
            Cli = cli_path(Root),
            HarnessDeps = filelib:is_dir(filename:join(Harness, "node_modules")),
            if
                Npx =:= false ->
                    {missing, "npx is not on PATH"};
                Cli =:= undefined ->
                    {missing, "no Copilot CLI (set COPILOT_CLI_PATH or run npm install in nodejs/)"};
                not HarnessDeps ->
                    {missing, "run npm install in test/harness/"};
                true ->
                    {ok, #{root => Root, harness => Harness, npx => Npx, cli => Cli}}
            end
    end.

%% Common Test runs each test in its log directory, so the repository root is
%% found by walking up from the directory rebar3 was started in, the
%% application directory, or this module's source.
repo_root() ->
    Source = proplists:get_value(source, ?MODULE:module_info(compile)),
    Starts = [os:getenv("PWD"), code:lib_dir(copilot_sdk), Source],
    find_repo_root([filename:absname(Start) || Start <- Starts, is_list(Start)]).

find_repo_root([]) ->
    undefined;
find_repo_root([Start | Rest]) ->
    case walk_up(Start) of
        undefined -> find_repo_root(Rest);
        Root      -> Root
    end.

walk_up(Dir) ->
    case filelib:is_regular(filename:join([Dir, "test", "harness", "server.ts"])) of
        true ->
            Dir;
        false ->
            case filename:dirname(Dir) of
                Dir    -> undefined;
                Parent -> walk_up(Parent)
            end
    end.

%% COPILOT_CLI_PATH (CI points it at nodejs/node_modules/.bin/copilot), then
%% the CLI installed in nodejs/node_modules.
cli_path(Root) ->
    FromEnv = case os:getenv("COPILOT_CLI_PATH") of
        false -> [];
        ""    -> [];
        Path  -> [Path]
    end,
    Modules = filename:join([Root, "nodejs", "node_modules"]),
    Candidates = FromEnv ++ [filename:join([Modules, ".bin", "copilot"]),
                             filename:join([Modules, "@github", "copilot", "npm-loader.js"])],
    case [filename:absname(C) || C <- Candidates, filelib:is_regular(C)] of
        [Found | _] -> Found;
        []          -> undefined
    end.

reset_dir(Dir) ->
    _ = file:del_dir_r(Dir),
    filelib:ensure_dir(filename:join(Dir, "placeholder")).

%% ---------------------------------------------------------------------------
%% Replaying CAPI proxy (test/harness/server.ts)
%% ---------------------------------------------------------------------------

start_harness(#{harness := Harness, npx := Npx} = Env, Config) ->
    {ok, _} = application:ensure_all_started(inets),
    %% Isolated directories outside the repository, as the other SDKs' E2E
    %% harnesses use.
    TempRoot = temp_root(),
    Home = filename:join(TempRoot, "home"),
    Work = filename:join(TempRoot, "work"),
    ok = reset_dir(Home),
    ok = reset_dir(Work),
    case start_proxy(Npx, Harness) of
        {ok, Proxy} ->
            ct:pal("Replay proxy listening at ~ts", [maps:get(url, Proxy)]),
            ok = register_copilot_user(Proxy, ?TOKEN),
            [{e2e, Env}, {proxy, Proxy}, {temp_root, TempRoot}, {home, Home}, {work, Work}
             | Config];
        {error, Reason} ->
            _ = file:del_dir_r(TempRoot),
            ct:fail({proxy_start_failed, Reason})
    end.

temp_root() ->
    Base = case [D || Name <- ["RUNNER_TEMP", "TMPDIR", "TEMP", "TMP"],
                      D <- [os:getenv(Name)], is_list(D), D =/= ""] of
        [Dir | _] -> Dir;
        []        -> "/tmp"
    end,
    filename:join(Base, "copilot-erlang-e2e-" ++ os:getpid() ++ "-" ++
                        integer_to_list(erlang:unique_integer([positive]))).

%% The proxy runs under a dedicated process that owns its port, so it outlives
%% init_per_suite. It prints `Listening: <url> <json>` on stdout, where the
%% JSON carries the CONNECT proxy URL and CA file.
start_proxy(Npx, Harness) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() -> proxy_init(Parent, Ref, Npx, Harness) end),
    receive
        {Ref, {ok, Url, ConnectProxy, CaFile}} ->
            {ok, #{pid => Pid, url => Url, connect_proxy_url => ConnectProxy, ca_file => CaFile}};
        {Ref, {error, _} = Error} ->
            Error
    after ?PROXY_START_TIMEOUT ->
        exit(Pid, kill),
        {error, proxy_start_timeout}
    end.

proxy_init(Parent, Ref, Npx, Harness) ->
    Settings = [{args, ["tsx", filename:join(Harness, "server.ts")]}, {cd, Harness},
                {line, 65536}, binary, use_stdio, exit_status, hide],
    try open_port({spawn_executable, Npx}, Settings) of
        Port ->
            case await_listening(Port, <<>>) of
                {ok, _Url, _ConnectProxy, _CaFile} = Listening ->
                    Parent ! {Ref, Listening},
                    proxy_loop(Port);
                {error, _} = Error ->
                    catch port_close(Port),
                    Parent ! {Ref, Error}
            end
    catch
        error:Reason ->
            Parent ! {Ref, {error, {spawn_failed, Reason}}}
    end.

await_listening(Port, Partial) ->
    receive
        {Port, {data, {noeol, Chunk}}} ->
            await_listening(Port, <<Partial/binary, Chunk/binary>>);
        {Port, {data, {eol, Chunk}}} ->
            Line = <<Partial/binary, Chunk/binary>>,
            case parse_listening(Line) of
                nomatch -> await_listening(Port, <<>>);
                Result  -> Result
            end;
        {Port, {exit_status, Status}} ->
            {error, {proxy_exited, Status}}
    after ?PROXY_START_TIMEOUT ->
        {error, proxy_start_timeout}
    end.

parse_listening(Line) ->
    case re:run(Line, <<"Listening: (http://[^\\s]+)\\s+(\\{.*\\})">>,
                [{capture, all_but_first, binary}]) of
        {match, [Url, Json]} ->
            case catch jsx:decode(Json, [return_maps]) of
                #{<<"connectProxyUrl">> := ConnectProxy, <<"caFilePath">> := CaFile} ->
                    {ok, Url, ConnectProxy, CaFile};
                Metadata ->
                    {error, {missing_connect_proxy_metadata, Metadata}}
            end;
        nomatch ->
            nomatch
    end.

proxy_loop(Port) ->
    receive
        {Port, {data, _}} ->
            proxy_loop(Port);
        {Port, {exit_status, _}} ->
            ok;
        stop ->
            catch port_close(Port),
            ok
    end.

stop_proxy(#{pid := Pid, url := Url}) ->
    %% The proxy exits after answering /stop; snapshots are never rewritten.
    _ = post_json(<<Url/binary, "/stop?skipWritingCache=true">>, #{}),
    MonRef = monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', MonRef, process, Pid, _} -> ok
    after 10000 ->
        exit(Pid, kill),
        ok
    end;
stop_proxy(_) ->
    ok.

%% Register the /copilot_internal/user response for the token the CLI uses,
%% with the proxy as the Copilot API endpoint.
register_copilot_user(#{url := Url}, Token) ->
    User = #{
        <<"login">>                 => <<"e2e-test-user">>,
        <<"id">>                    => 12345,
        <<"copilot_plan">>          => <<"individual_pro">>,
        <<"endpoints">>             => #{
            <<"api">>       => Url,
            <<"telemetry">> => <<"https://sdk-hydro-collector.example.com">>
        },
        <<"analytics_tracking_id">> => <<"e2e-test-tracking-id">>
    },
    {ok, 200, _} = post_json(<<Url/binary, "/copilot-user-config">>,
                             #{<<"token">> => Token, <<"response">> => User}),
    ok.

post_json(Url, Body) ->
    Request = {unicode:characters_to_list(Url), [], "application/json", jsx:encode(Body)},
    case httpc:request(post, Request, [{timeout, 60000}], [{body_format, binary}]) of
        {ok, {{_Version, Status, _Phrase}, _Headers, ResponseBody}} ->
            {ok, Status, ResponseBody};
        {error, _} = Error ->
            Error
    end.
