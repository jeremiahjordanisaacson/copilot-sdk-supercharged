%%%-------------------------------------------------------------------
%%% @doc Test harness for E2E tests.
%%%
%%% Spawns the shared replay proxy (npx tsx test/harness/server.ts),
%%% parses the `Listening: http://...` URL from stdout, and provides
%%% helpers for E2E test suites.
%%% @end
%%%-------------------------------------------------------------------
-module(e2e_test_harness).

-export([
    start_proxy/0,
    stop_proxy/1,
    proxy_url/1,
    configure/3,
    http_get/2,
    http_post/3
]).

-record(proxy, {
    port :: port(),
    url  :: string()
}).

%% @doc Start the replay proxy and return a handle.
start_proxy() ->
    %% Path from erlang/e2e to repo root test/harness/server.ts
    ServerPath = filename:join(["..", "..", "test", "harness", "server.ts"]),
    Cwd = filename:join(["..", "..", "test", "harness"]),
    AbsServerPath = filename:absname(ServerPath),
    AbsCwd = filename:absname(Cwd),

    Cmd = "npx tsx " ++ AbsServerPath,
    Port = open_port({spawn, Cmd}, [
        {cd, AbsCwd},
        {line, 1024},
        stderr_to_stdout,
        exit_status
    ]),

    %% Read the first line to get the listening URL
    Url = receive
        {Port, {data, {eol, Line}}} ->
            parse_proxy_url(Line);
        {Port, {data, {noeol, Partial}}} ->
            %% Might arrive in chunks
            receive
                {Port, {data, {eol, Rest}}} ->
                    parse_proxy_url(Partial ++ Rest)
            after 30000 ->
                error(proxy_start_timeout)
            end
    after 30000 ->
        error(proxy_start_timeout)
    end,

    os:putenv("COPILOT_API_URL", Url),
    ct:log("Proxy listening at ~s~n", [Url]),
    #proxy{port = Port, url = Url}.

%% @doc Stop the replay proxy.
stop_proxy(#proxy{port = Port}) ->
    catch port_close(Port),
    ok.

%% @doc Return the proxy URL.
proxy_url(#proxy{url = Url}) -> Url.

%% @doc Configure the proxy for a specific test snapshot.
configure(#proxy{url = Url}, FilePath, WorkDir) ->
    Body = io_lib:format("{\"filePath\":\"~s\",\"workDir\":\"~s\"}", [FilePath, WorkDir]),
    http_post_raw(Url ++ "/config", iolist_to_binary(Body)),
    ok.

%% @doc HTTP GET helper.
http_get(#proxy{url = BaseUrl}, Path) ->
    Url = BaseUrl ++ Path,
    inets:start(),
    ssl:start(),
    case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
        {ok, {_, _, Body}} -> {ok, Body};
        {error, Reason} -> {error, Reason}
    end.

%% @doc HTTP POST helper.
http_post(#proxy{url = BaseUrl}, Path, Body) ->
    Url = BaseUrl ++ Path,
    http_post_raw(Url, Body).

http_post_raw(Url, Body) ->
    inets:start(),
    ssl:start(),
    httpc:request(post, {Url,
        [{"Authorization", "Bearer fake-token-for-e2e-tests"}],
        "application/json",
        Body
    }, [], [{body_format, binary}]).

%% --- Internal ---

parse_proxy_url(Line) ->
    case re:run(Line, "Listening: (http://[^ \\r\\n]+)", [{capture, [1], list}]) of
        {match, [Url]} -> Url;
        nomatch -> error({bad_proxy_output, Line})
    end.
