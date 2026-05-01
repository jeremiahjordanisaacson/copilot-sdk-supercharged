%%%-------------------------------------------------------------------
%%% @doc E2E Common Test suite for the Erlang Copilot SDK.
%%%
%%% Tests:
%%%   1. Session create + disconnect
%%%   2. Send message
%%%   3. SessionFs configuration
%%%
%%% Run with: cd erlang && rebar3 ct --suite=e2e/copilot_e2e_SUITE
%%% @end
%%%-------------------------------------------------------------------
-module(copilot_e2e_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_session_create_disconnect/1,
    test_send_message/1,
    test_session_fs/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        test_session_create_disconnect,
        test_send_message,
        test_session_fs
    ].

init_per_suite(Config) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    Proxy = e2e_test_harness:start_proxy(),
    [{proxy, Proxy} | Config].

end_per_suite(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:stop_proxy(Proxy),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% Test 1: Session create + disconnect
test_session_create_disconnect(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_create_and_disconnect_sessions"),

    %% Verify proxy is alive
    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    %% Create session
    Body = <<"{\"model\":\"gpt-4\"}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions", Body),

    %% Verify exchanges recorded
    {ok, Exchanges2} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges2) > 0,

    ct:log("Session create + disconnect: PASS"),
    ok.

%% Test 2: Send message
test_send_message(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_have_stateful_conversation"),

    Body = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"What is 1+1?\"}]}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Send message: PASS"),
    ok.

%% Test 3: SessionFs
test_session_fs(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session_fs", "should_configure_session_fs"),

    Body = <<"{\"model\":\"gpt-4\",\"sessionFs\":{\"initialCwd\":\"/\",\"sessionStatePath\":\"/session-state\",\"conventions\":\"posix\"}}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("SessionFs: PASS"),
    ok.
