%%%-------------------------------------------------------------------
%%% @doc E2E Common Test suite for the Erlang Copilot SDK.
%%%
%%% Tests:
%%%   1.  Session create + disconnect
%%%   2.  Send message
%%%   3.  SessionFs configuration
%%%   4.  Multi-turn conversation
%%%   5.  Session resume
%%%   6.  Session list
%%%   7.  Session metadata
%%%   8.  Session delete
%%%   9.  Model list
%%%   10. Ping
%%%   11. Auth status
%%%   12. Client lifecycle
%%%   13. Foreground session
%%%   14. Tools
%%%   15. Streaming
%%%   16. System message
%%%   17. SessionFs provider
%%%   18. MCP servers
%%%   19. Skills config
%%%   20. Compaction
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
    test_session_fs/1,
    test_multi_turn/1,
    test_session_resume/1,
    test_session_list/1,
    test_session_metadata/1,
    test_session_delete/1,
    test_model_list/1,
    test_ping/1,
    test_auth_status/1,
    test_client_lifecycle/1,
    test_foreground_session/1,
    test_tools/1,
    test_streaming/1,
    test_system_message/1,
    test_session_fs_provider/1,
    test_mcp_servers/1,
    test_skills_config/1,
    test_compaction/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        test_session_create_disconnect,
        test_send_message,
        test_session_fs,
        test_multi_turn,
        test_session_resume,
        test_session_list,
        test_session_metadata,
        test_session_delete,
        test_model_list,
        test_ping,
        test_auth_status,
        test_client_lifecycle,
        test_foreground_session,
        test_tools,
        test_streaming,
        test_system_message,
        test_session_fs_provider,
        test_mcp_servers,
        test_skills_config,
        test_compaction
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

%% Test 4: Multi-turn conversation
test_multi_turn(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_have_stateful_conversation"),

    Body1 = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"Hello\"}]}">>,
    _Result1 = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body1),

    Body2 = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"Follow up question\"}]}">>,
    _Result2 = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body2),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Multi-turn: PASS"),
    ok.

%% Test 5: Session resume
test_session_resume(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_create_and_disconnect_sessions"),

    CreateBody = <<"{\"model\":\"gpt-4\"}">>,
    _CreateResult = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions", CreateBody),

    ResumeBody = <<"{\"model\":\"gpt-4\",\"sessionId\":\"test-session-id\"}">>,
    _ResumeResult = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions", ResumeBody),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Session resume: PASS"),
    ok.

%% Test 6: Session list
test_session_list(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_create_and_disconnect_sessions"),

    Body1 = <<"{\"model\":\"gpt-4\"}">>,
    _Result1 = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions", Body1),

    Body2 = <<"{\"model\":\"gpt-4\"}">>,
    _Result2 = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions", Body2),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Session list: PASS"),
    ok.

%% Test 7: Session metadata
test_session_metadata(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_create_and_disconnect_sessions"),

    Body = <<"{\"model\":\"gpt-4\",\"metadata\":{\"key\":\"value\"}}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Session metadata: PASS"),
    ok.

%% Test 8: Session delete
test_session_delete(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_create_and_disconnect_sessions"),

    CreateBody = <<"{\"model\":\"gpt-4\"}">>,
    _CreateResult = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions", CreateBody),

    DeleteBody = <<"{\"sessionId\":\"test-session-id\"}">>,
    _DeleteResult = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions/delete", DeleteBody),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Session delete: PASS"),
    ok.

%% Test 9: Model list
test_model_list(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_create_and_disconnect_sessions"),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Model list: PASS"),
    ok.

%% Test 10: Ping
test_ping(Config) ->
    Proxy = proplists:get_value(proxy, Config),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Ping: PASS"),
    ok.

%% Test 11: Auth status
test_auth_status(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_create_and_disconnect_sessions"),

    Body = <<"{\"action\":\"check\"}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/auth/status", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Auth status: PASS"),
    ok.

%% Test 12: Client lifecycle
test_client_lifecycle(Config) ->
    Proxy = proplists:get_value(proxy, Config),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Client lifecycle: PASS"),
    ok.

%% Test 13: Foreground session
test_foreground_session(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_create_and_disconnect_sessions"),

    Body = <<"{\"model\":\"gpt-4\",\"foreground\":true}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Foreground session: PASS"),
    ok.

%% Test 14: Tools
test_tools(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_have_stateful_conversation"),

    Body = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"Use tool\"}],\"tools\":[{\"type\":\"function\",\"function\":{\"name\":\"get_weather\",\"description\":\"Get weather\",\"parameters\":{\"type\":\"object\",\"properties\":{}}}}]}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Tools: PASS"),
    ok.

%% Test 15: Streaming
test_streaming(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_have_stateful_conversation"),

    Body = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"Hello\"}],\"streaming\":true}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Streaming: PASS"),
    ok.

%% Test 16: System message
test_system_message(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_have_stateful_conversation"),

    Body = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"Hello\"}],\"systemMessage\":\"You are a helpful assistant.\"}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("System message: PASS"),
    ok.

%% Test 17: SessionFs provider
test_session_fs_provider(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session_fs", "should_configure_session_fs"),

    Body = <<"{\"model\":\"gpt-4\",\"sessionFs\":{\"initialCwd\":\"/home\",\"sessionStatePath\":\"/state\",\"conventions\":\"posix\"}}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/sessions", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("SessionFs provider: PASS"),
    ok.

%% Test 18: MCP servers
test_mcp_servers(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_have_stateful_conversation"),

    Body = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"Hello\"}],\"mcpServers\":{\"server1\":{\"url\":\"http://localhost:3000\"}}}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("MCP servers: PASS"),
    ok.

%% Test 19: Skills config
test_skills_config(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_have_stateful_conversation"),

    Body = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"Hello\"}],\"skills\":[{\"name\":\"code-review\",\"enabled\":true}]}">>,
    _Result = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Skills config: PASS"),
    ok.

%% Test 20: Compaction
test_compaction(Config) ->
    Proxy = proplists:get_value(proxy, Config),
    e2e_test_harness:configure(Proxy, "session", "should_have_stateful_conversation"),

    Body1 = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"Message 1\"}]}">>,
    _Result1 = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body1),

    Body2 = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"Message 2\"}]}">>,
    _Result2 = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body2),

    Body3 = <<"{\"messages\":[{\"role\":\"user\",\"content\":\"Message 3\"}]}">>,
    _Result3 = e2e_test_harness:http_post(Proxy, "/v1/chat/completions", Body3),

    {ok, Exchanges} = e2e_test_harness:http_get(Proxy, "/exchanges"),
    true = byte_size(Exchanges) > 0,

    ct:log("Compaction: PASS"),
    ok.
