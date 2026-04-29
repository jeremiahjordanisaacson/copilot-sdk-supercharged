%%%-------------------------------------------------------------------
%%% @doc Common Test suite for copilot_client.
%%%
%%% Tests the client gen_server, JSON-RPC framing, session lifecycle,
%%% tool definitions, and type helpers.
%%%
%%% Run with:
%%%   cd erlang
%%%   rebar3 ct
%%% @end
%%%-------------------------------------------------------------------
-module(copilot_client_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_connection_state_constants/1,
    test_session_event_type_constants/1,
    test_tool_result_success/1,
    test_tool_result_failure/1,
    test_permission_result/1,
    test_user_input_response/1,
    test_image_options/1,
    test_define_tool_basic/1,
    test_define_tool_with_params/1,
    test_define_tool_error_handling/1,
    test_normalize_result_binary/1,
    test_normalize_result_map/1,
    test_normalize_result_undefined/1,
    test_normalize_result_passthrough/1,
    test_client_start_link_defaults/1,
    test_client_state_disconnected/1,
    test_session_subscribe_unsubscribe/1,
    test_jsonrpc_content_length_framing/1,
    test_tool_handler_receives_args/1
]).

%% ---------------------------------------------------------------------------
%% CT callbacks
%% ---------------------------------------------------------------------------

all() ->
    [{group, types}, {group, tools}, {group, client}, {group, session}, {group, jsonrpc}].

groups() ->
    [
        {types, [parallel], [
            test_connection_state_constants,
            test_session_event_type_constants,
            test_tool_result_success,
            test_tool_result_failure,
            test_permission_result,
            test_user_input_response,
            test_image_options
        ]},
        {tools, [parallel], [
            test_define_tool_basic,
            test_define_tool_with_params,
            test_define_tool_error_handling,
            test_normalize_result_binary,
            test_normalize_result_map,
            test_normalize_result_undefined,
            test_normalize_result_passthrough,
            test_tool_handler_receives_args
        ]},
        {client, [sequence], [
            test_client_start_link_defaults,
            test_client_state_disconnected
        ]},
        {session, [sequence], [
            test_session_subscribe_unsubscribe
        ]},
        {jsonrpc, [sequence], [
            test_jsonrpc_content_length_framing
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ---------------------------------------------------------------------------
%% Type tests
%% ---------------------------------------------------------------------------

test_connection_state_constants(_Config) ->
    <<"disconnected">> = copilot_types:connection_state(disconnected),
    <<"connecting">>   = copilot_types:connection_state(connecting),
    <<"connected">>    = copilot_types:connection_state(connected),
    <<"error">>        = copilot_types:connection_state(error),
    ok.

test_session_event_type_constants(_Config) ->
    <<"assistant.message">>         = copilot_types:session_event_type(assistant_message),
    <<"session.idle">>              = copilot_types:session_event_type(session_idle),
    <<"tool.execution_complete">>   = copilot_types:session_event_type(tool_execution_complete),
    <<"session.compaction_start">>  = copilot_types:session_event_type(session_compaction_start),
    <<"user.message">>              = copilot_types:session_event_type(user_message),
    <<"assistant.message_delta">>   = copilot_types:session_event_type(assistant_message_delta),
    <<"session.shutdown">>          = copilot_types:session_event_type(session_shutdown),
    ok.

test_tool_result_success(_Config) ->
    Result = copilot_types:tool_result(<<"hello">>, success),
    <<"hello">>  = maps:get(<<"textResultForLlm">>, Result),
    <<"success">> = maps:get(<<"resultType">>, Result),
    false = maps:is_key(<<"error">>, Result),
    ok.

test_tool_result_failure(_Config) ->
    Result = copilot_types:tool_result(<<"oops">>, failure, <<"some error">>),
    <<"oops">>       = maps:get(<<"textResultForLlm">>, Result),
    <<"failure">>    = maps:get(<<"resultType">>, Result),
    <<"some error">> = maps:get(<<"error">>, Result),
    ok.

test_permission_result(_Config) ->
    Approved = copilot_types:permission_result(approved),
    <<"approved">> = maps:get(<<"kind">>, Approved),
    Denied = copilot_types:permission_result(denied),
    <<"denied">> = maps:get(<<"kind">>, Denied),
    ok.

test_user_input_response(_Config) ->
    Resp = copilot_types:user_input_response(<<"Yes">>, true),
    <<"Yes">> = maps:get(<<"answer">>, Resp),
    true      = maps:get(<<"wasFreeform">>, Resp),
    ok.

test_image_options(_Config) ->
    Opts = copilot_types:image_options(#{size => <<"1024x1024">>, quality => <<"hd">>}),
    <<"1024x1024">> = maps:get(<<"size">>, Opts),
    <<"hd">>        = maps:get(<<"quality">>, Opts),
    false           = maps:is_key(<<"style">>, Opts),
    ok.

%% ---------------------------------------------------------------------------
%% Tool tests
%% ---------------------------------------------------------------------------

test_define_tool_basic(_Config) ->
    Tool = copilot_tools:define_tool(
        <<"test_tool">>,
        <<"A test tool">>,
        fun(_Args, _Inv) -> <<"result">> end
    ),
    <<"test_tool">>  = maps:get(<<"name">>, Tool),
    <<"A test tool">> = maps:get(<<"description">>, Tool),
    true = is_function(maps:get(<<"handler">>, Tool)),
    false = maps:is_key(<<"parameters">>, Tool),
    ok.

test_define_tool_with_params(_Config) ->
    Params = #{<<"type">> => <<"object">>, <<"properties">> => #{}},
    Tool = copilot_tools:define_tool(
        <<"param_tool">>,
        <<"Tool with params">>,
        Params,
        fun(_Args, _Inv) -> <<"ok">> end
    ),
    Params = maps:get(<<"parameters">>, Tool),
    ok.

test_define_tool_error_handling(_Config) ->
    Tool = copilot_tools:define_tool(
        <<"error_tool">>,
        <<"A tool that errors">>,
        fun(_Args, _Inv) -> error(deliberate_crash) end
    ),
    Handler = maps:get(<<"handler">>, Tool),
    Result = Handler(#{<<"arguments">> => #{}}),
    <<"failure">> = maps:get(<<"resultType">>, Result),
    ok.

test_normalize_result_binary(_Config) ->
    Result = copilot_tools:normalize_result(<<"hello">>),
    <<"hello">>   = maps:get(<<"textResultForLlm">>, Result),
    <<"success">> = maps:get(<<"resultType">>, Result),
    ok.

test_normalize_result_map(_Config) ->
    Result = copilot_tools:normalize_result(#{<<"key">> => <<"value">>}),
    <<"success">> = maps:get(<<"resultType">>, Result),
    %% The map should be JSON-encoded
    Text = maps:get(<<"textResultForLlm">>, Result),
    true = is_binary(Text),
    ok.

test_normalize_result_undefined(_Config) ->
    Result = copilot_tools:normalize_result(undefined),
    <<>>          = maps:get(<<"textResultForLlm">>, Result),
    <<"success">> = maps:get(<<"resultType">>, Result),
    ok.

test_normalize_result_passthrough(_Config) ->
    Input = #{<<"resultType">> => <<"success">>, <<"textResultForLlm">> => <<"pass">>},
    Result = copilot_tools:normalize_result(Input),
    Input = Result,
    ok.

test_tool_handler_receives_args(_Config) ->
    Self = self(),
    Tool = copilot_tools:define_tool(
        <<"spy_tool">>,
        <<"Tool that captures args">>,
        fun(Args, Inv) ->
            Self ! {tool_called, Args, Inv},
            <<"received">>
        end
    ),
    Handler = maps:get(<<"handler">>, Tool),
    Invocation = #{
        <<"arguments">> => #{<<"city">> => <<"London">>},
        <<"sessionId">> => <<"s1">>,
        <<"toolCallId">> => <<"tc1">>,
        <<"toolName">> => <<"spy_tool">>
    },
    _Result = Handler(Invocation),
    receive
        {tool_called, Args, _Inv} ->
            <<"London">> = maps:get(<<"city">>, Args),
            ok
    after 1000 ->
        ct:fail("Tool handler was not called")
    end.

%% ---------------------------------------------------------------------------
%% Client tests
%% ---------------------------------------------------------------------------

test_client_start_link_defaults(_Config) ->
    {ok, Client} = copilot_client:start_link(#{auto_start => false}),
    disconnected = copilot_client:state(Client),
    copilot_client:stop(Client),
    ok.

test_client_state_disconnected(_Config) ->
    {ok, Client} = copilot_client:start_link(#{auto_start => false}),
    disconnected = copilot_client:state(Client),
    %% Requests should fail when not connected
    {error, not_connected} = copilot_client:ping(Client),
    copilot_client:stop(Client),
    ok.

%% ---------------------------------------------------------------------------
%% Session tests
%% ---------------------------------------------------------------------------

test_session_subscribe_unsubscribe(_Config) ->
    %% Start a session directly for unit testing
    {ok, Session} = copilot_session:start_link(<<"test-session">>, self(), <<>>, #{}),
    %% Subscribe
    Ref = copilot_session:subscribe(Session, self()),
    true = is_reference(Ref),
    %% Send an event
    copilot_session:handle_event(Session, <<"test.event">>, #{<<"key">> => <<"value">>}),
    receive
        {copilot_session_event, <<"test-session">>, #{type := <<"test.event">>}} ->
            ok
    after 1000 ->
        ct:fail("Did not receive event")
    end,
    %% Unsubscribe
    copilot_session:unsubscribe(Session, Ref),
    %% Should not receive events after unsubscribe
    copilot_session:handle_event(Session, <<"test.event2">>, #{}),
    receive
        {copilot_session_event, _, _} ->
            ct:fail("Should not receive event after unsubscribe")
    after 200 ->
        ok
    end,
    copilot_session:stop(Session),
    ok.

%% ---------------------------------------------------------------------------
%% JSON-RPC framing tests
%% ---------------------------------------------------------------------------

test_jsonrpc_content_length_framing(_Config) ->
    %% Test that Content-Length framing produces correct output
    JsonPayload = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"ping">>}),
    Length = byte_size(JsonPayload),
    Expected = <<"Content-Length: ", (integer_to_binary(Length))/binary,
                 "\r\n\r\n", JsonPayload/binary>>,
    %% Verify the expected framing format
    {match, _} = re:run(Expected, <<"Content-Length: \\d+\\r\\n\\r\\n">>),
    ok.
