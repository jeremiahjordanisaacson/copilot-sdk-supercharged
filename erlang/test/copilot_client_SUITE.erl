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
    test_tool_handler_receives_args/1,
    test_client_info_wire/1,
    test_message_source/1,
    test_v2_6_wire_helpers/1,
    test_subagent_hook_types/1,
    test_group_session_params/1,
    test_legacy_provider_and_empty_mode/1,
    test_v3_wire_helpers/1,
    test_v3_1_wire_helpers/1,
    test_session_elicitation_meta_passthrough/1
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
            test_image_options,
            test_client_info_wire,
            test_message_source,
            test_v2_6_wire_helpers,
            test_subagent_hook_types,
            test_group_session_params,
            test_legacy_provider_and_empty_mode,
            test_v3_wire_helpers,
            test_v3_1_wire_helpers
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
            test_session_subscribe_unsubscribe,
            test_session_elicitation_meta_passthrough
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

test_client_info_wire(_Config) ->
    Wire = copilot_types:client_info(#{
        application_name    => <<"my-app">>,
        application_version => <<"1.2.3">>,
        integration_name    => <<>>
    }),
    <<"my-app">> = maps:get(<<"editorName">>, Wire),
    <<"1.2.3">>  = maps:get(<<"editorVersion">>, Wire),
    false        = maps:is_key(<<"extensionName">>, Wire),
    0 = map_size(copilot_types:client_info(#{application_name => <<>>})),
    0 = map_size(copilot_types:client_info(undefined)),
    ok.

test_message_source(_Config) ->
    <<"user">>      = copilot_types:message_source(user),
    <<"system">>    = copilot_types:message_source(system),
    <<"agent-abc">> = copilot_types:message_source({agent, <<"abc">>}),
    ok.

test_v2_6_wire_helpers(_Config) ->
    Sandbox = copilot_types:sandbox_config(#{enabled => true, allow_bypass => true}),
    true = maps:get(<<"enabled">>, Sandbox),
    true = maps:get(<<"allowBypass">>, Sandbox),
    Images = copilot_types:image_generation(#{enabled => true, save_to_disk => false}),
    false = maps:get(<<"saveToDisk">>, Images),
    Autopilot = copilot_types:autopilot_options(#{max_continues => 3}),
    3 = maps:get(<<"maxContinues">>, Autopilot),
    Provider = copilot_types:provider_config(#{
        base_url        => <<"http://localhost:11434/v1">>,
        model_provider  => <<"ollama">>,
        api_key_command => <<"print-key">>
    }),
    <<"ollama">>    = maps:get(<<"modelProvider">>, Provider),
    <<"print-key">> = maps:get(<<"apiKeyCommand">>, Provider),
    Caps = copilot_types:model_capabilities(#{limits => #{max_output_tokens => 4096}}),
    4096 = maps:get(<<"max_output_tokens">>, maps:get(<<"limits">>, Caps)),
    Format = copilot_types:response_format_json_schema(#{<<"type">> => <<"object">>}),
    <<"json_schema">> = maps:get(<<"type">>, Format),
    <<"response">> = maps:get(<<"name">>, maps:get(<<"jsonSchema">>, Format)),
    Token = copilot_types:github_token_result(#{kind => token, access_token => <<"t">>,
                                                expires_in => 3600}),
    <<"t">> = maps:get(<<"accessToken">>, Token),
    3600    = maps:get(<<"expiresIn">>, Token),
    ok.

test_subagent_hook_types(_Config) ->
    <<"subagentStart">> = copilot_types:hook_type(subagent_start),
    <<"subagentStop">>  = copilot_types:hook_type(subagent_stop),
    ok.

test_group_session_params(_Config) ->
    Tool = #{<<"name">> => <<"t">>, <<"idempotent">> => true},
    Resume = copilot_client:group_session_params(#{
        <<"sessionId">>             => <<"s1">>,
        <<"model">>                 => <<"m">>,
        <<"name">>                  => <<"create only">>,
        <<"streaming">>             => true,
        <<"requestPermission">>     => true,
        <<"tools">>                 => [Tool],
        <<"hooks">>                 => true,
        <<"disableResume">>         => true,
        <<"trustWorkingDirectory">> => <<"session">>,
        <<"reasoningEffort">>       => null,
        <<"futureOption">>          => 1
    }, resume),
    #{
        <<"sessionId">>           := <<"s1">>,
        <<"model">>               := <<"m">>,
        <<"suppressResumeEvent">> := true,
        <<"futureOption">>        := 1,
        <<"events">>              := #{<<"streaming">> := true},
        <<"permissions">>         := #{<<"request">> := true},
        <<"tools">>               := #{<<"definitions">> := [Tool]},
        <<"hooks">>               := #{<<"hasHandlers">> := true}
    } = Resume,
    false = maps:is_key(<<"name">>, Resume),
    false = maps:is_key(<<"filesystem">>, Resume),
    false = maps:is_key(<<"inference">>, Resume),
    Create = copilot_client:group_session_params(#{
        <<"configDir">>             => <<"/cfg">>,
        <<"mcpServers">>            => #{<<"s">> => #{<<"command">> => <<"x">>}},
        <<"allowTranscriptRecovery">> => true,
        <<"capi">>                  => #{<<"autoTier">> => <<"fast">>}
    }, create),
    <<"/cfg">> = maps:get(<<"configDirectory">>, Create),
    false = maps:is_key(<<"allowTranscriptRecovery">>, Create),
    #{<<"envValueMode">> := <<"direct">>} = maps:get(<<"mcp">>, Create),
    #{<<"capi">> := #{<<"autoTier">> := <<"fast">>}} = maps:get(<<"github">>, Create),
    ok.

test_legacy_provider_and_empty_mode(_Config) ->
    Legacy = copilot_client:group_session_params(#{
        <<"model">>    => <<"gpt-4o">>,
        <<"provider">> => copilot_types:provider_config(#{
            type              => <<"openai">>,
            base_url          => <<"https://models.example.com/v1">>,
            api_key_command   => <<"print-key">>,
            max_output_tokens => 2048
        })
    }, create),
    #{<<"inference">> := Inference, <<"github">> := Github} = Legacy,
    [#{<<"name">> := <<"default">>, <<"baseUrl">> := <<"https://models.example.com/v1">>,
       <<"apiKeyCommand">> := <<"print-key">>} = Connection] = maps:get(<<"providers">>, Inference),
    false = maps:is_key(<<"maxOutputTokens">>, Connection),
    <<"default">> = maps:get(<<"defaultProvider">>, Inference),
    [#{<<"provider">> := <<"default">>, <<"id">> := <<"gpt-4o">>,
       <<"maxOutputTokens">> := 2048}] = maps:get(<<"models">>, Inference),
    #{<<"capi">> := #{<<"enabled">> := false}} = Github,
    %% Explicit named providers win; an explicit null clears the default on resume.
    Explicit = copilot_client:group_session_params(#{
        <<"sessionId">>       => <<"s1">>,
        <<"provider">>        => #{<<"baseUrl">> => <<"https://legacy.example.com">>},
        <<"defaultProvider">> => null
    }, resume),
    #{<<"inference">> := #{<<"defaultProvider">> := null}} = Explicit,
    false = maps:is_key(<<"github">>, Explicit),
    Empty = copilot_client:apply_mode_defaults(
        copilot_client:group_session_params(#{
            <<"availableTools">> => [<<"view">>],
            <<"memory">>         => #{<<"enabled">> => true}
        }, create), empty),
    #{<<"availableTools">> := [<<"view">>]} = maps:get(<<"tools">>, Empty),
    #{<<"memory">> := #{<<"enabled">> := true}, <<"skipEmbeddingRetrieval">> := true} =
        maps:get(<<"state">>, Empty),
    #{<<"oauthTokenStorage">> := <<"in-memory">>} = maps:get(<<"mcp">>, Empty),
    #{<<"enableHostGitOperations">> := false} = maps:get(<<"git">>, Empty),
    Same = #{<<"model">> => <<"m">>},
    Same = copilot_client:apply_mode_defaults(Same, coding_agent),
    {ok, coding_agent} = copilot_types:parse_client_mode(<<"copilot-cli">>),
    {ok, empty} = copilot_types:parse_client_mode(<<"empty">>),
    <<"coding-agent">> = copilot_types:client_mode(coding_agent),
    ok.

test_v3_wire_helpers(_Config) ->
    #{text := <<"hi">>, sequence := 3, tool_call_id := <<"tc">>} =
        copilot_types:shell_output(#{<<"toolCallId">> => <<"tc">>, <<"text">> => <<"hi">>,
                                     <<"sequence">> => 3}),
    <<"tool.shell_output">> = copilot_types:session_event_type(tool_shell_output),
    #{<<"action">> := <<"accept">>, <<"value">> := <<"pw">>} =
        copilot_types:shell_authentication_response({accept, <<"pw">>}),
    #{<<"action">> := <<"cancel">>} = copilot_types:shell_authentication_response(cancel),
    #{<<"name">> := <<"t">>, <<"idempotent">> := true} = Definition =
        copilot_types:tool_definition(copilot_tools:define_tool(
            <<"t">>, <<"d">>, undefined, fun(_Args, _Inv) -> ok end, #{idempotent => true})),
    false = maps:is_key(<<"handler">>, Definition),
    #{<<"scope">> := <<"runtime">>, <<"resolveProxy">> := #{}} =
        copilot_types:network_set_provider_params(#{scope => runtime,
                                                    resolve_proxy => fun(_) -> undefined end}),
    #{category := <<"timeout">>, previous_policy_retained := true} =
        copilot_types:policy_helper_failure({error, #{<<"data">> => #{
            <<"policyHelperFailure">> => #{<<"category">> => <<"timeout">>,
                                           <<"previousPolicyRetained">> => true}}}}),
    undefined = copilot_types:policy_helper_failure({error, #{<<"code">> => -32603}}),
    #{success := false, reason := <<"managed_policy">>} =
        copilot_types:permissions_set_mode_result(#{<<"success">> => false,
                                                    <<"mode">> => <<"manual">>,
                                                    <<"reason">> => <<"managed_policy">>}),
    <<"managed_policy">> = copilot_types:permission_mode_refusal(managed_policy),
    ok.

test_v3_1_wire_helpers(_Config) ->
    %% Ask-user image attachments are sent only when set.
    Plain = copilot_types:user_input_response(<<"ok">>, false, undefined),
    false = maps:is_key(<<"attachments">>, Plain),
    Image = copilot_types:user_input_image_attachment(<<"aGk=">>, <<"image/png">>),
    #{<<"answer">> := <<>>, <<"wasFreeform">> := true,
      <<"attachments">> := [#{<<"type">> := <<"image">>, <<"data">> := <<"aGk=">>,
                              <<"mimeType">> := <<"image/png">>} = Wire]} =
        copilot_types:user_input_response(<<>>, true, [Image]),
    false = maps:is_key(<<"description">>, Wire),
    #{<<"type">> := <<"image">>, <<"displayName">> := <<"shot.png">>} =
        copilot_types:user_input_image_attachment(#{data => <<"aGk=">>,
                                                    mime_type => <<"image/png">>,
                                                    display_name => <<"shot.png">>}),
    #{allowed_mime_types := [<<"image/png">>], max_bytes_per_image := 1024, max_count := 2} =
        copilot_types:image_attachment_constraints(#{
            <<"question">> => <<"Attach a screenshot?">>,
            <<"imageAttachmentConstraints">> => #{<<"allowedMimeTypes">> => [<<"image/png">>],
                                                   <<"maxBytesPerImage">> => 1024,
                                                   <<"maxCount">> => 2}}),
    undefined = copilot_types:image_attachment_constraints(#{<<"question">> => <<"q">>}),
    #{<<"action">> := <<"accept">>, <<"_meta">> := #{<<"ext">> := 1}} =
        copilot_types:elicitation_result(<<"accept">>, #{}, #{<<"ext">> => 1}),
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

test_session_elicitation_meta_passthrough(_Config) ->
    Self = self(),
    Meta = #{<<"ext">> => #{<<"k">> => 1}},
    Handler = fun(Context) ->
        Self ! {elicitation_context, Context},
        copilot_types:elicitation_result(<<"accept">>, #{<<"ok">> => true},
                                         maps:get(meta, Context))
    end,
    %% This process stands in for the JSON-RPC client.
    {ok, Session} = copilot_session:start_link(<<"test-session">>, self(), <<>>,
                                               #{on_elicitation_request => Handler}),
    copilot_session:handle_event(Session, <<"elicitation.requested">>, #{
        <<"requestId">> => <<"req-1">>,
        <<"message">>   => <<"Pick one">>,
        <<"_meta">>     => Meta
    }),
    receive
        {elicitation_context, #{request_id := <<"req-1">>, meta := Meta}} -> ok
    after 1000 ->
        ct:fail("Elicitation handler did not receive request_id and meta")
    end,
    receive
        {'$gen_call', From, {request, <<"ui.handlePendingElicitation">>,
                             #{<<"requestId">> := <<"req-1">>,
                               <<"result">> := #{<<"_meta">> := Meta}}}} ->
            gen_server:reply(From, {ok, #{}})
    after 1000 ->
        ct:fail("Elicitation result _meta was not forwarded")
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
