%%%-------------------------------------------------------------------
%%% @doc Type specifications and record definitions for the Copilot SDK.
%%%
%%% Provides records, type specs, and constants used throughout
%%% the SDK. All public types are exported for use in client code.
%%% @end
%%%-------------------------------------------------------------------
-module(copilot_types).

%% API exports
-export([
    connection_state/1,
    session_event_type/1,
    tool_result/2,
    tool_result/3,
    permission_result/1,
    user_input_response/2,
    image_options/1,
    elicitation_result/1,
    elicitation_result/2,
    exit_plan_mode_response/1
]).

%% ---------------------------------------------------------------------------
%% Records
%% ---------------------------------------------------------------------------

-record(session_fs_config, {
    initial_cwd        :: binary(),
    session_state_path :: binary(),
    conventions        :: binary()
}).

-record(command_definition, {
    name        :: binary(),
    description :: binary() | undefined,
    handler     :: fun()
}).

-record(client_options, {
    cli_path         :: binary() | undefined,
    cli_url          :: binary() | undefined,
    cwd              :: binary() | undefined,
    log_level        :: binary() | undefined,
    github_token     :: binary() | undefined,
    use_logged_in    :: boolean() | undefined,
    auto_start       :: boolean() | undefined,
    auto_restart     :: boolean() | undefined,
    session_idle_timeout_seconds :: non_neg_integer() | undefined,
    session_fs       :: #session_fs_config{} | undefined,
    copilot_home     :: binary() | undefined,
    tcp_connection_token :: binary() | undefined,
    remote           :: boolean() | undefined,
    on_get_trace_context :: fun() | undefined
}).

-record(session_config, {
    model                :: binary() | undefined,
    reasoning_effort     :: binary() | undefined,
    tools                :: [map()] | undefined,
    system_message       :: map() | undefined,
    available_tools      :: [binary()] | undefined,
    excluded_tools       :: [binary()] | undefined,
    working_directory    :: binary() | undefined,
    streaming            :: boolean() | undefined,
    on_permission_request :: fun() | undefined,
    on_user_input_request :: fun() | undefined,
    hooks                :: map() | undefined,
    provider             :: map() | undefined,
    mcp_servers          :: map() | undefined,
    infinite_sessions    :: map() | undefined,
    skill_directories    :: [binary()] | undefined,
    disabled_skills      :: [binary()] | undefined,
    include_sub_agent_streaming_events :: boolean() | undefined,
    model_capabilities   :: map() | undefined,
    enable_config_discovery :: boolean() | undefined,
    github_token         :: binary() | undefined,
    commands             :: [#command_definition{}] | undefined,
    on_elicitation_request :: fun() | undefined,
    instruction_directories :: [binary()] | undefined
}).

-record(command_context, {
    session_id    :: binary(),
    command       :: binary(),
    command_name  :: binary(),
    args          :: binary()
}).

-record(elicitation_context, {
    session_id          :: binary(),
    message             :: binary(),
    requested_schema    :: map() | undefined,
    mode                :: binary() | undefined,
    elicitation_source  :: binary() | undefined,
    url                 :: binary() | undefined
}).

-record(elicitation_result, {
    action  :: binary(),
    content :: map() | undefined
}).

-record(trace_context, {
    traceparent :: binary() | undefined,
    tracestate  :: binary() | undefined
}).

-record(slash_command_input, {
    hint        :: binary(),
    completion  :: binary() | undefined
}).

-record(slash_command_info, {
    allow_during_agent_execution :: boolean(),
    description                  :: binary(),
    kind                         :: binary(),
    name                         :: binary(),
    aliases                      :: [binary()] | undefined,
    experimental                 :: boolean() | undefined,
    input                        :: #slash_command_input{} | undefined
}).

-record(commands_invoke_request, {
    name  :: binary(),
    input :: binary() | undefined
}).

-record(commands_list_request, {
    include_builtins        :: boolean() | undefined,
    include_client_commands :: boolean() | undefined,
    include_skills          :: boolean() | undefined
}).

-record(model_billing_token_prices, {
    batch_size   :: non_neg_integer() | undefined,
    cache_price  :: non_neg_integer() | undefined,
    input_price  :: non_neg_integer() | undefined,
    output_price :: non_neg_integer() | undefined
}).

-record(model_billing, {
    multiplier            :: float(),
    token_prices          :: #model_billing_token_prices{} | undefined,
    picker_price_category :: binary() | undefined
}).

%% Experimental
-record(remote_enable_request, {
    mode :: binary() | undefined
}).

%% Experimental
-record(remote_enable_result, {
    remote_steerable :: boolean(),
    url              :: binary() | undefined
}).

%% Experimental
-record(skills_load_diagnostics, {
    errors   :: [binary()],
    warnings :: [binary()]
}).

-record(session_event, {
    type :: binary(),
    data :: map() | undefined,
    id :: binary() | undefined,
    timestamp :: binary() | undefined,
    parent_id :: binary() | undefined,
    agent_id :: binary() | undefined,
    ephemeral :: boolean() | undefined
}).

-record(tool_definition, {
    name        :: binary(),
    description :: binary(),
    parameters  :: map() | undefined,
    handler     :: fun()
}).

-record(tool_result, {
    text_result_for_llm :: binary(),
    result_type         :: success | failure,
    error               :: binary() | undefined,
    tool_telemetry      :: map() | undefined
}).

-record(tool_invocation, {
    session_id   :: binary(),
    tool_call_id :: binary(),
    tool_name    :: binary(),
    arguments    :: map()
}).

-record(permission_request_result, {
    kind :: approved | denied
}).

-record(user_input_response, {
    answer       :: binary(),
    was_freeform :: boolean()
}).

-record(image_options, {
    size    :: binary() | undefined,
    quality :: binary() | undefined,
    style   :: binary() | undefined
}).

%% Export record types for external use
-export_type([
    client_options/0,
    session_config/0,
    session_event/0,
    tool_definition/0,
    tool_result/0,
    tool_invocation/0,
    permission_request_result/0,
    user_input_response/0,
    image_options/0,
    connection_state/0,
    session_event_type/0,
    session_fs_config/0,
    command_definition/0,
    command_context/0,
    elicitation_context/0,
    elicitation_result/0,
    trace_context/0,
    slash_command_input/0,
    slash_command_info/0,
    commands_invoke_request/0,
    commands_list_request/0,
    model_billing_token_prices/0,
    model_billing/0,
    remote_enable_request/0,
    remote_enable_result/0,
    skills_load_diagnostics/0
]).

-type client_options()           :: #client_options{}.
-type session_config()           :: #session_config{}.
-type session_event()            :: #session_event{}.
-type tool_definition()          :: #tool_definition{}.
-type tool_result()              :: #tool_result{}.
-type tool_invocation()          :: #tool_invocation{}.
-type permission_request_result() :: #permission_request_result{}.
-type user_input_response()      :: #user_input_response{}.
-type image_options()            :: #image_options{}.
-type session_fs_config()        :: #session_fs_config{}.
-type command_definition()       :: #command_definition{}.
-type command_context()          :: #command_context{}.
-type elicitation_context()      :: #elicitation_context{}.
-type elicitation_result()       :: #elicitation_result{}.
-type trace_context()            :: #trace_context{}.
-type slash_command_input()       :: #slash_command_input{}.
-type slash_command_info()        :: #slash_command_info{}.
-type commands_invoke_request()   :: #commands_invoke_request{}.
-type commands_list_request()     :: #commands_list_request{}.
-type model_billing_token_prices() :: #model_billing_token_prices{}.
-type model_billing()             :: #model_billing{}.
-type remote_session_mode()       :: binary().  %% "export" | "off" | "on"
-type remote_enable_request()    :: #remote_enable_request{}.
-type remote_enable_result()     :: #remote_enable_result{}.
-type skills_load_diagnostics()   :: #skills_load_diagnostics{}.
-type connection_state()         :: disconnected | connecting | connected | error.
-type session_event_type()       :: binary().

%% ---------------------------------------------------------------------------
%% Connection state constants
%% ---------------------------------------------------------------------------

-spec connection_state(atom()) -> binary().
connection_state(disconnected) -> <<"disconnected">>;
connection_state(connecting)   -> <<"connecting">>;
connection_state(connected)    -> <<"connected">>;
connection_state(error)        -> <<"error">>.

%% ---------------------------------------------------------------------------
%% Session event type constants
%% ---------------------------------------------------------------------------

-spec session_event_type(atom()) -> binary().
session_event_type(abort)                         -> <<"abort">>;
session_event_type(assistant_intent)              -> <<"assistant.intent">>;
session_event_type(assistant_message)             -> <<"assistant.message">>;
session_event_type(assistant_message_delta)       -> <<"assistant.message_delta">>;
session_event_type(assistant_reasoning)           -> <<"assistant.reasoning">>;
session_event_type(assistant_reasoning_delta)     -> <<"assistant.reasoning_delta">>;
session_event_type(assistant_turn_end)            -> <<"assistant.turn_end">>;
session_event_type(assistant_turn_start)          -> <<"assistant.turn_start">>;
session_event_type(assistant_usage)               -> <<"assistant.usage">>;
session_event_type(hook_end)                      -> <<"hook.end">>;
session_event_type(hook_start)                    -> <<"hook.start">>;
session_event_type(pending_messages_modified)     -> <<"pending_messages.modified">>;
session_event_type(session_compaction_complete)   -> <<"session.compaction_complete">>;
session_event_type(session_compaction_start)      -> <<"session.compaction_start">>;
session_event_type(session_error)                 -> <<"session.error">>;
session_event_type(session_handoff)               -> <<"session.handoff">>;
session_event_type(session_idle)                  -> <<"session.idle">>;
session_event_type(session_info)                  -> <<"session.info">>;
session_event_type(session_model_change)          -> <<"session.model_change">>;
session_event_type(session_resume)                -> <<"session.resume">>;
session_event_type(session_shutdown)              -> <<"session.shutdown">>;
session_event_type(session_snapshot_rewind)       -> <<"session.snapshot_rewind">>;
session_event_type(session_start)                 -> <<"session.start">>;
session_event_type(session_truncation)            -> <<"session.truncation">>;
session_event_type(session_usage_info)            -> <<"session.usage_info">>;
session_event_type(skill_invoked)                 -> <<"skill.invoked">>;
session_event_type(subagent_completed)            -> <<"subagent.completed">>;
session_event_type(subagent_failed)               -> <<"subagent.failed">>;
session_event_type(subagent_selected)             -> <<"subagent.selected">>;
session_event_type(subagent_started)              -> <<"subagent.started">>;
session_event_type(system_message)                -> <<"system.message">>;
session_event_type(tool_execution_complete)       -> <<"tool.execution_complete">>;
session_event_type(tool_execution_partial_result) -> <<"tool.execution_partial_result">>;
session_event_type(tool_execution_progress)       -> <<"tool.execution_progress">>;
session_event_type(tool_execution_start)          -> <<"tool.execution_start">>;
session_event_type(tool_user_requested)           -> <<"tool.user_requested">>;
session_event_type(user_message)                  -> <<"user.message">>;
session_event_type(command_execute)               -> <<"command.execute">>;
session_event_type(elicitation_requested)         -> <<"elicitation.requested">>;
session_event_type(capabilities_changed)          -> <<"capabilities.changed">>.

%% ---------------------------------------------------------------------------
%% Constructor helpers
%% ---------------------------------------------------------------------------

-spec tool_result(binary(), success | failure) -> map().
tool_result(Text, Type) ->
    tool_result(Text, Type, undefined).

-spec tool_result(binary(), success | failure, binary() | undefined) -> map().
tool_result(Text, Type, Error) ->
    Base = #{
        <<"textResultForLlm">> => Text,
        <<"resultType">> => atom_to_binary(Type, utf8)
    },
    case Error of
        undefined -> Base;
        _         -> Base#{<<"error">> => Error}
    end.

-spec permission_result(approved | denied) -> map().
permission_result(Kind) ->
    #{<<"kind">> => atom_to_binary(Kind, utf8)}.

-spec user_input_response(binary(), boolean()) -> map().
user_input_response(Answer, WasFreeform) ->
    #{
        <<"answer">>      => Answer,
        <<"wasFreeform">> => WasFreeform
    }.

-spec image_options(map()) -> map().
image_options(Opts) ->
    maps:filter(fun(_K, V) -> V =/= undefined end, #{
        <<"size">>    => maps:get(size, Opts, undefined),
        <<"quality">> => maps:get(quality, Opts, undefined),
        <<"style">>   => maps:get(style, Opts, undefined)
    }).

-spec elicitation_result(binary()) -> map().
elicitation_result(Action) ->
    elicitation_result(Action, undefined).

-spec elicitation_result(binary(), map() | undefined) -> map().
elicitation_result(Action, Content) ->
    Base = #{<<"action">> => Action},
    case Content of
        undefined -> Base;
        _         -> Base#{<<"content">> => Content}
    end.

-spec exit_plan_mode_response(boolean()) -> map().
exit_plan_mode_response(Approved) ->
    #{<<"approved">> => Approved}.
