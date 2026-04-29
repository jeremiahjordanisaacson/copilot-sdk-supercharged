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
    image_options/1
]).

%% ---------------------------------------------------------------------------
%% Records
%% ---------------------------------------------------------------------------

-record(client_options, {
    cli_path         :: binary() | undefined,
    cli_url          :: binary() | undefined,
    cwd              :: binary() | undefined,
    log_level        :: binary() | undefined,
    github_token     :: binary() | undefined,
    use_logged_in    :: boolean() | undefined,
    auto_start       :: boolean() | undefined,
    auto_restart     :: boolean() | undefined,
    session_idle_timeout_seconds :: non_neg_integer() | undefined
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
    include_sub_agent_streaming_events :: boolean() | undefined
}).

-record(session_event, {
    type :: binary(),
    data :: map() | undefined
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
    session_event_type/0
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
session_event_type(user_message)                  -> <<"user.message">>.

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
