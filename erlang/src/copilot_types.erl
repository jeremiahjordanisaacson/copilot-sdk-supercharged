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
    permission_result/2,
    user_input_response/2,
    image_options/1,
    elicitation_result/1,
    elicitation_result/2,
    tool_defer/1,
    system_message_section/1,
    github_attachment/1,
    hook_type/1,
    session_limits/1,
    memory_configuration/1,
    client_options_params/1,
    agent_definition/1
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
    request_handler       :: fun() | undefined,
    bearer_token_provider :: fun() | undefined,
    builtin_plugin_directories :: [binary()] | undefined,
    in_process            :: boolean() | undefined
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
    instruction_directories :: [binary()] | undefined,
    %% Upstream-sync session options (parity with @github/copilot-sdk)
    enable_citations         :: boolean() | undefined,
    excluded_builtin_agents  :: [binary()] | undefined,
    session_limits           :: map() | undefined,
    memory_config            :: map() | undefined,
    otlp_protocol            :: binary() | undefined,
    enable_web_socket_responses :: boolean() | undefined,
    exp_assignments          :: map() | undefined,
    rewind_enabled           :: boolean() | undefined,
    additional_directories   :: [binary()] | undefined,
    disabled_mcp_servers     :: [binary()] | undefined,
    github_mcp_tool_config   :: map() | undefined,
    canvas_provider          :: map() | undefined,
    custom_agents_local_only :: boolean() | undefined,
    tool_search              :: map() | undefined,
    experimental_mode        :: boolean() | undefined,
    content_exclusion        :: boolean() | undefined,
    on_mcp_auth_request      :: fun() | undefined
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
    elicitation_result/0
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

%% @doc Permission reply including an opaque decision context surfaced to the CLI.
-spec permission_result(approved | denied, map() | undefined) -> map().
permission_result(Kind, undefined) ->
    permission_result(Kind);
permission_result(Kind, DecisionContext) ->
    (permission_result(Kind))#{<<"decisionContext">> => DecisionContext}.

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

%% ---------------------------------------------------------------------------
%% Upstream-sync feature constants & helpers (parity with @github/copilot-sdk)
%% ---------------------------------------------------------------------------

%% @doc Tool "defer" loading policy: lazy via search (auto) or eager pre-load (never).
-spec tool_defer(auto | never) -> binary().
tool_defer(auto)  -> <<"auto">>;
tool_defer(never) -> <<"never">>.

%% @doc System-message section identifiers. The preamble section targets only the
%% identity preamble; preserve protects an addressable section from removal.
-spec system_message_section(atom()) -> binary().
system_message_section(preamble)          -> <<"preamble">>;
system_message_section(identity)          -> <<"identity">>;
system_message_section(tool_instructions) -> <<"tool_instructions">>;
system_message_section(preserve)          -> <<"preserve">>.

%% @doc GitHub-anchored attachment variants.
-spec github_attachment(atom()) -> binary().
github_attachment(github_commit)          -> <<"GitHubCommit">>;
github_attachment(github_release)         -> <<"GitHubRelease">>;
github_attachment(github_actions_job)     -> <<"GitHubActionsJob">>;
github_attachment(github_repository)      -> <<"GitHubRepository">>;
github_attachment(github_file_diff)       -> <<"GitHubFileDiff">>;
github_attachment(github_tree_comparison) -> <<"GitHubTreeComparison">>;
github_attachment(github_url)             -> <<"GitHubUrl">>;
github_attachment(github_file)            -> <<"GitHubFile">>;
github_attachment(github_snippet)         -> <<"GitHubSnippet">>.

%% @doc Hook name constants (include post_tool_use and pre_mcp_tool_call).
-spec hook_type(atom()) -> binary().
hook_type(pre_tool_use)          -> <<"preToolUse">>;
hook_type(post_tool_use)         -> <<"postToolUse">>;
hook_type(pre_mcp_tool_call)     -> <<"preMcpToolCall">>;
hook_type(user_prompt_submitted) -> <<"userPromptSubmitted">>;
hook_type(user_prompt_transformed) -> <<"userPromptTransformed">>;
hook_type(session_start)         -> <<"sessionStart">>;
hook_type(session_end)           -> <<"sessionEnd">>.

%% @doc Build a sessionLimits wire map from a per-session AI-credit budget.
-spec session_limits(map()) -> map().
session_limits(Opts) ->
    maps:filter(fun(_K, V) -> V =/= undefined end, #{
        <<"maxAiCredits">> => maps:get(max_ai_credits, Opts, undefined)
    }).

%% @doc Build a memory configuration wire map (opt-in persistent session memory).
-spec memory_configuration(map()) -> map().
memory_configuration(Opts) ->
    maps:filter(fun(_K, V) -> V =/= undefined end, #{
        <<"enabled">> => maps:get(enabled, Opts, undefined)
    }).

%% @doc Build a wire map of transport-level client options (builtin plugin
%% directories and in-process FFI transport), parity with @github/copilot-sdk.
-spec client_options_params(map()) -> map().
client_options_params(Opts) ->
    maps:filter(fun(_K, V) -> V =/= undefined end, #{
        <<"builtinPluginDirectories">> => maps:get(builtin_plugin_directories, Opts, undefined),
        <<"inProcess">>                => maps:get(in_process, Opts, undefined)
    }).

%% @doc Build a custom-agent/factory definition wire map, including the optional
%% input argument JSON schema (argsSchema).
-spec agent_definition(map()) -> map().
agent_definition(Opts) ->
    maps:filter(fun(_K, V) -> V =/= undefined end, #{
        <<"name">>        => maps:get(name, Opts, undefined),
        <<"description">> => maps:get(description, Opts, undefined),
        <<"argsSchema">>  => maps:get(args_schema, Opts, undefined)
    }).
