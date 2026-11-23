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
    agent_definition/1,
    client_info/1,
    auto_tier/1,
    ask_user_variant/1,
    message_source/1,
    sandbox_config/1,
    image_generation/1,
    autopilot_options/1,
    provider_config/1,
    model_capabilities/1,
    model_limits/1,
    github_token_result/1,
    response_format_json_schema/1,
    %% v3.0.0 (SDK protocol 4) helpers
    client_mode/1,
    parse_client_mode/1,
    named_provider_config/1,
    provider_model_config/1,
    tool_definition/1,
    shell_output/1,
    shell_authentication_request/1,
    shell_authentication_response/1,
    stored_memory/1,
    network_set_provider_params/1,
    policy_helper_failure/1,
    permission_mode_refusal/1,
    permissions_set_mode_result/1
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
    in_process            :: boolean() | undefined,
    %% Integrating application identity sent on the `connect` handshake:
    %% #{application_name, application_version, integration_name, integration_version}
    client_info           :: map() | undefined
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
    on_mcp_auth_request      :: fun() | undefined,
    %% v2.6.0 session options (parity with @github/copilot-sdk)
    auto_tier                :: binary() | undefined,
    ask_user_variant         :: binary() | undefined,
    github_token_provider    :: fun((map()) -> map()) | undefined,
    sandbox_config           :: map() | undefined,
    feature_flags            :: #{binary() => boolean()} | undefined,
    allowed_models           :: [binary()] | undefined,
    auth_client_id_metadata_url :: binary() | undefined,
    name                     :: binary() | undefined,
    agent_directories        :: [binary()] | undefined,
    hook_directories         :: [binary()] | undefined,
    interactive_user         :: boolean() | undefined,
    excluded_managed_settings :: [binary()] | undefined,
    enforce_managed_model_defaults :: boolean() | undefined,
    trust_working_directory  :: binary() | undefined,
    refresh_custom_instructions :: boolean() | undefined,
    allow_transcript_recovery :: boolean() | undefined,
    image_generation         :: map() | undefined,
    autopilot                :: map() | undefined
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
session_event_type(tool_shell_output)             -> <<"tool.shell_output">>;
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
hook_type(subagent_start)        -> <<"subagentStart">>;
hook_type(subagent_stop)         -> <<"subagentStop">>;
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

%% ---------------------------------------------------------------------------
%% v2.6.0 feature constants & helpers (parity with @github/copilot-sdk)
%% ---------------------------------------------------------------------------

%% @doc Build the `connect` handshake clientInfo wire map from the client_info
%% client option (#{application_name, application_version, integration_name,
%% integration_version}). Empty fields are dropped; an all-empty identity
%% yields an empty map, in which case the client skips `connect`.
-spec client_info(map() | undefined) -> map().
client_info(undefined) ->
    #{};
client_info(Info) when is_map(Info) ->
    Pairs = [
        {application_name,    <<"editorName">>},
        {application_version, <<"editorVersion">>},
        {integration_name,    <<"extensionName">>},
        {integration_version, <<"extensionVersion">>}
    ],
    lists:foldl(fun({Key, JsonKey}, Acc) ->
        case maps:get(Key, Info, undefined) of
            undefined -> Acc;
            <<>>      -> Acc;
            []        -> Acc;
            Value     -> Acc#{JsonKey => to_binary(Value)}
        end
    end, #{}, Pairs).

%% @doc Auto routing tier constants (session option auto_tier; experimental).
-spec auto_tier(efficiency | balance | intelligence | fast) -> binary().
auto_tier(efficiency)   -> <<"efficiency">>;
auto_tier(balance)      -> <<"balance">>;
auto_tier(intelligence) -> <<"intelligence">>;
auto_tier(fast)         -> <<"fast">>.

%% @doc Ask-user variant constants (session option ask_user_variant).
-spec ask_user_variant(legacy | elicitation) -> binary().
ask_user_variant(legacy)      -> <<"legacy">>;
ask_user_variant(elicitation) -> <<"elicitation">>.

%% @doc Message provenance for the `source` message option: `user`, `system`,
%% or `{agent, AgentId}` (=> <<"agent-", AgentId/binary>>). Binaries pass through.
-spec message_source(user | system | {agent, binary() | string()} | binary()) -> binary().
message_source(user)           -> <<"user">>;
message_source(system)         -> <<"system">>;
message_source({agent, Id})    -> <<"agent-", (to_binary(Id))/binary>>;
message_source(Source) when is_binary(Source) -> Source.

%% @doc Build a sandboxConfig wire map. allow_bypass is an opt-in that lets the
%% agent request an individual command outside the sandbox through the normal
%% permission flow.
-spec sandbox_config(map()) -> map().
sandbox_config(Opts) ->
    to_wire(Opts, [
        {enabled,                       <<"enabled">>},
        {user_policy,                   <<"userPolicy">>},
        {add_current_working_directory, <<"addCurrentWorkingDirectory">>},
        {sandbox_mcp_servers,           <<"sandboxMcpServers">>},
        {sandbox_lsp_servers,           <<"sandboxLspServers">>},
        {sandbox_extensions,            <<"sandboxExtensions">>},
        {allow_bypass,                  <<"allowBypass">>}
    ]).

%% @doc Build a session-level imageGeneration wire map
%% (#{enabled, watermark, save_to_disk}).
-spec image_generation(map()) -> map().
image_generation(Opts) ->
    to_wire(Opts, [
        {enabled,      <<"enabled">>},
        {watermark,    <<"watermark">>},
        {save_to_disk, <<"saveToDisk">>}
    ]).

%% @doc Build an autopilot options wire map (#{continuation_enabled, max_continues}).
-spec autopilot_options(map()) -> map().
autopilot_options(Opts) ->
    to_wire(Opts, [
        {continuation_enabled, <<"continuationEnabled">>},
        {max_continues,        <<"maxContinues">>}
    ]).

%% @doc Build a BYOK provider wire map. model_provider is one of openai,
%% anthropic, azure_openai, ollama, lm_studio, foundry_local or llama_cpp;
%% api_key_command is a command whose stdout is used as the API key.
-spec provider_config(map()) -> map().
provider_config(Opts) ->
    to_wire(Opts, [
        {type,            <<"type">>},
        {wire_api,        <<"wireApi">>},
        {base_url,        <<"baseUrl">>},
        {api_key,         <<"apiKey">>},
        {bearer_token,    <<"bearerToken">>},
        {model_provider,  <<"modelProvider">>},
        {api_key_command, <<"apiKeyCommand">>},
        {transport,       <<"transport">>},
        {azure,           <<"azure">>},
        {headers,         <<"headers">>},
        %% Model-specific fields: on SDK protocol 4 these move to an
        %% `inference.models` entry for the session model.
        {model_id,                  <<"modelId">>},
        {wire_model,                <<"wireModel">>},
        {max_prompt_tokens,         <<"maxPromptTokens">>},
        {max_context_window_tokens, <<"maxContextWindowTokens">>},
        {max_output_tokens,         <<"maxOutputTokens">>},
        {capabilities,              <<"capabilities">>}
    ]).

%% @doc Build a modelCapabilities override wire map; a `limits` map is
%% converted with model_limits/1.
-spec model_capabilities(map() | term()) -> map() | term().
model_capabilities(Opts) when is_map(Opts) ->
    Wire = to_wire(Opts, [{limits, <<"limits">>}, {supports, <<"supports">>}]),
    case maps:get(<<"limits">>, Wire, undefined) of
        Limits when is_map(Limits) -> Wire#{<<"limits">> => model_limits(Limits)};
        _                          -> Wire
    end;
model_capabilities(Other) ->
    Other.

%% @doc Build the model capabilities override `limits` wire map. Wire keys are
%% snake_case (max_prompt_tokens, max_output_tokens, max_context_window_tokens).
-spec model_limits(map()) -> map().
model_limits(Opts) ->
    to_wire(Opts, [
        {max_prompt_tokens,         <<"max_prompt_tokens">>},
        {max_output_tokens,         <<"max_output_tokens">>},
        {max_context_window_tokens, <<"max_context_window_tokens">>}
    ]).

%% @doc Convert a github_token_provider result into its wire map:
%% #{kind => token, access_token => T, token_type => Type, expires_in => Secs}
%% or #{kind => cancelled}.
-spec github_token_result(map()) -> map().
github_token_result(Result) ->
    to_wire(Result, [
        {kind,         <<"kind">>},
        {access_token, <<"accessToken">>},
        {token_type,   <<"tokenType">>},
        {expires_in,   <<"expiresIn">>}
    ]).

%% @doc Structured-output responseFormat wire object for a JSON Schema
%% (message option response_schema).
-spec response_format_json_schema(map()) -> map().
response_format_json_schema(Schema) ->
    #{
        <<"type">>       => <<"json_schema">>,
        <<"jsonSchema">> => #{
            <<"name">>   => <<"response">>,
            <<"strict">> => true,
            <<"schema">> => Schema
        }
    }.

%% ---------------------------------------------------------------------------
%% v3.0.0 helpers (SDK protocol 4 / Microsoft Copilot SDK 2.0 parity)
%% ---------------------------------------------------------------------------

%% @doc Client mode wire names: `coding_agent` => <<"coding-agent">> (the
%% default) and `empty` => <<"empty">>.
-spec client_mode(coding_agent | empty) -> binary().
client_mode(coding_agent) -> <<"coding-agent">>;
client_mode(empty)        -> <<"empty">>.

%% @doc Parse the `mode` client option. Accepts atoms, binaries or strings;
%% the legacy name "copilot-cli" is an alias of "coding-agent".
-spec parse_client_mode(term()) -> {ok, coding_agent | empty} | {error, term()}.
parse_client_mode(undefined)          -> {ok, coding_agent};
parse_client_mode(coding_agent)       -> {ok, coding_agent};
parse_client_mode(copilot_cli)        -> {ok, coding_agent};
parse_client_mode(empty)              -> {ok, empty};
parse_client_mode(<<"coding-agent">>) -> {ok, coding_agent};
parse_client_mode(<<"copilot-cli">>)  -> {ok, coding_agent};
parse_client_mode(<<"empty">>)        -> {ok, empty};
parse_client_mode(Mode) when is_list(Mode) ->
    parse_client_mode(unicode:characters_to_binary(Mode));
parse_client_mode(Other) ->
    {error, {invalid_client_mode, Other}}.

%% @doc Build a named BYOK provider connection (`inference.providers[]`):
%% #{name, type, base_url, api_key, api_key_command, bearer_token, wire_api,
%% transport, azure, headers, model_provider}. A list converts every entry.
-spec named_provider_config(map() | [map()]) -> map() | [map()].
named_provider_config(Providers) when is_list(Providers) ->
    [named_provider_config(Provider) || Provider <- Providers];
named_provider_config(Opts) ->
    to_wire(Opts, [
        {name,            <<"name">>},
        {type,            <<"type">>},
        {base_url,        <<"baseUrl">>},
        {api_key,         <<"apiKey">>},
        {api_key_command, <<"apiKeyCommand">>},
        {bearer_token,    <<"bearerToken">>},
        {wire_api,        <<"wireApi">>},
        {transport,       <<"transport">>},
        {azure,           <<"azure">>},
        {headers,         <<"headers">>},
        {model_provider,  <<"modelProvider">>}
    ]).

%% @doc Build a provider model definition (`inference.models[]`): #{provider,
%% id, model_id, wire_model, name, max_prompt_tokens, max_context_window_tokens,
%% max_output_tokens, capabilities}. Select it as <<"Provider/Id">>.
-spec provider_model_config(map() | [map()]) -> map() | [map()].
provider_model_config(Models) when is_list(Models) ->
    [provider_model_config(Model) || Model <- Models];
provider_model_config(Opts) ->
    to_wire(Opts, [
        {provider,                  <<"provider">>},
        {id,                        <<"id">>},
        {model_id,                  <<"modelId">>},
        {wire_model,                <<"wireModel">>},
        {name,                      <<"name">>},
        {max_prompt_tokens,         <<"maxPromptTokens">>},
        {max_context_window_tokens, <<"maxContextWindowTokens">>},
        {max_output_tokens,         <<"maxOutputTokens">>},
        {capabilities,              <<"capabilities">>}
    ]).

%% @doc Wire declaration of a tool (`tools.definitions[]`): the client-side
%% handler is dropped; `idempotent` marks a tool as safe to retry.
-spec tool_definition(map()) -> map().
tool_definition(Tool) ->
    Wire = to_wire(maps:without([<<"handler">>, handler], Tool), [
        {name,                    <<"name">>},
        {title,                   <<"title">>},
        {description,             <<"description">>},
        {parameters,              <<"parameters">>},
        {overrides_built_in_tool, <<"overridesBuiltInTool">>},
        {skip_permission,         <<"skipPermission">>},
        {defer,                   <<"defer">>},
        {metadata,                <<"metadata">>},
        {is_terminal,             <<"isTerminal">>},
        {namespace,               <<"namespace">>},
        {idempotent,              <<"idempotent">>}
    ]),
    maps:filter(fun(_Key, Value) -> Value =/= undefined end, Wire).

%% @doc Decode the data of a `tool.shell_output` event into
%% #{tool_call_id, text, sequence, stream}; `stream` is <<"stdout">>
%% (also when omitted), <<"stderr">> or <<"terminal">>.
-spec shell_output(map()) -> map().
shell_output(Data) ->
    #{
        tool_call_id => maps:get(<<"toolCallId">>, Data, <<>>),
        text         => maps:get(<<"text">>, Data, <<>>),
        sequence     => maps:get(<<"sequence">>, Data, 0),
        stream       => maps:get(<<"stream">>, Data, undefined)
    }.

%% @doc Decode a `shellAuthentication.request` into the map passed to the
%% `on_shell_authentication_request` handler: #{session_id, request_id,
%% program, prompt, input_kind, command, cwd}. `input_kind` is <<"secret">>,
%% <<"text">> or <<"confirm">>.
-spec shell_authentication_request(map()) -> map().
shell_authentication_request(Params) ->
    #{
        session_id => maps:get(<<"sessionId">>, Params, <<>>),
        request_id => maps:get(<<"requestId">>, Params, <<>>),
        program    => maps:get(<<"program">>, Params, <<>>),
        prompt     => maps:get(<<"prompt">>, Params, <<>>),
        input_kind => maps:get(<<"inputKind">>, Params, <<>>),
        command    => maps:get(<<"command">>, Params, <<>>),
        cwd        => maps:get(<<"cwd">>, Params, <<>>)
    }.

%% @doc Encode a shell authentication answer: `{accept, Value}` =>
%% #{action => accept, value}; anything else (`cancel`) declines the prompt.
%% Wire-shaped maps pass through.
-spec shell_authentication_response(term()) -> map().
shell_authentication_response({accept, Value}) ->
    #{<<"action">> => <<"accept">>, <<"value">> => to_binary(Value)};
shell_authentication_response(#{<<"action">> := _} = Wire) ->
    Wire;
shell_authentication_response(_Cancel) ->
    #{<<"action">> => <<"cancel">>}.

%% @doc Decode a `memoryStore.store` request into the map passed to the
%% `store_memory` callback: #{session_id, subject, fact, citations, reason}.
-spec stored_memory(map()) -> map().
stored_memory(Params) ->
    #{
        session_id => maps:get(<<"sessionId">>, Params, <<>>),
        subject    => maps:get(<<"subject">>, Params, <<>>),
        fact       => maps:get(<<"fact">>, Params, <<>>),
        citations  => maps:get(<<"citations">>, Params, <<>>),
        reason     => maps:get(<<"reason">>, Params, <<>>)
    }.

%% @doc Build the `network.setProvider` params from the `network` client
%% option: an empty object marks each registered callback (resolve_proxy,
%% authenticate_negotiate, authenticate_basic); `scope` is `connection` or
%% `runtime`.
-spec network_set_provider_params(map() | undefined) -> map().
network_set_provider_params(Network) when is_map(Network) ->
    Markers = lists:foldl(fun({Key, JsonKey}, Acc) ->
        case maps:get(Key, Network, undefined) of
            Callback when is_function(Callback, 1) -> Acc#{JsonKey => #{}};
            _                                      -> Acc
        end
    end, #{}, [
        {resolve_proxy,          <<"resolveProxy">>},
        {authenticate_negotiate, <<"authenticateNegotiate">>},
        {authenticate_basic,     <<"authenticateBasic">>}
    ]),
    case maps:get(scope, Network, undefined) of
        undefined -> Markers;
        Scope     -> Markers#{<<"scope">> => to_binary(Scope)}
    end;
network_set_provider_params(_) ->
    #{}.

%% @doc Extract the structured policy helper failure (`policyHelperFailure`)
%% from a failed session.create / session.resume error, its `data`, or a
%% managed settings snapshot. Returns #{category, failure_mode, message,
%% previous_policy_retained, reason, authority, code, details, diagnostic,
%% help_link} or `undefined`.
-spec policy_helper_failure(term()) -> map() | undefined.
policy_helper_failure({error, Error}) ->
    policy_helper_failure(Error);
policy_helper_failure(#{<<"data">> := Data}) when is_map(Data) ->
    policy_helper_failure(Data);
policy_helper_failure(#{<<"policyHelperFailure">> := Failure}) when is_map(Failure) ->
    #{
        category                 => maps:get(<<"category">>, Failure, <<>>),
        failure_mode             => maps:get(<<"failureMode">>, Failure, <<>>),
        message                  => maps:get(<<"message">>, Failure, <<>>),
        previous_policy_retained => maps:get(<<"previousPolicyRetained">>, Failure, false),
        reason                   => maps:get(<<"reason">>, Failure, <<>>),
        authority                => maps:get(<<"authority">>, Failure, undefined),
        code                     => maps:get(<<"code">>, Failure, undefined),
        details                  => maps:get(<<"details">>, Failure, undefined),
        diagnostic               => maps:get(<<"diagnostic">>, Failure, undefined),
        help_link                => maps:get(<<"helpLink">>, Failure, undefined)
    };
policy_helper_failure(_) ->
    undefined.

%% @doc Known refusal reasons of `session.permissions.setMode`.
-spec permission_mode_refusal(managed_policy) -> binary().
permission_mode_refusal(managed_policy) -> <<"managed_policy">>.

%% @doc Decode a `session.permissions.setMode` result into #{success, mode,
%% reason}; `reason` is <<"managed_policy">> when managed policy refused the
%% requested mode, otherwise `undefined`.
-spec permissions_set_mode_result(map()) -> map().
permissions_set_mode_result(Result) when is_map(Result) ->
    #{
        success => maps:get(<<"success">>, Result, false),
        mode    => maps:get(<<"mode">>, Result, undefined),
        reason  => maps:get(<<"reason">>, Result, undefined)
    }.

%% Convert known atom keys to their wire keys (dropping undefined values);
%% any other key passes through unchanged, so wire-shaped maps are accepted.
to_wire(Opts, Pairs) when is_map(Opts) ->
    maps:fold(fun(Key, Value, Acc) ->
        case lists:keyfind(Key, 1, Pairs) of
            {Key, _JsonKey} when Value =:= undefined -> Acc;
            {Key, JsonKey}                           -> Acc#{JsonKey => Value};
            false                                    -> Acc#{Key => Value}
        end
    end, #{}, Opts);
to_wire(Other, _Pairs) ->
    Other.

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V)   -> unicode:characters_to_binary(V);
to_binary(V) when is_atom(V)   -> atom_to_binary(V, utf8).
