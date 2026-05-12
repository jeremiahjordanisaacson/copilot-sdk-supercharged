%%%-------------------------------------------------------------------
%%% @doc Copilot session gen_server.
%%%
%%% Represents a single conversation session with the Copilot CLI.
%%% Manages event subscriptions, message sending, and tool execution
%%% state. Created by copilot_client:create_session/2.
%%%
%%% Usage:
%%%   {ok, Session} = copilot_client:create_session(Client, Config),
%%%   ok = copilot_session:subscribe(Session, self()),
%%%   {ok, MsgId} = copilot_session:send(Session, #{prompt => <<"Hello!">>}),
%%%   copilot_session:stop(Session).
%%% @end
%%%-------------------------------------------------------------------
-module(copilot_session).

-behaviour(gen_server).

%% API
-export([
    start_link/4,
    stop/1,
    send/2,
    send_and_wait/2,
    send_and_wait/3,
    subscribe/2,
    unsubscribe/2,
    get_messages/1,
    abort/1,
    handle_event/3,
    set_ready/1,
    session_id/1,
    workspace_path/1,
    handle_exit_plan_mode/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(DEFAULT_WAIT_TIMEOUT, 60000).

-record(state, {
    session_id      :: binary(),
    rpc             :: pid(),
    workspace_path  :: binary(),
    config          :: map(),
    subscribers     :: #{reference() => pid()},
    waiting         :: [{pid(), reference(), reference()}],
    last_assistant_event :: map() | undefined,
    ready           :: boolean(),
    hooks           :: map() | undefined,
    command_handlers :: #{binary() => fun()},
    elicitation_handler :: fun() | undefined,
    exit_plan_mode_handler :: fun() | undefined,
    trace_context_provider :: fun() | undefined
}).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

-spec start_link(binary(), pid(), binary(), map()) -> {ok, pid()}.
start_link(SessionId, Rpc, WorkspacePath, Config) ->
    gen_server:start_link(?MODULE, {SessionId, Rpc, WorkspacePath, Config}, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid, normal, 5000).

-spec send(pid(), map()) -> {ok, binary()} | {error, term()}.
send(Pid, Options) ->
    gen_server:call(Pid, {send, Options}).

-spec send_and_wait(pid(), map()) -> {ok, map()} | {error, term()}.
send_and_wait(Pid, Options) ->
    send_and_wait(Pid, Options, ?DEFAULT_WAIT_TIMEOUT).

-spec send_and_wait(pid(), map(), timeout()) -> {ok, map()} | {error, term()}.
send_and_wait(Pid, Options, Timeout) ->
    gen_server:call(Pid, {send_and_wait, Options, Timeout}, Timeout + 5000).

-spec subscribe(pid(), pid()) -> reference().
subscribe(Pid, Subscriber) ->
    gen_server:call(Pid, {subscribe, Subscriber}).

-spec unsubscribe(pid(), reference()) -> ok.
unsubscribe(Pid, Ref) ->
    gen_server:cast(Pid, {unsubscribe, Ref}).

-spec get_messages(pid()) -> {ok, [map()]} | {error, term()}.
get_messages(Pid) ->
    gen_server:call(Pid, get_messages).

-spec abort(pid()) -> ok | {error, term()}.
abort(Pid) ->
    gen_server:call(Pid, abort).

-spec handle_event(pid(), binary(), map()) -> ok.
handle_event(Pid, EventType, EventData) ->
    gen_server:cast(Pid, {event, EventType, EventData}).

-spec set_ready(pid()) -> ok.
set_ready(Pid) ->
    gen_server:cast(Pid, ready).

-spec session_id(pid()) -> binary().
session_id(Pid) ->
    gen_server:call(Pid, session_id).

-spec workspace_path(pid()) -> binary().
workspace_path(Pid) ->
    gen_server:call(Pid, workspace_path).

-spec handle_exit_plan_mode(pid(), map()) -> map().
handle_exit_plan_mode(Pid, Request) ->
    gen_server:call(Pid, {handle_exit_plan_mode, Request}).

%% ---------------------------------------------------------------------------
%% gen_server callbacks
%% ---------------------------------------------------------------------------

init({SessionId, Rpc, WorkspacePath, Config}) ->
    Hooks = maps:get(hooks, Config, undefined),
    %% Build command handler map from command definitions
    Commands = maps:get(commands, Config, []),
    CmdHandlers = lists:foldl(fun(Cmd, Acc) ->
        Name = maps:get(name, Cmd),
        Handler = maps:get(handler, Cmd),
        Acc#{Name => Handler}
    end, #{}, Commands),
    ElicitHandler = maps:get(on_elicitation_request, Config, undefined),
    ExitPlanModeHandler = maps:get(on_exit_plan_mode, Config, undefined),
    TraceContextProvider = maps:get(on_get_trace_context, Config, undefined),
    {ok, #state{
        session_id = SessionId,
        rpc = Rpc,
        workspace_path = WorkspacePath,
        config = Config,
        subscribers = #{},
        waiting = [],
        ready = false,
        hooks = Hooks,
        command_handlers = CmdHandlers,
        elicitation_handler = ElicitHandler,
        exit_plan_mode_handler = ExitPlanModeHandler,
        trace_context_provider = TraceContextProvider
    }}.

handle_call({send, Options}, _From, #state{rpc = Rpc, session_id = SessionId} = State) ->
    Req = #{
        <<"sessionId">> => SessionId,
        <<"prompt">>    => maps:get(prompt, Options, <<>>)
    },
    Req1 = maybe_add(<<"attachments">>, attachments, Options, Req),
    Req2 = maybe_add(<<"mode">>, mode, Options, Req1),
    Req3 = maybe_add(<<"responseFormat">>, response_format, Options, Req2),
    Req4 = maybe_add(<<"imageOptions">>, image_options, Options, Req3),
    Req5 = maybe_add(<<"requestHeaders">>, request_headers, Options, Req4),
    Req6 = inject_trace_context(Req5, State),
    case copilot_jsonrpc:request(Rpc, <<"session.send">>, Req6) of
        {ok, #{<<"messageId">> := MsgId}} ->
            {reply, {ok, MsgId}, State};
        {ok, _} ->
            {reply, {ok, undefined}, State};
        {error, _} = Err ->
            {reply, Err, State}
    end;

handle_call({send_and_wait, Options, Timeout}, From, State) ->
    #state{rpc = Rpc, session_id = SessionId} = State,
    Req = #{
        <<"sessionId">> => SessionId,
        <<"prompt">>    => maps:get(prompt, Options, <<>>)
    },
    Req1 = maybe_add(<<"attachments">>, attachments, Options, Req),
    Req2 = maybe_add(<<"mode">>, mode, Options, Req1),
    Req3 = maybe_add(<<"responseFormat">>, response_format, Options, Req2),
    Req4 = maybe_add(<<"imageOptions">>, image_options, Options, Req3),
    Req5 = maybe_add(<<"requestHeaders">>, request_headers, Options, Req4),
    Req6 = inject_trace_context(Req5, State),
    case copilot_jsonrpc:request(Rpc, <<"session.send">>, Req6) of
        {ok, _} ->
            TimerRef = erlang:send_after(Timeout, self(), {wait_timeout, From}),
            Waiting = [{From, TimerRef, undefined} | State#state.waiting],
            {noreply, State#state{waiting = Waiting, last_assistant_event = undefined}};
        {error, _} = Err ->
            {reply, Err, State}
    end;

handle_call({subscribe, Subscriber}, _From, #state{subscribers = Subs} = State) ->
    Ref = make_ref(),
    {reply, Ref, State#state{subscribers = Subs#{Ref => Subscriber}}};

handle_call(get_messages, _From, #state{rpc = Rpc, session_id = SessionId} = State) ->
    Result = copilot_jsonrpc:request(Rpc, <<"session.getMessages">>,
                                     #{<<"sessionId">> => SessionId}),
    {reply, Result, State};

handle_call(abort, _From, #state{rpc = Rpc, session_id = SessionId} = State) ->
    case copilot_jsonrpc:request(Rpc, <<"session.abort">>,
                                 #{<<"sessionId">> => SessionId}) of
        {ok, _} -> {reply, ok, State};
        Err     -> {reply, Err, State}
    end;

handle_call(session_id, _From, #state{session_id = Id} = State) ->
    {reply, Id, State};

handle_call(workspace_path, _From, #state{workspace_path = WP} = State) ->
    {reply, WP, State};

handle_call({handle_exit_plan_mode, Request}, _From,
            #state{exit_plan_mode_handler = Handler} = State) ->
    Result = case Handler of
        undefined ->
            #{<<"approved">> => true};
        _ ->
            try Handler(Request)
            catch _:_ -> #{<<"approved">> => true}
            end
    end,
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({event, EventType, EventData}, State) ->
    Event = #{type => EventType, data => EventData},
    %% Notify all subscribers
    maps:foreach(fun(_Ref, Pid) ->
        Pid ! {copilot_session_event, State#state.session_id, Event}
    end, State#state.subscribers),
    %% Track last assistant message for send_and_wait
    NewLastEvent = case EventType of
        <<"assistant.message">> -> Event;
        _                      -> State#state.last_assistant_event
    end,
    %% Check if session is idle to resolve waiting callers
    NewWaiting = case EventType of
        <<"session.idle">> ->
            lists:foreach(fun({From, TimerRef, _}) ->
                erlang:cancel_timer(TimerRef),
                Response = case NewLastEvent of
                    undefined -> {ok, #{}};
                    Evt       -> {ok, Evt}
                end,
                gen_server:reply(From, Response)
            end, State#state.waiting),
            [];
        _ ->
            State#state.waiting
    end,
    %% Invoke hooks
    maybe_invoke_hook(EventType, EventData, State),
    %% Handle command.execute events
    maybe_handle_command(EventType, EventData, State),
    %% Handle elicitation.requested events
    maybe_handle_elicitation(EventType, EventData, State),
    {noreply, State#state{
        waiting = NewWaiting,
        last_assistant_event = NewLastEvent
    }};

handle_cast({unsubscribe, Ref}, #state{subscribers = Subs} = State) ->
    {noreply, State#state{subscribers = maps:remove(Ref, Subs)}};

handle_cast(ready, State) ->
    {noreply, State#state{ready = true}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({wait_timeout, From}, #state{waiting = Waiting} = State) ->
    NewWaiting = lists:filter(fun({F, _, _}) ->
        case F of
            From ->
                gen_server:reply(From, {error, timeout}),
                false;
            _ ->
                true
        end
    end, Waiting),
    {noreply, State#state{waiting = NewWaiting}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

maybe_add(JsonKey, MapKey, Options, Acc) ->
    case maps:get(MapKey, Options, undefined) of
        undefined -> Acc;
        Value     -> Acc#{JsonKey => Value}
    end.

maybe_invoke_hook(<<"tool.execution_start">>, Data, #state{hooks = Hooks})
  when Hooks =/= undefined ->
    case maps:get(on_pre_tool_use, Hooks, undefined) of
        undefined -> ok;
        Hook      -> catch Hook(Data)
    end;
maybe_invoke_hook(<<"tool.execution_complete">>, Data, #state{hooks = Hooks})
  when Hooks =/= undefined ->
    case maps:get(on_post_tool_use, Hooks, undefined) of
        undefined -> ok;
        Hook      -> catch Hook(Data)
    end;
maybe_invoke_hook(<<"session.start">>, Data, #state{hooks = Hooks})
  when Hooks =/= undefined ->
    case maps:get(on_session_start, Hooks, undefined) of
        undefined -> ok;
        Hook      -> catch Hook(Data)
    end;
maybe_invoke_hook(<<"session.shutdown">>, Data, #state{hooks = Hooks})
  when Hooks =/= undefined ->
    case maps:get(on_session_end, Hooks, undefined) of
        undefined -> ok;
        Hook      -> catch Hook(Data)
    end;
maybe_invoke_hook(<<"session.error">>, Data, #state{hooks = Hooks})
  when Hooks =/= undefined ->
    case maps:get(on_error_occurred, Hooks, undefined) of
        undefined -> ok;
        Hook      -> catch Hook(Data)
    end;
maybe_invoke_hook(_EventType, _Data, _State) ->
    ok.

maybe_handle_command(<<"command.execute">>, Data,
                     #state{rpc = Rpc, session_id = SessionId,
                            command_handlers = Handlers}) ->
    RequestId = maps:get(<<"requestId">>, Data, <<>>),
    CommandName = maps:get(<<"commandName">>, Data, <<>>),
    Command = maps:get(<<"command">>, Data, <<>>),
    Args = maps:get(<<"args">>, Data, <<>>),
    case maps:get(CommandName, Handlers, undefined) of
        undefined ->
            catch copilot_jsonrpc:request(Rpc, <<"commands.handlePendingCommand">>,
                #{<<"requestId">> => RequestId,
                  <<"error">> => <<"Unknown command: ", CommandName/binary>>});
        Handler ->
            try
                Handler(#{
                    session_id => SessionId,
                    command => Command,
                    command_name => CommandName,
                    args => Args
                }),
                catch copilot_jsonrpc:request(Rpc, <<"commands.handlePendingCommand">>,
                    #{<<"requestId">> => RequestId})
            catch
                _:Reason ->
                    ErrMsg = iolist_to_binary(io_lib:format("~p", [Reason])),
                    catch copilot_jsonrpc:request(Rpc, <<"commands.handlePendingCommand">>,
                        #{<<"requestId">> => RequestId, <<"error">> => ErrMsg})
            end
    end,
    ok;
maybe_handle_command(_EventType, _Data, _State) ->
    ok.

maybe_handle_elicitation(<<"elicitation.requested">>, Data,
                         #state{rpc = Rpc, session_id = SessionId,
                                elicitation_handler = Handler})
  when Handler =/= undefined ->
    RequestId = maps:get(<<"requestId">>, Data, <<>>),
    Context = #{
        session_id => SessionId,
        message => maps:get(<<"message">>, Data, <<>>),
        requested_schema => maps:get(<<"requestedSchema">>, Data, undefined),
        mode => maps:get(<<"mode">>, Data, undefined),
        elicitation_source => maps:get(<<"elicitationSource">>, Data, undefined),
        url => maps:get(<<"url">>, Data, undefined)
    },
    try
        Result = Handler(Context),
        catch copilot_jsonrpc:request(Rpc, <<"ui.handlePendingElicitation">>,
            #{<<"requestId">> => RequestId, <<"result">> => Result})
    catch
        _:_ ->
            catch copilot_jsonrpc:request(Rpc, <<"ui.handlePendingElicitation">>,
                #{<<"requestId">> => RequestId,
                  <<"result">> => #{<<"action">> => <<"cancel">>}})
    end,
    ok;
maybe_handle_elicitation(_EventType, _Data, _State) ->
    ok.

inject_trace_context(Req, #state{trace_context_provider = undefined}) ->
    Req;
inject_trace_context(Req, #state{trace_context_provider = Provider}) ->
    try
        Ctx = Provider(),
        Req1 = case maps:get(traceparent, Ctx, undefined) of
            undefined -> Req;
            TP -> Req#{<<"traceparent">> => TP}
        end,
        case maps:get(tracestate, Ctx, undefined) of
            undefined -> Req1;
            TS -> Req1#{<<"tracestate">> => TS}
        end
    catch _:_ -> Req
    end.
