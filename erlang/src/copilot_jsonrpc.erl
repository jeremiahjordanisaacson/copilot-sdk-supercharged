%%%-------------------------------------------------------------------
%%% @doc JSON-RPC 2.0 client for Content-Length header framed stdio.
%%%
%%% Handles encoding/decoding of JSON-RPC messages over stdio using
%%% the Content-Length header framing protocol. Uses jsx for JSON
%%% serialization and manages request/response correlation via message
%%% IDs.
%%% @end
%%%-------------------------------------------------------------------
-module(copilot_jsonrpc).

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    request/3,
    request/4,
    notify/3,
    set_request_handler/3,
    stop/1
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

-define(JSONRPC_VERSION, <<"2.0">>).
-define(DEFAULT_TIMEOUT, 30000).

-record(state, {
    port             :: port() | undefined,
    next_id = 1      :: non_neg_integer(),
    pending          :: #{binary() => {pid(), reference()}},
    request_handlers :: #{binary() => fun()},
    buffer = <<>>    :: binary(),
    owner            :: pid()
}).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

-spec start_link(port(), pid()) -> {ok, pid()} | {error, term()}.
start_link(Port, Owner) ->
    gen_server:start_link(?MODULE, {Port, Owner}, []).

-spec request(pid(), binary(), map()) -> {ok, term()} | {error, term()}.
request(Pid, Method, Params) ->
    request(Pid, Method, Params, ?DEFAULT_TIMEOUT).

-spec request(pid(), binary(), map(), timeout()) -> {ok, term()} | {error, term()}.
request(Pid, Method, Params, Timeout) ->
    gen_server:call(Pid, {request, Method, Params}, Timeout).

-spec notify(pid(), binary(), map()) -> ok.
notify(Pid, Method, Params) ->
    gen_server:cast(Pid, {notify, Method, Params}).

-spec set_request_handler(pid(), binary(), fun()) -> ok.
set_request_handler(Pid, Method, Handler) ->
    gen_server:cast(Pid, {set_handler, Method, Handler}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid, normal, 5000).

%% ---------------------------------------------------------------------------
%% gen_server callbacks
%% ---------------------------------------------------------------------------

init({Port, Owner}) ->
    {ok, #state{
        port = Port,
        pending = #{},
        request_handlers = #{},
        owner = Owner
    }}.

handle_call({request, Method, Params}, From, State) ->
    #state{next_id = Id, port = Port, pending = Pending} = State,
    IdBin = integer_to_binary(Id),
    Message = #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"id">>      => IdBin,
        <<"method">>  => Method,
        <<"params">>  => Params
    },
    ok = send_message(Port, Message),
    NewPending = Pending#{IdBin => From},
    {noreply, State#state{next_id = Id + 1, pending = NewPending}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({notify, Method, Params}, State) ->
    #state{port = Port} = State,
    Message = #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"method">>  => Method,
        <<"params">>  => Params
    },
    ok = send_message(Port, Message),
    {noreply, State};

handle_cast({set_handler, Method, Handler}, State) ->
    #state{request_handlers = Handlers} = State,
    {noreply, State#state{request_handlers = Handlers#{Method => Handler}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    NewBuffer = <<(State#state.buffer)/binary, Data/binary>>,
    NewState = process_buffer(State#state{buffer = NewBuffer}),
    {noreply, NewState};

handle_info({Port, {exit_status, _Code}}, #state{port = Port} = State) ->
    State#state.owner ! {copilot_jsonrpc, port_closed},
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) when is_port(Port) ->
    catch port_close(Port),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal: message framing
%% ---------------------------------------------------------------------------

-spec send_message(port(), map()) -> ok.
send_message(Port, Message) ->
    JsonBin = jsx:encode(Message),
    Header = iolist_to_binary([
        <<"Content-Length: ">>,
        integer_to_binary(byte_size(JsonBin)),
        <<"\r\n\r\n">>
    ]),
    port_command(Port, <<Header/binary, JsonBin/binary>>),
    ok.

-spec process_buffer(#state{}) -> #state{}.
process_buffer(#state{buffer = Buffer} = State) ->
    case parse_frame(Buffer) of
        {ok, JsonBin, Rest} ->
            NewState = handle_message(JsonBin, State#state{buffer = Rest}),
            process_buffer(NewState);
        incomplete ->
            State
    end.

-spec parse_frame(binary()) -> {ok, binary(), binary()} | incomplete.
parse_frame(Buffer) ->
    case binary:match(Buffer, <<"\r\n\r\n">>) of
        {Pos, 4} ->
            HeaderPart = binary:part(Buffer, 0, Pos),
            case parse_content_length(HeaderPart) of
                {ok, Length} ->
                    BodyStart = Pos + 4,
                    Remaining = byte_size(Buffer) - BodyStart,
                    if
                        Remaining >= Length ->
                            JsonBin = binary:part(Buffer, BodyStart, Length),
                            Rest = binary:part(Buffer, BodyStart + Length,
                                               byte_size(Buffer) - BodyStart - Length),
                            {ok, JsonBin, Rest};
                        true ->
                            incomplete
                    end;
                error ->
                    incomplete
            end;
        nomatch ->
            incomplete
    end.

-spec parse_content_length(binary()) -> {ok, non_neg_integer()} | error.
parse_content_length(HeaderPart) ->
    Lines = binary:split(HeaderPart, <<"\r\n">>, [global]),
    parse_cl_lines(Lines).

parse_cl_lines([]) ->
    error;
parse_cl_lines([Line | Rest]) ->
    case Line of
        <<"Content-Length: ", LenBin/binary>> ->
            try binary_to_integer(string:trim(LenBin)) of
                Len -> {ok, Len}
            catch _:_ -> parse_cl_lines(Rest)
            end;
        _ ->
            parse_cl_lines(Rest)
    end.

%% ---------------------------------------------------------------------------
%% Internal: message dispatch
%% ---------------------------------------------------------------------------

-spec handle_message(binary(), #state{}) -> #state{}.
handle_message(JsonBin, State) ->
    case jsx:decode(JsonBin, [return_maps]) of
        #{<<"id">> := Id, <<"result">> := Result} ->
            handle_response(Id, {ok, Result}, State);
        #{<<"id">> := Id, <<"error">> := Error} ->
            handle_response(Id, {error, Error}, State);
        #{<<"id">> := Id, <<"method">> := Method, <<"params">> := Params} ->
            handle_server_request(Id, Method, Params, State);
        #{<<"method">> := Method, <<"params">> := Params} ->
            handle_notification(Method, Params, State);
        #{<<"method">> := Method} ->
            handle_notification(Method, #{}, State);
        _Other ->
            State
    end.

-spec handle_response(binary(), {ok, term()} | {error, term()}, #state{}) -> #state{}.
handle_response(Id, Result, #state{pending = Pending} = State) ->
    case maps:take(Id, Pending) of
        {From, NewPending} ->
            gen_server:reply(From, Result),
            State#state{pending = NewPending};
        error ->
            State
    end.

-spec handle_server_request(binary(), binary(), map(), #state{}) -> #state{}.
handle_server_request(Id, Method, Params, State) ->
    #state{request_handlers = Handlers, port = Port} = State,
    case maps:get(Method, Handlers, undefined) of
        undefined ->
            ErrorResp = #{
                <<"jsonrpc">> => ?JSONRPC_VERSION,
                <<"id">>      => Id,
                <<"error">>   => #{
                    <<"code">>    => -32601,
                    <<"message">> => <<"Method not found">>
                }
            },
            send_message(Port, ErrorResp);
        Handler ->
            spawn(fun() ->
                try Handler(Params) of
                    Result ->
                        Response = #{
                            <<"jsonrpc">> => ?JSONRPC_VERSION,
                            <<"id">>      => Id,
                            <<"result">>  => Result
                        },
                        send_message(Port, Response)
                catch
                    _:Reason ->
                        ErrResp = #{
                            <<"jsonrpc">> => ?JSONRPC_VERSION,
                            <<"id">>      => Id,
                            <<"error">>   => #{
                                <<"code">>    => -32603,
                                <<"message">> => iolist_to_binary(
                                    io_lib:format("~p", [Reason]))
                            }
                        },
                        send_message(Port, ErrResp)
                end
            end)
    end,
    State.

-spec handle_notification(binary(), map(), #state{}) -> #state{}.
handle_notification(Method, Params, State) ->
    State#state.owner ! {copilot_jsonrpc, notification, Method, Params},
    State.
