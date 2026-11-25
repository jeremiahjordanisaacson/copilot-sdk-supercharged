%%%-------------------------------------------------------------------
%%% @doc JSON-RPC 2.0 client for Content-Length header framed stdio.
%%%
%%% This process owns the port of the Copilot CLI, so the CLI's stdout is
%%% delivered here. The port runs in stream mode and the CLI's stderr is not
%%% merged into it (it is inherited from the Erlang VM), so only JSON-RPC
%%% frames are read. Frames follow the vscode-jsonrpc StreamMessageReader used
%%% by the CLI: headers up to the first CRLF CRLF, a case-insensitive
%%% Content-Length header, then exactly that many bytes of UTF-8 JSON. A frame
%%% may arrive in several chunks and one chunk may carry several frames. Uses
%%% jsx for JSON serialization and correlates responses with requests by
%%% message ID.
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

%% Internal: frame parsing (exported for unit tests).
-export([parse_frames/1]).

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
    port               :: port() | undefined,
    next_id = 1        :: pos_integer(),
    pending = #{}      :: #{binary() => gen_server:from()},
    request_handlers = #{} :: #{binary() => fun((term()) -> term())},
    buffer = <<>>      :: binary(),
    owner              :: pid()
}).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

%% @doc Start the JSON-RPC client linked to the caller.
%%
%% With `{spawn_executable, Executable, PortSettings}` this process opens the
%% port itself (stream mode, binary stdio, exit status), adding
%% `PortSettings` such as `{args, Args}`, `{env, Env}` and `{cd, Dir}`.
%% An already open port is also accepted; it is connected to this process.
%% Server notifications are sent to `Owner` as
%% `{copilot_jsonrpc, notification, Method, Params}`, and
%% `{copilot_jsonrpc, port_closed}` is sent when the CLI exits.
-spec start_link(port() | {spawn_executable, file:filename_all(), list()}, pid()) ->
    {ok, pid()} | {error, term()}.
start_link({spawn_executable, Executable, Settings}, Owner) ->
    case gen_server:start_link(?MODULE, {undefined, Owner}, []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {open_port, Executable, Settings}, ?DEFAULT_TIMEOUT) of
                ok ->
                    {ok, Pid};
                {error, _} = Error ->
                    catch stop(Pid),
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
start_link(Port, Owner) when is_port(Port) ->
    gen_server:start_link(?MODULE, {Port, Owner}, []).

-spec request(pid(), binary(), term()) -> {ok, term()} | {error, term()}.
request(Pid, Method, Params) ->
    request(Pid, Method, Params, ?DEFAULT_TIMEOUT).

%% @doc Send a request and wait for its response. Returns `{error, timeout}`
%% when no response arrives in time and `{error, {connection_closed, Reason}}`
%% when the CLI exits or the client is stopped.
-spec request(pid(), binary(), term(), timeout()) -> {ok, term()} | {error, term()}.
request(Pid, Method, Params, Timeout) ->
    try
        gen_server:call(Pid, {request, Method, Params}, Timeout)
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{Reason, _}  -> {error, {connection_closed, Reason}}
    end.

-spec notify(pid(), binary(), term()) -> ok.
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
    %% Port exits arrive as messages, and the port is closed in terminate/2
    %% when the owning client stops.
    process_flag(trap_exit, true),
    case is_port(Port) of
        true  -> true = erlang:port_connect(Port, self());
        false -> ok
    end,
    {ok, #state{port = Port, owner = Owner}}.

handle_call({open_port, Executable, Settings}, _From, #state{port = undefined} = State) ->
    PortSettings = [stream, binary, use_stdio, exit_status, hide | Settings],
    try open_port({spawn_executable, Executable}, PortSettings) of
        Port -> {reply, ok, State#state{port = Port}}
    catch
        error:Reason -> {reply, {error, {spawn_failed, Reason}}, State}
    end;

handle_call({request, Method, Params}, From, State) ->
    #state{next_id = Id, port = Port, pending = Pending} = State,
    IdBin = integer_to_binary(Id),
    Message = #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"id">>      => IdBin,
        <<"method">>  => Method,
        <<"params">>  => Params
    },
    case send_message(Port, Message) of
        ok ->
            {noreply, State#state{next_id = Id + 1, pending = Pending#{IdBin => From}}};
        {error, _} = Error ->
            {reply, Error, State#state{next_id = Id + 1}}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({notify, Method, Params}, #state{port = Port} = State) ->
    _ = send_message(Port, #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"method">>  => Method,
        <<"params">>  => Params
    }),
    {noreply, State};

handle_cast({send_json, Json}, #state{port = Port} = State) ->
    %% Responses to server requests, encoded by the handler processes.
    _ = send_json(Port, Json),
    {noreply, State};

handle_cast({set_handler, Method, Handler}, #state{request_handlers = Handlers} = State) ->
    {noreply, State#state{request_handlers = Handlers#{Method => Handler}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}}, #state{port = Port, buffer = Buffer} = State) ->
    {Frames, Rest} = parse_frames(<<Buffer/binary, Data/binary>>),
    {noreply, lists:foldl(fun handle_frame/2, State#state{buffer = Rest}, Frames)};

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    {stop, normal, connection_closed({exit_status, Status}, State)};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, normal, connection_closed({port_exit, Reason}, State)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port} = State) ->
    _ = fail_pending({connection_closed, closed}, State),
    case is_port(Port) of
        true  -> catch port_close(Port);
        false -> ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Internal: connection lifecycle
%% ---------------------------------------------------------------------------

%% The CLI exited: fail the pending requests and tell the owner.
connection_closed(Reason, #state{owner = Owner} = State) ->
    Owner ! {copilot_jsonrpc, port_closed},
    (fail_pending({connection_closed, Reason}, State))#state{port = undefined}.

fail_pending(Reason, #state{pending = Pending} = State) ->
    maps:foreach(fun(_Id, From) -> gen_server:reply(From, {error, Reason}) end, Pending),
    State#state{pending = #{}}.

%% ---------------------------------------------------------------------------
%% Internal: message framing
%% ---------------------------------------------------------------------------

-spec send_message(port() | undefined, map()) -> ok | {error, term()}.
send_message(Port, Message) ->
    case encode(Message) of
        {ok, Json}         -> send_json(Port, Json);
        {error, _} = Error -> Error
    end.

-spec send_json(port() | undefined, binary()) -> ok | {error, term()}.
send_json(undefined, _Json) ->
    {error, {connection_closed, not_started}};
send_json(Port, Json) ->
    Frame = [<<"Content-Length: ">>, integer_to_binary(byte_size(Json)), <<"\r\n\r\n">>, Json],
    try port_command(Port, Frame) of
        true -> ok
    catch
        error:badarg -> {error, {connection_closed, port_closed}}
    end.

encode(Message) ->
    try jsx:encode(Message) of
        Json -> {ok, Json}
    catch
        error:Reason -> {error, {invalid_json, Reason}}
    end.

%% @doc Split buffered stdout into the bodies of its complete frames and the
%% unconsumed rest (a partial frame, or `<<>>`). As in vscode-jsonrpc's
%% StreamMessageReader, the header block ends at the first CRLF CRLF, the
%% Content-Length header name is case-insensitive, other headers are ignored,
%% the length counts bytes, and a header block without a valid Content-Length
%% is dropped.
-spec parse_frames(binary()) -> {[binary()], binary()}.
parse_frames(Buffer) ->
    parse_frames(Buffer, []).

parse_frames(Buffer, Acc) ->
    case binary:match(Buffer, <<"\r\n\r\n">>) of
        nomatch ->
            {lists:reverse(Acc), Buffer};
        {HeaderSize, 4} ->
            <<Header:HeaderSize/binary, _:4/binary, Rest/binary>> = Buffer,
            case content_length(Header) of
                {ok, Length} when byte_size(Rest) >= Length ->
                    <<Body:Length/binary, Tail/binary>> = Rest,
                    parse_frames(Tail, [Body | Acc]);
                {ok, _Length} ->
                    {lists:reverse(Acc), Buffer};
                error ->
                    parse_frames(Rest, Acc)
            end
    end.

content_length(Header) ->
    header_content_length(binary:split(Header, [<<"\r\n">>, <<"\n">>], [global])).

header_content_length([]) ->
    error;
header_content_length([Line | Lines]) ->
    case binary:split(Line, <<":">>) of
        [Name, Value] ->
            case ascii_lowercase(trim(Name)) of
                <<"content-length">> -> parse_length(trim(Value));
                _                    -> header_content_length(Lines)
            end;
        _ ->
            header_content_length(Lines)
    end.

parse_length(Value) ->
    try binary_to_integer(Value) of
        Length when Length >= 0 -> {ok, Length};
        _                       -> error
    catch
        error:badarg -> error
    end.

%% Headers are ASCII; these helpers never fail on other bytes.
ascii_lowercase(Bin) ->
    << <<(ascii_lower(C))>> || <<C>> <= Bin >>.

ascii_lower(C) when C >= $A, C =< $Z -> C + 32;
ascii_lower(C)                       -> C.

trim(Bin) ->
    trim_trailing(trim_leading(Bin)).

trim_leading(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t; C =:= $\r ->
    trim_leading(Rest);
trim_leading(Bin) ->
    Bin.

trim_trailing(Bin) ->
    Size = byte_size(Bin),
    case Size > 0 andalso is_blank(binary:last(Bin)) of
        true  -> trim_trailing(binary:part(Bin, 0, Size - 1));
        false -> Bin
    end.

is_blank(C) ->
    C =:= $\s orelse C =:= $\t orelse C =:= $\r.

%% ---------------------------------------------------------------------------
%% Internal: message dispatch
%% ---------------------------------------------------------------------------

handle_frame(Body, State) ->
    try jsx:decode(Body, [return_maps]) of
        Message when is_map(Message) -> handle_message(Message, State);
        _                            -> State
    catch
        error:_ -> State
    end.

handle_message(#{<<"id">> := Id, <<"method">> := Method} = Message, State)
  when is_binary(Method) ->
    handle_server_request(Id, Method, maps:get(<<"params">>, Message, #{}), State);
handle_message(#{<<"method">> := Method} = Message, #state{owner = Owner} = State)
  when is_binary(Method) ->
    Owner ! {copilot_jsonrpc, notification, Method, maps:get(<<"params">>, Message, #{})},
    State;
handle_message(#{<<"id">> := Id, <<"error">> := Error}, State) ->
    handle_response(Id, {error, Error}, State);
handle_message(#{<<"id">> := Id} = Message, State) ->
    handle_response(Id, {ok, maps:get(<<"result">>, Message, null)}, State);
handle_message(_Other, State) ->
    State.

handle_response(Id, Reply, #state{pending = Pending} = State) ->
    case maps:take(response_key(Id), Pending) of
        {From, Rest} ->
            gen_server:reply(From, Reply),
            State#state{pending = Rest};
        error ->
            State
    end.

response_key(Id) when is_integer(Id) -> integer_to_binary(Id);
response_key(Id)                     -> Id.

%% Handlers run in their own process, so a slow handler (a prompt waiting for
%% a person) never blocks reading; the encoded response is written by this
%% process. A result that is not valid JSON is answered with an error, so the
%% CLI never waits for a response that cannot be sent.
handle_server_request(Id, Method, Params, #state{request_handlers = Handlers} = State) ->
    case maps:get(Method, Handlers, undefined) of
        undefined ->
            _ = send_message(State#state.port,
                             error_response(Id, -32601, <<"Method not found: ", Method/binary>>)),
            State;
        Handler ->
            Rpc = self(),
            _ = spawn(fun() ->
                Response = try Handler(Params) of
                    Result ->
                        #{<<"jsonrpc">> => ?JSONRPC_VERSION, <<"id">> => Id, <<"result">> => Result}
                catch
                    _:Reason ->
                        error_response(Id, -32603, format_reason(Reason))
                end,
                Json = case encode(Response) of
                    {ok, Encoded} ->
                        Encoded;
                    {error, Invalid} ->
                        {ok, Encoded} = encode(error_response(Id, -32603, format_reason(Invalid))),
                        Encoded
                end,
                gen_server:cast(Rpc, {send_json, Json})
            end),
            State
    end.

format_reason(Reason) ->
    unicode:characters_to_binary(io_lib:format("~tp", [Reason])).

error_response(Id, Code, Message) ->
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"id">>      => Id,
        <<"error">>   => #{<<"code">> => Code, <<"message">> => Message}
    }.
