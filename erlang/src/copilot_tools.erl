%%%-------------------------------------------------------------------
%%% @doc Tool definition helpers for the Copilot SDK.
%%%
%%% Provides functions to define tools that the Copilot agent can
%%% invoke. Handles argument parsing, result normalization, and
%%% error wrapping.
%%% @end
%%%-------------------------------------------------------------------
-module(copilot_tools).

-export([
    define_tool/3,
    define_tool/4,
    normalize_result/1
]).

%% @doc Define a tool with a name, description, and handler function.
%% The handler receives a map of parsed arguments and a tool invocation map.
-spec define_tool(binary(), binary(), fun()) -> map().
define_tool(Name, Description, Handler) ->
    define_tool(Name, Description, undefined, Handler).

%% @doc Define a tool with a name, description, JSON Schema parameters, and handler.
-spec define_tool(binary(), binary(), map() | undefined, fun()) -> map().
define_tool(Name, Description, Parameters, Handler) ->
    WrappedHandler = fun(Invocation) ->
        Args = case maps:get(<<"arguments">>, Invocation, #{}) of
            ArgBin when is_binary(ArgBin) ->
                try jsx:decode(ArgBin, [return_maps])
                catch _:_ -> #{}
                end;
            ArgMap when is_map(ArgMap) ->
                ArgMap;
            _ ->
                #{}
        end,
        try Handler(Args, Invocation) of
            Result ->
                normalize_result(Result)
        catch
            _Class:Reason ->
                copilot_types:tool_result(
                    <<"Invoking this tool produced an error. "
                      "Detailed information is not available.">>,
                    failure,
                    iolist_to_binary(io_lib:format("~p", [Reason]))
                )
        end
    end,
    Tool = #{
        <<"name">>        => Name,
        <<"description">> => Description,
        <<"handler">>     => WrappedHandler
    },
    case Parameters of
        undefined -> Tool;
        _         -> Tool#{<<"parameters">> => Parameters}
    end.

%% @doc Normalize a handler return value into a tool result map.
%% Accepts binaries, maps with resultType, other maps (JSON-encoded),
%% and atoms.
-spec normalize_result(term()) -> map().
normalize_result(undefined) ->
    copilot_types:tool_result(<<>>, success);
normalize_result(Result) when is_binary(Result) ->
    copilot_types:tool_result(Result, success);
normalize_result(#{<<"resultType">> := _} = Result) ->
    Result;
normalize_result(Result) when is_map(Result) ->
    copilot_types:tool_result(jsx:encode(Result), success);
normalize_result(Result) when is_list(Result) ->
    copilot_types:tool_result(jsx:encode(Result), success);
normalize_result(Result) ->
    copilot_types:tool_result(
        iolist_to_binary(io_lib:format("~p", [Result])),
        success
    ).
