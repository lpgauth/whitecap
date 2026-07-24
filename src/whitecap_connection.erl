-module(whitecap_connection).
-include("whitecap.hrl").

-compile(inline).
-compile({inline_size, 512}).

% ERTS_POTENTIALLY_LONG_GC_HSIZE * 0.5
% As of 2026-07-24, erl_gc.h defines this as (128*1024) words
-define(GC_THRESHOLD, 65535).

%% internal
-export([
    recv_loop/2,
    start/2
]).

-record(state, {
    bin_patterns :: whitecap_protocol:bin_patterns(),
    socket       :: gen_tcp:socket(),
    timestamp    :: integer()
}).

%% public
-spec start(gen_tcp:socket(), map()) -> pid().

start(Socket, Opts) ->
    proc_lib:spawn(?MODULE, recv_loop, [Socket, Opts]).

-spec recv_loop(gen_tcp:socket(), map()) -> ok.

recv_loop(Socket, Opts) ->
    {ok, BinPatterns} = whitecap_config:get(bin_patterns),
    recv_loop(<<>>, undefined, #state {
        bin_patterns = BinPatterns,
        socket = Socket,
        timestamp = os:system_time()
    }, 0, Opts).

%% private
close(Socket, KeepAlive, Timestamp) ->
    gen_tcp:close(Socket),
    telemetry:execute([whitecap, connections, close], #{}),
    telemetry:execute([whitecap, connections, stats], #{
        duration => duration(Timestamp),
        keep_alive => KeepAlive
    }).

duration(Timestamp) ->
    erlang:convert_time_unit(os:system_time() - Timestamp, native, microsecond).

parse_requests(Data, Req, #state {
        bin_patterns = BinPatterns,
        socket = Socket,
        timestamp = Timestamp
    } = State, N, Opts) ->

    case whitecap_protocol:request(Data, Req, BinPatterns) of
        {ok, #whitecap_req {state = done} = Req2, Rest} ->
            {ok, {Status, Headers, Body}} = whitecap_handler:handle(Req2, Opts),
            {ok, MaxKeepAlive} = whitecap_config:get(max_keepalive),
            case N + 1 >= MaxKeepAlive of
                true ->
                    Headers2 = force_connection_close(Headers),
                    Response = whitecap_handler:response(Status, Headers2, Body),
                    send(Socket, Response),
                    close(Socket, N + 1, Timestamp),
                    telemetry:execute([whitecap, connections, max_keepalive], #{}),
                    ok;
                false ->
                    Response = whitecap_handler:response(Status, Headers, Body),
                    case send(Socket, Response) of
                        ok ->
                            maybe_collect_garbage(),
                            parse_requests(Rest, undefined, State, N + 1, Opts);
                        {error, _} ->
                            close(Socket, N + 1, Timestamp),
                            ok
                    end
            end;
        {ok, #whitecap_req {} = Req2, Rest} ->
            recv_loop(Rest, Req2, State, N, Opts);
        {error, not_enough_data} ->
            recv_loop(Data, Req, State, N, Opts);
        {error, bad_request} ->
            send(Socket, whitecap_handler:response(400, [{"Connection", "close"}])),
            close(Socket, N, Timestamp),
            ok;
        {error, _Reason} ->
            send(Socket, whitecap_handler:response(501, [{"Connection", "close"}])),
            close(Socket, N, Timestamp),
            ok
    end.

send(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            ok;
        {error, Reason} = Error ->
            telemetry:execute([whitecap, connections, send_error],
                #{size => iolist_size(Data)}, #{reason => Reason}),
            Error
    end.

maybe_collect_garbage() ->
    case process_info(self(), total_heap_size) of
        {total_heap_size, W} when W > ?GC_THRESHOLD ->
            erlang:garbage_collect();
        _ ->
            ok
    end.

force_connection_close(Headers) ->
    [{<<"Connection">>, <<"close">>} | drop_connection(Headers)].

drop_connection([]) ->
    [];
drop_connection([{Key, _Value} = Header | T]) ->
    case string:equal(Key, <<"connection">>, true) of
        true ->
            drop_connection(T);
        false ->
            [Header | drop_connection(T)]
    end.

recv_loop(Buffer, Req, #state {socket = Socket, timestamp = Timestamp} = State, N, Opts) ->
    {ok, ReceiveTimeout} = whitecap_config:get(receive_timeout),
    case gen_tcp:recv(Socket, 0, ReceiveTimeout) of
        {ok, Data} ->
            Data2 = case Buffer of
                <<>> -> Data;
                _ -> <<Buffer/binary, Data/binary>>
            end,
            parse_requests(Data2, Req, State, N, Opts);
        {error, timeout} ->
            send(Socket, whitecap_handler:response(408, [{"Connection", "close"}])),
            close(Socket, N, Timestamp),
            telemetry:execute([whitecap, connections, timeout], #{}),
            ok;
        {error, closed} ->
            close(Socket, N, Timestamp),
            ok;
        {error, etimedout} ->
            close(Socket, N, Timestamp),
            ok
    end.
