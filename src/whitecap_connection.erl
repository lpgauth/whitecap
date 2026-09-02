-module(whitecap_connection).
-include("whitecap.hrl").

-compile(inline).
-compile({inline_size, 512}).

% ERTS_POTENTIALLY_LONG_GC_HSIZE * 0.5
% As of 2026-07-24, erl_gc.h defines this as (128*1024) words
-define(GC_THRESHOLD, 65535).
-define(SEND_TIMEOUT, 50).

%% internal
-export([
    recv_loop/2,
    start/2
]).

-record(state, {
    armed = false     :: boolean(),
    bin_patterns      :: whitecap_protocol:bin_patterns(),
    handle            :: reference(),
    handler_timeout   :: timeout(),
    keepalive_timeout :: timeout(),
    max_keepalive     :: pos_integer(),
    request_timeout   :: timeout(),
    socket            :: socket:socket(),
    timestamp         :: integer()
}).

%% public
-spec start(socket:socket(), map()) -> pid().

start(Socket, Opts) ->
    proc_lib:spawn(?MODULE, recv_loop, [Socket, Opts]).

-spec recv_loop(socket:socket(), map()) -> ok.

recv_loop(Socket, Opts) ->
    {ok, BinPatterns} = whitecap_config:get(bin_patterns),
    {ok, Connections} = whitecap_config:get(connections),
    {ok, HandlerTimeout} = whitecap_config:get(handler_timeout),
    {ok, KeepAliveTimeout} = whitecap_config:get(keepalive_timeout),
    {ok, MaxConnections} = whitecap_config:get(max_connections),
    {ok, MaxKeepAlive} = whitecap_config:get(max_keepalive),
    {ok, RequestTimeout} = whitecap_config:get(request_timeout),
    State = #state {
        bin_patterns = BinPatterns,
        handle = make_ref(),
        handler_timeout = HandlerTimeout,
        keepalive_timeout = KeepAliveTimeout,
        max_keepalive = MaxKeepAlive,
        request_timeout = RequestTimeout,
        socket = Socket,
        timestamp = os:system_time()
    },
    case MaxConnections of
        infinity ->
            recv_loop(<<>>, undefined, undefined, State, 0, Opts);
        _ ->
            %% Release the slot the acceptor claimed on every exit path,
            %% a crash included, so the count cannot drift upwards.
            try
                recv_loop(<<>>, undefined, undefined, State, 0, Opts)
            after
                atomics:sub(Connections, 1, 1)
            end
    end.

%% private
close(Socket, KeepAlive, Timestamp) ->
    _ = socket:close(Socket),
    telemetry:execute([whitecap, connections, close], #{}),
    telemetry:execute([whitecap, connections, stats], #{
        duration => duration(Timestamp),
        keep_alive => KeepAlive
    }).

duration(Timestamp) ->
    erlang:convert_time_unit(os:system_time() - Timestamp, native, microsecond).

parse_requests(Data, Req, Received, #state {
        bin_patterns = BinPatterns,
        handler_timeout = HandlerTimeout,
        max_keepalive = MaxKeepAlive,
        socket = Socket,
        timestamp = Timestamp
    } = State, N, Opts) ->

    case whitecap_protocol:request(Data, Req, BinPatterns) of
        {ok, #whitecap_req {state = done} = Req2, Rest} ->
            Req3 = Req2#whitecap_req {received = Received},
            {ok, {Status, Headers, Body}} =
                whitecap_handler:handle(Req3, Opts, HandlerTimeout),
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
                            parse_requests(Rest, undefined, Received, State, N + 1, Opts);
                        {error, _} ->
                            close(Socket, N + 1, Timestamp),
                            ok
                    end
            end;
        {ok, #whitecap_req {} = Req2, Rest} ->
            recv_loop(Rest, Req2, Received, State, N, Opts);
        {error, not_enough_data} ->
            recv_loop(Data, Req, Received, State, N, Opts);
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
    case socket:send(Socket, Data, ?SEND_TIMEOUT) of
        ok ->
            ok;
        {error, Reason} ->
            telemetry:execute([whitecap, connections, send_error],
                #{size => iolist_size(Data)}, #{reason => reason(Reason)}),
            {error, Reason}
    end.

%% socket returns {error, {Reason, PartialData}} when bytes were
%% transferred before the failure; strip the data for matching.
reason({Reason, _Data}) ->
    Reason;
reason(Reason) ->
    Reason.

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

%% Once a request is in flight (buffered bytes or a partially-read body)
%% the idle keep-alive timeout no longer applies; the shorter request
%% timeout bounds how long we wait for the rest to arrive.
recv_timeout(<<>>, undefined, #state {keepalive_timeout = KeepAliveTimeout}) ->
    KeepAliveTimeout;
recv_timeout(_Buffer, _Req, #state {request_timeout = RequestTimeout}) ->
    RequestTimeout.

%% With a known Content-Length still outstanding, ask for exactly the
%% remaining bytes so the body lands in one recv and one concat instead
%% of repeatedly re-buffering partial reads.
recv_length(Buffer, #whitecap_req {state = body, content_length = ContentLength}) ->
    ContentLength - byte_size(Buffer);
recv_length(_Buffer, _Req) ->
    0.

%% The accepted socket has {otp, select_read} enabled: every successful
%% recv re-arms the read select inside the same NIF call, so the steady
%% state is one $socket message plus one recv per request instead of a
%% recv that fails with EAGAIN, a select, and a second recv.
recv_loop(Buffer, Req, Received, #state {armed = false} = State, N, Opts) ->
    do_recv(Buffer, Req, Received, State, N, Opts);
recv_loop(Buffer, Req, Received, #state {
        handle = Handle,
        socket = Socket,
        timestamp = Timestamp
    } = State, N, Opts) ->

    Timeout = recv_timeout(Buffer, Req, State),
    receive
        {'$socket', Socket, select, Handle} ->
            do_recv(Buffer, Req, Received, State, N, Opts);
        {'$socket', Socket, abort, {Handle, _Reason}} ->
            close(Socket, N, Timestamp),
            ok
    after Timeout ->
        send(Socket, whitecap_handler:response(408, [{"Connection", "close"}])),
        telemetry:execute([whitecap, connections, timeout], #{}),
        close(Socket, N, Timestamp),
        ok
    end.

do_recv(Buffer, Req, Received, #state {
        handle = Handle,
        socket = Socket,
        timestamp = Timestamp
    } = State, N, Opts) ->

    RecvLength = recv_length(Buffer, Req),
    case socket:recv(Socket, RecvLength, [], Handle) of
        {select_read, {_SelectInfo, Data}} ->
            handle_data(Data, Buffer, Req, Received, State#state {armed = true}, N, Opts);
        {ok, Data} ->
            handle_data(Data, Buffer, Req, Received, State#state {armed = false}, N, Opts);
        {select, {_SelectInfo, Data}} ->
            handle_data(Data, Buffer, Req, Received, State#state {armed = true}, N, Opts);
        {select, _SelectInfo} ->
            recv_loop(Buffer, Req, Received, State#state {armed = true}, N, Opts);
        {error, _Reason} ->
            close(Socket, N, Timestamp),
            ok
    end.

handle_data(Data, Buffer, Req, Received, State, N, Opts) ->
    Received2 = case {Buffer, Req} of
        {<<>>, undefined} -> os:system_time();
        _ -> Received
    end,
    Data2 = case Buffer of
        <<>> -> Data;
        _ -> <<Buffer/binary, Data/binary>>
    end,
    parse_requests(Data2, Req, Received2, State, N, Opts).
