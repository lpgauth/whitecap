-module(whitecap_connection).
-include("whitecap.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    recv_loop/2,
    start_link/2
]).

-record(state, {
    bin_patterns,
    socket
}).

%% public
start_link(Socket, Opts) ->
    proc_lib:spawn_link(?MODULE, recv_loop, [Socket, Opts]).

recv_loop(Socket, Opts) ->
    recv_loop(<<>>, undefined, #state {
        bin_patterns = whitecap_protocol:bin_patterns(),
        socket = Socket
    }, 0, Opts).

%% private
parse_requests(Data, Req, #state {
        bin_patterns = BinPatterns,
        socket = Socket
    } = State, N, Opts) ->

    case whitecap_protocol:request(Data, Req, BinPatterns) of
        {ok, #whitecap_req {state = done} = Req2, Rest} ->
            {ok, {Status, Headers, Body}} = whitecap_handler:handle(Req2, Opts),
            {ok, MaxKeepAlive} = whitecap_config:get(max_keepalive),
            case N == MaxKeepAlive of
                true ->
                    Headers2 = overwrite_key(Headers, "Connection", "close", false),
                    Response = whitecap_handler:response(Status, Headers2, Body),
                    gen_tcp:send(Socket, Response),
                    gen_tcp:close(Socket),
                    telemetry:execute([whitecap, connections, close], #{}),
                    telemetry:execute([whitecap, connections, max_keepalive], #{}),
                    telemetry:execute([whitecap, connections, requests], #{keep_alive => N + 1}),
                    ok;
                false ->
                    Response = whitecap_handler:response(Status, Headers, Body),
                    gen_tcp:send(Socket, Response),
                    parse_requests(Rest, undefined, State, N, Opts)

            end;
        {ok, #whitecap_req {} = Req2, Rest} ->
            recv_loop(Rest, Req2, State, N, Opts);
        {error, not_enough_data} ->
            recv_loop(Data, Req, State, N, Opts);
        {error, Reason} ->
            io:format("parse error ~p~n", [Reason]),
            gen_tcp:send(Socket, whitecap_handler:response(501, [{"Connection", "close"}])),
            gen_tcp:close(Socket),
            telemetry:execute([whitecap, connections, close], #{}),
            telemetry:execute([whitecap, connections, requests], #{keep_alive => N}),
            ok
    end.

overwrite_key([], Key, Value2, false) ->
    [{Key, Value2}];
overwrite_key([], _Key, _Value2, true) ->
    [];
overwrite_key([{Key, _Value} | T], Key, Value2, _Overridden) ->
    [{Key, Value2} | overwrite_key(T, Key, Value2, true)];
overwrite_key([{Key, Value} | T], Key2, Value2, Overridden) ->
    [{Key, Value} | overwrite_key(T, Key2, Value2, Overridden)].

recv_loop(Buffer, Req, #state {socket = Socket} = State, N, Opts) ->
    {ok, ReceiveTimeout} = whitecap_config:get(receive_timeout),
    case gen_tcp:recv(Socket, 0, ReceiveTimeout) of
        {ok, Data} ->
            Data2 = <<Buffer/binary, Data/binary>>,
            parse_requests(Data2, Req, State, N, Opts);
        {error, timeout} ->
            gen_tcp:send(Socket, whitecap_handler:response(408, [{"Connection", "close"}])),
            gen_tcp:close(Socket),
            telemetry:execute([whitecap, connections, close], #{}),
            telemetry:execute([whitecap, connections, requests], #{keep_alive => N}),
            telemetry:execute([whitecap, connections, timeout], #{}),
            ok;
        {error, closed} ->
            gen_tcp:close(Socket),
            telemetry:execute([whitecap, connections, close], #{}),
            telemetry:execute([whitecap, connections, requests], #{keep_alive => N}),
            ok
    end.
