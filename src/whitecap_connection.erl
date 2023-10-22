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

-define(RECV_TIMEOUT, 300000).

%% public
start_link(Socket, Opts) ->
    proc_lib:spawn_link(?MODULE, recv_loop, [Socket, Opts]).

recv_loop(Socket, Opts) ->
    recv_loop(<<>>, undefined, #state {
        bin_patterns = whitecap_protocol:bin_patterns(),
        socket = Socket
    }, Opts).

%% private
parse_requests(Data, Req, #state {
        bin_patterns = BinPatterns,
        socket = Socket
    } = State, Opts) ->

    % TODO: add timeout (408 Request Timeout)
    case whitecap_protocol:request(Data, Req, BinPatterns) of
        {ok, #whitecap_req {state = done} = Req2, Rest} ->
            {ok, Response} = whitecap_handler:handle(Req2, Opts),
            gen_tcp:send(Socket, Response),
            parse_requests(Rest, undefined, State, Opts);
        {ok, #whitecap_req {} = Req2, Rest} ->
            recv_loop(Rest, Req2, State, Opts);
        {error, not_enough_data} ->
            recv_loop(Data, Req, State, Opts);
        {error, _Reason} ->
            gen_tcp:send(Socket, whitecap_handler:response(501, [])),
            recv_loop(Data, Req, State, Opts)
    end.

recv_loop(Buffer, Req, #state {socket = Socket} = State, Opts) ->
    case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of
        {ok, Data} ->
            Data2 = <<Buffer/binary, Data/binary>>,
            parse_requests(Data2, Req, State, Opts);
        {error, timeout} ->
            telemetry:execute([whitecap, connections, timeout], #{}),
            gen_tcp:send(Socket, whitecap_handler:response(408, [{"Connection", "close"}])),
            gen_tcp:close(Socket),
            ok;
        {error, closed} ->
            telemetry:execute([whitecap, connections, close], #{}),
            ok;
        {error, Reason} ->
            io:format("recv error ~p~n", [Reason]),
            ok
    end.
