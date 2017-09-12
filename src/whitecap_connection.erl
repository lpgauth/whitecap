-module(whitecap_connection).
-include("whitecap.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    recv_loop/1,
    start_link/1
]).

-record(state, {
    bin_patterns,
    socket
}).

%% public
start_link(Socket) ->
    proc_lib:spawn_link(?MODULE, recv_loop, [Socket]).

recv_loop(Socket) ->
    recv_loop(<<>>, undefined, #state {
        bin_patterns = whitecap_protocol:bin_patterns(),
        socket = Socket
    }).

%% private
parse_requests(Data, Req, #state {
        bin_patterns = BinPatterns,
        socket = Socket
    } = State) ->

    case whitecap_protocol:request(Data, Req, BinPatterns) of
        {ok, #whitecap_req {state = done}, Rest} ->
            gen_tcp:send(Socket, <<"HTTP/1.1 200 OK\r\n",
                "Server: whitecap\r\n",
                "Date: Tue, 07 Mar 2017 01:10:09 GMT\r\n",
                "Content-Length: 12\r\n\r\n",
                "hello world!">>),
            parse_requests(Rest, undefined, State);
        {ok, #whitecap_req {} = Req2, Rest} ->
            recv_loop(Rest, Req2, State);
        {error, not_enough_data} ->
            recv_loop(Data, Req, State);
        {error, _Reason} ->
            % TODO: 500?
            recv_loop(Data, Req, State)
    end.

recv_loop(Buffer, Req, #state {socket = Socket} = State) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Data2 = <<Buffer/binary, Data/binary>>,
            parse_requests(Data2, Req, State);
        {error, Reason} ->
            io:format("recv error ~p~n", [Reason]),
            ok
    end.
