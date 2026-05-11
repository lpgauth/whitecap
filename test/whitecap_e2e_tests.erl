-module(whitecap_e2e_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 18999).

e2e_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        [
            {"GET returns 200", fun get_returns_200/0},
            {"HEAD returns 200", fun head_returns_200/0},
            {"POST with body returns 200", fun post_with_body/0},
            {"Pipelined requests both answered",
                fun pipelined_requests/0},
            {"Unsupported verb closes with 501", fun unsupported_verb/0}
        ]}.

start() ->
    {ok, _} = application:ensure_all_started(whitecap),
    ok = whitecap:start_listeners(
        #{handler => test_handler, port => ?PORT}, 1),
    ok.

stop(_) ->
    application:stop(whitecap).

get_returns_200() ->
    {ok, Resp} = req(<<"GET / HTTP/1.1\r\nHost: x\r\n\r\n">>),
    ?assertMatch(<<"HTTP/1.1 200 OK", _/binary>>, Resp).

head_returns_200() ->
    {ok, Resp} = req(<<"HEAD /probe HTTP/1.1\r\nHost: x\r\n\r\n">>),
    ?assertMatch(<<"HTTP/1.1 200 OK", _/binary>>, Resp).

post_with_body() ->
    {ok, Resp} = req(<<"POST /x HTTP/1.1\r\nHost: x\r\n",
                       "Content-Length: 5\r\n\r\nhello">>),
    ?assertMatch(<<"HTTP/1.1 200 OK", _/binary>>, Resp).

pipelined_requests() ->
    {ok, S} = gen_tcp:connect("127.0.0.1", ?PORT,
        [binary, {active, false}], 1000),
    ok = gen_tcp:send(S, <<"GET /a HTTP/1.1\r\nHost: x\r\n\r\n",
                           "GET /b HTTP/1.1\r\nHost: x\r\n\r\n">>),
    Resp = drain(S, <<>>, 2),
    gen_tcp:close(S),
    ?assertEqual(2, count_matches(<<"HTTP/1.1 200 OK">>, Resp)).

unsupported_verb() ->
    {ok, Resp} = req(<<"DELETE / HTTP/1.1\r\nHost: x\r\n\r\n">>),
    ?assertMatch(<<"HTTP/1.1 501 Not Implemented", _/binary>>, Resp).

%% private
req(Bytes) ->
    {ok, S} = gen_tcp:connect("127.0.0.1", ?PORT,
        [binary, {active, false}], 1000),
    ok = gen_tcp:send(S, Bytes),
    Resp = gen_tcp:recv(S, 0, 1000),
    gen_tcp:close(S),
    Resp.

drain(_S, Acc, 0) ->
    Acc;
drain(S, Acc, N) ->
    case gen_tcp:recv(S, 0, 1000) of
        {ok, Data} ->
            Acc2 = <<Acc/binary, Data/binary>>,
            case count_matches(<<"HTTP/1.1 200 OK">>, Acc2) of
                M when M >= N -> Acc2;
                _ -> drain(S, Acc2, N)
            end;
        {error, _} ->
            Acc
    end.

count_matches(Needle, Hay) ->
    length(binary:matches(Hay, Needle)).
