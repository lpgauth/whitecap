-module(whitecap_e2e_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 18999).
-define(SECOND_PORT, 18998).
-define(CRASH_PORT, 18997).

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
            {"Unsupported verb closes with 501", fun unsupported_verb/0},
            {"Malformed request line closes with 400",
                fun malformed_request_line/0},
            {"Malformed Content-Length closes with 400",
                fun malformed_content_length/0},
            {"Crashing handler does not kill the acceptor",
                fun crashing_handler_isolated/0},
            {"Second start_listeners on a different port succeeds",
                fun second_start_listeners_different_port/0}
        ]}.

start() ->
    {ok, _} = application:ensure_all_started(whitecap),
    ok = whitecap:start_listeners(
        #{handler => test_handler, port => ?PORT}, 1),
    ok = whitecap:start_listeners(
        #{handler => bad_handler, port => ?CRASH_PORT}, 1),
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

malformed_request_line() ->
    {ok, Resp} = req(<<"GARBAGE\r\n\r\n">>),
    ?assertMatch(<<"HTTP/1.1 400 Bad Request", _/binary>>, Resp).

malformed_content_length() ->
    {ok, Resp} = req(<<"POST /x HTTP/1.1\r\nContent-Length: abc\r\n\r\n">>),
    ?assertMatch(<<"HTTP/1.1 400 Bad Request", _/binary>>, Resp),
    %% server keeps serving after a malformed request
    {ok, Resp2} = req(<<"GET / HTTP/1.1\r\nHost: x\r\n\r\n">>),
    ?assertMatch(<<"HTTP/1.1 200 OK", _/binary>>, Resp2).

crashing_handler_isolated() ->
    Name = list_to_atom(
        "whitecap_listener_" ++ integer_to_list(?CRASH_PORT) ++ "_1"),
    Acceptor = whereis(Name),
    ?assert(is_pid(Acceptor)),
    %% A burst of crashing connections exceeds the supervisor restart
    %% intensity if it can kill the acceptor; the app must stay up and
    %% the acceptor pid must be unchanged.
    [begin
        {ok, S} = gen_tcp:connect("127.0.0.1", ?CRASH_PORT,
            [binary, {active, false}], 1000),
        gen_tcp:send(S, <<"GET / HTTP/1.1\r\nHost: x\r\n\r\n">>),
        gen_tcp:recv(S, 0, 200),
        gen_tcp:close(S)
     end || _ <- lists:seq(1, 10)],
    timer:sleep(100),
    ?assertEqual(Acceptor, whereis(Name)),
    ?assert(lists:keymember(whitecap, 1, application:which_applications())).

second_start_listeners_different_port() ->
    ?assertEqual(ok,
        whitecap:start_listeners(
            #{handler => test_handler, port => ?SECOND_PORT}, 2)),
    {ok, Resp} = req(<<"GET / HTTP/1.1\r\nHost: x\r\n\r\n">>, ?SECOND_PORT),
    ?assertMatch(<<"HTTP/1.1 200 OK", _/binary>>, Resp).

%% private
req(Bytes) ->
    req(Bytes, ?PORT).

req(Bytes, Port) ->
    {ok, S} = gen_tcp:connect("127.0.0.1", Port,
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
