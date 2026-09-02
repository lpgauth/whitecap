-module(whitecap_max_connections_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 18997).
-define(MAX, 2).

max_connections_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        [
            {"Connections past max_connections are closed unanswered",
                fun limit/0},
            {"A closed connection frees its slot",
                fun release/0},
            {"A crashed worker frees its slot",
                fun release_on_crash/0}
        ]}.

start() ->
    application:stop(whitecap),
    application:set_env(whitecap, max_connections, ?MAX),
    {ok, _} = application:ensure_all_started(whitecap),
    ok = whitecap:start_listeners(
        #{handler => test_handler, port => ?PORT}, 1),
    ok.

stop(_) ->
    application:stop(whitecap),
    application:unset_env(whitecap, max_connections).

limit() ->
    Sockets = [connect() || _ <- lists:seq(1, ?MAX)],
    [?assertMatch(<<"HTTP/1.1 200 OK", _/binary>>, request(S)) || S <- Sockets],
    ?assertEqual(?MAX, whitecap:connections()),

    Extra = connect(),
    ?assertEqual({error, closed}, request(Extra)),
    ?assertEqual(?MAX, whitecap:connections()),

    [gen_tcp:close(S) || S <- [Extra | Sockets]],
    wait_for_connections(0).

release() ->
    Sockets = [connect() || _ <- lists:seq(1, ?MAX)],
    [?assertMatch(<<"HTTP/1.1 200 OK", _/binary>>, request(S)) || S <- Sockets],
    [gen_tcp:close(S) || S <- Sockets],
    wait_for_connections(0),

    S2 = connect(),
    ?assertMatch(<<"HTTP/1.1 200 OK", _/binary>>, request(S2)),
    gen_tcp:close(S2),
    wait_for_connections(0).

release_on_crash() ->
    ok = whitecap:start_listeners(
        #{handler => bad_handler, port => ?PORT + 1}, 1),
    {ok, S} = gen_tcp:connect("127.0.0.1", ?PORT + 1,
        [binary, {active, false}], 1000),
    ok = gen_tcp:send(S, <<"GET / HTTP/1.1\r\nHost: x\r\n\r\n">>),
    ?assertEqual({error, closed}, gen_tcp:recv(S, 0, 1000)),
    gen_tcp:close(S),
    wait_for_connections(0).

%% private
connect() ->
    {ok, S} = gen_tcp:connect("127.0.0.1", ?PORT,
        [binary, {active, false}], 1000),
    S.

request(Socket) ->
    ok = gen_tcp:send(Socket, <<"GET / HTTP/1.1\r\nHost: x\r\n\r\n">>),
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Response} ->
            Response;
        {error, _} = Error ->
            Error
    end.

wait_for_connections(N) ->
    wait_for_connections(N, 50).

wait_for_connections(N, 0) ->
    ?assertEqual(N, whitecap:connections());
wait_for_connections(N, Retries) ->
    case whitecap:connections() of
        N ->
            ok;
        _ ->
            timer:sleep(20),
            wait_for_connections(N, Retries - 1)
    end.
