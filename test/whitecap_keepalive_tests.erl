-module(whitecap_keepalive_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 18996).
-define(MAX, 2).

keepalive_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        [
            {"Serves exactly max_keepalive requests then closes",
                fun boundary/0}
        ]}.

start() ->
    application:stop(whitecap),
    application:set_env(whitecap, max_keepalive, ?MAX),
    {ok, _} = application:ensure_all_started(whitecap),
    ok = whitecap:start_listeners(
        #{handler => test_handler, port => ?PORT}, 1),
    ok.

stop(_) ->
    application:stop(whitecap),
    application:unset_env(whitecap, max_keepalive).

boundary() ->
    {ok, S} = gen_tcp:connect("127.0.0.1", ?PORT,
        [binary, {active, false}], 1000),
    %% Pipeline one more request than the limit; the server must answer
    %% exactly ?MAX of them and close, with the last carrying
    %% Connection: close.
    Req = <<"GET / HTTP/1.1\r\nHost: x\r\n\r\n">>,
    ok = gen_tcp:send(S, binary:copy(Req, ?MAX + 1)),
    Resp = recv_all(S, <<>>),
    gen_tcp:close(S),
    ?assertEqual(?MAX, count(<<"HTTP/1.1 200 OK">>, Resp)),
    ?assertEqual(1, count(<<"Connection: close">>, Resp)).

%% private
recv_all(S, Acc) ->
    case gen_tcp:recv(S, 0, 500) of
        {ok, Data} ->
            recv_all(S, <<Acc/binary, Data/binary>>);
        {error, _} ->
            Acc
    end.

count(Needle, Hay) ->
    length(binary:matches(Hay, Needle)).
