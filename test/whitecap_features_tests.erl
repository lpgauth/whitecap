-module(whitecap_features_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 18995).

features_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        [
            {"Request carries an arrival timestamp",
                fun received_timestamp/0},
            {"A body split across sends is assembled",
                fun split_body/0},
            {"A slow handler is bounded by handler_timeout (504)",
                fun handler_timeout/0},
            {"A stalled request is bounded by request_timeout (408)",
                fun request_timeout/0}
        ]}.

start() ->
    application:stop(whitecap),
    application:set_env(whitecap, handler_timeout, 100),
    application:set_env(whitecap, keepalive_timeout, 5000),
    application:set_env(whitecap, request_timeout, 300),
    {ok, _} = application:ensure_all_started(whitecap),
    ok = whitecap:start_listeners(
        #{handler => feature_handler, port => ?PORT}, 1),
    ok.

stop(_) ->
    application:stop(whitecap),
    application:unset_env(whitecap, handler_timeout),
    application:unset_env(whitecap, keepalive_timeout),
    application:unset_env(whitecap, request_timeout).

received_timestamp() ->
    {ok, Resp} = req(<<"GET /received HTTP/1.1\r\nHost: x\r\n\r\n">>),
    ?assertMatch(<<"HTTP/1.1 200 OK", _/binary>>, Resp),
    Received = binary_to_integer(body_of(Resp)),
    ?assert(Received > 0).

split_body() ->
    {ok, S} = gen_tcp:connect("127.0.0.1", ?PORT,
        [binary, {active, false}], 1000),
    ok = gen_tcp:send(S, <<"POST /echo HTTP/1.1\r\nHost: x\r\n",
                           "Content-Length: 10\r\n\r\nhel">>),
    timer:sleep(50),
    ok = gen_tcp:send(S, <<"loworld">>),
    {ok, Resp} = gen_tcp:recv(S, 0, 1000),
    gen_tcp:close(S),
    ?assertEqual(<<"helloworld">>, body_of(Resp)).

handler_timeout() ->
    {ok, Resp} = req(<<"GET /slow HTTP/1.1\r\nHost: x\r\n\r\n">>),
    ?assertMatch(<<"HTTP/1.1 504 Gateway Timeout", _/binary>>, Resp).

request_timeout() ->
    {ok, S} = gen_tcp:connect("127.0.0.1", ?PORT,
        [binary, {active, false}], 1000),
    %% A request line with no terminating blank line; the connection is
    %% past idle, so request_timeout (not keepalive_timeout) applies.
    ok = gen_tcp:send(S, <<"GET / HTTP/1.1\r\nHost: x\r\n">>),
    {ok, Resp} = gen_tcp:recv(S, 0, 2000),
    gen_tcp:close(S),
    ?assertMatch(<<"HTTP/1.1 408 Request Timeout", _/binary>>, Resp).

%% private
req(Bytes) ->
    {ok, S} = gen_tcp:connect("127.0.0.1", ?PORT,
        [binary, {active, false}], 1000),
    ok = gen_tcp:send(S, Bytes),
    Resp = gen_tcp:recv(S, 0, 1000),
    gen_tcp:close(S),
    Resp.

body_of(Resp) ->
    [_Head, Body] = binary:split(Resp, <<"\r\n\r\n">>),
    Body.
