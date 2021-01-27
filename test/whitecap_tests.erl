-module(whitecap_tests).
-include_lib("buoy/include/buoy.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("whitecap/include/whitecap.hrl").

request_test() ->
    whitecap_app:start(),
    buoy_app:start(),

    supervisor:start_child(whitecap_sup, ?CHILD(test, whitecap_acceptor)),

    Url = buoy_utils:parse_url(<<"http://localhost:8080/test">>),
    ok = buoy_pool:start(Url, [{pool_size, 1}]),
    {ok, Resp} = buoy:get(Url, #{timeout => 500}),

    ?assertEqual(200, Resp#buoy_resp.status_code).
