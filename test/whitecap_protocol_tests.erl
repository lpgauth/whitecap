-module(whitecap_protocol_tests).
-include_lib("whitecap/include/whitecap.hrl").
-include_lib("eunit/include/eunit.hrl").

request_test() ->
    Get = <<"GET / HTTP/1.1\r\nHost: 127.0.0.1:8080\r\n",
        "User-Agent: curl/7.54.0\r\n\r\n">>,

    ?assertEqual({ok, #whitecap_req {
        state = done,
        verb = get,
        path = <<"/">>,
        headers = [
            <<"Host: 127.0.0.1:8080">>,
            <<"User-Agent: curl/7.54.0">>
        ]}, <<>>}, whitecap_protocol:request(Get)),

    Post = <<"POST /hello HTTP/1.1\r\nHost: 127.0.0.1:8080\r\n",
        "User-Agent: curl/7.54.0\r\nContent-Length: 5\r\n\r\nhello">>,

    ?assertEqual({ok, #whitecap_req {
        state = done,
        verb = post,
        path = <<"/hello">>,
        headers = [
            <<"Host: 127.0.0.1:8080">>,
            <<"User-Agent: curl/7.54.0">>,
            <<"Content-Length: 5">>
        ],
        content_length = 5,
        body = <<"hello">>
    }, <<>>}, whitecap_protocol:request(Post)).

headers_test() ->
    ?assertEqual({ok, [
        {<<"Host">>, <<"127.0.0.1:8080">>},
        {<<"User-Agent">>, <<"curl/7.54.0">>},
        {<<"Content-Length">>, <<"5">>}
    ]}, whitecap_protocol:headers([
        <<"Host: 127.0.0.1:8080">>,
        <<"User-Agent: curl/7.54.0">>,
        <<"Content-Length: 5">>
    ])).
