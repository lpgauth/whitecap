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

head_test() ->
    Head = <<"HEAD /probe HTTP/1.1\r\nHost: x\r\n\r\n">>,
    ?assertMatch({ok, #whitecap_req {state = done, verb = head,
        path = <<"/probe">>}, <<>>}, whitecap_protocol:request(Head)).

put_test() ->
    Put = <<"PUT /resource HTTP/1.1\r\nContent-Length: 3\r\n\r\nabc">>,
    ?assertMatch({ok, #whitecap_req {state = done, verb = put,
        content_length = 3, body = <<"abc">>}, <<>>},
        whitecap_protocol:request(Put)).

content_length_zero_test() ->
    Post = <<"POST /x HTTP/1.1\r\nContent-Length: 0\r\n\r\n">>,
    ?assertMatch({ok, #whitecap_req {state = done, content_length = 0,
        body = undefined}, <<>>}, whitecap_protocol:request(Post)).

pipelined_test() ->
    Two = <<"GET /a HTTP/1.1\r\nHost: x\r\n\r\n",
            "GET /b HTTP/1.1\r\nHost: x\r\n\r\n">>,
    {ok, Req1, Rest1} = whitecap_protocol:request(Two),
    ?assertMatch(#whitecap_req {verb = get, path = <<"/a">>}, Req1),
    {ok, Req2, Rest2} = whitecap_protocol:request(Rest1),
    ?assertMatch(#whitecap_req {verb = get, path = <<"/b">>}, Req2),
    ?assertEqual(<<>>, Rest2).

partial_request_line_test() ->
    ?assertEqual({error, not_enough_data},
        whitecap_protocol:request(<<"GET /">>)).

partial_headers_test() ->
    ?assertEqual({error, not_enough_data},
        whitecap_protocol:request(<<"GET / HTTP/1.1\r\nHost: x\r\n">>)).

partial_body_test() ->
    Prefix = <<"POST /x HTTP/1.1\r\nContent-Length: 10\r\n\r\nhel">>,
    BinPatterns = whitecap_protocol:bin_patterns(),
    {ok, Req, Rest} = whitecap_protocol:request(Prefix, BinPatterns),
    ?assertMatch(#whitecap_req {state = body, content_length = 10}, Req),
    ?assertEqual(<<"hel">>, Rest),

    {ok, Req2, <<>>} = whitecap_protocol:request(<<Rest/binary, "loworld">>,
        Req, BinPatterns),
    ?assertMatch(#whitecap_req {state = done, body = <<"helloworld">>}, Req2).

bad_request_test() ->
    ?assertEqual({error, bad_request},
        whitecap_protocol:request(<<"NOT-HTTP\r\n\r\n">>)).

http_10_unsupported_test() ->
    ?assertEqual({error, unsupported_feature},
        whitecap_protocol:request(<<"GET / HTTP/1.0\r\n\r\n">>)).

unknown_verb_unsupported_test() ->
    ?assertEqual({error, unsupported_feature},
        whitecap_protocol:request(<<"DELETE / HTTP/1.1\r\n\r\n">>)).

chunked_unsupported_test() ->
    Req = <<"POST /x HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n">>,
    ?assertEqual({error, unsupported_feature},
        whitecap_protocol:request(Req)).

non_integer_content_length_test() ->
    Req = <<"POST /x HTTP/1.1\r\nContent-Length: abc\r\n\r\n">>,
    ?assertEqual({error, bad_request}, whitecap_protocol:request(Req)).

empty_content_length_test() ->
    Req = <<"POST /x HTTP/1.1\r\nContent-Length: \r\n\r\n">>,
    ?assertEqual({error, bad_request}, whitecap_protocol:request(Req)).

negative_content_length_test() ->
    Req = <<"POST /x HTTP/1.1\r\nContent-Length: -5\r\n\r\n">>,
    ?assertEqual({error, bad_request}, whitecap_protocol:request(Req)).

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

headers_no_colon_test() ->
    ?assertEqual({error, invalid_headers},
        whitecap_protocol:headers([<<"NoColonHere">>])).

headers_no_space_test() ->
    ?assertEqual({error, invalid_headers},
        whitecap_protocol:headers([<<"Host:127.0.0.1">>])).
