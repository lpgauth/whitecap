-module(whitecap_handler).
-include("whitecap.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([handle/2, response/2, response/3]).

%% public
handle(Req, #{handler := Handler} = Opts) ->
    try
        HandlerOpts = maps:get(handler_opts, Opts, #{}),
        Handler:handle(Req, HandlerOpts)
    catch
        E:R:ST ->
            io:format("error: ~p:~p~n~p", [E, R, ST]),
            {ok, response(500, [])}
    end.

response(Status, Headers) ->
  response(Status, Headers, <<>>).

response(Status, Headers, Body) ->
    ContentLength = integer_to_binary(iolist_size(Body)),
    Headers2 = [{<<"Content-Length">>, ContentLength} | Headers],
    [format_status(Status), format_headers(Headers2), <<"\r\n">>, Body].

%% private
format_headers(Headers) ->
    [format_header(Header) || Header <- Headers].

format_header({Key, Value}) ->
    [Key, <<": ">>, Value, <<"\r\n">>].

format_status(200) -> <<"HTTP/1.1 200 OK\r\n">>;
format_status(204) -> <<"HTTP/1.1 204 No Content\r\n">>;
format_status(301) -> <<"HTTP/1.1 301 Moved Permanently\r\n">>;
format_status(302) -> <<"HTTP/1.1 302 Found\r\n">>;
format_status(400) -> <<"HTTP/1.1 400 Bad Request\r\n">>;
format_status(403) -> <<"HTTP/1.1 403 Forbidden\r\n">>;
format_status(404) -> <<"HTTP/1.1 404 Not Found\r\n">>;
format_status(408) -> <<"HTTP/1.1 408 Request Timeout\r\n">>;
format_status(500) -> <<"HTTP/1.1 500 Internal Server Error\r\n">>;
format_status(501) -> <<"HTTP/1.1 501 Not Implemented\r\n">>;
format_status(502) -> <<"HTTP/1.1 502 Bad Gateway\r\n">>;
format_status({Code, Reason}) -> [<<"HTTP/1.1 ">>, integer_to_binary(Code), <<" ">>, Reason, <<"\r\n">>].
