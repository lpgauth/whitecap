-module(whitecap_handler).
-include("whitecap.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([handle/2, handle/3, response/2, response/3]).

-type status()  :: non_neg_integer() | {non_neg_integer(), iodata()}.
-type header()  :: {iodata(), iodata()}.
-type headers() :: [header()].

-export_type([status/0, header/0, headers/0]).

%% public
-spec handle(whitecap_req(), map()) -> {ok, {status(), headers(), iodata()}}.

handle(Req, Opts) ->
    handle(Req, Opts, infinity).

-spec handle(whitecap_req(), map(), timeout()) ->
    {ok, {status(), headers(), iodata()}}.

handle(Req, Opts, infinity) ->
    do_handle(Req, Opts);
handle(Req, Opts, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, MRef} = spawn_monitor(fun () ->
        Parent ! {Ref, do_handle(Req, Opts)}
    end),
    receive
        {Ref, Result} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, process, _Pid, Reason} ->
            logger:error("whitecap handler crashed: ~p", [Reason]),
            {ok, {500, [], <<>>}}
    after Timeout ->
        exit(Pid, kill),
        receive
            {Ref, _} ->
                receive {'DOWN', MRef, process, _Pid, _} -> ok end;
            {'DOWN', MRef, process, _Pid, _} ->
                ok
        end,
        telemetry:execute([whitecap, handler, timeout], #{}),
        {ok, {504, [], <<>>}}
    end.

-spec response(status(), headers()) -> iodata().

response(Status, Headers) ->
  response(Status, Headers, <<>>).

-spec response(status(), headers(), iodata()) -> iodata().

response(204, Headers, Body) ->
    [<<"HTTP/1.1 204 No Content\r\n">>, format_headers(Headers), <<"\r\n">>, Body];

response({204, _} = Status, Headers, Body) ->
    [format_status(Status), format_headers(Headers), <<"\r\n">>, Body];

response(Status, Headers, Body) ->
    ContentLength = integer_to_binary(iolist_size(Body)),
    Headers2 = [{<<"Content-Length">>, ContentLength} | Headers],
    [format_status(Status), format_headers(Headers2), <<"\r\n">>, Body].

%% private
do_handle(Req, #{handler := Handler} = Opts) ->
    try
        HandlerOpts = maps:get(handler_opts, Opts, #{}),
        Handler:handle(Req, HandlerOpts)
    catch
        E:R:ST ->
            logger:error("whitecap handler crashed: ~p:~p~n~p", [E, R, ST]),
            {ok, {500, [], <<>>}}
    end.

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
format_status(504) -> <<"HTTP/1.1 504 Gateway Timeout\r\n">>;
format_status({Code, Reason}) ->
    [<<"HTTP/1.1 ">>, integer_to_binary(Code), <<" ">>, Reason, <<"\r\n">>].
