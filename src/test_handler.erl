-module(test_handler).

-export([handle/1]).

handle(_Req) ->
    {ok, whitecap_handler:response(200, [])}.
