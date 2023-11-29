-module(test_handler).

-export([handle/2]).

handle(_Req, _Opts) ->
    {ok, whitecap_handler:response(200, [])}.
