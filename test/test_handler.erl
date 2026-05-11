-module(test_handler).

-export([handle/2]).

handle(_Req, _Opts) ->
    {ok, {200, [], <<>>}}.
