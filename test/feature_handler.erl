-module(feature_handler).
-include_lib("whitecap/include/whitecap.hrl").

-export([handle/2]).

handle(#whitecap_req {path = <<"/slow">>}, _Opts) ->
    timer:sleep(500),
    {ok, {200, [], <<"slow">>}};
handle(#whitecap_req {path = <<"/received">>, received = Received}, _Opts) ->
    {ok, {200, [], integer_to_binary(Received)}};
handle(#whitecap_req {path = <<"/echo">>, body = Body}, _Opts) ->
    {ok, {200, [], Body}};
handle(_Req, _Opts) ->
    {ok, {200, [], <<>>}}.
