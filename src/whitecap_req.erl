-module(whitecap_req).
-include("whitecap.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([body/1]).

body(_) ->
    ok.
