-module(whitecap).
-include("whitecap.hrl").

-export([
    start_listener/0
]).

%% public
start_listener() ->
    supervisor:start_child(whitecap_sup, ?CHILD(test, whitecap_acceptor)),
    supervisor:start_child(whitecap_sup, ?CHILD(test2, whitecap_acceptor)).
