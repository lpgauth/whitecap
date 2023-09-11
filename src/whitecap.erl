-module(whitecap).
-include("whitecap.hrl").

-export([
    start_listeners/1
]).

-define(N, 4).

%% public
start_listeners(Opts) ->
  start_listeners(?N, Opts).

start_listeners(0, _Opts) ->
  ok;
start_listeners(N, Opts) ->
  supervisor:start_child(whitecap_sup, ?CHILD(name(N), Opts, whitecap_acceptor)),
  start_listeners(N - 1, Opts).


%% ptivate
name(N) ->
  list_to_atom("whitecap_listener_" ++ integer_to_list(N)).
