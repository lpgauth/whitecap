-module(whitecap).
-include("whitecap.hrl").

-export([
    start_listeners/1,
    start_listeners/2
]).

-define(N, 4).

%% public
start_listeners(Opts) ->
  start_listeners(Opts, ?N).

start_listeners(_Opts, 0) ->
  ok;
start_listeners(Opts, N) ->
  supervisor:start_child(whitecap_sup, ?CHILD(name(N), Opts, whitecap_acceptor)),
  start_listeners(Opts, N - 1).


%% ptivate
name(N) ->
  list_to_atom("whitecap_listener_" ++ integer_to_list(N)).
