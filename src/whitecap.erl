-module(whitecap).
-include("whitecap.hrl").

-export([
    events/0,
    start_listeners/1,
    start_listeners/2
]).

-define(DEFAULT_LISTENERS, 4).

%% public
-spec events() ->
    [telemetry:event_name()].

events() ->
    [
        [whitecap, connections, accept],
        [whitecap, connections, accept_error],
        [whitecap, connections, close],
        [whitecap, connections, max_keepalive],
        [whitecap, connections, send_error],
        [whitecap, connections, stats],
        [whitecap, connections, timeout],
        [whitecap, handler, timeout]
    ].

-spec start_listeners(map()) ->
    ok | {error, term()}.

start_listeners(Opts) ->
    start_listeners(Opts, ?DEFAULT_LISTENERS).

-spec start_listeners(map(), non_neg_integer()) ->
    ok | {error, term()}.

start_listeners(_Opts, 0) ->
    ok;
start_listeners(Opts, N) ->
    case supervisor:start_child(whitecap_sup, ?CHILD(name(N, Opts), Opts, whitecap_acceptor)) of
        {ok, _Pid} ->
            start_listeners(Opts, N - 1);
        {error, _} = Error ->
            Error
    end.

%% private
name(N, Opts) ->
    Port = maps:get(port, Opts, 8080),
    list_to_atom(
        "whitecap_listener_" ++ integer_to_list(Port) ++ "_" ++ integer_to_list(N)
    ).
