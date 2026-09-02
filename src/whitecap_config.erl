-module(whitecap_config).

-export([
    init/0,
    get/1
]).

%% public
-spec init() -> ok.

init() ->
    ReceiveTimeout = env(receive_timeout, infinity),
    foil:new(?MODULE),
    foil:insert(?MODULE, bin_patterns, whitecap_protocol:bin_patterns()),
    foil:insert(?MODULE, connections, counters:new(1, [atomics])),
    foil:insert(?MODULE, handler_timeout, env(handler_timeout, infinity)),
    foil:insert(?MODULE, keepalive_timeout, env(keepalive_timeout, ReceiveTimeout)),
    foil:insert(?MODULE, max_connections, env(max_connections, infinity)),
    foil:insert(?MODULE, max_keepalive, env(max_keepalive, 10000)),
    foil:insert(?MODULE, request_timeout, env(request_timeout, ReceiveTimeout)),
    foil:load(?MODULE).

-spec get(atom()) -> {ok, term()} | {error, key_not_found}.

get(Key) ->
    foil:lookup(?MODULE, Key).

%% private
env(Key, Default) ->
    application:get_env(whitecap, Key, Default).
