-module(whitecap_config).

-export([
    init/0,
    get/1
]).

%% public
-spec init() -> ok.

init() ->
    foil:new(?MODULE),
    foil:insert(?MODULE, bin_patterns, whitecap_protocol:bin_patterns()),
    foil:insert(?MODULE, max_keepalive, env(max_keepalive, 10000)),
    foil:insert(?MODULE, receive_timeout, env(receive_timeout, infinity)),
    foil:load(?MODULE).

-spec get(atom()) -> {ok, term()} | {error, key_not_found}.

get(Key) ->
    foil:lookup(?MODULE, Key).

%% private
env(Key, Default) ->
    application:get_env(whitecap, Key, Default).
