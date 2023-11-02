-module(whitecap_config).

-export([
    init/0,
    get/1
]).

%% public
init() ->
    foil:new(?MODULE),
    foil:insert(?MODULE, max_keepalive, 10000),
    foil:insert(?MODULE, receive_timeout, infinity),
    foil:load(?MODULE).

get(Key) ->
    foil:lookup(?MODULE, Key).
