-module(whitecap_acceptor).
-include("whitecap.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    init/3,
    start_link/2
]).

%% public
-spec start_link(atom(), map()) ->
    {ok, pid()}.

start_link(Name, Opts) ->
    proc_lib:start_link(?MODULE, init, [Name, Opts, self()]).

-spec init(atom(), map(), pid()) ->
    no_return() | ok.

init(Name, Opts, Parent) ->
    case safe_register(Name) of
        true ->
            proc_lib:init_ack(Parent, {ok, self()}),
            Port = maps:get(port, Opts, 8080),
            {ok, LSocket} = listen(Port),
            loop(LSocket, Opts);
        {false, Pid} ->
            proc_lib:init_ack(Parent, {error, {already_started, Pid}})
    end.

%% private
listen(Port) ->
    Options = [
        {inet_backend, socket},
        binary,
        {active, false},
        {backlog, 4096},
        {reuseaddr, true}
    ] ++ so_reuseport(),

    gen_tcp:listen(Port, Options).

loop(LSocket, Opts) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    telemetry:execute([whitecap, connections, accept], #{}),
    whitecap_connection:start_link(Socket, Opts),
    loop(LSocket, Opts).

safe_register(Name) ->
    try register(Name, self()) of
        true ->
            true
    catch
        _:_ ->
            {false, whereis(Name)}
    end.

so_reuseport() ->
    case os:type() of
        {unix, linux} ->
            [{raw, 1, 15, <<1:32/native>>}];
        {unix, darwin} ->
            [{raw, 16#ffff, 16#0200, <<1:32/native>>}];
        _ ->
            []
    end.
