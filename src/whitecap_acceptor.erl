-module(whitecap_acceptor).
-include("whitecap.hrl").

-compile(inline).
-compile({inline_size, 512}).

%% internal
-export([
    init/2,
    start_link/1
]).

%% public
-spec start_link(atom()) ->
    {ok, pid()}.

start_link(Name) ->
    proc_lib:start_link(?MODULE, init, [Name, self()]).

-spec init(atom(), pid()) ->
    no_return() | ok.

init(Name, Parent) ->
    case safe_register(Name) of
        true ->
            process_flag(trap_exit, true),
            proc_lib:init_ack(Parent, {ok, self()}),
            {ok, LSocket} = listen(),
            loop(LSocket);
        {false, Pid} ->
            proc_lib:init_ack(Parent, {error, {already_started, Pid}})
    end.

%% private
listen() ->
    Options = [
        binary,
        {active, false},
        {backlog, 4096},
        {reuseaddr, true}
    ] ++ so_reuseport(),

    gen_tcp:listen(8080, Options).

loop(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    whitecap_connection:start_link(Socket),
    loop(LSocket).

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
