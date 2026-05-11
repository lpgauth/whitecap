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
            Ip = maps:get(ip, Opts, {0, 0, 0, 0}),
            Port = maps:get(port, Opts, 8080),
            case listen(Ip, Port) of
                {ok, LSocket} ->
                    proc_lib:init_ack(Parent, {ok, self()}),
                    loop(LSocket, Opts);
                {error, _} = Error ->
                    proc_lib:init_ack(Parent, Error)
            end;
        {false, Pid} ->
            proc_lib:init_ack(Parent, {error, {already_started, Pid}})
    end.

%% private
listen(Ip, Port) ->
    Options = [
        binary,
        {active, false},
        {backlog, 4096},
        {nodelay, true},
        {reuseaddr, true},
        {ip, Ip}
    ] ++ so_reuseport(),

    gen_tcp:listen(Port, Options).

loop(LSocket, Opts) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            telemetry:execute([whitecap, connections, accept], #{}),
            whitecap_connection:start_link(Socket, Opts),
            loop(LSocket, Opts);
        {error, closed} ->
            ok;
        {error, Reason} ->
            logger:warning("whitecap accept error: ~p", [Reason]),
            telemetry:execute([whitecap, connections, accept_error],
                #{}, #{reason => Reason}),
            loop(LSocket, Opts)
    end.

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
