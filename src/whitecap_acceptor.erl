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
                    {ok, Connections} = whitecap_config:get(connections),
                    {ok, MaxConnections} = whitecap_config:get(max_connections),
                    loop(LSocket, Opts, Connections, MaxConnections);
                {error, _} = Error ->
                    proc_lib:init_ack(Parent, Error)
            end;
        {false, Pid} ->
            proc_lib:init_ack(Parent, {error, {already_started, Pid}})
    end.

%% private
listen(Ip, Port) ->
    case socket:open(inet, stream, tcp) of
        {ok, LSocket} ->
            try
                ok = socket:setopt(LSocket, {socket, reuseaddr}, true),
                ok = so_reuseport(LSocket),
                ok = socket:setopt(LSocket, {socket, sndbuf}, 262144),
                ok = socket:bind(LSocket, #{family => inet, addr => Ip, port => Port}),
                ok = socket:listen(LSocket, 4096),
                {ok, LSocket}
            catch
                error:{badmatch, {error, _} = Error} ->
                    _ = socket:close(LSocket),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

loop(LSocket, Opts, Connections, MaxConnections) ->
    case socket:accept(LSocket) of
        {ok, Socket} ->
            case reserve(Connections, MaxConnections) of
                true ->
                    telemetry:execute([whitecap, connections, accept], #{}),
                    _ = socket:setopt(Socket, {tcp, nodelay}, true),
                    _ = socket:setopt(Socket, {otp, select_read}, true),
                    Pid = whitecap_connection:start(Socket, Opts),
                    _ = socket:setopt(Socket, {otp, controlling_process}, Pid);
                false ->
                    _ = socket:close(Socket),
                    telemetry:execute([whitecap, connections, max_connections], #{})
            end,
            loop(LSocket, Opts, Connections, MaxConnections);
        {error, closed} ->
            ok;
        {error, Reason} ->
            logger:warning("whitecap accept error: ~p", [Reason]),
            telemetry:execute([whitecap, connections, accept_error],
                #{}, #{reason => Reason}),
            loop(LSocket, Opts, Connections, MaxConnections)
    end.

%% The slot is claimed here, before the worker exists, and released by
%% the worker's exit path, so a worker that dies abnormally still gives
%% it back. Over the limit the socket is closed without a response: a
%% send from the accept loop would stall every pending accept behind one
%% slow client.
reserve(_Connections, infinity) ->
    true;
reserve(Connections, MaxConnections) ->
    case atomics:add_get(Connections, 1, 1) > MaxConnections of
        true ->
            atomics:sub(Connections, 1, 1),
            false;
        false ->
            true
    end.

safe_register(Name) ->
    try register(Name, self()) of
        true ->
            true
    catch
        _:_ ->
            {false, whereis(Name)}
    end.

so_reuseport(LSocket) ->
    case os:type() of
        {unix, OS} when OS =:= darwin; OS =:= linux ->
            socket:setopt(LSocket, {socket, reuseport}, true);
        _ ->
            ok
    end.
