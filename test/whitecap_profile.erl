-module(whitecap_profile).

-export([
    fprofx/0
]).

-define(N, 10000).
-define(P, 20).

%% public
-spec fprofx() -> ok.

fprofx() ->
    Filenames = filelib:wildcard("_build/default/lib/*/ebin/*.beam"),
    Rootnames = [filename:rootname(Filename, ".beam") ||
        Filename <- Filenames],
    lists:foreach(fun code:load_abs/1, Rootnames),

    fprofx:start(),
    {ok, Tracer} = fprofx:profile(start),
    fprofx:trace([start, {procs, new}, {tracer, Tracer}]),

    BinPatterns = whitecap_protocol:bin_patterns(),
    Post = <<"POST /hello HTTP/1.1\r\nHost: 127.0.0.1:8080\r\n",
        "User-Agent: curl/7.54.0\r\nContent-Length: 5\r\n\r\nhello">>,
    Self = self(),

    [spawn(fun () ->
        [whitecap_protocol:request(Post, undefined, BinPatterns)
            || _ <- lists:seq(1, ?N)],
        Self ! exit
    end) || _ <- lists:seq(1, ?P)],
    wait(),

    fprofx:trace(stop),
    fprofx:analyse([totals, {dest, ""}]),
    fprofx:stop(),

    ok.

%% private
wait() ->
    wait(?P).

wait(0) ->
    ok;
wait(X) ->
    receive
        exit ->
            wait(X - 1)
    end.
