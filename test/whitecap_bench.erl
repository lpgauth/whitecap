-module(whitecap_bench).

-export([
    run/0
]).

run() ->
    Name = whitecap_protocol,
    Post = <<"POST /hello HTTP/1.1\r\nHost: 127.0.0.1:8080\r\n",
        "User-Agent: curl/7.54.0\r\nContent-Length: 5\r\n\r\nhello">>,
    BinPatterns = whitecap_protocol:bin_patterns(),

    Fun = fun () ->
        whitecap_protocol:request(Post, undefined, BinPatterns),
        ok
    end,
    bench(Name, Fun).
    %
    % Name2 = erlang_decode_packet,
    % Fun2 = fun () ->
    %     whitecap_protocol:request2(Post),
    %     ok
    % end,
    % bench(Name2, Fun2).

bench(Name, Fun) ->
    Results = timing_hdr:run(Fun, [
        {name, Name},
        {concurrency, 32},
        {iterations, 1000000},
        {output, "output/" ++ atom_to_list(Name)}
    ]),
    io:format("~p~n", [Results]).
