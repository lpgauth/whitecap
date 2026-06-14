-module(bad_handler).

-export([handle/2]).

%% Returns a shape whitecap_connection does not expect, so the response
%% path badmatches and the connection worker crashes abnormally. Used to
%% prove such a crash does not take the acceptor down with it.
handle(_Req, _Opts) ->
    {ok, not_a_valid_response}.
