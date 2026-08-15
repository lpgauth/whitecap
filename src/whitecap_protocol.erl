-module(whitecap_protocol).
-include("whitecap.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    bin_patterns/0,
    headers/1,
    request/1,
    request/2,
    request/3
]).

-record(bin_patterns, {
    rn   :: binary:cp(),
    rnrn :: binary:cp()
}).

-type bin_patterns() :: #bin_patterns {}.
-type error()        :: {error, reason()}.
-type reason()       :: bad_request | not_enough_data | unsupported_feature.

-export_type([bin_patterns/0]).

%% public
-spec bin_patterns() ->
    bin_patterns().

bin_patterns() ->
    #bin_patterns {
        rn   = binary:compile_pattern(<<"\r\n">>),
        rnrn = binary:compile_pattern(<<"\r\n\r\n">>)
    }.

-spec headers([binary()]) ->
    {ok, [{binary(), undefined | binary()}]} | {error, invalid_headers}.

headers(Headers) ->
    parse_headers(Headers, []).

%% public
-spec request(binary()) ->
    {ok, whitecap_req(), binary()} | error().

request(Data) ->
    request(Data, bin_patterns()).

-spec request(binary(), bin_patterns()) ->
    {ok, whitecap_req(), binary()} | error().

request(Data, BinPatterns) ->
    request(Data, undefined, BinPatterns).

-spec request(binary(), undefined | whitecap_req(), bin_patterns()) ->
    {ok, whitecap_req(), binary()} | error().

request(Data, undefined, BinPatterns) ->
    case parse_status_line(Data, BinPatterns) of
        {Verb, Path, Rest} ->
            case split_headers(Rest, BinPatterns) of
                {undefined, Headers, Rest2} ->
                    {ok, #whitecap_req {
                        state = done,
                        verb = Verb,
                        path = Path,
                        headers = Headers
                    }, Rest2};
                {0, Headers, Rest2} ->
                    {ok, #whitecap_req {
                        state = done,
                        verb = Verb,
                        path = Path,
                        headers = Headers,
                        content_length = 0
                    }, Rest2};
                {ContentLength, Headers, Rest2} ->
                    request(Rest2, #whitecap_req {
                        state = body,
                        verb = Verb,
                        path = Path,
                        headers = Headers,
                        content_length = ContentLength
                    }, BinPatterns);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
request(Data, #whitecap_req {
        state = body,
        content_length = ContentLength
    } = Request, _BinPatterns) when byte_size(Data) >= ContentLength ->

    <<Body:ContentLength/binary, Rest/binary>> = Data,

    {ok, Request#whitecap_req {
        state = done,
        body = Body
    }, Rest};
request(Data, #whitecap_req {
        state = body
    } = Req, _BinPatterns) ->

    {ok, Req, Data}.

%% private
content_length(<<"Content-Length: ", Rest/binary>>, undefined) ->
    parse_content_length(Rest);
content_length(<<"content-length: ", Rest/binary>>, undefined) ->
    parse_content_length(Rest);
content_length(<<"Transfer-Encoding: chunked">>, undefined) ->
    {error, unsupported_feature};
content_length(<<"transfer-encoding: chunked">>, undefined) ->
    {error, unsupported_feature};
content_length(_Header, ContentLength) ->
    {ok, ContentLength}.

parse_content_length(Bin) ->
    try binary_to_integer(Bin) of
        N when N >= 0 ->
            {ok, N};
        _ ->
            {error, bad_request}
    catch
        error:badarg ->
            {error, bad_request}
    end.

parse_headers([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_headers([Header | T], Acc) ->
    case binary:split(Header, <<":">>) of
        [Header] ->
            {error, invalid_headers};
        [Key, <<>>] ->
            parse_headers(T, [{Key, undefined} | Acc]);
        [Key, <<" ", Value/binary>>] ->
            parse_headers(T, [{Key, Value} | Acc]);
        [_Key, _Value] ->
            {error, invalid_headers}
    end.

parse_status_line(Data, #bin_patterns {rn = Rn}) ->
    case binary:split(Data, Rn) of
        [Data] ->
            {error, not_enough_data};
        [Line, Rest] ->
            Size = byte_size(Line) - 9,
            case Line of
                <<VerbPath:Size/binary, " HTTP/1.1">> ->
                    case parse_verb_path(VerbPath) of
                        {ok, Verb, Path} ->
                            {Verb, Path, Rest};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                <<_:Size/binary, " HTTP/1.0">> ->
                    {error, unsupported_feature};
                _ ->
                    {error, bad_request}
            end
    end.

parse_verb_path(<<"GET ", Path/binary>>) ->
    {ok, get, Path};
parse_verb_path(<<"POST ", Path/binary>>) ->
    {ok, post, Path};
parse_verb_path(<<"PUT ", Path/binary>>) ->
    {ok, put, Path};
parse_verb_path(<<"HEAD ", Path/binary>>) ->
    {ok, head, Path};
parse_verb_path(_verb_path) ->
    {error, unsupported_feature}.

split_headers(Data, #bin_patterns {rn = Rn, rnrn = RnRn}) ->
    case binary:split(Data, RnRn) of
        [Data] ->
            {error, not_enough_data};
        [Headers, Rest] ->
            case split_header_lines(Headers, Rn, undefined) of
                {ok, Headers2, ContentLength} ->
                    {ContentLength, Headers2, Rest};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

split_header_lines(Bin, Rn, ContentLength) ->
    case binary:split(Bin, Rn) of
        [Header, Rest] ->
            case content_length(Header, ContentLength) of
                {ok, ContentLength2} ->
                    case split_header_lines(Rest, Rn, ContentLength2) of
                        {ok, Headers, ContentLength3} ->
                            {ok, [Header | Headers], ContentLength3};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        [Header] ->
            case content_length(Header, ContentLength) of
                {ok, ContentLength2} ->
                    {ok, [Header], ContentLength2};
                {error, Reason} ->
                    {error, Reason}
            end
    end.
