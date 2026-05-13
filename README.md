# whitecap

High-performance Erlang HTTP/1.1 server.

Whitecap optimizes the hot path of bidder-style traffic: short-lived requests, high concurrency, sub-millisecond targets. It is **not** a fully RFC 7230 conformant server — see [Non-goals](#non-goals).

## Install

```erlang
%% rebar.config
{deps, [
    {whitecap, "0.1.0"}
]}.
```

## Usage

A handler is any module exporting `handle/2`:

```erlang
-module(my_handler).
-export([handle/2]).

handle(_Req, _Opts) ->
    {ok, {200, [{<<"Content-Type">>, <<"text/plain">>}], <<"hello">>}}.
```

The return shape is `{ok, {Status, Headers, Body}}` where:

- `Status` is an integer status code (e.g. `200`) or `{Code, ReasonBinary}` for codes whitecap doesn't know about.
- `Headers` is a list of `{Key, Value}` iodata pairs. `Content-Length` is added automatically (except for 204).
- `Body` is iodata.

Then start one or more listeners (defaults to four `SO_REUSEPORT` acceptors):

```erlang
{ok, _} = application:ensure_all_started(whitecap),
ok = whitecap:start_listeners(#{
    handler => my_handler,
    handler_opts => #{},
    port => 8080,
    ip => {0, 0, 0, 0}
}).
```

## Configuration

Set via `sys.config` or `application:set_env/3` before `application:start(whitecap)`:

| Key               | Default    | Meaning                                         |
| ----------------- | ---------- | ----------------------------------------------- |
| `max_keepalive`   | `10000`    | Requests served per connection before close.    |
| `receive_timeout` | `infinity` | `gen_tcp:recv` timeout in ms. Set finite for slowloris protection. |

## Telemetry

Events emitted under the `[whitecap, connections, ...]` prefix:

| Event                              | Measurements             | Metadata        |
| ---------------------------------- | ------------------------ | --------------- |
| `[whitecap, connections, accept]`        | `#{}`                    | `#{}`           |
| `[whitecap, connections, accept_error]`  | `#{}`                    | `#{reason => term()}` |
| `[whitecap, connections, close]`         | `#{}`                    | `#{}`           |
| `[whitecap, connections, stats]`         | `#{duration => microseconds, keep_alive => integer()}` | `#{}` |
| `[whitecap, connections, timeout]`       | `#{}`                    | `#{}`           |
| `[whitecap, connections, max_keepalive]` | `#{}`                    | `#{}`           |

`duration` is microseconds (from `os:system_time/0` deltas converted via `erlang:convert_time_unit/3`).

## Non-goals

Whitecap intentionally trades HTTP/1.1 conformance for throughput. Clients are expected to be cooperative (the bidder use case). Known and deliberate deviations:

- **Case-sensitive header matching.** `Content-Length` and `Transfer-Encoding` must use the exact casing `Content-Length:` / `content-length:` / `Transfer-Encoding:` / `transfer-encoding:`. Any other casing is silently ignored. Use canonical casing.
- **Strict OWS in header values.** Exactly one space is allowed between the colon and the value.
- **No HTTP/1.0.** Status lines must end with ` HTTP/1.1`. 1.0 requests get `501 Not Implemented`.
- **No `Transfer-Encoding: chunked`.** Returns `501`.
- **Limited verbs.** GET, HEAD, POST, PUT only.
- **No size limits.** Request lines, header sections, and bodies are not bounded. Run whitecap only with trusted clients or a fronting proxy.
- **Acceptor does not transfer socket ownership** (`gen_tcp:controlling_process/2` is skipped). Works because connection workers use `{active, false}` + synchronous `recv`. Don't switch to active mode without revisiting this.

## Development

```
make compile       # debug_info + warn-everything
make eunit         # eunit + cover
make xref          # cross-reference checks
make dialyzer      # success-typing
make test          # xref + eunit + dialyzer
make profile       # fprofx → cachegrind
```

## License

MIT.
