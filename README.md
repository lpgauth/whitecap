# whitecap

High-performance Erlang HTTP/1.1 server.

Whitecap optimizes the hot path of bidder-style traffic: short-lived requests, high concurrency, sub-millisecond targets. It is **not** a fully RFC 7230 conformant server — see [Non-goals](#non-goals).

## Install

```erlang
%% rebar.config
{deps, [
    {whitecap, "0.1.5"}
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

The `#whitecap_req{}` record (see `include/whitecap.hrl`) carries a `received` field: `os:system_time()` in native units, stamped when the first byte of the request arrived. A handler with a deadline (e.g. an RTB `tmax`) can compute the time already spent on the wire and shed accordingly:

```erlang
Elapsed = erlang:convert_time_unit(os:system_time() - Received, native, microsecond).
```

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

| Key                 | Default    | Meaning                                         |
| ------------------- | ---------- | ----------------------------------------------- |
| `handler_timeout`   | `infinity` | Max handler run time in ms. A finite value runs the handler in a monitored process; a handler that overruns is killed with `exit(kill)` and the request answered with `504`. The kill does not release resources the handler held (pool checkouts, locks), so keep such handlers side-effect-safe. Leave `infinity` (no per-request process) when the handler enforces its own deadline. |
| `keepalive_timeout` | `infinity` | Idle `gen_tcp:recv` timeout in ms while waiting for the *next* request on a keep-alive connection. |
| `max_connections`   | `infinity` | Concurrent connections served across all listeners. Once reached, further accepted sockets are closed immediately without a response and `[whitecap, connections, max_connections]` fires. The count is VM-wide, shared by every listener. |
| `max_keepalive`     | `10000`    | Requests served per connection before close.    |
| `receive_timeout`   | `infinity` | **Deprecated.** Back-compat default for `keepalive_timeout` and `request_timeout` when they are unset. |
| `request_timeout`   | `infinity` | `gen_tcp:recv` timeout in ms *once a request has started arriving*. Set finite for slowloris protection without dropping healthy idle keep-alive connections. |

## Telemetry

Events emitted under the `[whitecap, ...]` prefix:

| Event                              | Measurements             | Metadata        |
| ---------------------------------- | ------------------------ | --------------- |
| `[whitecap, connections, accept]`        | `#{}`                    | `#{}`           |
| `[whitecap, connections, accept_error]`  | `#{}`                    | `#{reason => term()}` |
| `[whitecap, connections, close]`         | `#{}`                    | `#{}`           |
| `[whitecap, connections, max_connections]` | `#{}`                  | `#{}`           |
| `[whitecap, connections, max_keepalive]` | `#{}`                    | `#{}`           |
| `[whitecap, connections, send_error]`    | `#{size => bytes}`       | `#{reason => term()}` |
| `[whitecap, connections, stats]`         | `#{duration => microseconds, keep_alive => integer()}` | `#{}` |
| `[whitecap, connections, timeout]`       | `#{}`                    | `#{}`           |
| `[whitecap, handler, timeout]`           | `#{}`                    | `#{}`           |

`[whitecap, handler, timeout]` fires when a finite `handler_timeout` is exceeded and the request is answered with `504`.

`[whitecap, connections, accept]` fires only for connections handed to a worker; one rejected by `max_connections` fires `[whitecap, connections, max_connections]` instead.

`duration` is microseconds (from `os:system_time/0` deltas converted via `erlang:convert_time_unit/3`).

`whitecap:events/0` returns the list of event names whitecap emits, so consumers can attach handlers programmatically:

```erlang
[telemetry:attach({whitecap_handler, Event}, Event, fun ?MODULE:handle/4, []) || Event <- whitecap:events()].
```

## Pipelining and backpressure

Each connection worker is a single process serving a per-connection request loop: `gen_tcp:recv` for the request line and headers, an exact-length `recv` for a body with a known `Content-Length`, then per-request dispatch. There is **no concurrent in-flight cap per connection** — whitecap reads, dispatches, and writes one request at a time on the same socket. That is appropriate because:

- HTTP/1.1 pipelining is a sequence of requests sharing a socket; clients are expected to wait for response N before reading response N+1. Whitecap matches that contract.
- All connection-level backpressure comes from the TCP send buffer: if the client stops draining responses, the next `gen_tcp:send` blocks the connection worker, naturally throttling that one socket.

There is no shared in-flight queue across connections (that's what `max_keepalive` and connection count are for). If you need true pipelined parallelism inside one connection, whitecap is the wrong tool — see Non-goals.

Connection count itself is capped by `max_connections`. The slot is claimed by the acceptor before the worker is spawned and released when the worker exits, a crash included, so the count cannot drift upwards. `whitecap:connections/0` returns the current count.

## Non-goals

Whitecap intentionally trades HTTP/1.1 conformance for throughput. Clients are expected to be cooperative (the bidder use case). Known and deliberate deviations:

- **Case-sensitive header matching.** `Content-Length` and `Transfer-Encoding` must use the exact casing `Content-Length:` / `content-length:` / `Transfer-Encoding:` / `transfer-encoding:`. Any other casing is silently ignored. Use canonical casing.
- **Strict OWS in header values.** Exactly one space is allowed between the colon and the value.
- **No HTTP/1.0.** Status lines must end with ` HTTP/1.1`. 1.0 requests get `501 Not Implemented`.
- **No `Transfer-Encoding: chunked`.** Returns `501`.
- **Limited verbs.** GET, HEAD, POST, PUT only.
- **No size limits.** Request lines, header sections, and bodies are not bounded. Run whitecap only with trusted clients or a fronting proxy.
- **Connection workers run unlinked and own their socket.** Right after `accept`, the acceptor hands the socket to the worker via `gen_tcp:controlling_process/2` and spawns it with `proc_lib:spawn/3` (not `spawn_link`). A worker crash therefore closes only its own socket and cannot take the acceptor — or sibling connections — down with it. Workers use `{active, false}` + synchronous `recv`; don't switch to active mode without revisiting the ownership handoff.

## Development

```
make compile       # debug_info + warn-everything
make eunit         # eunit + cover
make xref          # cross-reference checks
make dialyzer      # success-typing
make test          # xref + eunit + dialyzer
```

## License

MIT.
