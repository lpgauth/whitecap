# Changelog

## 0.1.7

### Added

- `#whitecap_req{}` carries a `received` field: `os:system_time()` in
  native units, stamped when the first byte of the request arrived. A
  handler with a deadline (e.g. an RTB `tmax`) can measure the time
  already spent on the wire and shed accordingly.
- `handler_timeout` config (default `infinity`). A finite value runs
  the handler in a monitored process and, if it overruns, kills it and
  answers `504`, emitting a `[whitecap, handler, timeout]` telemetry
  event. `infinity` keeps the handler inline with no per-request
  process — the right choice when the handler enforces its own
  deadline.

### Changed

- The single `receive_timeout` is split into `keepalive_timeout` (idle
  wait for the next request on a keep-alive connection) and
  `request_timeout` (per-read wait once a request has started
  arriving). This allows a short in-request deadline for slowloris
  protection without dropping healthy idle keep-alive connections.
  `receive_timeout`, if set, remains the back-compat default for both;
  all three still default to `infinity`, so behaviour is unchanged
  unless a finite `request_timeout` is configured.
- A body with a known `Content-Length` is now read with an exact-length
  `gen_tcp:recv`, so it lands in one read and one concatenation instead
  of repeatedly re-buffering partial reads (previously O(n²) on a body
  fragmented across TCP segments).
- Per-connection config (`handler_timeout`, `keepalive_timeout`,
  `max_keepalive`, `request_timeout`) is read once when the connection
  starts rather than on every request/read.

## 0.1.6

### Changed

- Connection workers now proactively garbage-collect after sending a
  response when their heap grows past ~64K words (half of ERTS's
  `ERTS_POTENTIALLY_LONG_GC_HSIZE`). Above that threshold ERTS
  schedules a process's collection on a dirty CPU scheduler; with
  thousands of connections that offloading could starve the dirty
  schedulers, so the collection is forced inline instead.

## 0.1.5

### Fixed

- A non-integer, empty, or negative `Content-Length` no longer
  crashes the connection worker. `binary_to_integer/1` threw `badarg`
  on a non-integer value and a negative length caused a later
  badmatch; both propagated to the acceptor and, repeated within the
  supervisor restart window, took the whole application down. Such
  values now return `400 Bad Request`.
- A crashing connection worker can no longer take the acceptor (and
  its sibling connections) down with it. Workers are spawned unlinked
  with `proc_lib:spawn/3` and own their socket via
  `gen_tcp:controlling_process/2`, so an abnormal exit closes only
  that worker's socket.
- `whitecap_protocol:headers/1` returns `{error, invalid_headers}`
  for a header value that violates the single-space rule instead of
  raising `case_clause`.
- The keep-alive limit served `max_keepalive + 1` requests; it now
  serves exactly `max_keepalive`. The forced `Connection: close` also
  overwrites a handler-set `Connection` header of any casing or
  iodata form instead of emitting a duplicate.

### Changed

- `bin_patterns` is stored in `foil` rather than `persistent_term`,
  the same mechanism as the rest of the config. foil's
  compiled-module lookup is faster and carries the compiled patterns
  through its constant pool.
- `whitecap:events/0` and the README telemetry table now list the
  `[whitecap, connections, send_error]` event, which has been emitted
  since 0.1.4.

## 0.1.4

### Changed

- Listen sockets now set `sndbuf=256KB`, `send_timeout=50ms`, and
  `send_timeout_close=true` so accepted sockets inherit them. The
  default kernel sndbuf was too small for multi-segment writes
  under tight RTB timing, where ~20KB responses could be silently
  dropped.

### Fixed

- `gen_tcp:send` return values are now checked. On error the
  connection is closed instead of looping back into
  `parse_requests` against a dead socket, and a
  `[whitecap, connections, send_error]` telemetry event is emitted
  with the response size and reason.

## 0.1.3

### Added

- `whitecap:events/0` — returns the list of `telemetry:event_name()`
  values whitecap emits. Consumers can iterate this list to attach
  handlers programmatically instead of hardcoding event names that
  drift across releases:

  ```erlang
  [telemetry:attach(handler_id_for(E), E, fun handler/4, [])
      || E <- whitecap:events()].
  ```

- README gained a "Pipelining and backpressure" section. The
  per-connection request loop has no concurrent in-flight cap and
  relies on TCP send buffer pushback to throttle a slow client,
  which is appropriate for HTTP/1.1 pipelining semantics. The
  section spells out the contract so callers don't reach for the
  wrong tool.

## 0.1.2

### Fixed

- `whitecap:start_listeners/2` is now safe to call more than once per
  node. Listener processes are registered under names that include the
  configured port, so a second call with a different `port` no longer
  collides on `whitecap_listener_<N>` with the first call. Before this
  fix, the second `start_listeners` call always failed with
  `{error, {already_started, _}}` as soon as its countdown reached an
  `N` value already used by the first call (e.g. one listener on
  metrics port 9091 plus sixteen on the main port would crash on
  `whitecap_listener_1`).

## 0.1.1

Dep bumps + CI bootstrap. No source changes.

### Changed

- `foil` bumped from `0.1.3` to `0.1.4` (tighter `error/0` type;
  internal DRY refactor; behaviour unchanged).
- `metal` bumped from `0.1.1` to `0.1.2` (infrastructure refresh).
- `telemetry` bumped from `1.2.1` to `1.4.2`.
- Test profile: `buoy` switched from git ref `0.2.4` to the hex
  release `0.2.6` -- buoy itself bumped through to 0.2.6 with the
  same dep-modernization pass; the hex release transitively pulls
  in `shackle 0.7.1` (which replaced granderl with knot, fixing
  the OTP 27+ build break).
- Test profile: `timing` git ref pinned from `master` to tag `0.1.3`.
- Test profile: `fprofx` moved from `ransomr/fprofx` to
  `lpgauth/fprofx` (`otp_19` branch).

### Added

- GitHub Actions CI workflow. Matrix covers OTP 25, 26, 27, 28.
  (whitecap shipped 0.1.0 without any CI configured.)

### Removed

- `edoc` profile (was still configured to use `edown`; whitecap
  already uses `rebar3_ex_doc` for hex docs, so the edown path
  was unreachable).

## 0.1.0

Initial release.

### Features

- HTTP/1.1 GET, HEAD, POST, PUT.
- Multi-acceptor model using `SO_REUSEPORT` on Linux and Darwin.
- Configurable handler module via `whitecap:start_listeners/1,2`.
- Keep-alive with a configurable per-connection request cap (`max_keepalive`).
- Telemetry events under `[whitecap, connections, ...]`.

### Notes

- `[whitecap, connections, stats]` reports `duration` in **microseconds** (not seconds).
- See README "Non-goals" for deliberate departures from RFC 7230.
