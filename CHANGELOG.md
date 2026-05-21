# Changelog

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
