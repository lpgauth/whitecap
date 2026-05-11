# Changelog

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
