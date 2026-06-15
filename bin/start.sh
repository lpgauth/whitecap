#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

# test_handler is a test fixture, so build the test profile to make it
# available, then boot whitecap with it on the default port (8080).
rebar3 as test compile

exec erl \
    -pa _build/test/lib/*/ebin \
    -pa _build/test/lib/whitecap/test \
    -eval 'application:ensure_all_started(whitecap), whitecap:start_listeners(#{handler => test_handler})'
