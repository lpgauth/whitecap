erl -pa _build/compile/lib/*/ebin -eval 'application:start(sasl), application:ensure_all_started(whitecap), whitecap:start_listeners(#{handler => test_handler})'
