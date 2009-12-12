-module(main_tests).
-include_lib("eunit/include/eunit.hrl").

startup_test_() ->
	fun() ->
		?assertEqual(ok, application:start(emud))
	end.

shutdown_test_() ->
	fun() ->
		?assertEqual(ok, application:stop(emud))
	end.
