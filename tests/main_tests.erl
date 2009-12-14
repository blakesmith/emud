-module(main_tests).
-include_lib("eunit/include/eunit.hrl").

startup_test_() ->
	{"Start the application", 
		fun() ->
			?assertEqual(ok, application:start(emud))
		end
	}.

shutdown_test_() ->
	{"Stop the application",
		fun() ->
			?assertEqual(ok, application:stop(emud))
		end
	}.
