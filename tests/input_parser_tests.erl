-module(input_parser_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
	{"Main entry point for parser process",
		fun() ->
			?assertEqual("test", input_parser:parse("test"))
		end
	}.
