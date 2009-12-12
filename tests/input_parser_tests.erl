-module(input_parser_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
	fun() ->
		?assertEqual("test", input_parser:parse("test"))
	end.
