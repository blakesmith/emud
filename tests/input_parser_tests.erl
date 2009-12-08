-module(input_parser_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
	?_assert("test" == input_parser:parse("test")).
