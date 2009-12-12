-module(all).
-export([test/0]).

-define(TESTS_TO_RUN,
	[
		main,
		input_parser,
		db
	]).
title(T) ->
	io:format(user, "Running ~w\n", [T]).

test() ->
	lists:foreach(fun(T) ->
				title(T),
				T:test()
		end,
		?TESTS_TO_RUN).
