-module(auth_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/users.hrl").

setup() ->
	db:init().

teardown(_Value) ->
	mnesia:delete_table(users).

init_test_() ->
	{"Creates a disc auth table if it doesn't exist",
		setup, fun setup/0, fun teardown/1,
		fun() ->
				% First time, it shouldn't exist
				?assertEqual({atomic, ok}, auth:init()),
				% Second time, it should exist
				?assertEqual({aborted, {already_exists,users}}, auth:init())
		end
	}.

add_user_test_() ->
	{"Inserts a new user into the users table",
		setup, fun setup/0, fun teardown/1,
		fun() ->
				crypto:start(),
				auth:init(),
				L = "blake",
				P = "test",
				E = "bob@bob.com",
				ExpectedUser = #users{login=L, password=crypto:sha(P), email=E},
				?assertEqual(ExpectedUser, auth:add_user(L, P, E))
		end
	}.
