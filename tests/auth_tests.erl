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

select_user_test_() ->
	{"Finds a user record after it being inserted into the database",
		setup, fun setup/0, fun teardown/1,
		fun() ->
				auth:init(),
				L = "blake",
				P = "test",
				E = "bob@bob.com",
				ExpectedUser = #users{login=L, password=crypto:sha(P), email=E},
				auth:add_user(L, P, E),
				U = auth:find_user("blake"),
				?assertEqual(ExpectedUser, U)	
		end
	}.

check_credentials_test_() ->
	{"Checks if a users' password matches the password in the database",
		setup, fun setup/0, fun teardown/1,
		fun() ->
				auth:init(),
				L = "blake",
				P = "test",
				E = "bob@bob.com",
				auth:add_user(L, P, E),
				?assert(auth:check_credentials(L, P)),
				?assert(not auth:check_credentials(L, "bloop"))
		end
	}.
