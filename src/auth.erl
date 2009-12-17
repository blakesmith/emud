-module(auth).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("users.hrl").
-export([init/0, add_user/3, find_user/1]).

init() ->
	crypto:start(),
	db:create_disc_table(users, [login, password, email]).

add_user(Login, Password, Email) ->
	P1 = crypto:sha(Password),
	U = #users{login=Login, password=P1, email=Email},
	case db:insert(U) of
		{atomic, ok} ->
			U;	
		{aborted, Reason} ->
			{error, Reason}
	end.

find_user(Login) ->
	Results = db:select(qlc:q([X || X <- mnesia:table(users), X#users.login =:= Login])),
	lists:nth(1, Results).

