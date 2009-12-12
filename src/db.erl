-module(db).
-include_lib("eunit/include/eunit.hrl").
-export([init/0, create_ram_table/2, insert/1, select/1]).

init() ->
	mnesia:start().

create_ram_table(Name, Fields) ->
	mnesia:create_table(Name, [{attributes, Fields}, {ram_copies, [node()]}]).

insert(R) ->
	F = fun() -> mnesia:write(R) end,
	mnesia:transaction(F).

select(Q) ->
	F = fun() ->
			qlc:e(Q)
	end,
	{atomic, Val} = mnesia:transaction(F),
	Val.
