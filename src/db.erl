-module(db).
-include_lib("eunit/include/eunit.hrl").
-export([init/0, create_ram_table/2, create_disc_table/2, insert/1, select/1]).

init() ->
	mnesia:start().

create_table(Name, Fields, Type) ->
	mnesia:create_table(Name, [{attributes, Fields}, {Type, [node()]}]).

create_ram_table(Name, Fields) ->
	create_table(Name, Fields, ram_copies).

create_disc_table(Name, Fields) ->
	create_table(Name, Fields, disc_copies).

insert(R) ->
	F = fun() -> mnesia:write(R) end,
	mnesia:transaction(F).

select(Q) ->
	F = fun() ->
			qlc:e(Q)
	end,
	{atomic, Val} = mnesia:transaction(F),
	Val.
