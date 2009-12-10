-module(db).
-export([do_query/1, init/0, create_ram_table/2, write_record/1]).

init() ->
	mnesia:start().

create_ram_table(Name, Fields) ->
	mnesia:create_table(Name, [{attributes, Fields}, {ram_copies, [node()]}]).

write_record(R) ->
	F = fun() -> mnesia:write(R) end,
	mnesia:transaction(F).

do_query(Q) ->
	F = fun() ->
			qlc:e(Q)
	end,
	{atomic, Val} = mnesia:transaction(F),
	Val.
